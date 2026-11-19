use std::{
    env, fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
};

static NEXT_ID: AtomicU64 = AtomicU64::new(0);

struct Fixture(PathBuf);

impl Fixture {
    fn new() -> Self {
        let directory = env::temp_dir().join(format!(
            "sys providers-{}-{}",
            std::process::id(),
            NEXT_ID.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&directory).unwrap();
        Self(directory)
    }

    fn provider(&self, name: &str, code: u8) {
        let directory = self.0.join("config/providers.d").join(name);
        fs::create_dir_all(&directory).unwrap();
        for action in ["check", "status"] {
            #[cfg(windows)]
            let (extension, body) = (
                "ps1",
                format!("Write-Output '{name} {action}'; exit {code}"),
            );
            #[cfg(not(windows))]
            let (extension, body) = (
                "sh",
                format!("printf '%s\\n' '{name} {action}'; exit {code}"),
            );
            fs::write(directory.join(format!("{action}.{extension}")), body).unwrap();
        }
    }

    fn command(&self, args: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_sys"));
        command
            .args(args)
            .env("SYS_CONFIG_DIR", self.0.join("config"))
            // Provider-only operations must not need a dotfiles checkout.
            .env("SYS_DOTFILES_DIR", self.0.join("missing-checkout"))
            .env("SYS_PROVIDER_TIMEOUT_SECONDS", "15");
        command
    }

    fn run(&self, args: &[&str]) -> Output {
        self.command(args).output().unwrap()
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.0).unwrap();
    }
}

fn text(output: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn selected_provider_needs_no_checkout_and_preserves_spaces() {
    let fixture = Fixture::new();
    fixture.provider("work", 0);
    let output = fixture.run(&["status", "work"]);
    assert!(output.status.success(), "{}", text(&output));
    assert!(text(&output).contains("work status"));
    let output = fixture.run(&["check", "work"]);
    assert!(output.status.success(), "{}", text(&output));
}

#[test]
fn all_providers_run_in_order_even_after_failure() {
    let fixture = Fixture::new();
    fixture.provider("z-success", 0);
    fixture.provider("a-failure", 7);
    let output = fixture.run(&["status"]);
    assert_eq!(output.status.code(), Some(7), "{}", text(&output));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.find("a-failure status").unwrap() < stdout.find("z-success status").unwrap());
}

#[test]
fn missing_invalid_and_extra_arguments_fail_without_dispatch() {
    let fixture = Fixture::new();
    fixture.provider("work", 0);
    for args in [
        vec!["check", "missing"],
        vec!["status", "../work"],
        vec!["status", "work", "extra"],
    ] {
        let output = fixture.run(&args);
        assert!(!output.status.success());
        assert!(!String::from_utf8_lossy(&output.stdout).contains("work status"));
    }
}

#[test]
fn no_providers_is_explicit_and_does_not_create_config() {
    let fixture = Fixture::new();
    let output = fixture.run(&["status"]);
    assert!(output.status.success(), "{}", text(&output));
    assert!(text(&output).contains("no status providers registered"));
    assert!(!fixture.0.join("config").exists());
}

#[test]
fn baseline_failure_does_not_hide_provider_results() {
    let fixture = Fixture::new();
    fixture.provider("work", 0);
    let output = fixture.run(&["check"]);
    assert!(!output.status.success());
    assert!(text(&output).contains("baseline"));
    assert!(text(&output).contains("work check"));
}

#[test]
fn missing_entry_point_is_not_healthy() {
    let fixture = Fixture::new();
    fs::create_dir_all(fixture.0.join("config/providers.d/work")).unwrap();
    let output = fixture.run(&["status", "work"]);
    assert!(!output.status.success());
    assert!(text(&output).contains("entry point is missing"));
}

#[test]
fn a_hung_provider_is_stopped_and_later_providers_still_run() {
    let fixture = Fixture::new();
    fixture.provider("a-hung", 0);
    fixture.provider("z-success", 0);
    #[cfg(windows)]
    let (file, body) = ("status.ps1", "Start-Sleep -Seconds 60");
    #[cfg(not(windows))]
    let (file, body) = ("status.sh", "while :; do :; done");
    fs::write(
        fixture
            .0
            .join(Path::new("config/providers.d/a-hung"))
            .join(file),
        body,
    )
    .unwrap();
    let output = fixture
        .command(&["status"])
        .env("SYS_PROVIDER_TIMEOUT_SECONDS", "1")
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(text(&output).contains("timed out"));
    assert!(text(&output).contains("z-success status"));
}
