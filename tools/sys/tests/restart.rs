#![cfg(windows)]

use std::{env, fs, process::Command};

#[test]
fn restart_dispatches_countdown_and_rejects_invalid_arguments() {
    let root = env::temp_dir().join(format!("sys restart fixture-{}", std::process::id()));
    fs::create_dir_all(root.join("config/doom")).unwrap();
    fs::create_dir_all(root.join("scripts")).unwrap();
    fs::write(root.join("config/doom/init.el"), "").unwrap();
    fs::write(root.join("install.sh"), "").unwrap();
    fs::write(
        root.join("scripts/restart-emacs-windows.ps1"),
        "param([int]$Seconds)\nWrite-Output \"restart:$Seconds\"\nexit 0",
    )
    .unwrap();
    for (arguments, expected) in [
        (vec!["restart"], Some("restart:10")),
        (vec!["restart", "0"], Some("restart:0")),
        (vec!["restart", "30"], Some("restart:30")),
        (vec!["restart", "-1"], None),
        (vec!["restart", "oops"], None),
        (vec!["restart", "1", "extra"], None),
    ] {
        let output = Command::new(env!("CARGO_BIN_EXE_sys"))
            .args(arguments)
            .env("SYS_DOTFILES_DIR", &root)
            .output()
            .unwrap();
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        if let Some(expected) = expected {
            assert!(output.status.success() && text.contains(expected), "{text}");
        } else {
            assert!(!output.status.success(), "{text}");
            assert!(text.contains("non-negative countdown"), "{text}");
            assert!(!text.contains("restart:"), "{text}");
        }
    }
    fs::remove_dir_all(root).unwrap();
}
