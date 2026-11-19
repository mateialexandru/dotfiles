use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    time::{SystemTime, UNIX_EPOCH},
};

struct Fixture {
    root: PathBuf,
    repo: PathBuf,
}

fn checked(output: Output) -> String {
    assert!(
        output.status.success(),
        "command failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).trim().into()
}

fn git(path: &Path, args: &[&str]) -> String {
    checked(
        Command::new("git")
            .arg("-C")
            .arg(path)
            .args(args)
            .output()
            .unwrap(),
    )
}

impl Fixture {
    fn new() -> Self {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!("hack-test-{}-{nonce}", std::process::id()));
        let repo = root.join("sources").join("product");
        let remote = root.join("remote.git");
        fs::create_dir_all(&repo).unwrap();
        git(
            &root,
            &[
                "init",
                "--bare",
                "-q",
                "--initial-branch=main",
                remote.to_str().unwrap(),
            ],
        );
        git(&repo, &["init", "-q", "-b", "main"]);
        git(&repo, &["config", "user.name", "Hack test"]);
        git(
            &repo,
            &["config", "user.email", "hack-test@example.invalid"],
        );
        git(&repo, &["config", "commit.gpgsign", "false"]);
        git(
            &repo,
            &[
                "config",
                "core.hooksPath",
                root.join("hooks").to_str().unwrap(),
            ],
        );
        git(
            &repo,
            &["remote", "add", "origin", remote.to_str().unwrap()],
        );
        let fixture = Self { root, repo };
        fixture.commit("initial");
        git(&fixture.repo, &["push", "-q", "-u", "origin", "main"]);
        fixture
    }

    fn commit(&self, text: &str) -> String {
        fs::write(self.repo.join("file.txt"), text).unwrap();
        git(&self.repo, &["add", "file.txt"]);
        git(&self.repo, &["commit", "-q", "-m", text]);
        git(&self.repo, &["rev-parse", "HEAD"])
    }

    fn spawn(&self, task: &str) -> PathBuf {
        checked(
            Command::new(env!("CARGO_BIN_EXE_hack"))
                .arg(format!("product/{task}"))
                .env("HACK_SOURCE_ROOTS", self.root.join("sources"))
                .env("HACK_WORKTREE_ROOT", self.root.join("worktrees"))
                .env("HACK_CACHE", self.root.join("repos-cache"))
                .env("HACK_BRANCH_PREFIX", "user/test")
                .env("HACK_BASE_BRANCH", "main")
                .env("GIT_TERMINAL_PROMPT", "0")
                .output()
                .unwrap(),
        );
        self.root.join("worktrees").join("product").join(task)
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        if let Err(error) = fs::remove_dir_all(&self.root) {
            eprintln!(
                "could not remove test fixture {}: {error}",
                self.root.display()
            );
        }
    }
}

#[test]
fn resuming_a_local_branch_preserves_unpushed_commits() {
    let fixture = Fixture::new();
    git(&fixture.repo, &["checkout", "-q", "-b", "user/test/task"]);
    git(&fixture.repo, &["push", "-q", "origin", "user/test/task"]);
    let local_head = fixture.commit("local progress");
    git(&fixture.repo, &["checkout", "-q", "main"]);

    let worktree = fixture.spawn("task");

    assert_eq!(git(&worktree, &["rev-parse", "HEAD"]), local_head);
    assert_eq!(
        git(&fixture.repo, &["rev-parse", "user/test/task"]),
        local_head
    );
}

#[test]
fn a_remote_only_task_is_resumed_as_a_tracking_branch() {
    let fixture = Fixture::new();
    git(&fixture.repo, &["checkout", "-q", "-b", "user/test/task"]);
    let remote_head = fixture.commit("remote task");
    git(&fixture.repo, &["push", "-q", "origin", "user/test/task"]);
    git(&fixture.repo, &["checkout", "-q", "main"]);
    git(&fixture.repo, &["branch", "-D", "user/test/task"]);

    let worktree = fixture.spawn("task");

    assert_eq!(git(&worktree, &["rev-parse", "HEAD"]), remote_head);
    assert_eq!(
        git(&worktree, &["rev-parse", "--abbrev-ref", "@{upstream}"]),
        "origin/user/test/task"
    );
}

#[test]
fn new_tasks_start_at_the_remote_base_not_the_local_base() {
    let fixture = Fixture::new();
    let remote_head = git(&fixture.repo, &["rev-parse", "origin/main"]);
    fixture.commit("local base progress");

    let worktree = fixture.spawn("new-task");

    assert_eq!(git(&worktree, &["rev-parse", "HEAD"]), remote_head);
}
