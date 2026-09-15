use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    env,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
};

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Config {
    base_dir: PathBuf,
    #[serde(default = "default_prefix")]
    branch_prefix: String,
    #[serde(default = "default_base")]
    default_base_branch: String,
    #[serde(default)]
    default_repo: Option<String>,
    #[serde(default)]
    repos: BTreeMap<String, Repo>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Repo {
    url: String,
    #[serde(default)]
    base_branch: Option<String>,
}

fn default_base() -> String {
    "develop".into()
}

fn default_prefix() -> String {
    let user = env::var("USER")
        .or_else(|_| env::var("USERNAME"))
        .unwrap_or_else(|_| "me".into());
    format!("user/{user}")
}

fn home() -> Result<PathBuf> {
    env::var_os("HOME")
        .or_else(|| env::var_os("USERPROFILE"))
        .map(PathBuf::from)
        .ok_or_else(|| "HOME/USERPROFILE is not set".into())
}

fn hack_home() -> Result<PathBuf> {
    env::var_os("HACK_HOME")
        .map(PathBuf::from)
        .map(Ok)
        .unwrap_or_else(home)
}

fn config_path() -> Result<PathBuf> {
    Ok(hack_home()?.join(".config/hack/config.json"))
}

impl Config {
    fn load() -> Result<Self> {
        let path = config_path()?;
        if !path.exists() {
            return Ok(Self {
                base_dir: hack_home()?.join("worktree"),
                branch_prefix: default_prefix(),
                default_base_branch: default_base(),
                default_repo: None,
                repos: BTreeMap::new(),
            });
        }
        let text = fs::read_to_string(&path)
            .map_err(|e| format!("cannot read {}: {e}", path.display()))?;
        serde_json::from_str(&text).map_err(|e| format!("cannot parse {}: {e}", path.display()))
    }

    fn save(&self) -> Result<()> {
        let path = config_path()?;
        fs::create_dir_all(path.parent().unwrap()).map_err(|e| e.to_string())?;
        let text = serde_json::to_string_pretty(self).map_err(|e| e.to_string())?;
        fs::write(&path, format!("{text}\n"))
            .map_err(|e| format!("cannot write {}: {e}", path.display()))
    }

    fn repo(&self, alias: &str) -> Result<&Repo> {
        self.repos.get(alias).ok_or_else(|| {
            format!("unknown repository '{alias}'; add it with: hack repo add {alias} URL")
        })
    }

    fn base_for(&self, repo: &Repo) -> String {
        repo.base_branch
            .clone()
            .unwrap_or_else(|| self.default_base_branch.clone())
    }

    fn store(&self, alias: &str) -> PathBuf {
        self.base_dir.join(".trees").join(alias)
    }

    fn worktree(&self, alias: &str, task: &str) -> PathBuf {
        self.base_dir.join(alias).join(task)
    }
}

fn git<I, S>(dir: Option<&Path>, args: I) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut command = Command::new("git");
    if let Some(dir) = dir {
        command.arg("-C").arg(dir);
    }
    let output = command
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("could not run git: {e}"))?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).trim().into())
    } else {
        let message = String::from_utf8_lossy(&output.stderr).trim().to_string();
        Err(if message.is_empty() {
            format!("git exited with {}", output.status)
        } else {
            message
        })
    }
}

fn git_ok<I, S>(dir: &Path, args: I) -> bool
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(args)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

fn ensure_store(config: &Config, alias: &str) -> Result<PathBuf> {
    let repo = config.repo(alias)?;
    let store = config.store(alias);
    if !store.exists() {
        fs::create_dir_all(store.parent().unwrap()).map_err(|e| e.to_string())?;
        println!("Cloning {alias} metadata...");
        git(
            None,
            [
                OsStr::new("clone"),
                OsStr::new("--bare"),
                OsStr::new(&repo.url),
                store.as_os_str(),
            ],
        )?;
        git(
            Some(&store),
            [
                "config",
                "remote.origin.fetch",
                "+refs/heads/*:refs/remotes/origin/*",
            ],
        )?;
    }
    git(Some(&store), ["fetch", "--prune", "origin"])?;
    Ok(store)
}

fn split_spec(spec: &str) -> Result<(&str, &str)> {
    let (alias, task) = spec
        .split_once('/')
        .ok_or_else(|| "expected REPO/TASK".to_string())?;
    if alias.is_empty()
        || task.is_empty()
        || task.starts_with('/')
        || task.ends_with('/')
        || task.contains("..")
        || task.contains('\\')
    {
        return Err("repository and task must be safe, non-empty path names".into());
    }
    Ok((alias, task))
}

fn spawn(config: &Config, spec: &str) -> Result<()> {
    let (alias, task) = split_spec(spec)?;
    let repo = config.repo(alias)?;
    let base = config.base_for(repo);
    let store = ensure_store(config, alias)?;
    let path = config.worktree(alias, task);
    let branch = if config.branch_prefix.is_empty() {
        task.to_string()
    } else {
        format!("{}/{task}", config.branch_prefix.trim_end_matches('/'))
    };

    if path.exists() {
        println!("{}", path.display());
        return Ok(());
    }
    fs::create_dir_all(path.parent().unwrap()).map_err(|e| e.to_string())?;

    let remote_branch = format!("refs/remotes/origin/{branch}");
    let local_branch = format!("refs/heads/{branch}");
    let base_ref = format!("refs/remotes/origin/{base}");
    if !git_ok(&store, ["show-ref", "--verify", "--quiet", &base_ref]) {
        return Err(format!("origin/{base} does not exist for '{alias}'"));
    }

    if git_ok(&store, ["show-ref", "--verify", "--quiet", &local_branch]) {
        if git_ok(&store, ["show-ref", "--verify", "--quiet", &remote_branch]) {
            git(
                Some(&store),
                ["branch", "--force", &branch, &format!("origin/{branch}")],
            )?;
        }
        git(
            Some(&store),
            [
                OsStr::new("worktree"),
                OsStr::new("add"),
                path.as_os_str(),
                OsStr::new(&branch),
            ],
        )?;
    } else if git_ok(&store, ["show-ref", "--verify", "--quiet", &remote_branch]) {
        git(
            Some(&store),
            [
                OsStr::new("worktree"),
                OsStr::new("add"),
                OsStr::new("--track"),
                OsStr::new("-b"),
                OsStr::new(&branch),
                path.as_os_str(),
                OsStr::new(&format!("origin/{branch}")),
            ],
        )?;
    } else {
        git(
            Some(&store),
            [
                OsStr::new("worktree"),
                OsStr::new("add"),
                OsStr::new("-b"),
                OsStr::new(&branch),
                path.as_os_str(),
                OsStr::new(&format!("origin/{base}")),
            ],
        )?;
    }
    println!("{}", path.display());
    Ok(())
}

fn repo_command(config: &mut Config, args: &[String]) -> Result<()> {
    match args {
        [command] if command == "list" => {
            if config.repos.is_empty() {
                println!("No repositories. Add one with: hack repo add NAME URL [BASE]");
            }
            for (alias, repo) in &config.repos {
                println!("{alias}\t{}\t{}", config.base_for(repo), repo.url);
            }
            Ok(())
        }
        [command, alias, url] if command == "add" => add_repo(config, alias, url, None),
        [command, alias, url, base] if command == "add" => {
            add_repo(config, alias, url, Some(base.clone()))
        }
        [command, alias] if command == "remove" => {
            config.repo(alias)?;
            let store = config.store(alias);
            if store.exists() {
                let records = git(Some(&store), ["worktree", "list", "--porcelain"])?;
                if records
                    .split("\n\n")
                    .any(|record| record.lines().any(|line| line.starts_with("branch ")))
                {
                    return Err(format!(
                        "'{alias}' still has worktrees; remove them before forgetting it"
                    ));
                }
                fs::remove_dir_all(&store)
                    .map_err(|e| format!("cannot remove {}: {e}", store.display()))?;
            }
            config
                .repos
                .remove(alias)
                .ok_or_else(|| format!("unknown repository '{alias}'"))?;
            config.save()
        }
        _ => Err(
            "usage: hack repo add NAME URL [BASE] | hack repo list | hack repo remove NAME".into(),
        ),
    }
}

fn add_repo(config: &mut Config, alias: &str, url: &str, base: Option<String>) -> Result<()> {
    if alias.is_empty() || alias.contains(['/', '\\']) || alias == ".trees" {
        return Err("repository name must be a single safe path component".into());
    }
    config.repos.insert(
        alias.into(),
        Repo {
            url: url.into(),
            base_branch: base,
        },
    );
    config.save()?;
    println!("Added {alias}");
    Ok(())
}

fn list(config: &Config) -> Result<()> {
    let mut found = false;
    for alias in config.repos.keys() {
        let store = config.store(alias);
        if !store.exists() {
            continue;
        }
        let output = git(Some(&store), ["worktree", "list", "--porcelain"])?;
        let worktree_root = fs::canonicalize(config.base_dir.join(alias)).ok();
        for block in output.split("\n\n") {
            let path = block
                .lines()
                .find_map(|line| line.strip_prefix("worktree "));
            let branch = block
                .lines()
                .find_map(|line| line.strip_prefix("branch refs/heads/"));
            if let (Some(path), Some(branch)) = (path, branch) {
                let is_managed = worktree_root.as_ref().is_some_and(|root| {
                    fs::canonicalize(path).is_ok_and(|candidate| candidate.starts_with(root))
                });
                if is_managed {
                    println!("{alias}\t{branch}\t{path}");
                    found = true;
                }
            }
        }
    }
    if !found {
        println!("No worktrees.");
    }
    Ok(())
}

fn remove(config: &Config, spec: &str) -> Result<()> {
    let (alias, task) = split_spec(spec)?;
    let path = config.worktree(alias, task);
    if !path.exists() {
        return Err(format!("worktree '{spec}' does not exist"));
    }
    let store = ensure_store(config, alias)?;
    if !git(Some(&path), ["status", "--porcelain"])?.is_empty() {
        return Err(format!(
            "'{spec}' has uncommitted changes; refusing to remove it"
        ));
    }
    let branch = git(Some(&path), ["branch", "--show-current"])?;
    let repo = config.repo(alias)?;
    let base = format!("origin/{}", config.base_for(repo));
    if !git_ok(&store, ["merge-base", "--is-ancestor", &branch, &base]) {
        return Err(format!(
            "'{branch}' is not merged into {base}; refusing to remove it"
        ));
    }
    git(
        Some(&store),
        [
            OsStr::new("worktree"),
            OsStr::new("remove"),
            path.as_os_str(),
        ],
    )?;
    git(Some(&store), ["branch", "-d", &branch])?;
    println!("Removed {spec}");
    Ok(())
}

fn usage() {
    println!(
        "hack — catalog-driven Git worktrees\n\n\
         Usage:\n  hack REPO/TASK\n  hack repo add NAME URL [BASE]\n  hack repo list\n  hack repo remove NAME\n  hack list\n  hack remove REPO/TASK\n\n\
         A new task always starts at the freshly fetched origin/BASE."
    );
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut config = Config::load()?;
    match args.as_slice() {
        [] => {
            usage();
            Ok(())
        }
        [arg] if arg == "--help" || arg == "-h" || arg == "help" => {
            usage();
            Ok(())
        }
        [command, rest @ ..] if command == "repo" => repo_command(&mut config, rest),
        [command] if command == "list" => list(&config),
        [command, spec] if command == "remove" => remove(&config, spec),
        [spec] if spec.contains('/') => spawn(&config, spec),
        _ => Err("invalid arguments; run 'hack --help'".into()),
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("hack: {message}");
            ExitCode::FAILURE
        }
    }
}
