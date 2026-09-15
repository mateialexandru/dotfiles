use std::{
    env, fs,
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
};

type Result<T> = std::result::Result<T, String>;

#[derive(Debug)]
struct Repo {
    alias: String,
    path: PathBuf,
}

fn home() -> Result<PathBuf> {
    env::var_os("HOME")
        .or_else(|| env::var_os("USERPROFILE"))
        .map(PathBuf::from)
        .ok_or_else(|| "HOME/USERPROFILE is not set".into())
}

fn source_roots() -> Result<Vec<PathBuf>> {
    if let Some(value) = env::var_os("HACK_SOURCE_ROOTS") {
        return Ok(env::split_paths(&value).collect());
    }

    let user_home = home()?;
    #[allow(unused_mut)]
    let mut roots = vec![user_home.join("Source")];
    #[cfg(windows)]
    roots.push(PathBuf::from(r"C:\avd"));
    Ok(roots.into_iter().filter(|path| path.is_dir()).collect())
}

fn worktree_root() -> Result<PathBuf> {
    Ok(env::var_os("HACK_WORKTREE_ROOT")
        .map(PathBuf::from)
        .unwrap_or(home()?.join("worktree")))
}

fn cache_path() -> Result<PathBuf> {
    if let Some(path) = env::var_os("HACK_CACHE") {
        return Ok(PathBuf::from(path));
    }
    if let Some(directory) = env::var_os("XDG_CACHE_HOME") {
        return Ok(PathBuf::from(directory).join("hack/repos"));
    }
    Ok(home()?.join(".cache/hack/repos"))
}

fn git<I, S>(dir: &Path, args: I) -> Result<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<std::ffi::OsStr>,
{
    let output = Command::new("git")
        .arg("-C")
        .arg(dir)
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
    S: AsRef<std::ffi::OsStr>,
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

fn git_config(repo: &Path, key: &str) -> Option<String> {
    git(repo, ["config", "--get", key])
        .ok()
        .filter(|value| !value.is_empty())
}

fn has_git_marker(path: &Path) -> bool {
    path.join(".git").exists()
}

fn discover_in(directory: &Path, depth: usize, repos: &mut Vec<Repo>) {
    if depth == 0 {
        return;
    }
    let Ok(entries) = fs::read_dir(directory) else {
        return;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let name = entry.file_name().to_string_lossy().to_string();
        if name.starts_with('.') || matches!(name.as_str(), "node_modules" | "target") {
            continue;
        }
        if has_git_marker(&path) {
            repos.push(Repo { alias: name, path });
        } else {
            discover_in(&path, depth - 1, repos);
        }
    }
}

fn refresh_index() -> Result<Vec<Repo>> {
    let mut repos = Vec::new();
    for root in source_roots()? {
        discover_in(&root, 3, &mut repos);
    }
    repos.sort_by(|left, right| left.alias.cmp(&right.alias));
    let path = cache_path()?;
    fs::create_dir_all(path.parent().unwrap()).map_err(|e| e.to_string())?;
    let contents = repos
        .iter()
        .map(|repo| format!("{}\t{}", repo.alias, repo.path.display()))
        .collect::<Vec<_>>()
        .join("\n");
    fs::write(&path, format!("{contents}\n"))
        .map_err(|e| format!("cannot write {}: {e}", path.display()))?;
    Ok(repos)
}

fn load_index() -> Result<Vec<Repo>> {
    let path = cache_path()?;
    if !path.exists() {
        return refresh_index();
    }
    let contents =
        fs::read_to_string(&path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    Ok(contents
        .lines()
        .filter_map(|line| line.split_once('\t'))
        .map(|(alias, path)| Repo {
            alias: alias.into(),
            path: PathBuf::from(path),
        })
        .filter(|repo| has_git_marker(&repo.path))
        .collect())
}

fn matching(repos: Vec<Repo>, alias: &str) -> Vec<Repo> {
    repos
        .into_iter()
        .filter(|repo| repo.alias.eq_ignore_ascii_case(alias))
        .collect()
}

fn resolve(alias: &str) -> Result<Repo> {
    let had_index = cache_path()?.exists();
    let mut matches = matching(load_index()?, alias);
    if matches.is_empty() && had_index {
        matches = matching(refresh_index()?, alias);
    }
    match matches.as_slice() {
        [] => Err(format!(
            "unknown repository '{alias}'; clone it beneath a source root or set HACK_SOURCE_ROOTS"
        )),
        [repo] => Ok(Repo {
            alias: repo.alias.clone(),
            path: repo.path.clone(),
        }),
        _ => Err(format!(
            "repository name '{alias}' is ambiguous across the configured source roots"
        )),
    }
}

fn remote_ref_exists(repo: &Path, branch: &str) -> bool {
    git_ok(
        repo,
        [
            "show-ref",
            "--verify",
            "--quiet",
            &format!("refs/remotes/origin/{branch}"),
        ],
    )
}

fn base_branch(repo: &Path) -> Result<String> {
    if let Some(branch) =
        git_config(repo, "hack.baseBranch").or_else(|| env::var("HACK_BASE_BRANCH").ok())
    {
        if remote_ref_exists(repo, &branch) {
            return Ok(branch);
        }
        return Err(format!("configured base origin/{branch} does not exist"));
    }

    if remote_ref_exists(repo, "develop") {
        return Ok("develop".into());
    }

    let _ = git(repo, ["remote", "set-head", "origin", "--auto"]);
    if let Ok(head) = git(
        repo,
        ["symbolic-ref", "--short", "refs/remotes/origin/HEAD"],
    ) {
        if let Some(branch) = head.strip_prefix("origin/") {
            return Ok(branch.into());
        }
    }
    for candidate in ["main", "master"] {
        if remote_ref_exists(repo, candidate) {
            return Ok(candidate.into());
        }
    }
    Err("could not determine the remote base branch; set git config hack.baseBranch BRANCH".into())
}

fn branch_prefix(repo: &Path) -> String {
    if let Some(prefix) =
        git_config(repo, "hack.branchPrefix").or_else(|| env::var("HACK_BRANCH_PREFIX").ok())
    {
        return prefix.trim_matches('/').into();
    }
    let user = env::var("USER")
        .or_else(|_| env::var("USERNAME"))
        .unwrap_or_else(|_| "me".into());
    format!("user/{user}")
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

fn fetch(repo: &Path) -> Result<()> {
    println!("Fetching origin...");
    git(repo, ["fetch", "--prune", "origin"]).map(|_| ())
}

fn spawn(spec: &str) -> Result<()> {
    let (requested_alias, task) = split_spec(spec)?;
    let repo = resolve(requested_alias)?;
    fetch(&repo.path)?;

    let base = base_branch(&repo.path)?;
    let prefix = branch_prefix(&repo.path);
    let branch = if prefix.is_empty() {
        task.into()
    } else {
        format!("{prefix}/{task}")
    };
    let destination = worktree_root()?.join(&repo.alias).join(task);
    if destination.exists() {
        println!("{}", destination.display());
        return Ok(());
    }
    fs::create_dir_all(destination.parent().unwrap()).map_err(|e| e.to_string())?;

    let local_ref = format!("refs/heads/{branch}");
    let remote_ref = format!("refs/remotes/origin/{branch}");
    if git_ok(&repo.path, ["show-ref", "--verify", "--quiet", &local_ref]) {
        if git_ok(&repo.path, ["show-ref", "--verify", "--quiet", &remote_ref]) {
            git(
                &repo.path,
                ["branch", "--force", &branch, &format!("origin/{branch}")],
            )?;
        }
        git(
            &repo.path,
            [
                "worktree",
                "add",
                destination.to_string_lossy().as_ref(),
                &branch,
            ],
        )?;
    } else if git_ok(&repo.path, ["show-ref", "--verify", "--quiet", &remote_ref]) {
        git(
            &repo.path,
            [
                "worktree",
                "add",
                "--track",
                "-b",
                &branch,
                destination.to_string_lossy().as_ref(),
                &format!("origin/{branch}"),
            ],
        )?;
    } else {
        git(
            &repo.path,
            [
                "worktree",
                "add",
                "-b",
                &branch,
                destination.to_string_lossy().as_ref(),
                &format!("origin/{base}"),
            ],
        )?;
    }
    println!("{}", destination.display());
    Ok(())
}

fn list_repos(refresh: bool) -> Result<()> {
    let repos = if refresh {
        refresh_index()?
    } else {
        load_index()?
    };
    if repos.is_empty() {
        println!("No repositories found in the configured source roots.");
    }
    for repo in repos {
        println!("{}\t{}", repo.alias, repo.path.display());
    }
    Ok(())
}

fn managed_path(path: &str, root: &Path, alias: &str) -> bool {
    let expected = root.join(alias);
    match (fs::canonicalize(path), fs::canonicalize(expected)) {
        (Ok(candidate), Ok(expected)) => candidate.starts_with(expected),
        _ => false,
    }
}

fn list_worktrees() -> Result<()> {
    let root = worktree_root()?;
    let mut found = false;
    for repo in load_index()? {
        let records = git(&repo.path, ["worktree", "list", "--porcelain"])?;
        for record in records.split("\n\n") {
            let path = record
                .lines()
                .find_map(|line| line.strip_prefix("worktree "));
            let branch = record
                .lines()
                .find_map(|line| line.strip_prefix("branch refs/heads/"));
            if let (Some(path), Some(branch)) = (path, branch) {
                if managed_path(path, &root, &repo.alias) {
                    println!("{}\t{}\t{}", repo.alias, branch, path);
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

fn remove(spec: &str) -> Result<()> {
    let (requested_alias, task) = split_spec(spec)?;
    let repo = resolve(requested_alias)?;
    let destination = worktree_root()?.join(&repo.alias).join(task);
    if !destination.exists() {
        return Err(format!("worktree '{spec}' does not exist"));
    }
    if !git(&destination, ["status", "--porcelain"])?.is_empty() {
        return Err(format!(
            "'{spec}' has uncommitted changes; refusing to remove it"
        ));
    }

    fetch(&repo.path)?;
    let branch = git(&destination, ["branch", "--show-current"])?;
    let base = format!("origin/{}", base_branch(&repo.path)?);
    if !git_ok(&repo.path, ["merge-base", "--is-ancestor", &branch, &base]) {
        return Err(format!(
            "'{branch}' is not merged into {base}; refusing to remove it"
        ));
    }
    git(
        &repo.path,
        ["worktree", "remove", destination.to_string_lossy().as_ref()],
    )?;
    git(&repo.path, ["branch", "-d", &branch])?;
    println!("Removed {spec}");
    Ok(())
}

fn usage() {
    println!(
        "hack — discovered Git worktrees\n\n\
         Usage:\n  hack REPO/TASK\n  hack repos [--refresh]\n  hack list\n  hack remove REPO/TASK\n\n\
         Repositories are indexed beneath ~/Source or HACK_SOURCE_ROOTS.\n\
         Every spawn fetches and starts at origin/develop when it exists,\n\
         otherwise at the remote default branch."
    );
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    match args.as_slice() {
        [] => {
            usage();
            Ok(())
        }
        [arg] if matches!(arg.as_str(), "--help" | "-h" | "help") => {
            usage();
            Ok(())
        }
        [command] if command == "repos" => list_repos(false),
        [command, flag] if command == "repos" && flag == "--refresh" => list_repos(true),
        [command] if command == "list" => list_worktrees(),
        [command, spec] if command == "remove" => remove(spec),
        [spec] if spec.contains('/') => spawn(spec),
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
