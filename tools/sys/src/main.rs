use std::{
    env,
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
};

type Result<T> = std::result::Result<T, String>;

fn main() -> ExitCode {
    match run() {
        Ok(code) => ExitCode::from(code),
        Err(message) => {
            eprintln!("sys: {message}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<u8> {
    let mut args = env::args_os().skip(1);
    let Some(command) = args.next() else {
        print_help();
        return Ok(0);
    };
    let rest: Vec<OsString> = args.collect();

    match command.to_string_lossy().as_ref() {
        "help" | "--help" | "-h" => {
            print_help();
            Ok(0)
        }
        "version" | "--version" | "-V" => {
            println!("sys {}", env!("CARGO_PKG_VERSION"));
            Ok(0)
        }
        "install" => platform_script("install", &rest),
        "update" => {
            require_no_args("update", &rest)?;
            platform_script("update", &rest)
        }
        "check" => {
            require_no_args("check", &rest)?;
            platform_script("check", &rest)
        }
        "doom" => doom_command(&rest),
        "restart" => restart(&rest),
        "vanilla" => vanilla(&rest),
        "llm" => llm(&rest),
        other => Err(format!("unknown command '{other}' (run `sys help`)")),
    }
}

fn require_no_args(command: &str, args: &[OsString]) -> Result<()> {
    if args.is_empty() {
        Ok(())
    } else {
        Err(format!("{command} takes no arguments"))
    }
}

fn print_help() {
    println!(
        "sys — operate this development environment\n\n\
Usage: sys <command> [arguments]\n\n\
Commands:\n  \
  install           install or repair the environment\n  \
  update            upgrade packages, tools, and Doom\n  \
  check             validate the complete environment\n  \
  doom <action>     run Doom-specific operations (sync, doctor)\n  \
  restart [seconds] gracefully restart the macOS Emacs daemon\n  \
  vanilla           launch Emacs without configuration\n  \
  llm <action>      manage the macOS Ollama runtime\n  \
  help              show this help"
    );
}

fn home() -> Result<PathBuf> {
    env::var_os("HOME")
        .or_else(|| env::var_os("USERPROFILE"))
        .map(PathBuf::from)
        .ok_or_else(|| "HOME/USERPROFILE is not set".into())
}

fn is_repo(path: &Path) -> bool {
    path.join("config/doom/init.el").is_file() && path.join("install.sh").is_file()
}

fn repo() -> Result<PathBuf> {
    if let Some(path) = env::var_os("SYS_DOTFILES_DIR").map(PathBuf::from) {
        return is_repo(&path)
            .then_some(path)
            .ok_or_else(|| "SYS_DOTFILES_DIR is not a dotfiles checkout".into());
    }

    if let Ok(current) = env::current_dir() {
        if let Some(path) = current.ancestors().find(|path| is_repo(path)) {
            return Ok(path.to_path_buf());
        }
    }

    let built_from = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent);
    if let Some(path) = built_from.filter(|path| is_repo(path)) {
        return Ok(path.to_path_buf());
    }

    let conventional = home()?.join("Source/dotfiles");
    if is_repo(&conventional) {
        return Ok(conventional);
    }

    Err("cannot find the dotfiles checkout; set SYS_DOTFILES_DIR".into())
}

fn execute(program: impl AsRef<OsStr>, args: &[OsString], cwd: Option<&Path>) -> Result<u8> {
    let mut command = Command::new(program);
    command
        .args(args)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());
    if let Some(path) = cwd {
        command.current_dir(path);
    }
    let status = command.status().map_err(|error| error.to_string())?;
    Ok(status.code().unwrap_or(1).try_into().unwrap_or(1))
}

#[cfg(windows)]
fn ps_script(path: &Path, args: &[OsString], cwd: &Path) -> Result<u8> {
    let mut command_args = vec![
        OsString::from("-NoProfile"),
        OsString::from("-ExecutionPolicy"),
        OsString::from("Bypass"),
        OsString::from("-File"),
        path.as_os_str().to_owned(),
    ];
    command_args.extend_from_slice(args);
    execute("powershell.exe", &command_args, Some(cwd))
}

fn platform_script(name: &str, args: &[OsString]) -> Result<u8> {
    let root = repo()?;
    if name == "install" {
        // Windows locks a running executable. A direct bootstrap installs/updates sys;
        // an install launched by sys repairs everything except the caller itself.
        env::set_var("SYS_SKIP_SELF_INSTALL", "1");
    }
    #[cfg(windows)]
    {
        let path = match name {
            "install" => root.join("install.ps1"),
            "update" => root.join("scripts/update.ps1"),
            "check" => root.join("scripts/doctor.ps1"),
            _ => unreachable!(),
        };
        ps_script(&path, args, &root)
    }
    #[cfg(not(windows))]
    {
        let path = match name {
            "install" => root.join("install.sh"),
            "update" => root.join("scripts/update.sh"),
            "check" => root.join("scripts/sys-health.sh"),
            _ => unreachable!(),
        };
        let mut command_args = vec![path.into_os_string()];
        command_args.extend_from_slice(args);
        execute("bash", &command_args, Some(&root))
    }
}

fn doom_command(args: &[OsString]) -> Result<u8> {
    let Some((action, rest)) = args.split_first() else {
        return Err("doom requires an action: sync or doctor".into());
    };
    let action = action.to_string_lossy();
    match action.as_ref() {
        "sync" | "doctor" => doom(action.as_ref(), rest),
        other => Err(format!(
            "unknown doom action '{other}' (expected sync or doctor)"
        )),
    }
}

fn doom(action: &str, args: &[OsString]) -> Result<u8> {
    let directory = home()?.join(".config/emacs");
    #[cfg(windows)]
    {
        let root = repo()?;
        let script = root.join("scripts/invoke-doom.ps1");
        let mut command_args = vec![
            OsString::from("-Command"),
            OsString::from(action),
            OsString::from("-DoomDirectory"),
            directory.into_os_string(),
        ];
        command_args.extend_from_slice(args);
        ps_script(&script, &command_args, &root)
    }
    #[cfg(not(windows))]
    {
        let mut command_args = vec![OsString::from(action)];
        command_args.extend_from_slice(args);
        execute(&directory.join("bin/doom"), &command_args, Some(&repo()?))
    }
}

fn restart(args: &[OsString]) -> Result<u8> {
    #[cfg(target_os = "macos")]
    {
        let root = repo()?;
        let seconds = args
            .first()
            .cloned()
            .unwrap_or_else(|| OsString::from("10"));
        execute(
            "bash",
            &[
                root.join("scripts/restart-emacs-mac.sh").into_os_string(),
                seconds,
            ],
            Some(&root),
        )
    }
    #[cfg(not(target_os = "macos"))]
    {
        let _ = args;
        Err("restart is currently supported only on macOS".into())
    }
}

fn vanilla(args: &[OsString]) -> Result<u8> {
    if !args.is_empty() {
        return Err("vanilla takes no arguments".into());
    }
    Command::new("emacs")
        .arg("-Q")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map(|_| 0)
        .map_err(|error| format!("could not launch Emacs: {error}"))
}

fn llm(args: &[OsString]) -> Result<u8> {
    #[cfg(target_os = "macos")]
    {
        let root = repo()?;
        let mut command_args = vec![root.join("scripts/llm-mac.sh").into_os_string()];
        command_args.extend_from_slice(args);
        execute("bash", &command_args, Some(&root))
    }
    #[cfg(not(target_os = "macos"))]
    {
        let _ = args;
        Err("llm management is currently supported only on macOS".into())
    }
}
