use std::env::consts::OS;
use resolve_path::PathResolveExt;
use std::fs::metadata;

#[derive(Debug)]
pub enum LogLevel {
    Info,
    Warning,
    Error,
    Fatal,
}

#[macro_export]
macro_rules! log {
    ($level:expr, $fmt:expr $(, $arg:expr)*) => {{
        use colored::Colorize;

        let tag = match $level {
            LogLevel::Info => "[INFO]".bold().green(),
            LogLevel::Warning => "[WARNING]".bold().yellow(),
            LogLevel::Error => "[ERROR]".bold().red(),
            LogLevel::Fatal => panic!("{} {}", "[FATAL]".black().on_red().bold(), format!($fmt $(, $arg)*)),
        };
        println!("{} {}", tag, format!($fmt $(, $arg)*));
    }};
}


pub fn create_symlink(orig: &str, targ: &str) {
    // TODO: Resolve relative paths
    let res_orig = match orig.resolve().to_str() {
        Some(s) => s,
        None => panic!("I AM PANNICKING!!"),
    };

    let res_targ = match targ.resolve().to_str() {
        Some(s) => s,
        None => panic!("I AM PANNICKING!!"),
    };

    // For linux and macOS
    #[cfg (any (target_os = "linux", target_os = "macos"))] {
        match std::os::unix::fs::symlink(res_orig, targ) {
            Ok(_) => {},
            Err(e) => {
                log!(LogLevel::Fatal, "Unnable to create symlink from {} to {} in {}", orig, targ, OS);
            }
        }

    }

    // For Windows
    #[cfg (target_os = "windows")] {
        // TODO: make error message in this unwarp
        let md = metadata(orig).unwrap();

        if md.is_dir() {
            std::os::windows::fs::symlink_dir(orig, targ);
        } else {
            std::os::windows::fs::symlink_file(orig, targ);
        }
    }

    // Anything else throws error
    #[cfg (not (any (target_os = "windows", target_os = "linux", target_os = "macos")))]{
        panic!("[ERR] Unsupported OS: {}!", OS);
    }
}


