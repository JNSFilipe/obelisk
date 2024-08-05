use std::env::consts::OS;
use std::fs::metadata;

#[derive(Debug)]
pub enum LogLevel {
    Info,
    Warning,
    Error,
    Fatal,
}

macro_rules! log {
    ($level:expr, $fmt:expr $(, $arg:expr)*) => {{
        use colored::Colorize;
        let tag = match $level {
            obelisk::LogLevel::Info => {
                "[INFO]".bold().green()
            },
            obelisk::LogLevel::Warning => {
                "[WARNING]".bold().yellow()
            },
            obelisk::LogLevel::Error => {
                "[ERROR]".bold().red()
            },
            obelisk::LogLevel::Fatal => {
                panic!("{} {}", "[FATAL]".black().on_red().bold(), format!($fmt $(, $arg)*));
            },
        };
        println!("{} {}", tag, format!($fmt $(, $arg)*));
    }};
}
pub(crate) use log; 


// pub fn ob_create_symlink(orig: &str, targ: &str) {
//     if OS == "linux" || OS == "macos" {
//         std::os::unix::fs::symlink(orig, targ);
//     } else if OS == "windows" {
//         // TODO: make error message in this unwarp
//         let md = metadata(orig).unwrap();

//         if md.is_dir() {
//             std::os::windows::fs::symlink_dir(orig, targ);
//         } else {
//             std::os::windows::fs::symlink_file(orig, targ);
//         }
//     } else {
//         panic!("[ERR] Unsupported OS: {}!", OS);
//     }
// }
