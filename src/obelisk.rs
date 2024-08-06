use std::env::consts::OS;
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
