mod obelisk;

use obelisk::{LogLevel, create_symlink};

fn main() {
    log!(LogLevel::Info, "cenas {} {}" ,1,2);
    log!(LogLevel::Warning, "cenas {} {}" ,1,2);
    log!(LogLevel::Error, "cenas {} {}" ,1,2);
    log!(LogLevel::Fatal, "cenas {} {}" ,1,2);
    
}
