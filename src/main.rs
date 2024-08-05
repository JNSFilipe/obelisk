mod obelisk;
use obelisk as ob;

fn main() {
    ob::log!(ob::LogLevel::Info, "cenas {} {}" ,1,2);
    ob::log!(ob::LogLevel::Warning, "cenas {} {}" ,1,2);
    ob::log!(ob::LogLevel::Error, "cenas {} {}" ,1,2);
    ob::log!(ob::LogLevel::Fatal, "cenas {} {}" ,1,2);
}
