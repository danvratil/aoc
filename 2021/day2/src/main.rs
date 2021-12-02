use std::fs::File;
use std::io::{self, prelude::*, BufReader};
use text_io::scan;

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let buf = BufReader::new(file);

    let mut depth = 0;
    let mut aim = 0;
    let mut horiz = 0;
    for line in buf.lines() {
        let dir: String;
        let val: i32;
        let input = line?;
        scan!(input.bytes() => "{} {}", dir, val);

        match dir.as_str() {
            "forward" => {
                horiz += val;
                depth += val * aim;
            },
            "down" => aim += val,
            "up" => aim -= val,
            _ => {}
        }
    }

    println!("{}", depth * horiz);

    return Ok(());
}
