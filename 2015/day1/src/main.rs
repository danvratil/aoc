use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let mut buf = BufReader::new(file);

    let mut data = String::new();
    buf.read_to_string(& mut data)?;

    let mut floor = 0;
    for (i, c) in data.chars().enumerate() {
        match c {
            '(' => floor += 1,
            ')' => floor -= 1,
            _ => println!("Invalid character!")
        }
        if floor == -1 {
            println!("{}", i + 1);
            return Ok(());
        }
    }

    println!("{}", floor);

    return Ok(());
}
