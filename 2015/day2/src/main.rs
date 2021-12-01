use std::fs::File;
use std::io::{self, prelude::*, BufReader};
use text_io::scan;

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let buf = BufReader::new(file);

    let mut area = 0;
    let mut len = 0;
    for line in buf.lines() {
        let w: i32;
        let h: i32;
        let l: i32;
        let data = line.unwrap();
        scan!(data.bytes() => "{}x{}x{}", w, h, l);
        let s1 = w * h;
        let s2 = h * l;
        let s3 = w * l;
        area += (2 * s1) + (2 * s2) + (2 * s3) + [s1, s2, s3].iter().min().unwrap();

        let faces = [(2 * w) + (2 * h), (2 * w) + (2 * l), (2 * h) + (2 * l)];
        len += facest.iter().min().unwrap() + (w * h * l);
    }

    println!("{}", area);
    println!("{}", len);

    return Ok(());
}
