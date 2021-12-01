use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let buf = BufReader::new(file);

    let mut both_coords = [(0, 0), (0, 0)];
    let mut houses = vec![(0, 0)];
    let mut i = 0;
    for line in buf.lines() {
        for c in line.unwrap().chars() {
            let mut coords = & mut both_coords[i % 2];
            match c {
                '^' => coords.1 -= 1,
                'v' => coords.1 += 1,
                '<' => coords.0 -= 1,
                '>' => coords.0 += 1,
                _ => {}
            }

            match houses.iter().find(|s| s.0 == coords.0 && s.1 == coords.1) {
                Some(_) => {},
                None => houses.push(*coords)
            }

            i += 1;
        }
    }

    println!("{}", houses.len());

    return Ok(());
}
