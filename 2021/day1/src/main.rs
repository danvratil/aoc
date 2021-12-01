use std::fs::File;
use std::io::{self, prelude::* ,BufReader};

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let reader = BufReader::new(file);

    let mut prev_sum = 0;
    let mut vals: [i16; 3] = [0, 0, 0];
    let mut increments = 0;
    for (i, line) in reader.lines().enumerate() {
        // [1,2,3] => [2,3,new]
        vals = [vals[1], vals[2], line?.parse::<i16>().unwrap()];
        if i >= 2 {
            let new_sum = vals.iter().sum();
            if prev_sum > 0 && new_sum > prev_sum {
                increments += 1;
            }
            prev_sum = new_sum;
        }
    }

    println!("{}", increments);

    return Ok(());
}
