use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("input");
    let mut buf = BufReader::new(file?);

    let mut input_str = String::new();
    buf.read_to_string(&mut input_str)?;

    let mut counters = [0u64;9];

    let mut lanternfish: Vec<i32> = input_str.trim().split(',').collect::<Vec<&str>>().iter().map(|x| x.parse::<i32>().unwrap()).collect();
    for fish in lanternfish {
        counters[fish as usize] += 1;
    }

    for day in 0..256 {
        let zero_fish = counters[0];
        for i in 0..=7 {
            counters[i as usize] = counters[i + 1];
        }
        counters[8] = zero_fish;
        counters[6] += zero_fish;

        println!("Day {}: {:?}", day, counters)
    }

    println!("{}", counters.iter().sum::<u64>());

    Ok(())
}
