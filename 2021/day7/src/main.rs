use std::fs::File;
use std::io::{self, prelude::*, BufReader};
use math::round::ceil;

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let mut buf = BufReader::new(file);

    let mut input = String::new();
    buf.read_to_string(& mut input)?;

    let crabs: Vec<i32> = input.trim().split(',').collect::<Vec<&str>>().iter().map(|x| x.parse::<i32>().unwrap()).collect();

    let max_depth = crabs.iter().max().unwrap();

    let mut min_fuel = i64::MAX;
    for i in 0..=*max_depth {
        let mut fuel = 0i64;
        for crab in &crabs {
            let n = (crab - i).abs() as i64;
            let change = (n as f64 / 2.0) * (1 + n) as f64;
            fuel += change as i64;
        }

        println!("depth={}, fuel={}, min_fuel={}", i, fuel, min_fuel);
        if fuel < min_fuel {
            min_fuel = fuel;
        }
    }

    println!("{}", min_fuel);


    Ok(())
}
