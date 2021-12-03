use std::fs::File;
use std::io::{self, prelude::*, BufReader};

const BIT_WIDTH: u16 = 12;

fn count_bits(data: & Vec<u16>, offset: u16) -> (usize, usize) {
    let ones = data.iter().filter(|v| (*v >> offset) & 0x1 == 1).count();
    let zeroes = data.len() - ones;
    (ones, zeroes)
}

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let buf = BufReader::new(file);

    let values: Vec<u16> = buf.lines().filter_map(|l| l.ok()).map(|l| {
        let mut val: u16 = 0;
        for (i, _) in l.char_indices().filter(|(_, c)| *c == '1') {
            val |= 1 << (l.len() - i - 1);
        }
        val
    }).collect();

    /* Power consumption (part 1) */
    let mut gamma = 0;
    for i in 0..BIT_WIDTH {
        if values.iter().filter(|v| (*v >> i) & 0x1 == 1).count() > (values.len() / 2) {
            gamma |= 1 << i;
        }
    }

    let epsilon = !gamma & ((1 << BIT_WIDTH) - 1);
    println!("g={:#014b}, e={:#014b}", gamma, epsilon);
    println!("{}", gamma * epsilon);

    /* Life support rating (part 2) */
    let mut oxygen = values.clone();
    let mut co2 = values.clone();
    for i in (0..BIT_WIDTH).rev() {
        if oxygen.len() > 1 {
            let (ones, zeroes) = count_bits(&oxygen, i);
            let b = if ones >= zeroes { 1 } else { 0 };
            oxygen = oxygen.iter().filter(|v| (*v >> i) & 0x1 == b).cloned().collect();
        }
        if co2.len() > 1 {
            let (ones, zeroes) = count_bits(&co2, i);
            let b = if ones < zeroes { 1 } else { 0 };
            co2 = co2.iter().filter(|v| (*v >> i) & 0x1 == b).cloned().collect();
        }
    }

    println!("oxygen={:#014b}, co2={:#014b}", oxygen[0], co2[0]);
    println!("{}", oxygen[0] as u32 * co2[0] as u32);

    Ok(())
}
