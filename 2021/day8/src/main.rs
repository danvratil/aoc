use std::fs::File;
use std::io::{self, prelude::*, BufReader};
use std::collections::HashSet;
use std::ops::Sub;

/*
 * The positions and/or lettering of each segment:
 *
 *  aaa    000
 * b   c  1   2
 * b   c  1   2
 *  ddd    333
 * e   f  4   5
 * e   f  4   5
 *  ggg    666
 */


fn digit_positions(digit: u8) -> Vec<usize> {
    match digit {
        0 => vec!(0, 1, 2, 4, 5, 6),
        1 => vec!(2, 5),
        2 => vec!(0, 2, 3, 4, 5, 6),
        3 => vec!(0, 2, 3, 5, 6),
        4 => vec!(1, 2, 3, 5),
        5 => vec!(0, 1, 3, 5, 6),
        6 => vec!(1, 3, 4, 5, 6),
        7 => vec!(0, 2, 5),
        8 => vec!(0, 1, 2, 3, 4, 5, 6),
        9 => vec!(0, 1, 2, 3, 5),
        _ => panic!("Invalid digit!")
    }
}

fn candidate_positions(value: &str) -> Vec<usize> {
    match value.len() {
        2 => digit_positions(1),
        3 => digit_positions(7),
        4 => digit_positions(4),
        //7 => digit_positions(8), // intentionally ignore digit 8: it does not help at all
        _ => Vec::new()
    }
}

fn possible_digits_by_len(value: &str) -> Vec<u8> {
    match value.len() {
        2 => vec!(1),
        3 => vec!(7),
        4 => vec!(4),
        5 => vec!(2, 3, 5),
        6 => vec!(6, 9, 0),
        7 => vec!(8),
        _ => panic!("Invalid length")
    }
}

fn positions_for_letter(letter: char, candidates: &[HashSet<char>;7]) -> Vec<usize> {
    let mut positions = Vec::<usize>::new();
    for i in 0..8 {
        if candidates[i].contains(&letter) {
            positions.push(i);
        }
    }
    positions
}

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let buf = BufReader::new(file);

    for l in buf.lines() {
        //let line = l.unwrap();
        let line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";

        let (input, value) = line.split_once('|').unwrap();
        let mut numbers = input.split(' ').collect::<Vec<_>>();
        let values = value.split(' ').collect::<Vec<_>>();

        let mut candidates: [HashSet<char>;7] = Default::default();
        let mut num_map: [String;10] = Default::default();

        // Find shortest number
        loop {
            let mut shortest = numbers.iter().min_by_key(|&s| s.len()).unwrap().chars().collect::<HashSet<char>>();
            let known_letters = HashSet::<char>::new();
            candidates.iter().for_each(|x| { known_letters.union(&x); });

            let digits = possible_digits_by_len(shortest);
            if digits.len() == 1 {
                shortest.sub(&known_letters);
            } else {
                break;
            }
            numbers.retain(|&x| x != *shortest);
        }

        for num in &numbers {
            if num.len() == 2 {
                candidates[2] = num.chars().collect();
                candidates[5] = num.chars().collect();
            }
        }
        for num in &numbers {
            if num.len() == 3 {
                candidates[0] = num.chars().collect::<HashSet<char>>().sub(&candidates[2]).sub(&candidates[5]);
            } else if num.len() == 4 {
                candidates[1] = num.chars().collect::<HashSet<char>>().sub(&candidates[2]).sub(&candidates[5]);
                candidates[3] = num.chars().collect::<HashSet<char>>().sub(&candidates[2]).sub(&candidates[5]);
            }
        }

        println!("Candidates: {:?}", candidates);
        println!("Num_map: {:?}", num_map);
        break;
    }


    Ok(())
}
