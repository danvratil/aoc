use std::fs::File;
use std::io::{self, prelude::*, BufReader};

/*
fn is_vowel(c: char) -> bool {
    match c {
        'a' | 'e' | 'i' | 'o' | 'u' => true,
        _ => false
    }
}

fn has_three_vowels(val: & String) -> bool {
    val.chars().filter(|x| is_vowel(*x)).count() >= 3
}

fn has_letter_twice_in_row(val: & String) -> bool {
    for i in 0 .. val.len() -1 {
        if val.chars().nth(i) == val.chars().nth(i + 1) {
            return true;
        }
    }
    false
}

fn doesnt_contain_bad_strings(val: & String) -> bool {
    !(val.contains("ab") || val.contains("cd") || val.contains("pq") || val.contains("xy"))
}
*/

fn contains_pair_of_doubles(val: & String) -> bool {
    (0 .. val.len() - 1).any(|i| {
        let dbl = &val.as_str()[i..i+2];
        val[0..i].contains(dbl) || val[i + 2..].contains(dbl)
    })
}

fn contains_symmetric_triplet(val: & String) -> bool {
    (0 .. val.len() - 2).any(|i| val.chars().nth(i) == val.chars().nth(i + 2))
}



fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let buf = BufReader::new(file);

    /*
    println!("{}", buf.lines()
            .filter(|s| has_three_vowels(&s.as_ref().unwrap()))
            .filter(|s| has_letter_twice_in_row(&s.as_ref().unwrap()))
            .filter(|s| doesnt_contain_bad_strings(&s.as_ref().unwrap()))
            .count());
    */
    println!("{}", buf.lines()
             .filter(|s| contains_pair_of_doubles(s.as_ref().unwrap()))
             .filter(|s| contains_symmetric_triplet(s.as_ref().unwrap()))
             .count());

    return Ok(());
}
