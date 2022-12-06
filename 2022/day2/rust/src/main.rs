use std::fs::File;
use std::io;
use std::io::*;
use std::io::BufRead;
use std::result::Result;

enum Object {
    Rock = 0,
    Paper = 1,
    Scissors = 2
}

enum Outcome {
    Win,
    Draw,
    Loss
}

impl Object {
    fn score(&self) -> u32 {
        match *self {
            Object::Rock => 1,
            Object::Paper => 2,
            Object::Scissors => 3
        }
    }
}

impl TryFrom<&str> for Object {
    type Error = &'static str;

    fn try_from(d: &str) -> Result<Self, Self::Error> {
        match d {
            "A" | "X" => Ok(Object::Rock),
            "B" | "Y" => Ok(Object::Paper),
            "C" | "Z" => Ok(Object::Scissors),
            _ => Err("Invalid input")
        }
    }
}

impl Outcome {
    fn score(&self) -> u32 {
        match *self {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Loss => 0
        }
    }
}

static GAME: [[Outcome;3];3] = [
    //              Rock     Paper   Scissors
    /*     Rock */ [ Outcome::Draw, Outcome::Loss, Outcome::Win  ],
    /*    Paper */ [ Outcome::Win,  Outcome::Draw, Outcome::Loss ],
    /* Scissors */ [ Outcome::Loss, Outcome::Win , Outcome::Draw ]
];

fn calculate_score(o1: Object, o2: Object) -> u32 {
    o2.score() + GAME[o2 as usize][o1 as usize].score()
}

fn main() -> io::Result<()> {
    let file = File::open("input")?;
    let buf = BufReader::new(file);

    let mut score = 0;
    for line in buf.lines() {
        let l = line.unwrap();
        let mut s = l.split(' ');
        let col1 = Object::try_from(s.next().unwrap()).unwrap();
        let col2 = Object::try_from(s.next().unwrap()).unwrap();
        score += calculate_score(col1, col2);
    }

    println!("{}", score);

    Result::Ok(())
}
