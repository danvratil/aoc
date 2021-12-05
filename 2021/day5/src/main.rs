use std::fs::File;
use std::io::{self, prelude::*, BufReader};
use text_io::scan;

#[derive(Debug)]
struct Line {
    start: (i32, i32),
    end: (i32, i32)
}

fn main() -> io::Result<()> {
    let file = File::open("input");
    let buf = BufReader::new(file?);

    let mut board = [[0i32;1000];1000];

    let mut overlap = 0;
    for line in buf.lines() {
        let mut new_line = Line{start: (0, 0), end: (0, 0)};
        let line_b = line?;
        scan!(line_b.bytes() => "{},{} -> {},{}", new_line.start.0, new_line.start.1, new_line.end.0, new_line.end.1);

        let mut d_x = 0;
        let mut d_y = 0;
        if new_line.start.0 == new_line.end.0 { // horizontal
            d_y = if new_line.start.1 < new_line.end.1 { 1i32 } else { -1 };
        } else if new_line.start.1 == new_line.end.1 { // vertical
            d_x = if new_line.start.0 < new_line.end.0 { 1i32 } else { -1 };
        } else { // diagonal
            d_x = if new_line.start.0 < new_line.end.0 { 1i32 } else { -1 };
            d_y = if new_line.start.1 < new_line.end.1 { 1i32 } else { -1 };
        }
        let mut pos = new_line.start;
        while pos != new_line.end {
            board[pos.1 as usize][pos.0 as usize] += 1;
            if board[pos.1 as usize][pos.0 as usize] == 2 {
                overlap += 1;
            }
            pos.0 += d_x;
            pos.1 += d_y;
        }
        board[pos.1 as usize][pos.0 as usize] += 1;
        if board[pos.1 as usize][pos.0 as usize] == 2 {
            overlap += 1;
        }

    }

    println!("{}", overlap);

    Ok(())
}
