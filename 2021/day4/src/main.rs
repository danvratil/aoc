use std::fs::File;
use std::io::{self, prelude::*, BufReader};
use arrayvec::ArrayVec;

type Board = [[(u16, bool);5];5];


fn read_board<R: BufRead>(buf: & mut R) -> io::Result<Board> {
    let mut board: Board = [[(0,false);5];5];
    for i in 0..5 {
        let mut line_buf = String::new();
        match buf.read_line(& mut line_buf) {
            Err(e) => return Err(e),
            Ok(_) => {
                let trimmed = line_buf.trim();
                if trimmed.is_empty() {
                    return Err(io::Error::new(io::ErrorKind::Other, "EOF"));
                }
                board[i] = line_buf.trim().split_whitespace().map(|x| (x.parse::<u16>().unwrap(), false)).collect::<ArrayVec<_, 5>>().into_inner().unwrap();
            }
        }
    }

    Ok(board)
}

fn update_board(board: & mut Board, drawn: u16) {
    for row in 0..5 {
        for col in 0..5 {
            if board[row][col].0 == drawn {
                board[row][col].1 = true;
                return
            }
        }
    }
}

fn board_wins(board: & Board) -> bool {
    for x in 0..5 {
        // check each row
        if board[x].iter().all(|(_, m)| *m) {
            return true;
        }
        // check each column
        if board.iter().all(|row| row[x].1 == true) {
            return true;
        }
    }
    false
}

fn board_score(board: & Board, drawn: u16) -> u32{
    board.iter().map(|row| { row.iter().filter_map(|(v, m)| if !*m { Some(v) } else { None }).sum::<u16>() }).sum::<u16>() as u32 * drawn as u32
}

fn main() -> io::Result<()> {
    let file = File::open("input");
    let mut buf = BufReader::new(file?);

    // Read input line
    let mut line_buf = String::new();
    buf.read_line(& mut line_buf)?;
    let input_line: Vec<u16> = line_buf.trim().split(',').map(|x| x.parse::<u16>().unwrap()).collect();

    buf.read_line(& mut line_buf)?; // empty line

    let mut boards = Vec::<Board>::new();
    loop {
        match read_board(& mut buf) {
            Err(_) => break,
            Ok(board) => boards.push(board)
        }

        buf.read_line(& mut line_buf)?; // skip empty line
    }

    // Let's play bingo!

    for drawn in input_line {
        let mut i = 0;
        while i < boards.len() {
            let mut board =  boards[i];
            update_board(& mut board, drawn);
            boards[i] = board;
            if board_wins(&board) {
                if boards.len() == 1 {
                    println!("{}", board_score(&board, drawn));
                    return Ok(());
                }
                boards.remove(i);
            } else {
                i += 1;
            }
        }
    }

    Ok(())
}
