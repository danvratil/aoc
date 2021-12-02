use rayon::prelude::*;

fn check_digest(val: u64) -> Option<u64> {
    let digest = md5::compute(("yzbqklnj".to_owned() + &val.to_string()).as_bytes());
    if format!("{:x}", digest).starts_with("000000") {
        Some(val)
    } else {
        None
    }
}

fn main() {
    println!("{}", (0..u64::MAX).into_par_iter().find_map_first(check_digest).unwrap());
}
