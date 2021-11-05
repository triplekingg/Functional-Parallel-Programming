fn isPrime(n: u64) -> bool{
    let end = (n/2+1);
    for i in 2..end{
        if n%i==0 || n==2{
            return false;
        }
    }
    return n > 1;
}

//Test Cases
fn main() {
    println!("{:?} Test passed",assert_eq!(true,isPrime(13)));
    println!("{:?} Test passed",assert_eq!(true,isPrime(2)));
    println!("{:?} Test passed",assert_eq!(false,isPrime(99)));
    println!("{:?} Test passed",assert_eq!(true,isPrime(73)));
}