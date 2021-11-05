use rayon::prelude::*;

fn isPrime(n: u64) -> bool{
    let end = (n/2+1);
    for i in 2..end{
        if n%i==0 || n==2{
            return false;
        }
    }
    return n > 1;
}

fn filterPrimes(input: &Vec<u64>) -> Vec<u64>{
    let mut par_iter = input.to_vec().into_par_iter().filter(|&x| isPrime(x));
    let primes:Vec<u64> = par_iter.collect();
    primes
}

//Test Cases for filterPrimes
//Please note that test cases for isPrime is in the parallelsPrime.rs file
fn main() {
    println!("{:?} Test passed",assert_eq!(vec![2, 3, 5, 7, 11, 13, 17, 19],filterPrimes(&vec![1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20])));
    println!("{:?} Test passed",assert_eq!(vec![101, 599, 857],filterPrimes(&vec![1,35,99,101,150,180,210,123,213,599,931,857])));
}