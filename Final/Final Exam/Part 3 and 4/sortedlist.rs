use rayon::prelude::*;
use std::iter::FromIterator;

fn insertList(x: i64, L: Vec<i64>) -> Vec<i64>{
    let mut v = L.to_vec();
    v.push(x);
    v.sort();
    v
}

fn filter(x: i64, L: Vec<i64>) -> Vec<i64>{
    let mut par_iter = L.to_vec().into_par_iter().filter(|&i| i<x );
    let xs:Vec<i64> = par_iter.collect();
    xs
}

fn searchParallel(x: i64, L: Vec<i64>) -> bool{
    return L.to_vec().into_par_iter().any(|&i| i = x);
}

fn numWrongOrder(L: Vec<i64>) -> i64{

    // Find length of the longest subsequence of numbers that are sorted
    fn longestSeq(arr: Vec<i64>, n: i64) -> i64 {
        let mut result = 0;
        let mut k:usize = 1;
        let mut l:usize = 0;
        //initializing array
        let mut part = Vec::from_iter(arr[0..n as usize].iter().cloned());
        let mut lis:Vec<i64> = part.to_vec().iter().map(|mut x| 1).collect();
        //

        for i in 1..n {
            l = 0;
            loop {
                if l > k {
                    break;
                }
                if (arr[k] > arr[l]) && lis[k]<lis[l]+1{
                    lis[k] = lis[l] + 1;
                }
                l +=1;
            }
            k+=1;
        }
        k=0;
        for i in 0..n {
            if result<lis[k] {
                result = lis[k];
            }
            k+=1;
        }
        return result;
    }

    //Find minimum number of deletions required
    fn minimumNumberOfDeletions(arr:Vec<i64>, length: i64) -> i64{
        let len = longestSeq(arr,length);
        return length-len
    }
    let size = L.len() as i64;
    return minimumNumberOfDeletions(L,size)

}







fn main() {

    //Tests for insertList
    println!("Test passed{:?}", assert_eq!(vec![1,3,5,21,22,26,30],insertList(22,vec![1,3,5,21,26,30])));
    println!("Test passed{:?}", assert_eq!(vec![100,114,120,150,250,314,412,524,600],insertList(120,vec![100,114,150,250,314,412,524,600])));

    //Test for searchParallel
    println!("Test passed{:?}", assert_eq!(true,searchParallel(5,vec![1,3,5,21,26,30])));
    println!("Test passed{:?}", assert_eq!(false,searchParallel(99,vec![1,3,5,21,26,30])));

    //Test for filter
    println!("Test passed{:?}", assert_eq!(vec![1,2,3,5,13,16,21],filter(22,vec![1,2,3,5,13,16,21,26,30])));
    println!("Test passed{:?}", assert_eq!(vec![1,2,3,5,13,16,21,26,30],filter(100,vec![1,2,3,5,13,16,21,26,30])));

    //Test for numWrongOrder
    println!("Test passed{:?}", assert_eq!(2,numWrongOrder(vec![1,4,5,99,6,7,55,8]))); //Remove 99 and 55
    println!("Test passed{:?}", assert_eq!(3,numWrongOrder(vec![5, 6, 1, 7, 4,99,8,9]))); //Remove 1,4 and 99



}
