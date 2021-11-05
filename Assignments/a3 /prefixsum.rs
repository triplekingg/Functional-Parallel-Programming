extern crate rayon;
use rayon::join;




struct Node {
    left: Option<Box<Node>>,
    val: u64,
    right: Option<Box<Node>>,
}

fn combine(l: &[u64], r: &[u64]) -> Vec<u64> {
    return [l, r].concat();
}

fn prefix_sum(xs: &Vec<u64>) -> Vec<u64> {
    fn generateTree(vector: &Vec<u64>) -> Node {
        let len: usize = vector.len();
        if len==0{
            return Node {
                left: None,
                val: 0,
                right: None,
            }
        }
        else if len==1{
            return Node {
                left: None,
                val: vector[0],
                right: None,
            }
        }
        else{
            let (left, right) = vector.split_at(len / 2);
            let (left_node, right_node) = join(
                || generateTree(&left.clone().to_vec()),
                || generateTree(&right.clone().to_vec()),
            );
            let l_val = left_node.val.clone();
            let r_val = right_node.val.clone();
            return Node {
                left: Some(Box::new(left_node)),
                val: l_val + r_val,
                right: Some(Box::new(right_node)),
            };
        }
    }

    fn generateVect(tree: &Option<Box<Node>>, ac: u64) -> Vec<u64> {
        if !tree.is_none() {
            let tree_node = tree.as_ref().unwrap();
            if !(tree_node.left.is_some() && tree_node.right.is_some()) {
                return vec![ac];
            } else {
                let (left_vector, right_vector) = join(
                    || {
                        generateVect(
                            &tree_node.left,
                            ac - tree_node.right.as_ref().unwrap().val,
                        )
                    },
                    || generateVect(&tree_node.right, ac),
                );
                return combine(&left_vector, &right_vector);
            }
        } else {
            return vec![0];
        }
    }

    let tree = generateTree(&xs.clone());
    let max = tree.val.clone();
    let vector = generateVect(&Some(Box::new(tree)), max);
    match xs.len() {
        len if len < 1 => return vector,
        _ => return combine(&*vec![0], &*vector),
    }
}



fn main() {
    let vec1: Vec<u64> = vec![1, 3, 5, 2];
    let vec2: Vec<u64> = vec![0, 3, 3];
    let vec3: Vec<u64> = vec![];

    println!("{:?} => {:?}", &vec1, prefix_sum(&vec1));
    println!("{:?} => {:?}", &vec2, prefix_sum(&vec2));
    println!("{:?} => {:?}", &vec3, prefix_sum(&vec3));
}
