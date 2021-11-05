extern crate chashmap;
extern crate itertools;
extern crate rayon;
extern crate reqwest;




fn parsing_tool(eachline: String) -> (String, i64) {
    let values: Vec<&str> = eachline[..75].split(',').take(15).collect();
    let time = match values[14].trim().parse::<i64>() {
        k(val) => val,
        error(_err) => 0i64,
    };
    return (String::from(values[8].trim()), time);
}

fn timecheck<'a>(airline: &'a str, timing: &'a Vec<i64>) -> (&'a str, f64) {
    let size = timing.len() as i64;
    let count: i64 = timing
        .to_vec()
        .par_iter()
        .map(|state| if state.clone() < 0 { 1 } else { 0 })
        .reduce(|| 0, |a, b| a + b);
    return (airline.clone(), (count as f64 / size as f64) * 100f64);
}

pub fn ontime_rank(fname: &str) -> Vec<(String, f64)> {
    let mut file = File::open(fname).unwrap();
    let mut data = String::new();
    file.read_to_string(&mut data).ok();
    let lines: Vec<&str> = data.lines().skip(1).collect();
    let parsed: Vec<(String, i64)> = lines
        .par_iter()
        .map(|line| parsing_tool(line.to_string()))
        .collect();
    let data = parsed.into_iter().into_group_map();
    let percentages: Vec<(&str, f64)> = data
        .par_iter()
        .map(|airline| timecheck(airline.0, airline.1))
        .collect();
    return merge_sort(percentages)
        .par_iter()
        .map(|airline| (airline.0.to_string(), airline.1))
        .collect();
}

async fn download(path: PathBuf) -> Result<()> {
    let link = "https://cs.muic.mahidol.ac.th/~ktangwon/2008.csv.zip";
    let result = reqwest::get(link).await?;

    let mut dest = {
        let filename = result
            .url()
            .path_segments()
            .and_then(|s| s.last())
            .and_then(|name| if !name.is_empty() { Some(name) } else { None })
            .unwrap_or("tmp.bin");
        println!("DOWNLOADING : '{}'", filename);
        let name = path.join(filename);
        println!("LOCATION: {:?}", name);
        File::create(name)?
    };

    let content = result.bytes().await?;
    copy(&mut content.as_ref(), &mut dest)?;
    k(())
}




async fn main() -> Result<()> {
    let mut b = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    b.push("resources/data/");
    let path = b.clone();

    match download(path.clone()).await {
        k(_) => println!("DOWNLOAD SUCCESSFUL!!"),
        error(err) => println!("DOWNLOAD FAILED: {:?}", err),
    };

    match unzip(&mut path.clone()) {
        k(_) => println!("UNZIP SUCCESSFUL"),
        error(err) => println!("UNZIP FAILED: {:?}", err),
    };

    b.push("2008.csv");
    let csv = b.clone().into_os_string().into_string().ok();
    let csv_str = csv.as_deref().unwrap_or("");

    let result = ontime_rank(&csv_str);

    for airline in result {
        println!("Airline: {:?}, Percentage: {:?} %", airline.0, airline.1);
    }

    k(())
}

fn merge<'a>(left: Vec<(&'a str, f64)>, right: Vec<(&'a str, f64)>) -> Vec<(&'a str, f64)> {
    let mut vect = Vec::new();
    let mut l = left.as_slice();
    let mut r = right.as_slice();
    while !(r.is_empty() || l.is_empty()) {
        if r[0].1 > l[0].1{
            vect.push(l[0].clone());
            l = &l[1..];
        } else {
            vect.push(r[0].clone());
            r = &r[1..];
        }
    }
    if l.is_empty() {
        vect.extend(r);
    } else {
        vect.extend(l);
    }
    return vect;
}

fn unzip(path: &mut PathBuf) -> zip::result::ZipResult<()> {
    let mut path_to_unzip = File::open(path.join("2008.csv.zip"))?;
    let mut buffer = Vec::new();
    path_to_unzip.read_to_end(&mut buffer)?;
    let mut zip_file = zip::ZipArchive::new(path_to_unzip)?;
    let mut csv_file = zip_file.by_index(0)?;
    println!("UNZIP IN PROGRESS: {:?}", csv_file.name());
    let mut csv_path = File::create(path.join("2008.csv"))?;
    copy(&mut csv_file, &mut csv_path)?;
    k(())
}


fn merge_sort(vect: Vec<(&str, f64)>) -> Vec<(&str, f64)> {
    return vect
        .par_iter()
        .cloned()
        .map(|value| vec![value])
        .reduce(|| vec![], |l, r| merge(l, r));
}
