fn web_demo(url: &str) -> Result<(), reqwest::Error> {
    let body = reqwest::get(url)? .text()?;
    let doc =
        select::document::Document::from(body.as_str());
    use select::predicate::Name;
    let all_links: Vec<&str> = doc.find(Name("a"))
        .flat_map(|node| node.attr("href")) .collect();
    for &link in &all_links {
        println!(">> {}", link);
    }
    Ok(())
}

fn main() {
    web_demo("https://www.thaienquirer.com/");
}
