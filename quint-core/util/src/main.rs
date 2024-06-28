use util::hashtable::QHashMap;

fn main() {
  let mut map = QHashMap::new();
  map.insert(1, "one".to_string());
  map.insert(2, "two".to_string());

  // map.remove(&1);

  let val = match map.get(&1) {
    Some(v) => v,
    None => "default Value",
  };

  let val1 = map.get(&2).unwrap(); // error
  let temp = "defualt".to_string();
  let val2 = map.get(&3).unwrap_or(&temp);

  println!("value: {}", val);
  println!("value: {}", val1);
  println!("value: {}", val2);

  for (k, v) in map {
    print!("key: {}, value: {}", k, v);
  }
}