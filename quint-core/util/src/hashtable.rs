use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::fmt::{self, Debug};
use std::ops::{Index, IndexMut};

#[derive(Clone)]
struct QHashMap<K, V>
where 
  K: Eq + Hash + Clone,
  V: Clone,
{
	es: Vec<Vec<Entry<K, V>>>,
	length: usize,
}

#[derive(Clone)]
struct Entry<K, V>
where 
	K: Eq + Hash + Clone,
	V: Clone,
{
	k: K,
	v: V
}

impl<K, V> QHashMap<K, V> 
where 
	K: Eq + Hash + Clone + Debug,
	V: Clone + Debug,
{
	const INITIAL_SIZE: usize = 16;
	const MAX_BUCKET_SIZE: usize = 20;
	const LIMIT_FACTOR: usize = 32;
	const INCREMENT_FACTOR: usize = 3;
	const DECREMENT_FACTOR: usize = 2;
	const COMPACT_LIMIT: usize = 16;

	fn new() -> Self {
		Self { 
			es: vec![vec![]; Self::INITIAL_SIZE], 
			length: 0 
		}
	}
	
	fn fnv(data: usize, start: usize) -> usize {
		const FNV_PRIME: usize = if std::mem::size_of::<usize>() == 4 {
			16777619
		} else {
			1099511628211
		};
		const FNV_BASIS: usize = if std::mem::size_of::<usize>() == 4 {
			2166136261
		} else {
			14695981039346656037
		};
		(start ^ data).wrapping_mul(FNV_PRIME)
	}

	fn insert(&mut self, k: &K) -> bool {
		let index = self.hash(k) % self.es.len();
		let bucket = &mut self.es[index];
		if let Some(pos) = bucket.iter().position(|e| e.k == *k) {
			bucket.swap_remove(pos);
			self.length -= 1;
			true
		} else {
				false
		}
	}

	fn get(&self, k: &K) -> Option<&V> {
		let index = self.hash(k) % self.es.len();
		for entry in &self.es[index] {
			if entry.k == *k {
				return Some(&entry.v)
			}
		}
		None
	}

	fn get_mut(&mut self, k: &K) -> Option<&mut V> {
		let index = self.hash(k) % self.es.len();
		for entry in &mut self.es[index] {
				if entry.k == *k {
						return Some(&mut entry.v);
				}
		}
		None
	}	

	fn constains_key(&self, k: &K) -> bool {
		self.get(k).is_some()
	}

	fn remove(&mut self, k: &K) -> bool {
		let index = self.hash(k) % self.es.len();
		let bucket = &mut self.es[index];
		if let Some(pos) = bucket.iter().position(|e| e.k == *k) {
			bucket.swap_remove(pos);
			self.length -= 1;
			true
		} else {
			false
		}
	}

	fn hash<T: Hash>(&self, t: &T) -> usize {
		let mut s = std::collections::hash_map::DefaultHasher::new();
		t.hash(&mut s);
		s.finish() as usize
	}

	fn realloc(&mut self) {
		let mut new_es = vec![vec![]; self.es.len() * Self::INCREMENT_FACTOR];
		self.length = 0;
		for bucket in &self.es {
			for entry in bucket {
				let index = self.hash(&entry.k) % new_es.len();
				new_es[index].push(entry.clone());
			}
		}
		self.es = new_es;
	}

	fn compact(&mut self) {
		let mut new_es = vec![vec![]; self.es.len() / Self::DECREMENT_FACTOR];
		self.length = 0;

		for bucket in &self.es {
			for entry in bucket {
				let index = self.hash(&entry.k) % new_es.len();
				new_es[index].push(entry.clone());
			}
		}
		self.es = new_es;
	}

	fn clear(&mut self) {
		self.es = vec![vec![]; Self::INITIAL_SIZE];
		self.length = 0;
	}

	fn to_string(&self) -> String {
		let mut result = String::from("[");
		for bucket in &self.es {
			for entry in bucket {
				result.push_str(&format!("{:?}:{:?}, ", entry.k, entry.v));
			}
		}
		if result.len() > 1 {
			result.truncate(result.len() - 2);
		}
		result.push(']');
		result
	}
}

impl<K, V> fmt::Debug for QHashMap<K, V>
where 
	K: Eq + Hash + Clone + Debug,
	V: Clone + Debug
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.to_string())
	}
}

impl<K, V> Index<K> for QHashMap<K, V> 
where 
	K: Eq + Hash + Clone + Debug,
	V: Clone + Debug
{
	type Output = V;

	fn index(&self, index: K) -> &Self::Output {
			self.get(&index).expect("key not found")
	}
}

impl<K, V> IndexMut<K> for QHashMap<K, V>
where 
	K: Eq + Hash + Clone + Debug,
	V: Clone + Debug,
{
	fn index_mut(&mut self, index: K) -> &mut Self::Output {
		self.get_mut(&index).expect("key not found")
  }
}