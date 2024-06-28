use core::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub, Rem, BitAnd, BitOr, BitXor, Not, Shl, Shr};

use num_bigint::BigInt;
use num_traits::{sign, One, Signed, ToPrimitive, Zero};
use crate::hashtable::QHashMap;

pub type ℤ = BigInt;
pub type MapX<K, V> = QHashMap<K, V>;

pub fn pow(mut a: ℤ, mut b: ℤ) -> ℤ {
  assert!(b >= Zero::zero());
  
  let mut r = BigInt::one();
  while b > Zero::zero() {
    if &b % 2 == One::one() {
      r *= &a;
    }
    a = &a * &a;
    b /= 2;
  }
  r
}

pub fn gcd(mut a: ℤ, mut b: ℤ) -> ℤ {
  if b < Zero::zero() {
    return if a < Zero::zero() {
      -gcd(-a, -b)
    } else {
      gcd(a, -b)
    };
  }

  let mut f = BigInt::one();
  loop {
    if a == b {
      return a * &f;
    }
    if b > a {
      std::mem::swap(&mut a, &mut b);
    }
    if b == Zero::zero() {
      return a * &f;
    }

    if &a % 2 == Zero::zero() && &b % 2 == Zero::zero() {
      f *= 2;
      a /= 2;
      b /= 2;
    } else if &b % 2 == Zero::zero() {
      b /= 2;
    } else if &a % 2 == Zero::zero() {
      a /= 2;
    } else {
      a -= &b;  
    }
  }
}

pub fn lcm(a: ℤ, b: ℤ) -> ℤ {
  &a * (&b /gcd(a.clone(), b.clone()))
}

#[derive(Clone, Debug)]
pub struct ℚ {
  num: ℤ,
  den: ℤ
}

impl ℚ {
  pub fn new(num: ℤ, den: ℤ) -> Self {
    let (num, den) = if den < Zero::zero() {
      (-num, -den)
    } else {
      (num, den)
    };
    let d = gcd(num.abs(), den.clone());
    let num = num / d.clone();
    let den = den / d;
    Self { num, den }
  }

  pub fn from_int(num: i64) -> Self {
    Self::new(num.into(), One::one())
  }

  pub fn from_ints(num: i64, den: i64) -> Self {
    Self::new(num.into(), den.into())
  }

  fn reciprocal(&self) -> Self {
    Self::new(self.den.clone(), self.num.clone())
  }

  pub fn pow(self, mut b: ℤ) -> Self {
    let mut a = self;
    if b < Zero::zero() {
      b = -b;
      a = a.reciprocal();
    }
    Self::new(pow(a.num, b.clone()), pow(a.den, b))
  }
}

impl Add for ℚ {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
      Self::new(
        self.num * rhs.den.clone() + rhs.num * self.den.clone(), 
        self.den * rhs.den
      )
  }
}

impl Sub for ℚ {
  type Output = Self;

  fn sub(self, rhs: Self) -> Self::Output {
    self + (-rhs)
  }
}

impl Mul for ℚ {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self::Output {
    Self::new(self.num * rhs.num, self.den * rhs.den)
  }
}

impl Div for ℚ {
  type Output = Self;

  fn div(self, rhs: Self) -> Self::Output {
    self * rhs.reciprocal()
  }
}

impl Neg for ℚ {
  type Output = Self;

  fn neg(self) -> Self::Output {
    Self::new(-self.num, self.den)
  }
}

impl PartialEq for ℚ {
  fn eq(&self, other: &Self) -> bool {
    self.num == other.num && self.den == other.den
  }
}

impl PartialEq<i64> for ℚ {
  fn eq(&self, other: &i64) -> bool {
    self.num == other * &self.den
  }
}

impl PartialOrd for ℚ {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some((&self.num * &other.den).cmp(&(&other.num * &self.den)))
  }
}

impl PartialOrd<i64> for ℚ {
  fn partial_cmp(&self, other: &i64) -> Option<std::cmp::Ordering> {
    Some(self.num.cmp(&(self.den.clone() * other)))
  }
}

impl fmt::Display for ℚ {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.den == One::one() {
      write!(f, "{}", self.num)
    } else {
        write!(f, "{}/{}", self.num, self.den)
    }
  }
}

pub fn to_long(a: &ℤ) -> i64 {
  a.to_i64().unwrap()
}

pub fn to_double(a: &ℤ) -> f64 {
  a.to_f64().unwrap()
}

pub fn to_double_q(a: &ℚ) -> f64 {
  to_double(&a.num) / to_double(&a.den)
}

pub fn nCr(n: ℤ, r: ℤ) -> ℤ {
  if r > n {
    return ℤ::zero();
  }
  let mut c = ℤ::one();
  let mut k: ℤ = ℤ::zero();
  while k < r {
    c = c * (n.clone() - k.clone()) / (k.clone() + ℤ::one());
    k = k + ℤ::one();
  }
  c
}

fn abs_z(x: &ℤ) -> ℤ {
  if x < &Zero::zero() {
    -x
  } else {
    x.clone()
  }
}

fn abs_q(x: &ℚ) -> ℚ {
  ℚ::new(abs_z(&x.num), x.den.clone())
}

fn ceildiv(a: ℤ, b: ℤ) -> ℤ {
  let sign = (a < Zero::zero()) ^ (b < Zero::zero());
  let (a, b) = (abs_z(&a), abs_z(&b));
  if sign {
    -(&a / &b)
  } else {
    (&a + &b - ℤ::one()) / &b
  }
}

fn ceilmod(a: ℤ, b: ℤ) -> ℤ {
  if b == Zero::zero() {
    return a;
  }
  let sign = (a < ℤ::zero()) ^ (b < ℤ::zero());
  let r = &a % &b;
  if !sign && r != Zero::zero() {
    r - &b
  } else {
    r
  }
}

#[derive(Clone, Debug)]
pub struct BitInt
{
  signed: bool,
  nbits: usize,
  val: ℤ
}

impl BitInt {
  pub fn new(nbits: usize, val: ℤ, signed: bool) -> Self {
    let mut instance = Self { nbits, val, signed };
    instance.wrap();
    instance
  }

  fn wrap(&mut self) {
    let mask = (BigInt::one() << self.nbits) - BigInt::one();
    self.val &= mask.clone();
    if self.signed && self.nbits > 0 && (self.val.clone() & (BigInt::one() << (self.nbits - 1))) != BigInt::zero() {
      self.val -= mask + BigInt::one();
    }
  }

  fn op_binary(&self, other: &BitInt, op: &str) -> Self{
    assert_eq!(self.nbits, other.nbits);
    let result = match op {
      "+" => &self.val + &other.val,
      "-" => &self.val - &other.val,
      "*" => &self.val * &other.val,
      "/" => &self.val / &other.val,
      "%" => &self.val % &other.val,
      "&" => &self.val & &other.val,
      "|" => &self.val | &other.val,
      "^" => &self.val ^ &other.val,
      _=> panic!("Unsupported binary operator")
    };
    BitInt::new(self.nbits, result, self.signed)
  }

  fn op_binary_shift(&self, rhs: usize, op: &str) -> Self {
    let result = match op {
      "<<" => &self.val << rhs,
      ">>" => &self.val >> rhs,
      _=> panic!("Unsupported shift operator")
    };
    BitInt::new(self.nbits, result, self.signed)
  }

  fn op_unary(&self, op: &str) -> Self {
    let result = match op {
      "!" => !&self.val,
      "-" =>  -&self.val,
      _=> panic!("Unsupported unary operator")
    };
    BitInt::new(self.nbits, result, self.signed)
  }

  fn to_hash(&self) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    self.nbits.hash(&mut hasher);
    self.val.to_str_radix(10).hash(&mut hasher);
    hasher.finish()
  }
}

impl fmt::Display for BitInt {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.val)
  }
}

impl PartialEq for BitInt {
  fn eq(&self, other: &Self) -> bool {
    self.val == other.val && self.signed == other.signed
  }
}

impl Eq for BitInt {}

impl PartialOrd for BitInt {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    if self.signed == other.signed {
      Some(self.val.cmp(&other.val))
    } else {
        panic!("Cannot compare BitInt with different signedness")
    }
  }
}

impl Ord for BitInt {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.partial_cmp(other).unwrap()
  }
}

pub fn abs(x: BitInt) -> BitInt {
  if x.signed {
    if x.val < ℤ::zero() { -x } else { x }
  } else {
    x
  }
}

impl Neg for BitInt {
  type Output = Self;

  fn neg(self) -> Self::Output {
    if self.signed {
      return BitInt::new(self.nbits, -self.val, self.signed);   
    } else {
      self
    }
  }
}

impl Add for BitInt {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
    self.op_binary(&rhs, "+")
  }
}

impl Sub for BitInt {
  type Output = Self;

  fn sub(self, rhs: Self) -> Self::Output {
    self.op_binary(&rhs, "-")
  }
}

impl Mul for BitInt {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self::Output {
    self.op_binary(&rhs, "*")
  }
}

impl Div for BitInt {
  type Output = Self;

  fn div(self, rhs: Self) -> Self::Output {
    self.op_binary(&rhs, "/")
  }
}

impl Rem for BitInt {
  type Output = Self;

  fn rem(self, rhs: Self) -> Self::Output {
    self.op_binary(&rhs, "%")
  }
}

impl BitAnd for BitInt {
  type Output = Self;

  fn bitand(self, rhs: Self) -> Self::Output {
    self.op_binary(&rhs, "&")
  } 
}

impl BitOr for BitInt {
  type Output = Self;

  fn bitor(self, rhs: Self) -> Self::Output {
    self.op_binary(&rhs, "|")
  } 
}

impl BitXor for BitInt {
  type Output = Self;

  fn bitxor(self, rhs: Self) -> Self::Output {
    self.op_binary(&rhs, "^")
  }   
}

impl Not for BitInt {
  type Output = Self;

  fn not(self) -> Self::Output {
    self.op_unary("!")
  }
}

impl Shl<usize> for BitInt {
  type Output = Self;

  fn shl(self, rhs: usize) -> Self::Output {
    self.op_binary_shift(rhs, "<<")
  }
}

impl Shr<usize> for BitInt {
  type Output = Self;

  fn shr(self, rhs: usize) -> Self::Output {
    self.op_binary_shift(rhs, ">>")
  }
}