use std::{ptr, sync::{Arc, Mutex}};

use once_cell::sync::Lazy;

use crate::ast::expression::{ArcMutExpr, Expression};

pub enum Annotation {
 Null,
 Mfree,
 Qfree
}

fn is_same_type(lhs: ArcMutExpr, rhs: ArcMutExpr) -> bool {
  return ptr::eq(lhs.lock().unwrap().eval().lock().unwrap().as_ptr(), rhs.lock().unwrap().eval().lock().unwrap().as_ptr());
}

enum NumericType {
  Null,
  Bool,
  ℕt,
  ℤt,
  ℚt,
  ℝ,
  ℂ
}

struct TypeTy {}

static INSTANCE: Lazy<TypeTy> = Lazy::new(|| {
  TypeTy {  }
});

impl TypeTy {
  fn get_instance() -> &'static TypeTy {
    &INSTANCE
  }
  fn paly(&self) {
    
  }
}

fn test() {
  let a = TypeTy::get_instance();
  a.paly();
}