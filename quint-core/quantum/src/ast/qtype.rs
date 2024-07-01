use std::{borrow::BorrowMut, ptr, sync::{Arc, Mutex}};
use std::collections::HashMap;
use once_cell::sync::Lazy;

use crate::ast::expression::{ArcMutExpr, Expression, CopyArgs, Location, SemState, Identifier, Node};

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

struct CTypeTy {
  base: Option<Arc<Mutex<dyn Expression>>>,
}

impl CTypeTy {
  fn new() -> Arc<Mutex<Self>> {
    let mut instance = Arc::new(Mutex::new(CTypeTy { base: None}));
    {
      let mut mut_instance = instance.borrow_mut();
      mut_instance.lock().unwrap().base = Some(Arc::clone(&instance))
    }
    instance
  }
}

impl Expression for CTypeTy{
  fn bracket(&self) -> usize {
    1
}

fn set_type_expr(&mut self, type_expr: &Option<ArcMutExpr>) {
    
}

fn copy_impl(&self, args: CopyArgs) -> Box<dyn Expression> {
  Box::new(CTypeTy {base: None})
}

fn eval_impl(&self, ntype: &Option<ArcMutExpr>) -> ArcMutExpr {
  Arc::new(Mutex::new(CTypeTy {base: None}))
}

fn expr_type(&self) -> Option<ArcMutExpr> {
    None
}

fn set_brackets(&mut self, brackets: usize) {
    
}

fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
  false
}

fn substitute_impl(&self, subst: &HashMap<String, ArcMutExpr>) -> ArcMutExpr {
  Arc::new(Mutex::new(CTypeTy {base: None}))
}

fn unify_impl(&self, 
                rhs: ArcMutExpr,
                subst: &HashMap<String, ArcMutExpr>,
                meet: bool) -> bool {
    false
}

fn combine_type(&self, rhs: &ArcMutExpr, meet: bool) {
    
}

fn components(&self) -> Vec<ArcMutExpr> {
  Vec::new()
}

fn subexpressions(&self) -> Vec<ArcMutExpr> {
  Vec::new()
}

fn is_subtype(&self, rhs: &ArcMutExpr) -> bool {
  false
}

fn is_tuple(&self) {
    
}
}

impl Node for CTypeTy {
  fn loc(&self) -> &Location {
      &Location { line: 0, column: 0 }
  }

  fn kind(&self) -> &str {
      "id"
  }

  fn sstate(&self) -> &SemState {
      &SemState::Completed
  }

  fn set_loc(&self, location: &Location) {
      
  }

  fn set_sstate(&self, ss: &SemState) {
      
  }
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