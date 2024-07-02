use std::{borrow::BorrowMut, cell::RefCell, ptr, rc::Rc, sync::{Arc, Mutex}};
use std::collections::HashMap;
use once_cell::sync::Lazy;

use crate::ast::util::ctype;
use crate::ast::expression::{RcMutExpr, Expression, CopyArgs, Location, SemState, Identifier, Node};

pub enum Annotation {
 Null,
 Mfree,
 Qfree
}

fn is_same_type(lhs: RcMutExpr, rhs: RcMutExpr) -> bool {
  return lhs.borrow().equals(rhs);
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

fn get_numeric(which: i32, classical: bool) {
     
}

pub fn combine_types(lhs: Option<RcMutExpr>, rhs: Option<RcMutExpr>, meet: bool, allow: bool) -> Option<RcMutExpr> {
  if lhs.is_none() {
    return rhs;
  }
  if rhs.is_none() {
    return lhs;
  }
  lhs
}

pub struct CTypeTy {
  pub base: Option<Rc<RefCell<dyn Expression>>>,
}

impl CTypeTy {
  pub fn new() -> Rc<RefCell<CTypeTy>> {
    let instance = Rc::new(RefCell::new(CTypeTy { base: None }));
    instance
  }

  pub fn init(&mut self, self_rc: Rc<RefCell<dyn Expression>>) {
    self.base = Some(self_rc);
  }
}

impl Expression for CTypeTy{
  fn bracket(&self) -> usize {
    1
  }

  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn copy_impl(&self, args: CopyArgs) -> RcMutExpr {
    match self.base.clone() {
      None => ctype(),
      Some(t) => t
    }
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    match self.base.clone() {
      None => ctype(),
      Some(t) => t
    }
  }

  fn expr_type(&self) -> Option<RcMutExpr> {
      None
  }

  fn set_brackets(&mut self, brackets: usize) {
      
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
    false
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    match self.base.clone() {
      None => ctype(),
      Some(t) => t
    }
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
      false
  }

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) {
      
  }

  fn components(&self) -> Vec<RcMutExpr> {
    Vec::new()
  }

  fn subexpressions(&self) -> Vec<RcMutExpr> {
    Vec::new()
  }

  fn is_subtype(&self, rhs: &RcMutExpr) -> bool {
    false
  }

  fn is_tuple(&self) {
      
  }

  fn equals(&self, other: RcMutExpr) -> bool {
    if let Some(t) = other.borrow().downcast_ref::<CTypeTy>() {
      return true;
    }
    false
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
    print!("play")
  }
}

fn test() {
  let a = TypeTy::get_instance();
  a.paly();
}