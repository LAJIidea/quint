use std::{borrow::BorrowMut, cell::RefCell, cmp::{max, min}, ptr, rc::Rc, result, sync::{Arc, Mutex}};
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

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum NumericType {
  Null,
  Bool,
  ℕt,
  ℤt,
  ℚt,
  ℝ,
  ℂ
}

fn which_numeric(t: RcMutExpr) -> NumericType {
  if let Some(_) = t.borrow().downcast_ref::<BoolTy>() {
    return NumericType::Bool;
  }
  if let Some(_) = t.borrow().downcast_ref::<ℝTy>() {
    return NumericType::ℝ;
  }
  if let Some(_) = t.borrow().downcast_ref::<ℚTy>() {
    return NumericType::ℚt;
  }
  if let Some(_) = t.borrow().downcast_ref::<ℕTy>() {
    return NumericType::ℕt;
  }
  if let Some(_) = t.borrow().downcast_ref::<ℤTy>() {
    return NumericType::ℤt;
  }
  if let Some(_) = t.borrow().downcast_ref::<ℂTy>() {
    return NumericType::ℂ;
  }
  NumericType::Null
}

fn get_numeric(which: NumericType, classical: bool) -> Option<RcMutExpr> {
  match which {
    NumericType::Bool => {
      return Some(Rc::new(RefCell::new(BoolTy {})));
    }
    NumericType::ℕt => {
      return Some(Rc::new(RefCell::new(BoolTy {})));
    }
    NumericType::ℤt => {
      return Some(Rc::new(RefCell::new(BoolTy {})));
    }
    NumericType::ℚt => {
      return Some(Rc::new(RefCell::new(BoolTy {})));
    }
    NumericType::ℝ => {
      return Some(Rc::new(RefCell::new(BoolTy {})));
    }
    NumericType::ℂ => {
      return Some(Rc::new(RefCell::new(BoolTy {})));
    }
    _ => None
  }
}

pub fn combine_types(lhs: Option<RcMutExpr>, rhs: Option<RcMutExpr>, meet: bool, allow: bool) -> Option<RcMutExpr> {
  if lhs.is_none() {
    return rhs;
  }
  if rhs.is_none() {
    return lhs;
  }
  if let (Some(t1), Some(t2)) = (lhs, rhs) {
    if t1.borrow().equals(t2.clone()) {
      return Some(t1);
    }
    let l = t1.borrow().eval();
    let r = t2.borrow().eval();
    let wl = which_numeric(l.clone());
    let wr = which_numeric(r.clone());
    match (&wl, &wr) {
      (NumericType::Null, NumericType::Null) => { return Some(l.borrow().combine_type(&r, meet)); }
      (NumericType::Null, _) => {return None;}
      (_, NumericType::Null) => { return None; }
      _ => {}
    }
    let result = get_numeric(if meet {min(wl, wr)} else {max(wl, wr)}, if meet {t1.borrow().is_classical() || t2.borrow().is_classical()} else { t1.borrow().is_classical() && t2.borrow().is_classical() });
    
    return result;
  }
  None
}

pub struct QNumericTy {
  pub base: Option<Rc<RefCell<dyn Expression>>>,
  pub annotation: Annotation
}

impl QNumericTy {
  fn new() -> Self {
    QNumericTy { base: Some(ctype()), annotation: Annotation::Null }
  }
}

impl Expression for QNumericTy {
  fn expr_type(&self) -> Option<RcMutExpr> {
    None
  }
  
  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn set_brackets(&mut self, brackets: usize) {
      
  }

  fn bracket(&self) -> usize {
    1
  }

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
    return exp;
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
    false
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
    false
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

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn is_tuple(&self) {
      
  }

  fn equals(&self, other: RcMutExpr) -> bool {
    if let Some(t) = other.borrow().downcast_ref::<QNumericTy>() {
      return true;
    }
    false
  }
}

impl Node for QNumericTy {
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

pub struct BoolTy {

}

impl Expression for BoolTy {
  fn expr_type(&self) -> Option<RcMutExpr> {
    None
  }
  
  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn set_brackets(&mut self, brackets: usize) {
      
  }

  fn bracket(&self) -> usize {
    1
  }

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
    false
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
    false
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

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn is_tuple(&self) {
      
  }
}

impl Node for BoolTy {
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

pub struct ℚTy {

}

impl Expression for ℚTy {
  fn expr_type(&self) -> Option<RcMutExpr> {
    None
  }
  
  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn set_brackets(&mut self, brackets: usize) {
      
  }

  fn bracket(&self) -> usize {
    1
  }

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
    false
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
    false
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

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn is_tuple(&self) {
      
  }
}

impl Node for ℚTy {
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

pub struct ℤTy {

}

impl Expression for ℤTy {
  fn expr_type(&self) -> Option<RcMutExpr> {
    None
  }
  
  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn set_brackets(&mut self, brackets: usize) {
      
  }

  fn bracket(&self) -> usize {
    1
  }

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
    false
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
    false
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

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn is_tuple(&self) {
      
  }
}

impl Node for ℤTy {
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

pub struct ℕTy {

}

impl Expression for ℕTy {
  fn expr_type(&self) -> Option<RcMutExpr> {
    None
  }
  
  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn set_brackets(&mut self, brackets: usize) {
      
  }

  fn bracket(&self) -> usize {
    1
  }

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
    false
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
    false
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

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn is_tuple(&self) {
      
  }
}

impl Node for ℕTy {
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

pub struct ℝTy {

}

impl Expression for ℝTy {
  fn expr_type(&self) -> Option<RcMutExpr> {
    None
  }
  
  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn set_brackets(&mut self, brackets: usize) {
      
  }

  fn bracket(&self) -> usize {
    1
  }

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
    false
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
    false
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

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn is_tuple(&self) {
      
  }
}

impl Node for ℝTy {
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

pub struct ℂTy {

}

impl Expression for ℂTy {
  fn expr_type(&self) -> Option<RcMutExpr> {
    None
  }
  
  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn set_brackets(&mut self, brackets: usize) {
      
  }

  fn bracket(&self) -> usize {
    1
  }

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool {
    false
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
    false
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

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
  }

  fn is_tuple(&self) {
      
  }
}

impl Node for ℂTy {
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

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
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

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(BoolTy {}))
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