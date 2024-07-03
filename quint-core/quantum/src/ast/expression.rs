use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;
use std::{clone, fmt, ptr};
use std::collections::HashMap;
use std::sync::{Mutex, Once, Arc};
use std::hash::{Hash, Hasher};
use downcast_rs::{Downcast, impl_downcast};

use crate::ast::qtype::Annotation;

pub enum SemState {
  Initial,
  Started,
  Completed,
  Error
}

pub struct Location {
  pub line: usize,
  pub column: usize
}

pub trait Node {
  fn loc(&self) -> &Location;
  fn kind(&self) -> &str;
  fn sstate(&self) -> &SemState;
  fn set_loc(&self, location: &Location);
  fn set_sstate(&self, ss: &SemState);
}

type ArcExpr = Arc<dyn Expression>;
pub type ArcMutExpr = Arc<Mutex<dyn Expression>>;
pub type RcMutExpr = Rc<RefCell<dyn Expression>>;
type BoxExpr = Box<dyn Expression>;
type RcExpr = Rc<dyn Expression>;

pub trait Expression: Node + Downcast {
  fn expr_type(&self) -> Option<RcMutExpr>;
  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>);
  fn bracket(&self) -> usize;
  fn set_brackets(&mut self, brackets: usize);

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr;
  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr;

  fn eval(&self) -> RcMutExpr {
    let ntype = match self.expr_type() {
        None => None,
        Some(t) => {
          if self.equals(t.clone()) {
            None
          } else {
            Some(t.borrow().eval())
          }
        }
    };
    let mut r = self.eval_impl(&ntype);
    if r.borrow().expr_type().is_none() {
      r.borrow_mut().set_type_expr(&ntype)
    } else if self.equals(r.clone()) {
      return r;  
    } else {
      match (r.borrow().expr_type(), ntype) {
        (Some(val1), Some(val2)) => {
          if !val1.borrow().equals(val2) {
            panic!("this error")
          }
        }
        (Some(_), None) => panic!("this error"),
        (_, Some(_)) => panic!("this error"),
        (None, None) => {}
      }
    }
    if r.borrow().loc().line == 0 {
      r.borrow().set_loc(self.loc());
    }
    r.borrow().set_sstate(&SemState::Completed);
    r 
  }

  fn free_vars_impl(&self, f: &mut dyn FnMut(Identifier) -> bool) -> bool;

  fn has_free_var(&self, id: &Id) -> bool {
    let mut found = false;
    if self.free_vars_impl(&mut |ident| {
      if ident.id == *id {
        return true;
      }
      false
    }) {
      return true;
    }

    if let Some(type_expr) = &self.expr_type() {
      if !self.equals(type_expr.clone()) {
        if type_expr.borrow().has_free_var(id) {
          return true;
        }
      }
    }
    found
  }

  fn substitute(&self, name: &str, exp: RcMutExpr) -> RcMutExpr {
    let mut subst = std::collections::HashMap::new();
    subst.insert(name.to_string(), exp);
    self.substitute_map(&subst)
  }

  fn substitute_map(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr {
    let mut r = self.substitute_impl(subst);
    if let Some(type_expr) = self.expr_type() {
      if r.borrow().expr_type().is_none() {
        if self.equals(type_expr.clone()) {
          r.borrow_mut().set_type_expr(&Some(Rc::clone(&r)));
        } else {
          r.borrow_mut().set_type_expr(&Some(type_expr.borrow().substitute_map(subst)));
        }
      }
    }
    r
  }

  fn substitute_impl(&self, subst: &HashMap<String, RcMutExpr>) -> RcMutExpr;

  fn unify_impl(&self, 
                rhs: RcMutExpr,
                subst: &HashMap<String, RcMutExpr>,
                meet: bool) -> bool;
  fn unify(&self, 
           rhs: RcMutExpr,
           subst: &HashMap<String, RcMutExpr>,
           meet: bool) -> bool {
    self.unify_impl(Rc::clone(&rhs), subst, meet) || self.eval().borrow().unify_impl(rhs, subst, meet)  
  }   

  fn components(&self) -> Vec<RcMutExpr>;

  fn subexpressions(&self) -> Vec<RcMutExpr>;

  fn is_subtype(&self, rhs: &RcMutExpr) -> bool;
  
  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr;

  fn is_tuple(&self);

  fn get_classical(&self) {}

  fn get_quantum(&self) {}

  fn get_annotation(&self) {}

  fn is_classical(&self) -> bool { false }

  fn equals(&self, other: RcMutExpr) -> bool {
    ptr::eq(self.as_ptr(), other.borrow().as_ptr())
  }

  fn as_ptr(&self) -> *const () {
    self as *const _ as *const ()
  }
}

impl_downcast!(Expression);

pub struct CopyArgs {
  pub prev_semantic: bool
}

#[derive(Clone, Debug)]
pub struct Id {
  // Rc用于单线程中，Arc用于多线程中, Arc保证原子性
  str: Option<Arc<String>>
}

impl Id {
  fn new()  -> Self{
    Self { str: None }
  }

  pub fn intern(s: &str) -> Id {
    static INIT: Once = Once::new();
    static mut UNIQ: Option<Mutex<HashMap<String, Arc<String>>>> = None;

    INIT.call_once(|| {
      unsafe {
        UNIQ = Some(Mutex::new(HashMap::new()));
      }
    });

    let mut uniq = unsafe {
        UNIQ.as_ref().unwrap().lock().unwrap()
    };

    // if let Some(interned_str) = uniq.get(s) {
    //     Id::from_interned_string(interned_str.clone())
    // } else {
    //     let interned_str = s.to_string();
    //     uniq.insert(interned_str.clone(), interned_str.clone());
    //     Id::from_interned_string(interned_str)
    // }
    let interned_str = uniq.entry(s.to_string()).or_insert_with(|| Arc::new(s.to_string()));
    Id::from_interned_string(interned_str.clone())
  }

  fn from_interned_string(s: Arc<String>) -> Id {
    let mut id = Id::new();
    if !s.is_empty() {
      id.str = Some(s);
    }
    id
  }

  fn to_string(&self) -> &str {
    match &self.str {
      Some(s) => s,
      None => ""
    }
  }
}

impl PartialEq for Id {
  fn eq(&self, other: &Self) -> bool {
    match (&self.str, &other.str) {
        (Some(s1), Some(s2)) => ptr::eq(s1.as_ptr(), s2.as_ptr()),
        (None, None) => true,
        _=> false,
    }
  }
}

impl Eq for Id {}

impl std::fmt::Display for Id {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}

pub struct Identifier {
  pub base: Option<Arc<Mutex<dyn Expression>>>,
  pub id: Id
}


impl Expression for Identifier {
  fn bracket(&self) -> usize {
      1
  }

  fn set_type_expr(&mut self, type_expr: &Option<RcMutExpr>) {
      
  }

  fn copy_impl(&self, exp: RcMutExpr, args: CopyArgs) -> RcMutExpr {
    Rc::new(RefCell::new(Identifier{base: None, id: Id::intern("s")}))
  }

  fn eval_impl(&self, ntype: &Option<RcMutExpr>) -> RcMutExpr {
    Rc::new(RefCell::new(Identifier{base: None, id: Id::intern("s")}))
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
    Rc::new(RefCell::new(Identifier{base: None, id: Id::intern("s")}))
  }

  fn unify_impl(&self, 
                  rhs: RcMutExpr,
                  subst: &HashMap<String, RcMutExpr>,
                  meet: bool) -> bool {
      false
  }

  fn combine_type(&self, rhs: &RcMutExpr, meet: bool) -> RcMutExpr {
    Rc::new(RefCell::new(Identifier{base: None, id: Id::intern("s")}))
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
}

impl Node for Identifier {
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