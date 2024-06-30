use std::rc::Rc;
use std::{clone, fmt, ptr};
use std::collections::HashMap;
use std::sync::{Mutex, Once, Arc};
use std::hash::{Hash, Hasher};

use crate::ast::qtype::Annotation;

pub enum SemState {
  Initial,
  Started,
  Completed,
  Error
}

pub struct Location {
  line: usize,
  column: usize
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
type BoxExpr = Box<dyn Expression>;
type RcExpr = Rc<dyn Expression>;

pub trait Expression: Node {
  fn expr_type(&self) -> Option<ArcMutExpr>;
  fn set_type_expr(&mut self, type_expr: &Option<ArcMutExpr>);
  fn bracket(&self) -> usize;
  fn set_brackets(&mut self, brackets: usize);

  fn copy_impl(&self, args: CopyArgs) -> Box<dyn Expression>;
  fn eval_impl(&self, ntype: &Option<ArcMutExpr>) -> ArcMutExpr;

  fn eval(&self) -> ArcMutExpr {
    let ntype = match self.expr_type() {
        None => None,
        Some(t) => {
          if ptr::eq(self.as_ptr(), t.lock().unwrap().as_ptr()) {
            None
          } else {
            Some(t.lock().unwrap().eval())
          }
        }
    };
    let mut r = self.eval_impl(&ntype);
    if r.lock().unwrap().expr_type().is_none() {
      r.lock().unwrap().set_type_expr(&ntype)
    } else if ptr::eq(self.as_ptr(), r.lock().unwrap().as_ptr()) {
      return r;  
    } else {
      match (r.lock().unwrap().expr_type(), ntype) {
        (Some(val1), Some(val2)) => {
          if !ptr::eq(val1.lock().unwrap().as_ptr(), val2.lock().unwrap().as_ptr()) {
            panic!("this error")
          }
        }
        (Some(_), None) => panic!("this error"),
        (_, Some(_)) => panic!("this error"),
        (None, None) => {}
      }
    }
    if r.lock().unwrap().loc().line == 0 {
      r.lock().unwrap().set_loc(self.loc());
    }
    r.lock().unwrap().set_sstate(&SemState::Completed);
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
      if !ptr::eq(self.as_ptr(), type_expr.lock().unwrap().as_ptr()) {
        if type_expr.lock().unwrap().has_free_var(id) {
          return true;
        }
      }
    }
    found
  }

  fn substitute(&self, name: &str, exp: ArcMutExpr) -> ArcMutExpr {
    let mut subst = std::collections::HashMap::new();
    subst.insert(name.to_string(), exp);
    self.substitute_map(&subst)
  }

  fn substitute_map(&self, subst: &HashMap<String, ArcMutExpr>) -> ArcMutExpr {
    let mut r = self.substitute_impl(subst);
    if let Some(type_expr) = self.expr_type() {
      if r.lock().unwrap().expr_type().is_none() {
        if ptr::eq(self.as_ptr(), type_expr.lock().unwrap().as_ptr()) {
          r.lock().unwrap().set_type_expr(&Some(Arc::clone(&r)));
        } else {
          r.lock().unwrap().set_type_expr(&Some(type_expr.lock().unwrap().substitute_map(subst)));
        }
      }
    }
    r
  }

  fn substitute_impl(&self, subst: &HashMap<String, ArcMutExpr>) -> ArcMutExpr;

  fn unify_impl(&self, 
                rhs: ArcMutExpr,
                subst: &HashMap<String, ArcMutExpr>,
                meet: bool) -> bool;
  fn unify(&self, 
           rhs: ArcMutExpr,
           subst: &HashMap<String, ArcMutExpr>,
           meet: bool) -> bool {
    self.unify_impl(Arc::clone(&rhs), subst, meet) || self.eval().lock().unwrap().unify_impl(rhs, subst, meet)  }   

  fn components(&self) -> Vec<ArcMutExpr>;

  fn subexpressions(&self) -> Vec<ArcMutExpr>;

  fn is_subtype(&self, rhs: &ArcMutExpr) -> bool;
  
  fn combine_type(&self, rhs: &ArcMutExpr, meet: bool);

  fn is_tuple(&self);

  fn get_classical(&self) {}

  fn get_quantum(&self) {}

  fn get_annotation(&self) {}

  fn as_ptr(&self) -> *const () {
    self as *const _ as *const ()
  }
}


struct CopyArgs {
  prev_semantic: bool
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

struct Identifier {
  base: Option<Arc<Mutex<dyn Expression>>>,
  id: Id
}


impl Expression for Identifier {
  fn bracket(&self) -> usize {
      1
  }

  fn set_type_expr(&mut self, type_expr: &Option<ArcMutExpr>) {
      
  }

  fn copy_impl(&self, args: CopyArgs) -> Box<dyn Expression> {
    Box::new(Identifier{base: None, id: Id::intern("")})
  }

  fn eval_impl(&self, ntype: &Option<ArcMutExpr>) -> ArcMutExpr {
    Arc::new(Mutex::new(Identifier{base: None, id: Id::intern("s")}))
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
    Arc::new(Mutex::new(Identifier{base: None, id: Id::intern("s")}))
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