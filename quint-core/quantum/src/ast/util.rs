use std::{cell::RefCell, rc::Rc};

use crate::ast::qtype::{Annotation, CTypeTy, QNumericTy, TypeTy};

use super::expression::RcMutExpr;

pub fn ctype () -> Rc<RefCell<CTypeTy>> {
  thread_local! {
    static INSTANCE:Rc<RefCell<CTypeTy>> = CTypeTy::new();
  }
  let instance = INSTANCE.with(|instance| instance.clone());
  let rc_instance = Rc::clone(&instance);
  instance.borrow_mut().init(rc_instance);
  instance
}


pub fn qnumeric () -> Rc<RefCell<QNumericTy>> {
  thread_local! {
    static INSTANCE:Rc<RefCell<QNumericTy>> = Rc::new(RefCell::new(QNumericTy {base: Some(ctype()), annotation: Annotation::Null}));
  }
  let instance = INSTANCE.with(|instance| instance.clone());
  instance
}

pub fn is_qnumeric(e: RcMutExpr) -> bool {
  if let Some(t) = e.borrow().expr_type() {
    return t.borrow().equals(qnumeric());
  };
  false
}

pub fn type_ty() -> Rc<RefCell<TypeTy>> {
  thread_local! {
    static INSTANCE:Rc<RefCell<TypeTy>> = Rc::new(RefCell::new(TypeTy::new()));
  }
  let instance = INSTANCE.with(|instance| instance.clone());
  instance
}
