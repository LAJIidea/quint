use std::{cell::RefCell, rc::Rc};

use crate::ast::qtype::CTypeTy;

pub fn ctype () -> Rc<RefCell<CTypeTy>> {
  thread_local! {
    static INSTANCE:Rc<RefCell<CTypeTy>> = CTypeTy::new();
  }
  let instance = INSTANCE.with(|instance| instance.clone());
  let rc_instance = Rc::clone(&instance);
  instance.borrow_mut().init(rc_instance);
  instance
}