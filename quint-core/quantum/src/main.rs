use quantum::ast::expression::{Expression, Id, Identifier};

use std::{ptr, rc::Rc, sync::{Arc, Mutex}};


trait B: Send + Sync {
    // fn getA(self: Arc<Self>) -> A;
    // fn print(&self);
    fn getA(self: Arc<Self>) -> A 
    where Self: Sized + 'static{
        A { base: self }
    }
    fn print(&self);

    // fn test() -> Option<Box<dyn B>>;
  }
  
  struct A {
    base: Arc<dyn B>
  }

  impl A {
    fn getB(&self) -> Arc<dyn B> {
        self.base.clone()
    }
  }

  struct C {
    id: String
  }

  impl B for C {
    // fn getA(self: Arc<Self>) -> A {
    //     A { base: self }
    // }
    fn print(&self) {
        print!("{}", self.id)
    }
  }

struct D {
  ty: Box<D>,
  id: i32
}

impl D {
  fn set_id(&mut self, id: i32) {
    self.id = id;
  }
}

fn main() {
    let c = Arc::new(C{id: "hello".to_string()});
    let a = c.getA();
    let b = a.getB();
    b.print();
    let mut  d = Arc::new(1);
    d = Arc::new(2);
    println!("{}", d);
    // let mut e = Box::new(D{id: 1, Box::new()});
    let var = Identifier{base: None, id: Id::intern("")};
    let ptr1 = var.as_ptr();
    let ptr2 = var.as_ptr();
    if ptr::eq(ptr1, ptr2) {
      print!("true")
    }

    let var1 = Arc::new(Mutex::new(Identifier{base: None, id: Id::intern("2")}));
    let va2 = Arc::clone(&var1);
    if ptr::eq(var1.lock().unwrap().as_ptr(), va2.lock().unwrap().as_ptr()) {
      println!("true")
    }
}