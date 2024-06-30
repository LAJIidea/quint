// use quantum::ast::expression::Id;

use std::sync::Arc;


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

fn main() {
    let c = Arc::new(C{id: "hello".to_string()});
    let a = c.getA();
    let b = a.getB();
    b.print();
    let mut  d = Arc::new(1);
    d = Arc::new(2);
    println!("{}", d);
}