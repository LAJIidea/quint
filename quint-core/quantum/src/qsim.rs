mod state;
mod variable;

struct QSim {
  sourcefile: String,
}

impl QSim {
  fn new(sourcefile: String) -> QSim {
    QSim { sourcefile }
  }

  fn run(&self) {
    println!("Running simulation from file: {}", self.sourcefile);
  }
}