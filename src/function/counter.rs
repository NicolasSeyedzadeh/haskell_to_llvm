pub struct Counter {
    count: usize,
}

impl Counter {
    pub fn new() -> Self {
        Counter { count: 0 }
    }
    pub fn increment(&mut self) -> usize {
        self.count += 1;
        self.count
    }
}
