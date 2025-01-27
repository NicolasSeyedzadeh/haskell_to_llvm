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

    /// Get the current value of the counter without incrementing
    pub fn value(&self) -> usize {
        self.count
    }
}
