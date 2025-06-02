use std::ops;

/// A heap-allocated with EVM-style helper operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack<T>(Vec<T>);

impl<T> ops::Deref for Stack<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> ops::DerefMut for Stack<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Stack<T> {
    pub fn from_vec(vec: Vec<T>) -> Self {
        Self(vec)
    }

    pub fn get_top_down(&self, i: usize) -> Option<&T> {
        self.get(self.len() - i)
    }

    pub fn push(&mut self, value: T) -> &mut T {
        self.0.push(value);
        unsafe { self.0.last_mut().unwrap_unchecked() }
    }
}

impl<T: Clone> Stack<T> {
    /// Duplicate the item that's `depth` deep (1-indexed) to the top.
    ///
    /// # Example
    /// ```
    /// use eth_ir_backend::Stack;
    /// let mut stack = Stack::from_vec(vec![30, 20, 10]);
    /// stack.dup(2);
    /// assert_eq!(stack.as_slice(), &[30, 20, 10, 20]);
    /// ```
    pub fn dup(&mut self, depth: usize) -> &mut T {
        let value = self[self.len() - depth].clone();
        self.push(value)
    }

    /// Swap the item that's `depth` deep (1-indexed) relative to the top element with the top.
    ///
    /// # Example
    /// ```
    /// use eth_ir_backend::Stack;
    /// let mut stack = Stack::from_vec(vec![30, 20, 10]);
    /// stack.swap_to_top(1);
    /// assert_eq!(stack.as_slice(), &[30, 10, 20]);
    /// ```
    pub fn swap_to_top(&mut self, depth: usize) {
        let len = self.len();
        self.swap(len - 1, len - depth - 1);
    }
}

impl<T: PartialEq> Stack<T> {
    pub fn get_lowest_depth(&self, value: &T) -> Option<usize> {
        self.iter().rev().position(|x| x == value).map(|i| i + 1)
    }
}

impl<T> FromIterator<T> for Stack<T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        Self::from_vec(Vec::from_iter(iter))
    }
}
