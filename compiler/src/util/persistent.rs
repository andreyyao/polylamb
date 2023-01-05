/// General purpose persistent structure with levels
pub struct Persistent<T: Clone> {
    /// snapshots
    versions: Vec<T>,
}

impl<T: Clone> Persistent<T> {
    /// Empty context
    pub fn new(content: T) -> Persistent<T> {
        Persistent {
            versions: vec![content],
        }
    }

    /// Mutable reference to current version
    pub fn current(&mut self) -> &mut T {
        self.versions.last_mut().unwrap()
    }

    /// Enters a new version
    pub fn enter(&mut self) {
        self.versions.push(self.versions.last().unwrap().clone());
    }

    /// Exits from current version and reverts to prior version
    pub fn exeunt(&mut self) {
        self.versions.pop();
    }
}
