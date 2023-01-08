/// General purpose persistent structure with levels
pub struct Persist<T: Clone> {
    /// snapshots
    versions: Vec<T>,
}

impl<T: Clone> Persist<T> {
    /// Empty context
    pub fn new(content: T) -> Persist<T> {
        Persist {
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

/// Adventures into next level
macro_rules! adventure {
    ($v: ident, $e:expr, $ctxt_name: ident) => {
        $ctxt_name.enter();
        let $v = $e;
        $ctxt_name.exeunt()
    };
}

pub(crate) use adventure;
