/// General purpose persistent structure with levels
pub struct Snapshot<T: Clone> {
    /// snapshots
    snapshots: Vec<T>,
}

impl<T: Clone> Snapshot<T> {
    /// Empty context
    pub fn new(content: T) -> Snapshot<T> {
        Snapshot {
            snapshots: vec![content],
        }
    }

    /// Mutable reference to current version
    pub fn current(&mut self) -> &mut T {
        self.snapshots.last_mut().unwrap()
    }

    /// Enters a new version
    pub fn enter(&mut self) {
        self.snapshots.push(self.snapshots.last().unwrap().clone());
    }

    /// Exits from current version and reverts to prior version
    pub fn exeunt(&mut self) {
        self.snapshots.pop();
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
