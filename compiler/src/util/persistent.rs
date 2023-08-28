/// General purpose persistent structure with levels
pub struct Snapshot<T: Clone + Default> {
    /// snapshots
    snapshots: Vec<T>,
}

impl<T: Clone + Default> Snapshot<T> {
    pub fn new(initial: T) -> Self {
        Snapshot {
            snapshots: vec![initial],
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

impl<T: Clone + Default> Default for Snapshot<T> {
    fn default() -> Snapshot<T> {
        Snapshot {
            snapshots: vec![T::default()],
        }
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
