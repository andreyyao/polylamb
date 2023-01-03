use std::collections::HashMap;

/// General purpose context
pub struct Context<T: Clone> {
    /// Binding from id to the value, basically
    map: HashMap<String, T>,
    /// What to revert to. `None` means to remove it from `bindings`
    diffs: Vec<Vec<(String, Option<T>)>>,
}

impl<T: Clone> Context<T> {
    /// Empty context
    pub fn new() -> Context<T> {
        Context {
            map: HashMap::new(),
            diffs: vec![vec![]],
        }
    }

    /// Retrieve value of id
    pub fn get(&self, id: &str) -> &T {
        self.map.get(id).unwrap()
    }

    /// Bind a id to the current context
    pub fn bind(&mut self, id: &str, val: &T) {
        let old = self.map.insert(id.to_string(), val.clone());
        let top = self.diffs.last_mut().unwrap();
        top.push((id.to_string(), old));
    }

    /// Enters a local context
    pub fn enter(&mut self) {
        self.diffs.push(vec![]);
    }

    /// Exits from a local context and reverts to parent context
    pub fn exeunt(&mut self) {
        let diff = self.diffs.pop().unwrap();
        for (name, bind) in diff {
            match bind {
                Some(typ) => self.map.insert(name, typ),
                None => self.map.remove(&name),
            };
        }
    }
}
