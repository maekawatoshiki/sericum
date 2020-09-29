use super::ast::AST;
use super::lexer::{Error, Lexer, Result};
use rustc_hash::FxHashMap;
use std::collections::VecDeque;

pub struct Parser<'a> {
    pub lexer: &'a mut Lexer,
}

pub struct Env<T: Clone>(pub VecDeque<FxHashMap<String, T>>);

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Parser<'a> {
        Self { lexer }
    }

    pub fn parse(&mut self) -> Result<Vec<AST>> {
        let mut node = vec![];
        while let Ok(n) = self.read_toplevel() {
            node.push(n);
        }
        Ok(node)
    }

    pub fn parse_as_expr(&mut self) -> Result<AST> {
        todo!()
    }

    fn read_toplevel(&mut self) -> Result<AST> {
        todo!()
    }
}

impl<T: Clone> Env<T> {
    pub fn new() -> Env<T> {
        let mut env = VecDeque::new();
        env.push_back(FxHashMap::default());
        Env(env)
    }

    pub fn push(&mut self) {
        let localenv = (*self.0.back().unwrap()).clone();
        self.0.push_back(localenv);
    }

    pub fn pop(&mut self) {
        self.0.pop_back();
    }

    pub fn add(&mut self, name: String, val: T) {
        self.0.back_mut().unwrap().insert(name, val);
    }

    pub fn add_global(&mut self, name: String, val: T) {
        // self.0[0].insert(name.clone(), val.clone());
        // self.0.back_mut().unwrap().insert(name, val);
        for env in &mut self.0 {
            env.insert(name.clone(), val.clone());
        }
    }

    pub fn is_local(&self) -> bool {
        self.0.len() > 1
    }

    pub fn back_mut(&mut self) -> Option<&mut FxHashMap<String, T>> {
        self.0.back_mut()
    }

    pub fn get(&mut self, name: &str) -> Option<&T> {
        self.0.back_mut().unwrap().get(name)
    }

    pub fn contains(&mut self, name: &str) -> bool {
        self.0.back_mut().unwrap().contains_key(name)
    }
}
