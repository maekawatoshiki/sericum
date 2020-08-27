use crate::codegen::common::machine::{frame_object::*, function::*, module::*};
use crate::traits::pass::ModulePassTrait;
use rustc_hash::FxHashSet;

pub struct EliminateFrameIndex {}

pub struct EliminateFrameIndexOnFunction<'a> {
    func: &'a mut MachineFunction,
}

impl ModulePassTrait for EliminateFrameIndex {
    type M = MachineModule;

    fn name(&self) -> &'static str {
        "EliminateFrameIndex"
    }

    fn run_on_module(&mut self, module: &mut Self::M) {
        self.run_on_module(module)
    }
}

impl EliminateFrameIndex {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_on_module(&mut self, module: &mut MachineModule) {
        for (_, func) in &mut module.functions {
            EliminateFrameIndexOnFunction { func }.run();
        }
    }
}

impl<'a> EliminateFrameIndexOnFunction<'a> {
    pub fn run(&mut self) {
        if self.func.is_internal {
            return;
        }

        let mut mem_insts = vec![];
        let mut m = FxHashSet::default();

        for (_bb_id, _bb, iseq) in self.func.body.mbb_iter() {
            for (id, inst) in iseq {
                if let Some(&FrameIndexInfo {
                    idx: FrameIndexKind::Local(i),
                    ..
                }) = Self::get_frame_index(inst)
                {
                    m.insert(i);
                }
                mem_insts.push(id);
            }
        }

        let frame_objects = FrameObjectsInfo::new(&self.func.types, self.func);

        for id in mem_insts {
            let inst = &mut self.func.body.inst_arena[id];
            Self::replace_frame_index(&frame_objects, inst);
        }

        self.func.local_mgr.locals.retain(|local| match local.idx {
            FrameIndexKind::Arg(_) => true,
            FrameIndexKind::Local(i) if m.contains(&i) => true,
            FrameIndexKind::Local(_) => false,
        });
    }
}
