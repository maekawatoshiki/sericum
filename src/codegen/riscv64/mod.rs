pub mod asm;
pub mod dag;
pub mod exec;
pub mod frame_object;
pub mod machine;

use crate::{
    codegen::common::{dag::combine, machine::module::MachineModule},
    ir,
    ir::module::Module,
    ir::types::*,
    traits::pass::ModulePassManager,
};

impl TypeSize for Type {
    fn size_in_byte(&self, tys: &Types) -> usize {
        match self {
            Type::Int1 => 1,
            Type::Int8 => 1,
            Type::Int32 => 4,
            Type::Int64 => 8,
            Type::F64 => 8,
            Type::Array(id) => tys.base.borrow().non_primitive_types[*id]
                .as_array()
                .size_in_byte(tys),
            Type::Struct(id) => tys.base.borrow().non_primitive_types[*id]
                .as_struct()
                .size_in_byte(tys),
            Type::Pointer(_) => 8,
            Type::Function(_) => unimplemented!(),
            Type::Void => 0,
        }
    }

    fn size_in_bits(&self, tys: &Types) -> usize {
        self.size_in_byte(tys) * 8
    }
}

impl TypeSize for ArrayType {
    fn size_in_byte(&self, tys: &Types) -> usize {
        self.elem_ty.size_in_byte(tys) * self.len
    }

    fn size_in_bits(&self, tys: &Types) -> usize {
        self.size_in_byte(tys) * 8
    }
}

impl TypeSize for StructType {
    fn size_in_byte(&self, tys: &Types) -> usize {
        let mut size_total = 0;
        let calc_padding = |off, align| -> usize {
            if off % align == 0 {
                0
            } else {
                align - off % align
            }
        };
        for ty in &self.fields_ty {
            size_total += {
                let size = ty.size_in_byte(tys);
                size + calc_padding(size_total, size)
            };
        }
        size_total
    }

    fn size_in_bits(&self, tys: &Types) -> usize {
        self.size_in_byte(tys) * 8
    }
}

pub fn standard_conversion_into_machine_module(module: &mut Module) -> MachineModule {
    ir::gather_ret::GatherReturns::new().run_on_module(module);

    let mut dag_module = dag::convert::ConvertToDAG::new(module).convert_module();

    let mut pass_mgr = ModulePassManager::new();
    pass_mgr.add_pass(combine::Combine::new());
    pass_mgr.add_pass(dag::legalize::Legalize::new());
    pass_mgr.add_pass(dag::isel::MISelector::new());
    pass_mgr.run_on_module(&mut dag_module);
    // println!("{:?}", dag_module);

    let mut machine_module = dag::mc_convert::convert_module(dag_module);

    let mut pass_mgr = ModulePassManager::new();
    pass_mgr.add_pass(machine::phi_elimination::PhiElimination::new());
    pass_mgr.add_pass(machine::branch_folding::BranchFolding::new());
    // pass_mgr.add_pass(machine::two_addr::TwoAddressConverter::new());
    pass_mgr.add_pass(machine::regalloc::RegisterAllocator::new());
    pass_mgr.add_pass(machine::pro_epi_inserter::PrologueEpilogueInserter::new());
    pass_mgr.add_pass(machine::replace_copy::ReplaceCopyWithProperMInst::new());
    // pass_mgr.add_pass(machine::replace_data::ReplaceConstFPWithMemoryRef::new());
    pass_mgr.run_on_module(&mut machine_module);

    machine_module
}
