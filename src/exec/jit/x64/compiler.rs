// TODO: Better assembler than dynasm-rs?

use super::regalloc;
use crate::{
    ir::{basic_block::*, function::*, module::*, opcode::*, types::*, value::*},
    pass::*,
};
use dynasmrt::*;
use rustc_hash::FxHashMap;

const REGISTER_OFFSET: usize = 10; // Instruction.reg.reg=0 means r10

#[rustfmt::skip]
macro_rules! reg {
    ($f:expr; $instr_id:expr) => {{
        $f.instr_table[$instr_id].reg.borrow().reg.unwrap()
            .shift(REGISTER_OFFSET).as_u8()
    }};
    ($instr:expr) => {{
        $instr.reg.borrow().reg.unwrap()
            .shift(REGISTER_OFFSET).as_u8()
    }};
}

#[rustfmt::skip]
macro_rules! vreg {
    ($f:expr ; $instr_id:expr) => {{
        $f.instr_table[$instr_id].vreg
    }};
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericValue {
    Int32(i32),
    None,
}

#[derive(Debug)]
pub struct AllocaManager {
    locals: FxHashMap<VirtualRegister, usize>,
    cur_size: usize,
}

pub struct JITCompiler<'a> {
    module: &'a Module,
    asm: x64::Assembler,
    function_map: FxHashMap<FunctionId, DynamicLabel>,
    alloca_mgr: AllocaManager,
    internal_func: FxHashMap<String, u64>, // fn
    pass_mgr: PassManager,
    phi_map: FxHashMap<BasicBlockId, (Value, InstructionId)>,
}

impl<'a> JITCompiler<'a> {
    pub fn new(module: &'a Module) -> Self {
        regalloc::RegisterAllocator::new(module).analyze();

        Self {
            module,
            asm: x64::Assembler::new().unwrap(),
            function_map: FxHashMap::default(),
            alloca_mgr: AllocaManager::new(),
            internal_func: {
                vec![("cilk.println.i32".to_string(), cilk_println_i32 as _)]
                    .into_iter()
                    .collect::<FxHashMap<_, _>>()
            },
            pass_mgr: {
                let mut pass_mgr = PassManager::new();
                pass_mgr.add_pass(Box::new(
                    dead_code_elimination::DeadCodeEliminationPass::new(),
                ));
                pass_mgr
            },
            phi_map: FxHashMap::default(),
        }
    }

    pub fn add_pass(&mut self, pass: Box<dyn Pass>) {
        self.pass_mgr.add_pass(pass);
    }

    pub fn run(&mut self, id: FunctionId, args: Vec<GenericValue>) -> GenericValue {
        let f_entry = self.get_function_entry_label(id);
        let entry = self.asm.offset();

        let rsp_offset = if args.len() % 2 != 0 {
            dynasm!(self.asm; sub rsp, 8);
            8
        } else {
            0
        };

        for arg in args.iter().rev() {
            match arg {
                GenericValue::Int32(i) => {
                    dynasm!(self.asm
                        ; mov r11, *i
                        ; push r11);
                }
                GenericValue::None => unreachable!(),
            }
        }

        dynasm!(self.asm
                ; call =>f_entry
                ; add rsp, 8*(args.len() as i32)+rsp_offset
                ; ret);

        self.asm.commit();
        let executor = self.asm.reader();
        let buf = executor.lock();
        let f: extern "C" fn() -> u64 = unsafe { ::std::mem::transmute(buf.ptr(entry)) };

        match &self
            .module
            .function_ref(id)
            .ty
            .get_function_ty()
            .unwrap()
            .ret_ty
        {
            Type::Int32 => GenericValue::Int32(f() as i32),
            Type::Void => {
                f();
                GenericValue::None
            }
            _ => unimplemented!(),
        }
    }

    pub fn compile_module(&mut self) {
        self.pass_mgr.run_as_necessary(&self.module);

        for (f_id, _) in &self.module.functions {
            self.compile_function(f_id);
        }
    }

    fn compile_function(&mut self, id: FunctionId) {
        let f = self.module.function_ref(id);
        let f_entry = self.get_function_entry_label(id);

        self.collect_phi(f);
        self.collect_alloca(f);

        let local_size = self.alloca_mgr.local_size() as i32;

        dynasm!(self.asm
            ; => f_entry
            ; push rbp
            ; mov rbp, rsp
            ; sub rsp, roundup(local_size + 8, 16) - 8
        );

        let mut bbs: FxHashMap<BasicBlockId, DynamicLabel> = FxHashMap::default();

        for (bb_id, bb) in &f.basic_blocks {
            if bb_id.index() != 0 {
                let label = *bbs
                    .entry(bb_id)
                    .or_insert_with(|| self.asm.new_dynamic_label());
                dynasm!(self.asm; =>label);
            }

            for val in &*bb.iseq.borrow() {
                let instr_id = val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];

                match &instr.opcode {
                    Opcode::Alloca(_) | Opcode::Phi(_) => {}
                    Opcode::Load(op1) => self.compile_load(&f, &instr, op1),
                    Opcode::Store(op1, op2) => self.compile_store(&f, op1, op2),
                    Opcode::Add(op1, op2) => self.compile_add(&f, &instr, op1, op2),
                    Opcode::Sub(v1, v2) => {
                        let rn = reg!(instr);
                        match (v1, v2) {
                            (
                                Value::Argument(ArgumentValue { index, .. }),
                                Value::Immediate(ImmediateValue::Int32(i)),
                            ) => dynasm!(self.asm
                                    ; mov Ra(rn), [rbp+8*(2+*index as i32)]
                                    ; sub Ra(rn), *i),
                            (Value::Argument(a1), Value::Argument(a2)) => dynasm!(self.asm
                                    ; mov Ra(rn), [rbp+8*(2+a1.index as i32)]
                                    ; sub Ra(rn), [rbp+8*(2+a2.index as i32)]),
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::Mul(v1, v2) => {
                        let rn = reg!(instr);
                        match (v1, v2) {
                            (
                                Value::Instruction(InstructionValue { id: id1, .. }),
                                Value::Instruction(InstructionValue { id: id2, .. }),
                            ) => {
                                let reg1 = reg!(f; *id1);
                                let reg2 = reg!(f; *id2);
                                dynasm!(self.asm; mov Ra(rn), Ra(reg1));
                                dynasm!(self.asm; imul Ra(rn), Ra(reg2));
                            }
                            (
                                Value::Instruction(InstructionValue { id: id1, .. }),
                                Value::Immediate(ImmediateValue::Int32(i)),
                            ) => {
                                let reg1 = reg!(f; *id1);
                                dynasm!(self.asm; imul Ra(rn), Ra(reg1), *i);
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::Rem(v1, v2) => {
                        let rn = reg!(instr);
                        match (v1, v2) {
                            (
                                Value::Argument(ArgumentValue { index, .. }),
                                Value::Instruction(InstructionValue { id: id1, .. }),
                            ) => {
                                let reg1 = reg!(f; *id1);
                                dynasm!(self.asm
                                    ; mov rax, [rbp+8*(2+*index as i32)]
                                    ; mov rdx, 0
                                    ; idiv Ra(reg1)
                                    ; mov Ra(rn), rdx);
                            }
                            (
                                Value::Argument(ArgumentValue { index, .. }),
                                Value::Immediate(ImmediateValue::Int32(i)),
                            ) => {
                                dynasm!(self.asm
                                    ; mov rax, [rbp+8*(2+*index as i32)]
                                    ; mov Ra(rn), *i
                                    ; mov rdx, 0
                                    ; idiv Ra(rn)
                                    ; mov Ra(rn), rdx);
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Opcode::Call(callee, args) => self.compile_call(&f, &instr, callee, args),
                    Opcode::CondBr(cond, b1, b2) => match cond {
                        Value::Instruction(iv) => {
                            let rn = reg!(f; iv.id);
                            dynasm!(self.asm ; cmp Rb(rn), 1);
                            let l1 = *bbs
                                .entry(*b1)
                                .or_insert_with(|| self.asm.new_dynamic_label());
                            let l2 = *bbs
                                .entry(*b2)
                                .or_insert_with(|| self.asm.new_dynamic_label());

                            self.merge_phi(f, bb_id);

                            dynasm!(self.asm ; je =>l1 ; jmp =>l2);
                        }
                        _ => unimplemented!(),
                    },
                    Opcode::Br(bb) => {
                        let label = *bbs
                            .entry(*bb)
                            .or_insert_with(|| self.asm.new_dynamic_label());

                        self.merge_phi(f, bb_id);

                        dynasm!(self.asm; jmp =>label);
                    }
                    Opcode::ICmp(kind, v1, v2) => {
                        let reg_num = reg!(instr);
                        match kind {
                            ICmpKind::Le => match (v1, v2) {
                                (Value::Argument(arg), Value::Immediate(n)) => {
                                    dynasm!(self.asm
                                            ; cmp QWORD [rbp+8*(2+arg.index as i32)], n.as_int32());
                                    dynasm!(self.asm; setle Rb(reg_num));
                                }
                                (Value::Instruction(iv), Value::Argument(arg)) => {
                                    let rn = reg!(f; iv.id);
                                    dynasm!(self.asm
                                            ; cmp Rd(rn), [rbp+8*(2+arg.index as i32)]
                                            ; setle Rb(reg_num as u8));
                                }
                                (
                                    Value::Instruction(iv),
                                    Value::Immediate(ImmediateValue::Int32(i)),
                                ) => {
                                    let rn = reg!(f; iv.id);
                                    dynasm!(self.asm
                                            ; cmp Rd(rn), *i
                                            ; setle Rb(reg_num as u8));
                                }
                                _ => unimplemented!(),
                            },
                            ICmpKind::Eq => match (v1, v2) {
                                (Value::Argument(arg), Value::Immediate(n)) => {
                                    dynasm!(self.asm
                                        ; cmp QWORD [rbp+8*(2+arg.index as i32)], n.as_int32()
                                        ; sete Rb(reg_num as u8));
                                }
                                (Value::Instruction(instr), Value::Immediate(n)) => {
                                    let reg1 = reg!(f; instr.id);
                                    dynasm!(self.asm
                                        ; cmp Ra(reg1), n.as_int32()
                                        ; sete Rb(reg_num as u8));
                                }
                                e => unimplemented!("{:?}", e),
                            },
                        }
                    }
                    Opcode::Ret(v) => {
                        match v {
                            Value::Instruction(iv) => {
                                let rn = reg!(f; iv.id);
                                dynasm!(self.asm; mov eax, Rd(rn));
                            }
                            Value::Immediate(ImmediateValue::Int32(x)) => {
                                dynasm!(self.asm ; mov eax, *x);
                            }
                            Value::None => {}
                            _ => unimplemented!(),
                        }
                        dynasm!(self.asm; mov rsp, rbp; pop rbp; ret);
                    }
                }
            }
        }

        self.alloca_mgr.reset();
    }

    fn get_function_entry_label(&mut self, f_id: FunctionId) -> DynamicLabel {
        if self.function_map.contains_key(&f_id) {
            return *self.function_map.get(&f_id).unwrap();
        }

        let f_entry = self.asm.new_dynamic_label();
        self.function_map.insert(f_id, f_entry);
        f_entry
    }

    fn push_args(&mut self, f: &Function, args: &[Value]) {
        for arg in args.iter().rev() {
            match arg {
                Value::Instruction(InstructionValue { id, .. }) => {
                    dynasm!(self.asm; push Ra(reg!(f; *id)))
                }
                Value::Immediate(ImmediateValue::Int32(i)) => {
                    dynasm!(self.asm; mov rax, *i; push rax)
                }
                Value::Argument(arg) => dynasm!(self.asm; push AWORD [rbp+8*(2+arg.index as i32)]),
                _ => unimplemented!(),
            }
        }
    }

    fn assign_args_to_regs(&mut self, caller: &Function, args: &[Value]) {
        let pregs = vec![7, 6, 2, 1, 8, 9]; // rdi, rsi, rdx, rcx, r8, r9
        for (i, arg) in args.iter().enumerate() {
            let r = match pregs.get(i) {
                Some(r) => *r as u8,
                None => unimplemented!("too many arguments for internal function"),
            }; // TODO
            match arg {
                Value::Instruction(InstructionValue { id, .. }) => {
                    dynasm!(self.asm; mov Ra(r), Ra(reg!(caller; *id)))
                }
                Value::Immediate(ImmediateValue::Int32(i)) => dynasm!(self.asm; mov Ra(r), *i),
                Value::Argument(arg) => {
                    dynasm!(self.asm; mov Ra(r), AWORD [rbp+8*(2+arg.index as i32)])
                }
                _ => unimplemented!(),
            }
        }
    }

    fn compile_load(&mut self, func: &Function, instr: &Instruction, op1: &Value) {
        match op1 {
            Value::Instruction(v) => {
                let n = self.alloca_mgr.get_local_offset(vreg!(func; v.id)).unwrap() as i32;
                dynasm!(self.asm; mov Rd(reg!(instr)), [rbp-n]);
            }
            e => unimplemented!("{:?}", e),
        }
    }

    fn compile_store(&mut self, func: &Function, op1: &Value, op2: &Value) {
        match (op1, op2) {
            (Value::Instruction(src), Value::Instruction(dst)) => {
                let n = self
                    .alloca_mgr
                    .get_local_offset(vreg!(func; dst.id))
                    .unwrap() as i32;
                dynasm!(self.asm; mov DWORD [rbp-n], Rd(reg!(func; src.id)));
            }
            (Value::Immediate(ImmediateValue::Int32(i)), Value::Instruction(dst)) => {
                let n = self
                    .alloca_mgr
                    .get_local_offset(vreg!(func; dst.id))
                    .unwrap() as i32;
                dynasm!(self.asm; mov DWORD [rbp-n], *i);
            }
            e => unimplemented!("{:?}", e),
        }
    }

    fn compile_add(&mut self, f: &Function, instr: &Instruction, op1: &Value, op2: &Value) {
        let rn = reg!(instr);
        match (op1, op2) {
            (Value::Argument(a), Value::Immediate(ImmediateValue::Int32(i))) => {
                dynasm!(self.asm; mov Ra(rn), [rbp+8*(2+a.index as i32)]; add Ra(rn), *i)
            }
            (Value::Instruction(v), Value::Argument(a)) => {
                dynasm!(self.asm; mov Ra(rn), Ra(reg!(f; v.id)); add Ra(rn), [rbp+8*(2+a.index as i32)])
            }
            (Value::Instruction(v1), Value::Instruction(v2)) => {
                dynasm!(self.asm; mov Ra(rn), Ra(reg!(f; v1.id)); add Ra(rn), Ra(reg!(f; v2.id)))
            }
            (Value::Instruction(v), Value::Immediate(ImmediateValue::Int32(i))) => {
                dynasm!(self.asm; mov Ra(rn), Ra(reg!(f; v.id)); add Ra(rn), *i)
            }
            (Value::Argument(a1), Value::Argument(a2)) => {
                dynasm!(self.asm; mov Ra(rn), [rbp+8*(2+a1.index as i32)]; add Ra(rn), [rbp+8*(2+a2.index as i32)])
            }
            _ => unimplemented!(),
        }
    }

    fn compile_call(&mut self, f: &Function, instr: &Instruction, callee: &Value, args: &[Value]) {
        let returns = callee
            .get_type(&self.module)
            .get_function_ty()
            .unwrap()
            .ret_ty
            != Type::Void;
        match callee {
            Value::Function(callee_id) => {
                let callee_entity = self.module.function_ref(*callee_id);
                let mut rsp_offset = 0;

                let mut save_regs = vec![];
                for (_, i) in &f.instr_table {
                    // TODO
                    if i.reg.borrow().reg.is_none() {
                        continue;
                    }
                    let bgn = i.vreg;
                    let end = match i.reg.borrow().last_use {
                        Some(last_use) => vreg!(f; last_use),
                        None => continue,
                    };
                    if bgn < instr.vreg && instr.vreg < end {
                        save_regs.push(reg!(i));
                    }
                }

                when_debug!(println!("saved register: {:?}", save_regs));

                for save_reg in &save_regs {
                    dynasm!(self.asm; push Ra(*save_reg));
                    rsp_offset += 8;
                }

                if !callee_entity.internal {
                    rsp_offset += args.len() * 8;
                    self.push_args(f, args);
                }

                if callee_entity.internal {
                    self.assign_args_to_regs(f, args);
                    let callee = self.internal_func.get(&callee_entity.name).unwrap();
                    let rsp_adjust = roundup(rsp_offset as i32, 16) - rsp_offset as i32; // rsp is 16-byte aligned
                    dynasm!(self.asm
                        ; sub rsp, rsp_adjust
                        ; mov rax, QWORD *callee as _
                        ; call rax
                        ; add rsp, rsp_adjust
                    );
                } else {
                    let f_entry = self.get_function_entry_label(*callee_id);
                    dynasm!(self.asm; call => f_entry);
                }

                if returns {
                    dynasm!(self.asm; mov Ra(reg!(instr)), rax);
                }

                if !callee_entity.internal {
                    dynasm!(self.asm; add rsp, 8*(args.len() as i32));
                }

                for save_reg in save_regs.iter().rev() {
                    dynasm!(self.asm; pop Ra(*save_reg));
                }
            }
            _ => unimplemented!(),
        }
    }

    fn collect_alloca(&mut self, f: &Function) {
        for (_, bb) in &f.basic_blocks {
            for val in &*bb.iseq.borrow() {
                let instr_id = val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];
                if let Opcode::Alloca(ty) = &instr.opcode {
                    self.alloca_mgr.allocate(instr.vreg, ty);
                }
            }
        }
    }

    fn collect_phi(&mut self, f: &Function) {
        for (_, bb) in &f.basic_blocks {
            for val in &*bb.iseq.borrow() {
                let instr_id = val.get_instr_id().unwrap();
                let instr = &f.instr_table[instr_id];

                match &instr.opcode {
                    Opcode::Phi(pairs) => {
                        for (val, bb) in pairs {
                            self.phi_map.insert(*bb, (*val, instr_id));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn merge_phi(&mut self, f: &Function, bb_id: BasicBlockId) {
        some_then!((val, asgn), self.phi_map.get(&bb_id), {
            match val {
                Value::Instruction(iv) => {
                    dynasm!(self.asm; mov Ra(reg!(f; *asgn)), Ra(reg!(f; iv.id) as u8));
                }
                Value::Immediate(ImmediateValue::Int32(i)) => {
                    dynasm!(self.asm; mov Ra(reg!(f; *asgn)),*i);
                }
                _ => unimplemented!(),
            }
        })
    }
}

impl AllocaManager {
    pub fn new() -> Self {
        Self {
            locals: FxHashMap::default(),
            cur_size: 0,
        }
    }

    pub fn reset(&mut self) {
        self.cur_size = 0;
        self.locals.clear();
    }

    pub fn allocate(&mut self, vreg: VirtualRegister, ty: &Type) {
        self.cur_size += ty.size_in_byte();
        self.locals.insert(vreg, self.cur_size);
    }

    pub fn get_local_offset(&self, vreg: VirtualRegister) -> Option<usize> {
        self.locals.get(&vreg).map(|x| *x)
    }

    pub fn local_size(&self) -> usize {
        self.cur_size
    }
}

trait TypeSize {
    fn size_in_byte(&self) -> usize;
}

impl TypeSize for Type {
    fn size_in_byte(&self) -> usize {
        match self {
            Type::Int1 => 1,
            Type::Int32 => 4,
            Type::Pointer(_) => 8,
            Type::Function(_) => unimplemented!(),
            Type::Void => 0,
        }
    }
}

fn roundup(n: i32, align: i32) -> i32 {
    (n + align - 1) & !(align - 1)
}

// Internal function cilk.println.i32
#[no_mangle]
pub extern "C" fn cilk_println_i32(i: i32) {
    println!("{}", i);
}
