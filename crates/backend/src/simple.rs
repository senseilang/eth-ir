use crate::Stack;
use alloy_primitives::U256;
use core::ops::Range;
use eth_ir::*;
use evm_glue::opcodes::Opcode as AsmOp;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct SimpleStaticAllocator {
    lowest_safe_offset: u32,
}

impl SimpleStaticAllocator {
    fn lowest_safe_offset(&self) -> u32 {
        self.lowest_safe_offset
    }

    fn respecting_reserved(reserved_memory: &[Range<u32>]) -> Self {
        let lowest_safe_offset = reserved_memory.iter().map(|r| r.end).max().unwrap_or(0);
        Self { lowest_safe_offset }
    }

    fn alloc(&mut self, bytes: u32) -> u32 {
        let offset = self.lowest_safe_offset;
        self.lowest_safe_offset = self
            .lowest_safe_offset
            .checked_add(bytes)
            .expect("Static Allocator OOM");
        offset
    }

    fn alloc_u256(&mut self, bytes: u32) -> U256 {
        U256::from(self.alloc(bytes))
    }
}

fn compile_ir_mem_primitives(
    ir: &mut IR,
    simple_static_allocator: &mut SimpleStaticAllocator,
    fmp_slot: U256,
) {
    let local_gen = &mut ir.local_gen;

    for bb in ir
        .functions
        .iter_mut()
        .flat_map(|func| func.basic_blocks.iter_mut())
    {
        let mut new_stmts = Vec::new();
        for stmt in bb.statements.drain(..) {
            match stmt.op {
                IROp::StaticAllocZeroed | IROp::StaticAllocAnyBytes => {
                    let bytes: u32 = match stmt.arg1().unwrap() {
                        OpInput::Constant(l) => l.try_into().unwrap(),
                        op_inp => panic!("Unexpected {op_inp:?}"),
                    };

                    let new_offset = simple_static_allocator.alloc_u256(bytes);
                    let to = stmt.out1().unwrap();
                    new_stmts.push(stmt!(to => Set #new_offset));
                }
                op @ (IROp::DynamicAllocZeroed | IROp::DynamicAllocAnyBytes) => {
                    let to = stmt.out1().unwrap();
                    new_stmts.push(stmt!(to => Mload #fmp_slot));

                    let updated_fmp_local = local_gen.get_new();
                    let bytes_local = stmt.arg1().unwrap();

                    new_stmts.push(stmt!(updated_fmp_local => Add to, >bytes_local));
                    new_stmts.push(stmt!(Mstore #fmp_slot, updated_fmp_local));

                    if matches!(op, IROp::DynamicAllocZeroed) {
                        let cdz_local = local_gen.get_new();
                        new_stmts.push(stmt!(cdz_local => Calldatasize));
                        new_stmts.push(stmt!(Calldatacopy to, cdz_local, >bytes_local));
                    }
                }
                IROp::StaticFree => { /* no-op, drop statement */ }
                IROp::DynamicGetFreePtr => {
                    let to = stmt.out1().unwrap();
                    new_stmts.push(stmt!(to => Mload #fmp_slot));
                }
                _ => {
                    new_stmts.push(stmt);
                }
            }
        }
        bb.statements = new_stmts;
    }
}

#[derive(Debug, Clone, Default)]
pub struct BackendConfig {
    fmp_slot: Option<u32>,
    reserved_memory: Vec<Range<u32>>,
}

fn range_includes<T: PartialOrd>(larger: &Range<T>, smaller: &Range<T>) -> bool {
    larger.start <= smaller.start && smaller.end <= larger.end
}

impl BackendConfig {
    pub fn reserve_range(&mut self, new_range: Range<u32>) {
        for resv in self.reserved_memory.iter_mut() {
            if range_includes(resv, &new_range) {
                return;
            }
            if range_includes(&new_range, resv) {
                *resv = new_range;
                return;
            }
        }
        self.reserved_memory.push(new_range);
    }

    pub fn with_fmp_slot(mut self, fmp_slot: u32) -> Self {
        self.fmp_slot = Some(fmp_slot);
        self.reserve_range(fmp_slot..fmp_slot + 32);
        self
    }
}

pub fn generate_from(mut ir: IR, config: &BackendConfig) {
    let mut allocator = SimpleStaticAllocator::respecting_reserved(&config.reserved_memory);
    let fmp_slot = config.fmp_slot.unwrap_or_else(|| allocator.alloc(32));
    let fmp_slot = U256::from(fmp_slot);

    compile_ir_mem_primitives(&mut ir, &mut allocator, fmp_slot);
}

fn depth_to_dup_op(depth: usize) -> Option<AsmOp> {
    let op = match depth {
        1 => AsmOp::DUP1,
        2 => AsmOp::DUP2,
        3 => AsmOp::DUP3,
        4 => AsmOp::DUP4,
        5 => AsmOp::DUP5,
        6 => AsmOp::DUP6,
        7 => AsmOp::DUP7,
        8 => AsmOp::DUP8,
        9 => AsmOp::DUP9,
        10 => AsmOp::DUP10,
        11 => AsmOp::DUP11,
        12 => AsmOp::DUP12,
        13 => AsmOp::DUP13,
        14 => AsmOp::DUP14,
        15 => AsmOp::DUP15,
        16 => AsmOp::DUP16,
        0 => panic!("invalid depth"),
        _ => return None,
    };
    Some(op)
}
fn rel_depth_to_swap_op(depth: usize) -> Option<AsmOp> {
    let op = match depth {
        1 => AsmOp::SWAP1,
        2 => AsmOp::SWAP2,
        3 => AsmOp::SWAP3,
        4 => AsmOp::SWAP4,
        5 => AsmOp::SWAP5,
        6 => AsmOp::SWAP6,
        7 => AsmOp::SWAP7,
        8 => AsmOp::SWAP8,
        9 => AsmOp::SWAP9,
        10 => AsmOp::SWAP10,
        11 => AsmOp::SWAP11,
        12 => AsmOp::SWAP12,
        13 => AsmOp::SWAP13,
        14 => AsmOp::SWAP14,
        15 => AsmOp::SWAP15,
        16 => AsmOp::SWAP16,
        0 => panic!("invalid depth"),
        _ => return None,
    };
    Some(op)
}

#[derive(Debug, Clone)]
enum ScheduledStep {
    IROp(IROp),
    AsmOp(AsmOp),
}

const MAX_STACK_DEPTH: usize = 16;

fn u256_to_push(value: &U256) -> AsmOp {
    macro_rules! u256_to_push_byte_len_match {
        ($($bytes:literal, $push:ident);*) => {
            match value.byte_len() {
                $(
                    $bytes => {
                        let mut bytes = [0u8; $bytes];
                        bytes.copy_from_slice(&value.to_be_bytes::<32>()[32 - $bytes..]);
                        AsmOp::$push(bytes)
                    }
                )*,
                _ => panic!("Unexpected byte len {}", value.byte_len())
            }
        };
    }
    u256_to_push_byte_len_match!(
         1, PUSH1;   2, PUSH2;   3, PUSH3;   4, PUSH4;   5, PUSH5;   6, PUSH6;   7, PUSH7;   8, PUSH8;
         9, PUSH9;  10, PUSH10; 11, PUSH11; 12, PUSH12; 13, PUSH13; 14, PUSH14; 15, PUSH15; 16, PUSH16;
        17, PUSH17; 18, PUSH18; 19, PUSH19; 20, PUSH20; 21, PUSH21; 22, PUSH22; 23, PUSH23; 24, PUSH24;
        25, PUSH25; 26, PUSH26; 27, PUSH27; 28, PUSH28; 29, PUSH29; 30, PUSH30; 31, PUSH31; 32, PUSH32
    )
}

fn update_highest_spill_offset(
    highest_spill_offset: &mut u32,
    local_allocator: &SimpleStaticAllocator,
) {
    *highest_spill_offset = (*highest_spill_offset).max(local_allocator.lowest_safe_offset());
}

fn stack_schedule_inputs(
    stmt: &Statement,
    stack: &mut Stack<Option<LocalId>>,
    steps: &mut Vec<ScheduledStep>,
    highest_spill_offset: &mut u32,
    allocator: &SimpleStaticAllocator,
) {
    // Go through arguments bottom up making sure they're on the stack.
    for inp in stmt.args.iter().rev() {
        // It's fine to be reusing the same memory between inputs as we always `mstore` the
        // slots before reading them. We keep track of the highest slot touched in
        // `highest_spill_offset` so we can allocate the area to ensure it's safe-guarded.
        let mut inp_allocator = allocator.clone();
        match inp {
            &OpInput::Local(local) => {
                let depth = stack
                    .get_lowest_depth(&Some(local))
                    .expect("Reference to undefined local");
                match depth_to_dup_op(depth) {
                    Some(dup_op) => {
                        steps.push(ScheduledStep::AsmOp(dup_op));
                        stack.push(Some(local));
                    }
                    None => {
                        // Stack too deep, need to spill stack to memory.
                        // Implementation here is super naive, spilling the top until it finds
                        // the target variable.
                        // TODO optimize: In most cases it'd be more efficient to spill
                        // the variable earlier & in isolation. Furthermore spilled variables
                        // are not remembered.

                        // Spill the top until we can dup the target value with DUP16.
                        let mut spilled = Vec::new();
                        for _ in MAX_STACK_DEPTH..depth {
                            let spill_offset = inp_allocator.alloc_u256(32);
                            spilled.push((stack.pop().unwrap(), spill_offset));
                            steps.push(ScheduledStep::AsmOp(u256_to_push(&spill_offset)));
                            steps.push(ScheduledStep::AsmOp(AsmOp::MSTORE));
                        }
                        // Now DUP16 previously too deep value.
                        steps.push(ScheduledStep::AsmOp(
                            depth_to_dup_op(MAX_STACK_DEPTH).unwrap(),
                        ));
                        // Spill that variable too incase we had to spill more than 16 variables.
                        let target_spill_offset = inp_allocator.alloc_u256(32);
                        steps.push(ScheduledStep::AsmOp(u256_to_push(&target_spill_offset)));
                        steps.push(ScheduledStep::AsmOp(AsmOp::MSTORE));
                        // Now unspill all the values back onto the stack.
                        while let Some((in_mem_local, spilled_to_offset)) = spilled.pop() {
                            steps.push(ScheduledStep::AsmOp(u256_to_push(&spilled_to_offset)));
                            steps.push(ScheduledStep::AsmOp(AsmOp::MLOAD));
                            stack.push(in_mem_local);
                        }
                        // Now bring our target value to the top of the stack.
                        steps.push(ScheduledStep::AsmOp(u256_to_push(&target_spill_offset)));
                        steps.push(ScheduledStep::AsmOp(AsmOp::MLOAD));
                        stack.push(Some(local));
                    }
                }
            }
            OpInput::Constant(constant) => {
                steps.push(ScheduledStep::AsmOp(u256_to_push(constant)));
                stack.push(None);
            }
        }
        update_highest_spill_offset(highest_spill_offset, &inp_allocator);
    }
}

fn stack_schedule_shuffle_to_target_layout(
    target_layout: &[LocalId],
    original_stack: &mut Stack<Option<LocalId>>,
    steps: &mut Vec<ScheduledStep>,
    highest_spill_offset: &mut u32,
    allocator: &SimpleStaticAllocator,
) {
    let mut spill_allocator = allocator.clone();
    let target_stack: Stack<LocalId> = target_layout.iter().copied().collect();
    let mut stack: Stack<LocalId> = original_stack
        .iter()
        .map(|local| local.expect("temp const left on stack"))
        .collect();

    let mut target_local_counts: HashMap<LocalId, u16> = HashMap::with_capacity(stack.len());
    for &local in target_layout {
        *target_local_counts.entry(local).or_default() += 1;
    }
    let mut current_local_counts: HashMap<LocalId, u16> = HashMap::with_capacity(stack.len());
    for &local in stack.iter() {
        *current_local_counts.entry(local).or_default() += 1;
    }

    // Remove any bad elements at the very top
    loop {
        let to_remove = stack.len().checked_sub(target_stack.len()).unwrap();
        let top = *stack.last().unwrap();

        if to_remove > 0
            && current_local_counts[&top] > target_local_counts.get(&top).copied().unwrap_or(0)
        {
            stack.pop();
            *current_local_counts.get_mut(&top).unwrap() -= 1;
            steps.push(ScheduledStep::AsmOp(AsmOp::POP));
            continue;
        }

        break;
    }

    update_highest_spill_offset(highest_spill_offset, &spill_allocator);
    *original_stack = stack.iter().copied().map(Some).collect();
}

fn simple_stack_schedule(
    bb: &BasicBlock,
    allocator: &SimpleStaticAllocator,
) -> (Vec<ScheduledStep>, u32) {
    let mut steps = Vec::new();
    // Some values are locals and `None` are temporary constants.
    let mut stack: Stack<_> = bb.parameters.iter().map(|&local| Some(local)).collect();
    let mut highest_spill_offset: u32 = allocator.lowest_safe_offset();

    for stmt in bb.statements.iter() {
        stack_schedule_inputs(
            stmt,
            &mut stack,
            &mut steps,
            &mut highest_spill_offset,
            allocator,
        );
        steps.push(ScheduledStep::IROp(stmt.op));
        // Pop inputs simulating their consumption by the operation.
        for _ in 0..stmt.args.len() {
            stack.pop().unwrap();
        }
        // Push resulting outputs.
        for out in stmt.outputs.iter().rev() {
            stack.push(Some(*out));
        }
    }

    if !matches!(bb.end, BBEnd::LastStmtTerminates) {
        stack_schedule_shuffle_to_target_layout(
            &bb.outputs,
            &mut stack,
            &mut steps,
            &mut highest_spill_offset,
            allocator,
        );
    }

    // match bb.end {
    //     BBEnd::LastStmtTerminates => {
    //         // Last statement was a terminating one, nothing more to do.
    //     },
    //     BB
    // }

    (steps, highest_spill_offset)
}
