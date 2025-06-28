pub mod index;
pub mod operation;

use crate::{index::*, operation::Operation};
use alloy_primitives::U256;
use index_vec::IndexVec;
use std::ops::Range;

/// Implemented in a data oriented way. Instead of each basic block and function holding its own
/// vector of items they're all stored contiguously in the top level program
#[derive(Debug, Clone)]
pub struct Program {
    pub entry: FunctionId,
    
    // Top Level IR Structure
    pub functions: IndexVec<FunctionId, Function>,
    pub basic_blocks: IndexVec<BasicBlockId, BasicBlock>,
    pub operations: IndexVec<OperationIndex, Operation>,
    // Small IR Pieces
    pub large_opcode_locals: IndexVec<LocalIndex, LocalId>,
    pub data_bytes: IndexVec<DataOffset, u8>,
    pub large_consts: IndexVec<LargeConstId, U256>,

    pub cases: IndexVec<CasesId, Cases>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub entry: BasicBlockId,
    pub outputs: u32,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub inputs: Range<LocalIndex>,
    pub outputs: Range<LocalIndex>,
    pub operations: Range<OperationIndex>,
    pub control: Control,
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub condition: LocalId,
    pub non_zero_target: BasicBlockId,
    pub zero_target: BasicBlockId,
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub condition: LocalId,
    pub fallback: Option<BasicBlockId>,
    pub cases: CasesId,
}

// TODO: Optimized memory layout.
#[derive(Debug, Clone)]
pub struct Cases {
    pub cases: Vec<(U256, BasicBlockId)>,
}

#[derive(Debug, Clone)]
pub enum Control {
    LastOpTerminates,
    ContinuesTo(BasicBlockId),
    Branches(Branch),
    Switch(Switch),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn control_memory_layout() {
        assert_eq!(std::mem::size_of::<Control>(), 16, "changed desired control size");
        assert_eq!(std::mem::align_of::<Control>(), 4, "changed desired control alignment");
    }
}
