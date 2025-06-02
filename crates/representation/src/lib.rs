use alloy_primitives::U256;

// Type aliases for clarity
pub type LocalId = u32; // Symbol IDs (e.g., $0, $1)
pub type BBId = u32; // Basic block IDs
pub type FnId = u32; // Function IDs
pub type Literal = U256; // Constants (e.g., 34, used in assignments or switch)
pub type DataId = u32; // Dense IDs for data section bytestrings

#[derive(Debug, Clone)]
pub struct LocalGenerator {
    next_local_id: LocalId,
}
impl LocalGenerator {
    pub fn get_new(&mut self) -> LocalId {
        let local = self.next_local_id;
        self.next_local_id += 1;
        local
    }
}

// Top-level IR structure
#[derive(Debug, Clone)]
pub struct IR {
    pub functions: Vec<Function>,
    pub data_section: Vec<Vec<u8>>, // Bytestrings for codecopy, indexed by DataId
    pub local_gen: LocalGenerator,
}

impl IR {
    pub fn iter_bbs_mut(&mut self) -> impl Iterator<Item = &mut BasicBlock> {
        self.functions
            .iter_mut()
            .flat_map(|func| func.basic_blocks.iter_mut())
    }

    pub fn iter_statements_mut(&mut self) -> impl Iterator<Item = &mut Statement> {
        self.iter_bbs_mut().flat_map(|bb| bb.statements.iter_mut())
    }
}

// Function definition
#[derive(Debug, Clone)]
pub struct Function {
    pub id: FnId,
    pub basic_blocks: Vec<BasicBlock>,
    pub ret_label: Option<LocalId>, // Return destination symbol (None for main)
    pub inputs: Vec<LocalId>,       // Input symbols
    pub output_count: u32,          // Number of outputs (0 for main)
}
// Basic block defin
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub parameters: Vec<LocalId>, // Live variables as input parameters
    pub outputs: Vec<LocalId>,    // Output symbols passed to successors
    pub statements: Vec<Statement>,
    pub end: BBEnd,
}

// Statements within a basic block
#[derive(Debug, Clone)]
pub struct Statement {
    pub outputs: Vec<LocalId>, // Symbols assigned to (e.g., $4 or $3, $2)
    pub op: IROp,
    pub args: Vec<OpInput>,
}

impl Statement {
    pub fn arg1(&self) -> Option<OpInput> {
        (self.args.len() == 1).then(|| self.args[0])
    }

    pub fn out1(&self) -> Option<LocalId> {
        (self.outputs.len() == 1).then(|| self.outputs[0])
    }
}

impl Statement {
    pub fn assign(to: LocalId, op: IROp, args: Vec<OpInput>) -> Self {
        Self {
            outputs: vec![to],
            op,
            args,
        }
    }

    pub fn assign_const(to: LocalId, value: Literal) -> Self {
        Self::assign(to, IROp::Set, vec![OpInput::Constant(value)])
    }
}

#[macro_export]
macro_rules! stmt {
    ($to:expr => $op:ident $($tail:tt)*) => {
        Statement {
            outputs: vec![$to],
            op: IROp::$op,
            args: stmt_args!([$($tail)*] -> []),
        }
    };
    ($op:ident $($tail:tt)*) => {
        Statement {
            outputs: vec![],
            op: IROp::$op,
            args: stmt_args!([$($tail)*] -> []),
        }
    };
}

#[macro_export]
macro_rules! stmt_args {
    ([#$const:expr, $($todo:tt)*] -> [$($processed:tt)*]) => {
        stmt_args!([$($todo)*] -> [OpInput::Constant($const),])
    };
    ([#$const:expr] -> [$($processed:tt)*]) => {
        vec![$($processed)* OpInput::Constant($const), ]
    };
    ([>$op_inp:expr, $($todo:tt)*] -> [$($processed:tt)*]) => {
        stmt_args!([$($todo)*] -> [$op_inp,])
    };
    ([>$op_inp:expr] -> [$($processed:tt)*]) => {
        vec![$($processed)* $op_inp, ]
    };
    ([$local:expr, $($todo:tt)*] -> [$($processed:tt)*]) => {
        stmt_args!([$($todo)*] -> [OpInput::Local($local),])
    };
    ([$local:expr] -> [$($processed:tt)*]) => {
        vec![$($processed)* OpInput::Local($local), ]
    };
    ([] -> [$($processed:tt)*]) => {
        vec![$($processed)* ]
    }

}

#[macro_export]
macro_rules! op_input {
    (#$const:expr) => {
        OpInput::Constant($expr)
    };
}

// Symbols: Inputs to opcodes (either local variables or data section offsets)
#[derive(Debug, Clone, Copy)]
pub enum OpInput {
    /// The symbols of the IR, local to every basic block, assign once and never change.
    Local(LocalId), // e.g., $34
    /// A constant, at most 256-bits
    Constant(Literal), // e.g., 34
}

/// EVM Opcodes with outputs
#[derive(Debug, Clone, Copy)]
pub enum IROp {
    // Arithemtic Logic
    Add,
    Mul,
    Sub,
    // EVM opcodes with no return value
    Sstore, // sstore key, value
    // EVM opcodes with return value
    Mload, // mload offset -> value
    Mstore,

    Calldatacopy,
    Calldatasize,

    // Control Flow
    Stop,
    Revert,
    Return,
    Invalid,

    // Internal function call
    ICall(FnId), // icall @wow $34, $4, $163

    /// A reference to the start offset of the data object in the final produced bytecode
    DataOffset(DataId), // e.g., data section entry at ID 0

    // Builtins
    Set, // Sets local to constant/other local
    StaticAllocZeroed,
    StaticAllocAnyBytes,
    StaticFree,
    DynamicAllocZeroed,
    DynamicAllocAnyBytes,
    DynamicGetFreePtr,
}

// Control flow termination for basic blocks
#[derive(Debug, Clone)]
pub struct BranchEnd {
    pub condition: OpInput,    // Symbol or constant to test
    pub goto_if_nonzero: BBId, // Target if condition is nonzero
    pub goto_else: BBId,       // Target if condition is zero
}

#[derive(Debug, Clone)]
pub struct SwitchEntry {
    pub value: Literal, // Constant to match
    pub goto: BBId,     // Target basic block
}

#[derive(Debug, Clone)]
pub struct SwitchEnd {
    pub switch_value: OpInput,          // Symbol or constant to switch on
    pub switch_table: Vec<SwitchEntry>, // (value, target) pairs
    pub goto_else: BBId,                // Optional else branch
}

#[derive(Debug, Clone)]
pub enum BBEnd {
    LastStmtTerminates,

    // Control Flow
    InternalReturn {
        dest: LocalId, // Return destination symbol
    },
    Goto(BBId),
    Branch(BranchEnd),
    Switch(SwitchEnd),
}
