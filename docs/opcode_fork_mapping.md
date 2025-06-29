# EVM Opcodes by Introduction Fork

This document shows when each EVM opcode was introduced.

## Summary

- **Frontier**: 129 opcodes
- **Homestead**: 1 opcodes
- **Byzantium**: 5 opcodes
- **Constantinople**: 5 opcodes
- **Istanbul**: 2 opcodes
- **London**: 1 opcodes
- **Merge**: 1 opcodes
- **Shanghai**: 1 opcodes
- **Cancun**: 5 opcodes

## Frontier
*Introduced 129 opcodes*

- `0x00`: Halts execution
- `0x01`: Addition operation
- `0x02`: Multiplication operation
- `0x03`: Subtraction operation
- `0x04`: Integer division operation
- `0x05`: Signed integer division operation (truncated)
- `0x06`: Modulo remainder operation
- `0x07`: Signed modulo remainder operation
- `0x08`: Modulo addition operation
- `0x09`: Modulo multiplication operation
- `0x0A`: Exponential operation
- `0x0B`: Extend length of two’s complement signed integer
- `0x10`: Less-than comparison
- `0x11`: Greater-than comparison
- `0x12`: Signed less-than comparison
- `0x13`: Signed greater-than comparison
- `0x14`: Equality comparison
- `0x15`: Is-zero comparison
- `0x16`: Bitwise AND operation
- `0x17`: Bitwise OR operation
- `0x18`: Bitwise XOR operation
- `0x19`: Bitwise NOT operation
- `0x1A`: Retrieve single byte from word
- `0x20`: Compute Keccak-256 hash
- `0x30`: Get address of currently executing account
- `0x31`: Get balance of the given account
- `0x32`: Get execution origination address
- `0x33`: Get caller address
- `0x34`: Get deposited value by the instruction/transaction responsible for this execution
- `0x35`: Get input data of current environment
- `0x36`: Get size of input data in current environment
- `0x37`: Copy input data in current environment to memory
- `0x38`: Get size of code running in current environment
- `0x39`: Copy code running in current environment to memory
- `0x3A`: Get price of gas in current environment
- `0x3B`: Get size of an account’s code
- `0x3C`: Copy an account’s code to memory
- `0x40`: Get the hash of one of the 256 most recent complete blocks
- `0x41`: Get the block’s beneficiary address
- `0x42`: Get the block’s timestamp
- `0x43`: Get the block’s number
- `0x44`: Get the block’s difficulty
- `0x45`: Get the block’s gas limit
- `0x50`: Remove item from stack
- `0x51`: Load word from memory
- `0x52`: Save word to memory
- `0x53`: Save byte to memory
- `0x54`: Load word from storage
- `0x55`: Save word to storage
- `0x56`: Alter the program counter
- `0x57`: Conditionally alter the program counter
- `0x58`: Get the value of the program counter prior to the increment corresponding to this instruction
- `0x59`: Get the size of active memory in bytes
- `0x5A`: Get the amount of available gas, including the corresponding reduction for the cost of this instruction
- `0x5B`: Mark a valid destination for jumps
- `0x60`: Place 1 byte item on stack
- `0x61`: Place 2 byte item on stack
- `0x62`: Place 3 byte item on stack
- `0x63`: Place 4 byte item on stack
- `0x64`: Place 5 byte item on stack
- `0x65`: Place 6 byte item on stack
- `0x66`: Place 7 byte item on stack
- `0x67`: Place 8 byte item on stack
- `0x68`: Place 9 byte item on stack
- `0x69`: Place 10 byte item on stack
- `0x6A`: Place 11 byte item on stack
- `0x6B`: Place 12 byte item on stack
- `0x6C`: Place 13 byte item on stack
- `0x6D`: Place 14 byte item on stack
- `0x6E`: Place 15 byte item on stack
- `0x6F`: Place 16 byte item on stack
- `0x70`: Place 17 byte item on stack
- `0x71`: Place 18 byte item on stack
- `0x72`: Place 19 byte item on stack
- `0x73`: Place 20 byte item on stack
- `0x74`: Place 21 byte item on stack
- `0x75`: Place 22 byte item on stack
- `0x76`: Place 23 byte item on stack
- `0x77`: Place 24 byte item on stack
- `0x78`: Place 25 byte item on stack
- `0x79`: Place 26 byte item on stack
- `0x7A`: Place 27 byte item on stack
- `0x7B`: Place 28 byte item on stack
- `0x7C`: Place 29 byte item on stack
- `0x7D`: Place 30 byte item on stack
- `0x7E`: Place 31 byte item on stack
- `0x7F`: Place 32 byte (full word) item on stack
- `0x80`: Duplicate 1st stack item
- `0x81`: Duplicate 2nd stack item
- `0x82`: Duplicate 3rd stack item
- `0x83`: Duplicate 4th stack item
- `0x84`: Duplicate 5th stack item
- `0x85`: Duplicate 6th stack item
- `0x86`: Duplicate 7th stack item
- `0x87`: Duplicate 8th stack item
- `0x88`: Duplicate 9th stack item
- `0x89`: Duplicate 10th stack item
- `0x8A`: Duplicate 11th stack item
- `0x8B`: Duplicate 12th stack item
- `0x8C`: Duplicate 13th stack item
- `0x8D`: Duplicate 14th stack item
- `0x8E`: Duplicate 15th stack item
- `0x8F`: Duplicate 16th stack item
- `0x90`: Exchange 1st and 2nd stack items
- `0x91`: Exchange 1st and 3rd stack items
- `0x92`: Exchange 1st and 4th stack items
- `0x93`: Exchange 1st and 5th stack items
- `0x94`: Exchange 1st and 6th stack items
- `0x95`: Exchange 1st and 7th stack items
- `0x96`: Exchange 1st and 8th stack items
- `0x97`: Exchange 1st and 9th stack items
- `0x98`: Exchange 1st and 10th stack items
- `0x99`: Exchange 1st and 11th stack items
- `0x9A`: Exchange 1st and 12th stack items
- `0x9B`: Exchange 1st and 13th stack items
- `0x9C`: Exchange 1st and 14th stack items
- `0x9D`: Exchange 1st and 15th stack items
- `0x9E`: Exchange 1st and 16th stack items
- `0x9F`: Exchange 1st and 17th stack items
- `0xA0`: Append log record with no topics
- `0xA1`: Append log record with one topic
- `0xA2`: Append log record with two topics
- `0xA3`: Append log record with three topics
- `0xA4`: Append log record with four topics
- `0xF0`: Create a new account with associated code
- `0xF1`: Message-call into an account
- `0xF2`: Message-call into this account with alternative account’s code
- `0xF3`: Halt execution returning output data
- `0xFF`: Halt execution and register account for later deletion or send all Ether to address (post-Cancun)

## Homestead
*Introduced 1 opcodes*

- `0xFE`: Designated invalid instruction

## Byzantium
*Introduced 5 opcodes*

- `0x3D`: Get size of output data from the previous call from the current environment
- `0x3E`: Copy output data from the previous call to memory
- `0xF4`: Message-call into this account with an alternative account’s code, but persisting the current values for sender and value
- `0xFA`: Static message-call into an account
- `0xFD`: Halt execution reverting state changes but returning data and remaining gas

## Constantinople
*Introduced 5 opcodes*

- `0x1B`: Left shift operation
- `0x1C`: Logical right shift operation
- `0x1D`: Arithmetic (signed) right shift operation
- `0x3F`: Get hash of an account’s code
- `0xF5`: Create a new account with associated code at a predictable address

## Istanbul
*Introduced 2 opcodes*

- `0x46`: Get the chain ID
- `0x47`: Get balance of currently executing account

## London
*Introduced 1 opcodes*

- `0x48`: Get the base fee

## Merge
*Introduced 1 opcodes*

- `0x44_MERGE`: Get the previous block’s RANDAO mix

## Shanghai
*Introduced 1 opcodes*

- `0x5F`: Place value 0 on stack

## Cancun
*Introduced 5 opcodes*

- `0x49`: Get versioned hashes
- `0x4A`: Returns the value of the blob base-fee of the current block
- `0x5C`: Load word from transient storage
- `0x5D`: Save word to transient storage
- `0x5E`: Copy memory areas



## CSV Format

```csv
Opcode,Fork,Description
0x00,Frontier,"Halts execution"
0x01,Frontier,"Addition operation"
0x02,Frontier,"Multiplication operation"
0x03,Frontier,"Subtraction operation"
0x04,Frontier,"Integer division operation"
0x05,Frontier,"Signed integer division operation (truncated)"
0x06,Frontier,"Modulo remainder operation"
0x07,Frontier,"Signed modulo remainder operation"
0x08,Frontier,"Modulo addition operation"
0x09,Frontier,"Modulo multiplication operation"
0x0A,Frontier,"Exponential operation"
0x0B,Frontier,"Extend length of two’s complement signed integer"
0x10,Frontier,"Less-than comparison"
0x11,Frontier,"Greater-than comparison"
0x12,Frontier,"Signed less-than comparison"
0x13,Frontier,"Signed greater-than comparison"
0x14,Frontier,"Equality comparison"
0x15,Frontier,"Is-zero comparison"
0x16,Frontier,"Bitwise AND operation"
0x17,Frontier,"Bitwise OR operation"
0x18,Frontier,"Bitwise XOR operation"
0x19,Frontier,"Bitwise NOT operation"
0x1A,Frontier,"Retrieve single byte from word"
0x1B,Constantinople,"Left shift operation"
0x1C,Constantinople,"Logical right shift operation"
0x1D,Constantinople,"Arithmetic (signed) right shift operation"
0x20,Frontier,"Compute Keccak-256 hash"
0x30,Frontier,"Get address of currently executing account"
0x31,Frontier,"Get balance of the given account"
0x32,Frontier,"Get execution origination address"
0x33,Frontier,"Get caller address"
0x34,Frontier,"Get deposited value by the instruction/transaction responsible for this execution"
0x35,Frontier,"Get input data of current environment"
0x36,Frontier,"Get size of input data in current environment"
0x37,Frontier,"Copy input data in current environment to memory"
0x38,Frontier,"Get size of code running in current environment"
0x39,Frontier,"Copy code running in current environment to memory"
0x3A,Frontier,"Get price of gas in current environment"
0x3B,Frontier,"Get size of an account’s code"
0x3C,Frontier,"Copy an account’s code to memory"
0x3D,Byzantium,"Get size of output data from the previous call from the current environment"
0x3E,Byzantium,"Copy output data from the previous call to memory"
0x3F,Constantinople,"Get hash of an account’s code"
0x40,Frontier,"Get the hash of one of the 256 most recent complete blocks"
0x41,Frontier,"Get the block’s beneficiary address"
0x42,Frontier,"Get the block’s timestamp"
0x43,Frontier,"Get the block’s number"
0x44,Frontier,"Get the block’s difficulty"
0x44_MERGE,Merge,"Get the previous block’s RANDAO mix"
0x45,Frontier,"Get the block’s gas limit"
0x46,Istanbul,"Get the chain ID"
0x47,Istanbul,"Get balance of currently executing account"
0x48,London,"Get the base fee"
0x49,Cancun,"Get versioned hashes"
0x4A,Cancun,"Returns the value of the blob base-fee of the current block"
0x50,Frontier,"Remove item from stack"
0x51,Frontier,"Load word from memory"
0x52,Frontier,"Save word to memory"
0x53,Frontier,"Save byte to memory"
0x54,Frontier,"Load word from storage"
0x55,Frontier,"Save word to storage"
0x56,Frontier,"Alter the program counter"
0x57,Frontier,"Conditionally alter the program counter"
0x58,Frontier,"Get the value of the program counter prior to the increment corresponding to this instruction"
0x59,Frontier,"Get the size of active memory in bytes"
0x5A,Frontier,"Get the amount of available gas, including the corresponding reduction for the cost of this instruction"
0x5B,Frontier,"Mark a valid destination for jumps"
0x5C,Cancun,"Load word from transient storage"
0x5D,Cancun,"Save word to transient storage"
0x5E,Cancun,"Copy memory areas"
0x5F,Shanghai,"Place value 0 on stack"
0x60,Frontier,"Place 1 byte item on stack"
0x61,Frontier,"Place 2 byte item on stack"
0x62,Frontier,"Place 3 byte item on stack"
0x63,Frontier,"Place 4 byte item on stack"
0x64,Frontier,"Place 5 byte item on stack"
0x65,Frontier,"Place 6 byte item on stack"
0x66,Frontier,"Place 7 byte item on stack"
0x67,Frontier,"Place 8 byte item on stack"
0x68,Frontier,"Place 9 byte item on stack"
0x69,Frontier,"Place 10 byte item on stack"
0x6A,Frontier,"Place 11 byte item on stack"
0x6B,Frontier,"Place 12 byte item on stack"
0x6C,Frontier,"Place 13 byte item on stack"
0x6D,Frontier,"Place 14 byte item on stack"
0x6E,Frontier,"Place 15 byte item on stack"
0x6F,Frontier,"Place 16 byte item on stack"
0x70,Frontier,"Place 17 byte item on stack"
0x71,Frontier,"Place 18 byte item on stack"
0x72,Frontier,"Place 19 byte item on stack"
0x73,Frontier,"Place 20 byte item on stack"
0x74,Frontier,"Place 21 byte item on stack"
0x75,Frontier,"Place 22 byte item on stack"
0x76,Frontier,"Place 23 byte item on stack"
0x77,Frontier,"Place 24 byte item on stack"
0x78,Frontier,"Place 25 byte item on stack"
0x79,Frontier,"Place 26 byte item on stack"
0x7A,Frontier,"Place 27 byte item on stack"
0x7B,Frontier,"Place 28 byte item on stack"
0x7C,Frontier,"Place 29 byte item on stack"
0x7D,Frontier,"Place 30 byte item on stack"
0x7E,Frontier,"Place 31 byte item on stack"
0x7F,Frontier,"Place 32 byte (full word) item on stack"
0x80,Frontier,"Duplicate 1st stack item"
0x81,Frontier,"Duplicate 2nd stack item"
0x82,Frontier,"Duplicate 3rd stack item"
0x83,Frontier,"Duplicate 4th stack item"
0x84,Frontier,"Duplicate 5th stack item"
0x85,Frontier,"Duplicate 6th stack item"
0x86,Frontier,"Duplicate 7th stack item"
0x87,Frontier,"Duplicate 8th stack item"
0x88,Frontier,"Duplicate 9th stack item"
0x89,Frontier,"Duplicate 10th stack item"
0x8A,Frontier,"Duplicate 11th stack item"
0x8B,Frontier,"Duplicate 12th stack item"
0x8C,Frontier,"Duplicate 13th stack item"
0x8D,Frontier,"Duplicate 14th stack item"
0x8E,Frontier,"Duplicate 15th stack item"
0x8F,Frontier,"Duplicate 16th stack item"
0x90,Frontier,"Exchange 1st and 2nd stack items"
0x91,Frontier,"Exchange 1st and 3rd stack items"
0x92,Frontier,"Exchange 1st and 4th stack items"
0x93,Frontier,"Exchange 1st and 5th stack items"
0x94,Frontier,"Exchange 1st and 6th stack items"
0x95,Frontier,"Exchange 1st and 7th stack items"
0x96,Frontier,"Exchange 1st and 8th stack items"
0x97,Frontier,"Exchange 1st and 9th stack items"
0x98,Frontier,"Exchange 1st and 10th stack items"
0x99,Frontier,"Exchange 1st and 11th stack items"
0x9A,Frontier,"Exchange 1st and 12th stack items"
0x9B,Frontier,"Exchange 1st and 13th stack items"
0x9C,Frontier,"Exchange 1st and 14th stack items"
0x9D,Frontier,"Exchange 1st and 15th stack items"
0x9E,Frontier,"Exchange 1st and 16th stack items"
0x9F,Frontier,"Exchange 1st and 17th stack items"
0xA0,Frontier,"Append log record with no topics"
0xA1,Frontier,"Append log record with one topic"
0xA2,Frontier,"Append log record with two topics"
0xA3,Frontier,"Append log record with three topics"
0xA4,Frontier,"Append log record with four topics"
0xF0,Frontier,"Create a new account with associated code"
0xF1,Frontier,"Message-call into an account"
0xF2,Frontier,"Message-call into this account with alternative account’s code"
0xF3,Frontier,"Halt execution returning output data"
0xF4,Byzantium,"Message-call into this account with an alternative account’s code, but persisting the current values for sender and value"
0xF5,Constantinople,"Create a new account with associated code at a predictable address"
0xFA,Byzantium,"Static message-call into an account"
0xFD,Byzantium,"Halt execution reverting state changes but returning data and remaining gas"
0xFE,Homestead,"Designated invalid instruction"
0xFF,Frontier,"Halt execution and register account for later deletion or send all Ether to address (post-Cancun)"
```
