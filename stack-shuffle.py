import heapq
from typing import List, Tuple, Dict, Optional, Set
from dataclasses import dataclass
from enum import Enum


class OpType(Enum):
    POP = "pop"
    SWAP1 = "swap1"
    SWAP2 = "swap2"
    SWAP3 = "swap3"
    SWAP4 = "swap4"
    DUP1 = "dup1"
    DUP2 = "dup2"
    DUP3 = "dup3"
    DUP4 = "dup4"
    SET_LOCAL = "set_local"
    READ_LOCAL = "read_local"


@dataclass
class Operation:
    op_type: OpType
    cost: int
    local_idx: Optional[int] = None

    def __str__(self):
        if self.op_type in (OpType.SET_LOCAL, OpType.READ_LOCAL):
            return f"{self.op_type.value}_{self.local_idx}"
        return self.op_type.value


@dataclass
class State:
    stack: Tuple[str, ...]
    locals: Dict[int, str]
    locals_set: Set[int]  # Track which locals have been set
    cost: int
    operations: List[Operation]

    def __lt__(self, other):
        return self.cost < other.cost

    def __hash__(self):
        return hash((self.stack, tuple(sorted(self.locals.items())), tuple(sorted(self.locals_set))))

    def __eq__(self, other):
        return (self.stack == other.stack and
                self.locals == other.locals and
                self.locals_set == other.locals_set)


def apply_operation(state: State, op: Operation) -> Optional[State]:
    """Apply an operation to a state and return the new state, or None if invalid."""
    stack = list(state.stack)
    locals_dict = state.locals.copy()
    locals_set = state.locals_set.copy()

    if op.op_type == OpType.POP:
        if len(stack) < 1:
            return None
        stack.pop(0)

    elif op.op_type == OpType.SWAP1:
        if len(stack) < 2:
            return None
        stack[0], stack[1] = stack[1], stack[0]

    elif op.op_type == OpType.SWAP2:
        if len(stack) < 3:
            return None
        stack[0], stack[2] = stack[2], stack[0]

    elif op.op_type == OpType.SWAP3:
        if len(stack) < 4:
            return None
        stack[0], stack[3] = stack[3], stack[0]

    elif op.op_type == OpType.SWAP4:
        if len(stack) < 5:
            return None
        stack[0], stack[4] = stack[4], stack[0]

    elif op.op_type == OpType.DUP1:
        if len(stack) < 1:
            return None
        stack.insert(0, stack[0])

    elif op.op_type == OpType.DUP2:
        if len(stack) < 2:
            return None
        stack.insert(0, stack[1])

    elif op.op_type == OpType.DUP3:
        if len(stack) < 3:
            return None
        stack.insert(0, stack[2])

    elif op.op_type == OpType.DUP4:
        if len(stack) < 4:
            return None
        stack.insert(0, stack[3])

    elif op.op_type == OpType.SET_LOCAL:
        if len(stack) < 1:
            return None
        value = stack.pop(0)
        locals_dict[op.local_idx] = value
        locals_set.add(op.local_idx)

    elif op.op_type == OpType.READ_LOCAL:
        if op.local_idx not in locals_dict:
            return None
        stack.insert(0, locals_dict[op.local_idx])

    new_operations = state.operations + [op]
    new_cost = state.cost + op.cost

    return State(
        stack=tuple(stack),
        locals=locals_dict,
        locals_set=locals_set,
        cost=new_cost,
        operations=new_operations
    )


def get_possible_operations(state: State, max_locals: int = 5) -> List[Operation]:
    """Get all possible operations from the current state."""
    ops = []
    stack_len = len(state.stack)

    # Direct stack operations (cost 3)
    if stack_len >= 1:
        ops.append(Operation(OpType.POP, 3))
        ops.append(Operation(OpType.DUP1, 3))
    if stack_len >= 2:
        ops.append(Operation(OpType.SWAP1, 3))
        ops.append(Operation(OpType.DUP2, 3))
    if stack_len >= 3:
        ops.append(Operation(OpType.SWAP2, 3))
        ops.append(Operation(OpType.DUP3, 3))
    if stack_len >= 4:
        ops.append(Operation(OpType.SWAP3, 3))
        ops.append(Operation(OpType.DUP4, 3))
    if stack_len >= 5:
        ops.append(Operation(OpType.SWAP4, 3))

    # Local variable operations
    if stack_len >= 1:
        for i in range(max_locals):
            if i in state.locals_set:
                # Overwriting existing local (cost 6)
                ops.append(Operation(OpType.SET_LOCAL, 6, i))
            else:
                # Setting fresh local (cost 9)
                ops.append(Operation(OpType.SET_LOCAL, 9, i))

    for i in state.locals:
        # Reading local (cost 6)
        ops.append(Operation(OpType.READ_LOCAL, 6, i))

    return ops


def heuristic(current: State, target: Tuple[str, ...]) -> int:
    """Estimate minimum cost to reach target from current state."""
    # Simple heuristic: count mismatched positions
    current_stack = list(current.stack)
    target_stack = list(target)

    # If we have more elements than target, we need pops
    if len(current_stack) > len(target_stack):
        return (len(current_stack) - len(target_stack)) * 3

    # Count positions that need to be fixed
    mismatches = 0
    for i in range(min(len(current_stack), len(target_stack))):
        if current_stack[i] != target_stack[i]:
            mismatches += 1

    # Add cost for missing elements
    missing = len(target_stack) - len(current_stack)

    return (mismatches + missing) * 3  # Minimum cost per operation


def find_optimal_transformation(input_stack: List[str], target_stack: List[str], max_locals: int = 5) -> Tuple[List[Operation], int]:
    """Find the minimum-cost sequence of operations to transform input_stack to target_stack."""
    initial_state = State(
        stack=tuple(input_stack),
        locals={},
        locals_set=set(),
        cost=0,
        operations=[]
    )

    target = tuple(target_stack)

    # Priority queue: (priority, state)
    pq = [(heuristic(initial_state, target), initial_state)]
    visited = set()

    while pq:
        _, current = heapq.heappop(pq)

        # Check if we've reached the target
        if current.stack == target:
            return current.operations, current.cost

        # Skip if we've already visited this state
        state_key = (current.stack, tuple(
            sorted(current.locals.items())), tuple(sorted(current.locals_set)))
        if state_key in visited:
            continue
        visited.add(state_key)

        # Try all possible operations
        for op in get_possible_operations(current, max_locals):
            new_state = apply_operation(current, op)
            if new_state is None:
                continue

            # Skip if we've already visited this state
            new_state_key = (new_state.stack, tuple(
                sorted(new_state.locals.items())), tuple(sorted(new_state.locals_set)))
            if new_state_key in visited:
                continue

            # Add to priority queue with f-score (g + h)
            priority = new_state.cost + heuristic(new_state, target)
            heapq.heappush(pq, (priority, new_state))

    # No solution found
    return [], -1


# Example usage and test cases
if __name__ == "__main__":
    # Test case 1: Simple swap
    input1 = ["a", "b", "c"]
    target1 = ["b", "a", "c"]
    ops1, cost1 = find_optimal_transformation(input1, target1)
    print(f"Test 1: {input1} -> {target1}")
    print(f"Operations: {[str(op) for op in ops1]}")
    print(f"Total cost: {cost1}\n")

    # Test case 2: Duplicate element
    input2 = ["a", "b", "c"]
    target2 = ["a", "a", "b", "c"]
    ops2, cost2 = find_optimal_transformation(input2, target2)
    print(f"Test 2: {input2} -> {target2}")
    print(f"Operations: {[str(op) for op in ops2]}")
    print(f"Total cost: {cost2}\n")

    # Test case 3: Complex rearrangement
    input3 = ["a", "b", "c", "d"]
    target3 = ["c", "a", "d", "b"]
    ops3, cost3 = find_optimal_transformation(input3, target3)
    print(f"Test 3: {input3} -> {target3}")
    print(f"Operations: {[str(op) for op in ops3]}")
    print(f"Total cost: {cost3}\n")

    # Test case 4: Using locals might be cheaper
    input4 = ["a", "b", "c", "d", "e"]
    target4 = ["e", "a", "b", "c", "d"]
    ops4, cost4 = find_optimal_transformation(input4, target4)
    print(f"Test 4: {input4} -> {target4}")
    print(f"Operations: {[str(op) for op in ops4]}")
    print(f"Total cost: {cost4}\n")

    # Test case 5: Removing elements
    input5 = ["a", "b", "c", "d"]
    target5 = ["c", "d"]
    ops5, cost5 = find_optimal_transformation(input5, target5)
    print(f"Test 5: {input5} -> {target5}")
    print(f"Operations: {[str(op) for op in ops5]}")
    print(f"Total cost: {cost5}\n")

    # Run randomized testing
    test_random_inputs()
