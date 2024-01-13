# Bytecode Instruction Reference

## INTERNAL_FETCH (0x00)

### Description

### Arguments

```
INTERNAL_FETCH ()
```

## END (0x01)

### Description

Indicates the end of the script.

### Arguments

```
END ()
```

## RETURN (0x02)

### Description

Returns from the script and kills it and its child if it has one. If it has a parent that it is blocking, copies the local variables and flags to the parent.

### Arguments

```
RETURN ()
```

## LABEL (0x03)

### Description

Creates a label that can be jumped to with a goto statement. Up to 16 labels can be created per script.

### Arguments

```
LABEL (id: int)
```

## GOTO (0x04)

### Description

Jumps execution to a position in the script specified by a label id.

### Arguments

```
GOTO (id: var)
```

## LOOP (0x05)

### Description

Repeatedly executes the intructions in the next loop body a specified number of times. A count of 0 will repeat indefinitely until a BREAK_LOOP instruction is encountered. Hangs if the loop depth is greater or equal to 8.

### Arguments

```
LOOP (count: int)
```

## END_LOOP (0x06)

### Description

Designates the end of a loop body. Hangs if the loop depth is less than 0.

### Arguments

```
END_LOOP ()
```

## BREAK_LOOP (0x07)

### Description

Breaks out of a loop and jumps execution to immediately after the next END_LOOP instruction. Hangs if the loop depth is less than 0 or it reaches an `END` instruction first.

### Arguments

```
BREAK_LOOP ()
```

## WAIT_FRAMES (0x08)

### Description

Blocks the current script's execution for the specified number of frames.

### Arguments

```
WAIT_FRAMES (frames: var)
```

## WAIT_SECS (0x09)

### Description

Blocks the current script's execution for the specified number of seconds.

### Arguments

```
WAIT_SECS (seconds: float)
```

## IF_EQ (0x0A)

### Description

Executes the following instructions if `ls == rs`, otherwise jumps execution to immediately after the next `ELSE` or `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
IF_EQ (ls: var, rs: var)
```

## IF_NE (0x0B)

### Description

Executes the following instructions if `ls != rs`, otherwise jumps execution to immediately after the next `ELSE` or `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
IF_NE (ls: var, rs: var)
```

## IF_LT (0x0C)

### Description

Executes the following instructions if `ls < rs`, otherwise jumps execution to immediately after the next `ELSE` or `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
IF_LT (ls: var, rs: var)
```

## IF_GT (0x0D)

### Description

Executes the following instructions if `ls > rs`, otherwise jumps execution to immediately after the next `ELSE` or `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
IF_GT (ls: var, rs: var)
```

## IF_LE (0x0E)

### Description

Executes the following instructions if `ls <= rs`, otherwise jumps execution to immediately after the next `ELSE` or `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
IF_LE (ls: var, rs: var)
```

## IF_GE (0x0F)

### Description

Executes the following instructions if `ls >= rs`, otherwise jumps execution to immediately after the next `ELSE` or `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
IF_GE (ls: var, rs: var)
```

## IF_FLAG (0x10)

### Description

Executes the following instructions if `val & flag != 0`, otherwise jumps execution to immediately after the next `ELSE` or `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
IF_FLAG (val: var, flag: int)
```

## IF_NOT_FLAG (0x11)

### Description

Executes the following instructions if `val & flag == 0`, otherwise jumps execution to immediately after the next `ELSE` or `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
IF_NOT_FLAG (val: var, flag: int)
```

## ELSE (0x12)

### Description

Jumps execution to immediately after the next `END_IF` instruction. Hangs if it reaches the `END` instruction first.

### Arguments

```
ELSE ()
```

## END_IF (0x13)

### Description

Designates the end of an if statement block.

### Arguments

```
END_IF ()
```

## SWITCH (0x14)

### Description

Designates the start of a switch block. Stores the contents of `val` as the current switch block value. Hangs if the switch depth is greater than or equal to 8.

### Arguments

```
SWITCH (val: var)
```

## SWITCH_CONST (0x15)

### Description

Designates the start of a switch block. Stores val as the current switch block value. Hangs if the switch depth is greater than or equal to 8.

### Arguments

```
SWITCH_CONST (val: int)
```

## CASE_EQ (0x16)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val == switch value`, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_EQ (val: var)
```

## CASE_NE (0x17)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val != switch value`, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_NE (val: var)
```

## CASE_LT (0x18)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val < switch value`, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_LT (val: var)
```

## CASE_GT (0x19)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val > switch value`, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_GT (val: var)
```

## CASE_LE (0x1A)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val <= switch value`, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_LE (val: var)
```

## CASE_GE (0x1B)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val >= switch value`, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_GE (val: var)
```

## CASE_DEFAULT (0x1C)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_DEFAULT ()
```

## CASE_OR_EQ (0x1D)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val == switch value` or any of the following `CASE_OR_EQ` values equal the switch value until `END_CASE_GROUP` is encountered, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_OR_EQ (val: var)
```

## CASE_AND_EQ (0x1E)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val == switch value` and all of the following `CASE_AND_EQ` values equal the switch value until `END_CASE_GROUP` is encountered, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_AND_EQ (val: var)
```

## CASE_FLAG (0x1F)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `val & switch value != 0`, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_FLAG (val: int)
```

## END_CASE_GROUP (0x20)

### Description

Ends a `CASE_OR_EQ` or `CASE_AND_EQ` matching group.

### Arguments

```
END_CASE_GROUP ()
```

## CASE_RANGE (0x21)

### Description

Jumps execution to immediately after the next `END_SWITCH` if the switch value has already been matched. If not, executes the following instructions if `min <= switch value <= max`, otherwise, jumps execution to the next case statement. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
CASE_RANGE (min: var, max: var)
```

## BREAK_SWITCH (0x22)

### Description

Jumps execution to immediately after the next `END_SWITCH`. Hangs if it reaches the `END` instruction first or if the switch depth is less than 0.

### Arguments

```
BREAK_SWITCH ()
```

## END_SWITCH (0x23)

### Description

Designates the end of a switch statement block. Hangs if the switch depth is less than 0.

### Arguments

```
END_SWITCH ()
```

## SET (0x24)

### Description

Sets the contents of `var` to the contents of `val`.

### Arguments

```
SET (var: var, val: var)
```

## SET_CONST (0x25)

### Description

Sets the contents of `var` to `val`.

### Arguments

```
SET_CONST (var: var, val: int)
```

## SETF (0x26)

### Description

Sets the contents of `var` to the contents of `val`.

### Arguments

```
SETF (var: var, val: float_var)
```

## ADD (0x27)

### Description

Adds the contents of `addend` to the contents of `res`, storing the result in `res`.

### Arguments

```
ADD (addend: var, res: var)
```

## SUB (0x28)

### Description

Subtracts the contents of `minuend` from the contents of `res`, storing the result in `res`.

### Arguments

```
SUB (minuend: var, res: var)
```

## MUL (0x29)

### Description

Multiplies the contents of `multiplier` with the contents of `res`, storing the result in `res`.

### Arguments

```
MUL (multiplier: var, res: var)
```

## DIV (0x2A)

### Description

Divides the contents of `res` from the contents of `dividend`, storing the quotient in `res`.

### Arguments

```
DIV (dividend: var, res: var)
```

## MOD (0x2B)

### Description

Divides the contents of `res` from the contents of `dividend`, storing the remainder in `res`.

### Arguments

```
MOD (dividend: var, res: var)
```

## ADDF (0x2C)

### Description

Adds the contents of `addend` to the contents of `res`, storing the result in `res`.

### Arguments

```
ADDF (addend: float_var, res: float_var)
```

## SUBF (0x2D)

### Description

Subtracts the contents of `minuend` from the contents of `res`, storing the result in `res`.

### Arguments

```
SUBF (minuend: float_var, res: float_var)
```

## MULF (0x2E)

### Description

Multiplies the contents of `multiplier` with the contents of `res`, storing the result in `res`.

### Arguments

```
MULF (multiplier: float_var, res: float_var)
```

## DIVF (0x2F)

### Description

Divides the contents of ` ` from the contents of `dividend`, storing the quotient in `res`.

### Arguments

```
DIVF (dividend: float_var, res: float_var)
```

## USE_BUF (0x30)

### Description

Sets the script's buffer pointer to the contents of `ptr`.

### Arguments

```
USE_BUF (ptr: var)
```

## BUF_READ1 (0x31)

### Description

Pops one integer from the script's buffer pointer, storing it in `var1`. The buffer pointer is advanced by one position.

### Arguments

```
BUF_READ1 (var1: var)
```

## BUF_READ2 (0x32)

### Description

Pops two integers from the script's buffer pointer, storing them in `var1` and `var2`. The buffer pointer is advanced by two positions.

### Arguments

```
BUF_READ2 (var1: var, var2: var)
```

## BUF_READ3 (0x33)

### Description

Pops three integers from the script's buffer pointer, storing them in `var1`, `var2`, and `var3`. The buffer pointer is advanced by three positions.

### Arguments

```
BUF_READ3 (var1: var, var2: var, var3: var)
```

## BUF_READ4 (0x34)

### Description

Pops four integers from the script's buffer pointer, storing them in `var1`, `var2`, `var3`, and `var4`. The buffer pointer is advanced by four positions.

### Arguments

```
BUF_READ4 (var1: var, var2: var, var3: var, var4: var)
```

## BUF_PEEK (0x35)

### Description

Gets the integer from the script's int buffer pointer at the offset stored in `idx`, storing the result in `res`. Does not advance the buffer pointer.

### Arguments

```
BUF_PEEK (res: var, idx: var)
```

## USE_FBUF (0x36)

### Description

Sets the script's buffer pointer to the contents of `ptr`.

### Arguments

```
USE_FBUF (ptr: var)
```

## FBUF_READ1 (0x37)

### Description

Pops one float from the script's buffer pointer, storing it in `var1`. The buffer pointer is advanced by one position.

### Arguments

```
FBUF_READ1 (var1: float_var)
```

## FBUF_READ2 (0x38)

### Description

Pops two floats from the script's buffer pointer, storing them in `var1` and `var2`. The buffer pointer is advanced by two positions.

### Arguments

```
FBUF_READ2 (var1: float_var, var2: float_var)
```

## FBUF_READ3 (0x39)

### Description

Pops three floats from the script's buffer pointer, storing them in `var1`, `var2`, and `var3`. The buffer pointer is advanced by three positions.

### Arguments

```
FBUF_READ3 (var1: float_var, var2: float_var, var3: float_var)
```

## FBUF_READ4 (0x3A)

### Description

Pops four floats from the script's buffer pointer, storing them in `var1`, `var2`, `var3`, and `var4`. The buffer pointer is advanced by four positions.

### Arguments

```
FBUF_READ4 (var1: float_var, var2: float_var, var3: float_var, var4: float_var)
```

## FBUF_PEEK (0x3B)

### Description

Gets the float from the script's buffer pointer at the offset stored in `idx`, storing the result in `res`. Does not advance the buffer pointer.

### Arguments

```
FBUF_PEEK (res: float_var, idx: var)
```

## USE_ARRAY (0x3C)

### Description

Sets the script's array pointer to the contents of `ptr`.

### Arguments

```
USE_ARRAY (ptr: var)
```

## USE_FLAGS (0x3D)

### Description

Sets the script's flag array pointer to the contents of `ptr`.

### Arguments

```
USE_FLAGS (ptr: var)
```

## MALLOC_ARRAY (0x3E)

### Description

Allocates an array of `size` words, setting the script's array pointer to the allocated array and storing the pointer in `res`. The array is padded to 0x10 bytes.

### Arguments

```
MALLOC_ARRAY (size: var, res: var)
```

## BITWISE_AND (0x3F)

### Description

ANDs the contents of `val` with the contents of `res`, storing the result in `res`.

### Arguments

```
BITWISE_AND (res: var, val: var)
```

## BITWISE_AND_CONST (0x40)

### Description

ANDs `val` with the contents of `res`, storing the result in `res`.

### Arguments

```
BITWISE_AND_CONST (res: var, val: int)
```

## BITWISE_OR (0x41)

### Description

ORs the contents of `val` with the contents of `res`, storing the result in `res`.

### Arguments

```
BITWISE_OR (res: var, val: var)
```

## BITWISE_OR_CONST (0x42)

### Description

ORs `val` with the contents of `res`, storing the result in `res`.

### Arguments

```
BITWISE_OR_CONST (res: var, val: var)
```

## CALL (0x43)

### Description

Calls `func` with the provided arguments. Blocks the script's execution if the function does not return 2, calling the function every frame until it does.

### Arguments

```
CALL (func: var, args...)
```

## EXEC (0x44)

### Description

Creates a new script with the same priority and group of the current script and starts execution at the address stored in `script`. Copies the current script's local variables, flags, and array and flag array pointers to the new script. Hangs if there are already 128 scripts in the script list.

### Arguments

```
EXEC (script: var)
```

## EXEC_GET_TID (0x45)

### Description

Identical to `EXEC` but stores the id of the new script in `id`.

### Arguments

```
EXEC_GET_TID (script: var, id: var)
```

## EXEC_WAIT (0x46)

### Description

Creates a new script and blocks the current script's execution until it finishes executing.

### Arguments

```
EXEC_WAIT (script: var)
```

## BIND_TRIGGER (0x47)

### Description

Binds a trigger of type `type` to a script, executing the script whenever the trigger is activated. Stores the address of the trigger in `trigger`.

### Arguments

```
BIND_TRIGGER (script: var, type: int, collider_id: var, interact_prompt: int, trigger: var)
```

## UNBIND (0x48)

### Description

Deletes the current script's trigger.

### Arguments

```
UNBIND ()
```

## KILL_THREAD (0x49)

### Description

Kills every script with the id stored in `id` as well as their children. If it has a parent that it is blocking, copies the local variables and flags to the parent.

### Arguments

```
KILL_THREAD (id: var)
```

## JUMP (0x4A)

### Description

Jumps execution to the contents of `script`, clearing all current labels and loading labels from the new script.

### Arguments

```
JUMP (script: var)
```

## SET_PRIORITY (0x4B)

### Description

Sets the priority of the current script to the contents of `priority`.

### Arguments

```
SET_PRIORITY (priority: var)
```

## SET_TIMESCALE (0x4C)

### Description

Sets the timescale of the current script to the contents of `scale`.

### Arguments

```
SET_TIMESCALE (scale: float_var)
```

## SET_GROUP (0x4D)

### Description

Sets the group number of the current script to the contents of `group`.

### Arguments

```
SET_GROUP (group: var)
```

## BIND_PADLOCK (0x4E)

### Description

Similar to `BIND` but displays a prompt containing a list of selectable items.

### Arguments

```
BIND_PADLOCK (script: var, type: int, collider_id: var, item_list: var, trigger: int, interact_prompt: int)
```

## SUSPEND_GROUP (0x4F)

### Description

Suspends execution of all scripts with the group number stored in `group`.

### Arguments

```
SUSPEND_GROUP (group: var)
```

## RESUME_GROUP (0x50)

### Description

Resumes execution of all scripts with the group number stored in `group`.

### Arguments

```
RESUME_GROUP (group: var)
```

## SUSPEND_OTHERS (0x51)

### Description

Suspends execution of all scripts with the group number stored in `group` except the current script.

### Arguments

```
SUSPEND_OTHERS (group: var)
```

## RESUME_OTHERS (0x52)

### Description

Resumes execution of all scripts with the group number stored in `group` except the current script.

### Arguments

```
RESUME_OTHERS (group: var)
```

## SUSPEND_THREAD (0x53)

### Description

Suspends every script with the id stored in `id` as well as their children.

### Arguments

```
SUSPEND_THREAD (id: var)
```

## RESUME_THREAD (0x54)

### Description

Resumes every script with the id stored in `id` as well as their children.

### Arguments

```
RESUME_THREAD (id: var)
```

## IS_THREAD_RUNNING (0x55)

### Description

Checks if a script with the id stored in `id` is in the current script list, storing 1 in `out` if it is and 0 otherwise.

### Arguments

```
IS_THREAD_RUNNING (id: var, out: var)
```

## THREAD (0x56)

### Description

Executes the following instructions in a new script, copying the current script's local variables, flags, and array and flag array pointers to the new script. Is not killed if the parent script is terminated. Hangs if there are already 128 scripts in the script list.

### Arguments

```
THREAD ()
```

## END_THREAD (0x57)

### Description

Designates the end of the thread block.

### Arguments

```
END_THREAD ()
```

## CHILD_THREAD (0x58)

### Description

Behaves like `THREAD` but the new script is killed if its parent is.

### Arguments

```
CHILD_THREAD ()
```

## END_CHILD_THREAD (0x59)

### Description

Designates the end of the child thread block.

### Arguments

```
END_CHILD_THREAD ()
```

## DEBUG_LOG (0x5A)

### Description

Does nothing.

### Arguments

```
DEBUG_LOG ()
```

## DEBUG_PRINT_VAR (0x5B)

### Description

Prints `var` and its value.

### Arguments

```
DEBUG_PRINT_VAR (var: var)
```

## OP_92 (0x5C)

### Description

Saves the current script pointer (unused).

### Arguments

```
OP_92 ()
```

## OP_93 (0x5D)

### Description

Does nothing.

### Arguments

```
OP_93 ()
```

## OP_94 (0x5E)

### Description

Checks if the current script is in the script list. Always returns 1.

### Arguments

```
OP_94 ()
```
