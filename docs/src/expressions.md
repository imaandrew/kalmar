# Expressions

## Assignment

Variables may be assigned values using the `=` operator. They may be assigned integers, floating point numbers, values stored in variables, and references to other variables.

```
Var[0] = 5
Var[0] = 5.5
Var[0] = Var[1]
Var[0] = &Var[1]
```

## Arithmetic

Arithmetic operators operate on a value stored in a variable and store the result back in the original variable. 

    // Adds 5 to the value stored in Var[0]
    Var[0] += 5

The following operators are provided:

| Operator | Operation | Valid operands |
|----------|-----------|----------------|
| += | Addition | Integers, Floats |
| -= | Subtraction | Integers, Floats |
| *= | Multipication | Integers, Floats |
| /= | Division | Integers, Floats |
| %= | Modulus | Integers |
| \|= | Bitwise OR | Integers, Variables |
| &= | Bitwise AND | Integers, Variables |

## Constant Expressions

Constant expressions are expressions that can be resolved by the compiler at compile time. They are provided for use in cases when representing a value as an expression may be more clear than using its fully evaluated form. These expressions may only include numbers and the following operators:

```
arithmetic:
+, -, *, /, %, &

boolean:
==, !=, <, <=, >, >=
```

Additionally, parentheses may be used to explicitly specify operator precedence

## Buffers and Arrays

### Script Buffer

The script buffer is a preexisting array that stores 32-bit integers which can be popped off by the current script.

The buffer must be assigned before use. It can be assigned by doing the following:

```
Buffer = Var[0]
```

Integers can be retrieved in order from the buffer. Note that this will advance the buffer by however many integers were accessed

```
Var[1], Var[2], Var[3] <- Buffer
```

Integers can also be retrieved by index without advancing the buffer

```
Var[2] = Buffer[1]
```

### Script Array

The script array and flag array are arrays that can either be created within the script or be allocated before the script. They must be assigned before use

An array can be allocated with a specific size using the `alloc` function. This will return the allocated array pointer and also overwrite the current script's array pointer to match

```
Var[2] = alloc(5)
```

If an array already exists, it can be set as the current script array or flag array by assigning either the variable `Array` or `FlagArray`

```
Array = Var[2]
FlagArray = Var[3]
```

Values in the array can be accessed by index

```
Var[3] = Array[5]
Var[4] = FlagArray[0]
```
