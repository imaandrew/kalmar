# Control Flow

## Wait

The `wait` function blocks execution of the script for a specified number of frames, while `wait_sec` blocks execution for a specified number of seconds

```
wait(5) // blocks script for 5 frames
wait_sec(22) // blocks script for 22 seconds
```

## If statements

An if statement conditionally executes a set of statements if a provided expression evaluates to true

```
if expr {
    // does something if expr is true
}
```

An alternate set of statements to be executed if the expression evaluates to false can be specified using an else statement

```
if expr {
    // does something if expr is true
} else {
    // does something if expr is false
}
```

Multiple conditions can also be chained together

```
if expr1 {
    // does something if expr1 is true
} else if expr2 {
    // does something if expr2 is true
} else {
    // does something if neither are true
}
```

The following conditions are available:

|||
|----|----|
| a == b | a is equal to b |
| a != b | a is not equal to b |
| a < b | a is less than b |
| a <= b | a is less than or equal to b |
| a > b | a is greater than b |
| a >= b | a is greater than or equal to b |
| a & b | the bits of b are set on a |

The inverse of any condition can be tested by prefixing the condition with a `!` character and surrounding it with parentheses

For example: `a == b is equivalent to !(a != b)`

## Labels and Goto statements

A label specifies a statement that can be jumped to using a goto statement. The scope of a label is limited to the script it is defined in.

```
if Var[0] == 6 {
    goto error
}

error:
return
```

## Loops

A loop executes a set of statements a specified number of times

```
loop 5 {
    // do something 5 times
}
```

If the loop count is 0 or is omitted, the loop will run indefinitely unless broken out of

```
loop {
    // this code will run forever...
    
    if expr { // until this condition is met
        bloop
    }
}
```

The `bloop` keyword can be used to break out of any loop whether or not it executes a finite number of times.

## Switch statements

A switch statement selects a set of statements to execute based on which condition matches the specified expression.

```
switch Var[0] {
    3 {
        // do something if Var[0] == 3
    }
    != 5 {
        // do something if Var[0] != 5
    }
}
```

The following conditions are available:

|||
|-|-|
|a | expression is equal to a |
|== a | expression is equal to a |
|!= a | expression is not equal to a |
|< a | expression is less than a |
|<= a | expression is less than or equal to a |
|> a | expression is greater than a |
|>= a | expression is greater than or equal to a |
|a | b | expression is equal to a or b |
|a & b | expression is equal to a and b |
|a..b | expression is between a and b (inclusive) |
| & a | the bits of expression are set on a |

Additionally, the `default` keyword can be used to designate a set of statements to execute if none of the previous conditions match.

The `bcase` keyword can be used to break out of any case statement and jump execution to immediately after the switch statement ends.

## Jump statements

A jump statement jumps execution to another script. The script to be jumped to can be defined anywhere in the file containing the script that contains the jump instruction.

```
scr script1 {
    // script body
}

scr script 2 {
    if Var[5] == 3 {
        jump script1
    }
}
```
