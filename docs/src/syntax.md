# Syntax

## Scripts

Scripts are defined using the `scr` keyword followed by the name of the script and a set of curly brackets containing the contents of the script

```
scr cool_script {
    Var[0] = 2
    if Var[1] == 4 {
        return
    }
    Var[1] = 6
    return
}
```

Note that newlines are used to terminate each statement

## Comments

Line comments are denoted with `//` and run to the end of the line. They can be placed at the beginning of a line:

```
// this is a line comment
Var[0] = 5
```

or at the end of a line

```
scr main {
    Var[0] = 5 // this is also a valid comment
}
```

Block comments are denoted with `/* */` where everything in between the delimiters is commented out. They can be used in place of line comments but can additionally be placed in the middle of statements and can span multiple lines:

```
/*
This entire block
is
a
comment
*/

Var[0] = 2 + /* this is a valid statement */ 5
```
