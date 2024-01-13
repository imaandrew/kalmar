# Types

## Constants

All integers are stored as signed 32-bit numbers. They can be written in any of the following forms

| Base        | Example       |
|-------------|---------------|
| Decimal     | `523`         |
| Binary      | `0b100110`    |
| Octal       | `0o1342172`   |
| Hexadecimal | `0x41414141`  |

Fractional numbers are interpreted as 32-bit floating point numbers. They are converted to fixed-point numbers with a scaling factor of 1024 during compilation.

## Variables

| Name       | Size  | Description |
|------------|-------|-------------|
| Var        | 0x10  | Variables local to the current script |
| Flag       | 0x60  | Flags local to the current script |
| MapVar     | 0x10  | Global variables cleared upon map transition |
| MapFlag    | 0x60  | Global flags cleared upon map transition |
| AreaByte   | 0x10  | Bytes stored in save file. Cleared upon area transition |
| AreaFlag   | 0x100 | Flags stored in save file. Cleared upon area transition |
| GameByte   | 0x200 | Bytes stored in save file. Never cleared |
| GameFlag   | 0x800 | Flags stored in save file. Never cleared |
| Array      |   -   | Current script array. Must be assigned before use |
| FlagArray  |   -   | Current script flag array. Must be assigned before use|
| Buffer     |   -   | Current script buffer. Must be assigned before use |
