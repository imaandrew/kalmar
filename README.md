# Kalmar

A compiler for the Paper Mario 64 scripting language.

## Usage

Compiling a single script

```bash
> kalmar -o output.bin file.scr
```

Supplying a symbol file in the format

```
sym1 = 0x80000000
sym2 = 0x80031530
```

```bash
> kalmar -o output.bin -s syms.txt file.scr
```

## Documentation

Kalmar documentation and a scripting language reference can be found at https://imaandrew.github.io/kalmar/

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
