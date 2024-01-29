# bytebraise
This project is a proof-of-concept experiment to see how much of [BitBake](https://github.com/openembedded/bitbake) can be implemented in Rust.

## Status
This is very early code and still somewhat a testbed for ideas.

## What is implemented
* Lexer and parser for the BitBake language, backed by [Rowan](https://github.com/rust-analyzer/rowan)
* Partial re-implementation of [`DataSmart`](https://github.com/openembedded/bitbake/blob/master/lib/bb/data_smart.py)
  * Notably lacking: caching, variable history, and the new `:` override syntax
* Proof-of-concept Python support - enable with the `python` feature when building
  * See for example: https://github.com/Agilent/bytebraise/blob/main/src/data_smart/variable_parse.rs

## FAQ
**Q:** Do you intend to replace BitBake?<br/>
**A:** Definitely not.

**Q:** Then why build this?<br/>
**A:** Mostly just to see if it's possible and as an exercise to learn Rust. I also wanted a deeper understanding of BitBake, and what better way to get that than by reimplementing it?

**Q:** What could it be used for?<br/>
**A:** Right now, not much. Eventually, it could be useful for linters and formatters. What's especially nice about Rust is that you can build statically-linked executables and thus avoid the [nightmare](https://xkcd.com/1987/) of distributing Python applications. Do however note that it's not currently feasible to statically-link applications using bytebraise's optional `python` support, due to [this issue](https://github.com/PyO3/pyo3/issues/416).


## License
bytebraise is licensed under the GNU General Public License version 2.0. See LICENSE.GPL-2.0-only for further details.

Portions of this work have been derived from [Rust Analyzer](https://github.com/rust-analyzer/rust-analyzer), which is MIT
licensed. Those derived portions are clearly marked at the top of the files. The MIT license is included (LICENSE.MIT)
for your reference.

## Disclaimer
This is not an official Agilent product. No support is implied.
