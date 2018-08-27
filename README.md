# cmm_of_wasm

A compiler from WebAssembly to native code, via the OCaml backend.
See [this blog post](http://simonjf.com/2018/08/27/cmm-of-wasm.html)
for motivation and a technical explanation.

The compiler only supports the `amd64` backend of OCaml at the moment (integer size is assumed to be 64-bit).

## Building

The compiler currently uses some functionality not present in the main OCaml compiler. To get `cmm_of_wasm` up and running, you'll need to install an opam switch running https://github.com/simonjf/ocaml/tree/patched-branch.

You will also need `libwasm` on an `opam pin`: https://github.com/SimonJF/libwasm.

I believe the only other dependencies are `dune` and `getopt`.

After that, simply run `make`.

## Usage

`./cmm_of_wasm <.wat or .wasm file>`

There are other options, too:

`./cmm_of_wasm --help`

This will generate `.o` and `.h` files, which you should be able to link with a C project.

## Running the test suite

Firstly, ensure you've cloned all submodules:

`git submodule update --recursive --remote`

After this, you should be able to `cd tests/wasm/` and run `./run-all-tests.py`.

## Project structure

  * `src/lib/cmmcompile`: Compiler from the Stackless IR to CMM code
  * `src/lib/ir`: Compilation from the WebAssembly AST to a stackless intermediate representation
  * `src/lib/util`: Utilities
  * `src/bin`: Entry point and build utilities (I will probably split off the build utilities into `lib` at some point)
  * `src/rts`: C runtime system, adapted from the `wasm2c` RTS (see https://github.com/webassembly/wabt)
  * `test/ounit`: Small OUnit test suite for function type hashes
  * `test/wasm`: Scaffolding for running the WebAssembly test suite, adapted from `wasm2c`'s infrastructure

## Acknowledgements

Thanks to Stephen Dolan, KC Sivaramakrishnan, and Mark Shinwell for useful discussions. Thanks also to Pierre Chambart for sharing an early prototype, and whose IR I used.

This work was funded by an internship at OCaml Labs, who also provided a truly wonderful working environment.

