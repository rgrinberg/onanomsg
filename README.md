# Onanomsg

Ctypes based bindings to nanomsg for OCaml (alpha)

## Installation

Currently you must have the HEAD version of ctypes. Because of the reliance
on `string_opt`

Please install [nanomsg](https://github.com/250bpm/nanomsg) first.

```
ocaml setup.ml -configure
ocaml setup.ml -all
ocaml setup.ml -install
```

## Overview

For now these bindings are as close possible to the C interface but that is
likely to change in the future.
