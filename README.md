# ONanomsg

`ctypes`-based bindings to [nanomsg](https://github.com/250bpm/nanomsg) for OCaml

## Installation

The dependencies are:
* cstruct
* ctypes > 0.2
* ppx_deriving
* ipaddr
* [nanomsg](https://github.com/250bpm/nanomsg)
* (optional) lwt > 2.4.6


```
opam pin add .
```

## Examples

See `lib_test/suite.ml`

## Overview

For now these bindings are as close possible to the C interface but that is
likely to change in the future.

## TODO

- Add support for devices

## License

ONanomsg is licensed under the [WTFPL](http://www.wtfpl.net/). See LICENSE.
