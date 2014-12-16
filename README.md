# Onanomsg

Ctypes based bindings to nanomsg for OCaml (alpha)

## Installation

The dependencies are:
* ctypes > 0.2
* lwt > 2.4.6
* ppx_deriving
* ipaddr
* [nanomsg](https://github.com/250bpm/nanomsg)

```
opam pin add .
```

## Examples

See the `examples` directory.

## Overview

For now these bindings are as close possible to the C interface but that is
likely to change in the future.

## TODO

- Set socket type specific options
- Wrap getsockopt
- Add support for devices

## License

Onanomsg is licensed under the [WTFPL](http://www.wtfpl.net/). See LICENSE.
