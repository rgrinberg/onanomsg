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

## Example

```
let () = 
  let open Onanomsg.Domain in
  let open Onanomsg.Socket in
  let address = "inproc://t2t" in
  let sub = socket ~domain:Af_sp ~sock_type:sub in
  let endpoint = Onanomsg.connect sub ~address in
  Onanomsg.subscribe sub ~topic:"";
  let packet = "foo bar baz" in
  let pub = socket ~domain:Af_sp ~sock_type:pub in
  let endpoint = Onanomsg.bind pub ~address in
  Onanomsg.send pub packet;
  let recv_msg = Onanomsg.recv sub in
  Printf.printf "Received: %s\n" recv_msg;
  Onanomsg.close pub;
  Onanomsg.close sub
```

## Overview

For now these bindings are as close possible to the C interface but that is
likely to change in the future.

## TODO
- Set socket type specific options
- Wrap getsockopt
- Add support for devices
- Get rid of oasis

## License

Onanomsg is licensed under the [WTFPL](http://www.wtfpl.net/). See LICENSE.
