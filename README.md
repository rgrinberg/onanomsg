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
  let address = "inproc://t2t" in
  let sub = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Sub in
  let endpoint = Onanomsg.connect sub ~address in
  Onanomsg.subscribe sub ~topic:"";
  let packet = "foo bar baz" in
  let pub = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Pub in
  let endpoint = Onanomsg.bind pub ~address in
  let (`Read _) = Onanomsg.send pub packet in
  let recv_msg = Onanomsg.recv sub in
  Printf.printf "Received: %s\n" recv_msg;
  Onanomsg.close pub;
  Onanomsg.close sub
```

## Overview

For now these bindings are as close possible to the C interface but that is
likely to change in the future.
