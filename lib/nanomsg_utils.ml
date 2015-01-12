open Nanomsg_ctypes

let int_of_duration = function `Inf -> -1 | `Ms x -> x
let int_of_bool = function false -> 0 | true -> 1
let bool_of_int = function 0 -> false | _ -> true

module Opt = struct
  let run = function
    | Some v -> v
    | None -> invalid_arg "Opt.run"
end

module Ipaddr = struct
  include Ipaddr
  let pp = pp_hum
end

module Symbol = struct
  type t = {
    sp_value: int;
    sp_name: string;
    sp_ns: int;
    sp_type: int;
    sp_unit: int;
  }

  let table = Hashtbl.create 13

  (* This will be run at program start. *)
  let () =
    let open Ctypes in
    let rec inner i =
      let sp = make nn_symbol_properties in
      let ret = nn_symbol_info i (addr sp) (sizeof nn_symbol_properties) in
      if ret = 0 then () else
        let sp' =
          {
            sp_value = getf sp nnsym_value;
            sp_name = getf sp nnsym_name;
            sp_ns = getf sp nnsym_ns;
            sp_type = getf sp nnsym_type;
            sp_unit = getf sp nnsym_unit;
          } in
        Hashtbl.add table sp'.sp_name sp';
        inner @@ succ i
    in inner 0

  let value_of_name_exn name = let sp = Hashtbl.find table name in sp.sp_value
  let value_of_name name = try Some (value_of_name_exn name) with _ -> None
  let of_name_exn = Hashtbl.find table
  let of_name name = try Some (of_name_exn name) with _ -> None
  let errvalue_of_errno_exn errno =
    try
      Hashtbl.iter (fun k v ->
          if v.sp_value = errno then failwith v.sp_name)
        table; raise Not_found
    with Failure name -> name
  let errvalue_of_errno errno =
    try Some (errvalue_of_errno_exn errno) with Not_found -> None
end

exception Error of string * string

let throw () =
  let code = nn_errno () in
  let err_string = nn_strerror code in
  let err_value =
    if code > 156384712
    then Symbol.errvalue_of_errno_exn code
    else "" in
  raise (Error (err_value, err_string))

let raise_if cond f =
  let res = f () in
  if cond res then throw () else res

let raise_negative = raise_if (fun x -> x < 0)
let raise_notequal v = raise_if (fun x -> x <> v)
