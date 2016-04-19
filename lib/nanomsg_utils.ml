open Ctypes
module C = Nanomsg_bindings.C(Nanomsg_generated)

let int_of_duration = function `Inf -> -1 | `Ms x -> x
let int_of_bool = function false -> 0 | true -> 1
let bool_of_int = function 0 -> false | _ -> true

module Opt = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)
end

module Res = struct
  open Result

  let get_exn = function
    | Ok x -> x
    | Error _ -> invalid_arg "Result.get_exn"
  let map f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e
  let (>|=) e f = map f e
  let (>>=) x f = match x with
    | Error e -> Error e
    | Ok x -> f x
  let catch x ~ok ~err = match x with
    | Error e -> err e
    | Ok y -> ok y
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
    let rec inner i =
      let open C in
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

let error () =
  let code = C.nn_errno () in
  let err_string = C.nn_strerror code in
  let err_value =
    if code > 156384712
    then Symbol.errvalue_of_errno_exn code
    else "" in
  Result.Error (err_value, err_string)

let maybe_error cond f =
  let res = f () in
  if cond res then error () else Result.Ok res

let maybe_error_ign cond f =
  let res = f () in
  if cond res then error () else Result.Ok ()

let error_if_negative = maybe_error (fun x -> x < 0)
let error_if_notequal v = maybe_error (fun x -> x <> v)
let error_if_negative_ign = maybe_error_ign (fun x -> x < 0)
let error_if_notequal_ign v = maybe_error_ign (fun x -> x <> v)
