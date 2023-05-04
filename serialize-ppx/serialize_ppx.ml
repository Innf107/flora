open Ppxlib

let expand_str ~loc ~path _ = raise (Failure "BBB")

let str_type_decl_generator =
  Deriving.Generator.make_noarg
    expand_str

let deriver =
  Deriving.add
    ~str_type_decl:str_type_decl_generator
    "serialize"
