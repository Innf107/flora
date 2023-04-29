exception TODO of string

let todo loc = raise (TODO loc)

exception Panic of string * string

let panic loc message = raise (Panic (loc, message))
