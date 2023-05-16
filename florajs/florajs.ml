open Js_of_ocaml
open Flora

type eval_result_or_error = < status : Js.js_string Js.t Js.readonly_prop >
type reified_value = < kind : Js.js_string Js.t Js.readonly_prop >

let rec reify_value : Syntax.value -> reified_value Js.t = function
  | Syntax.Nil ->
      (object%js
         val kind = Js.string "Nil"
       end
        :> reified_value Js.t)
  | Number n ->
      (object%js
         val kind = Js.string "Number"
         val value = Js.number_of_float n
       end
        :> reified_value Js.t)
  | String str ->
      (object%js
         val kind = Js.string "String"
         val value = Js.string str
       end
        :> reified_value Js.t)
  | Bool bool ->
      (object%js
         val kind = Js.string "Bool"
         val value = Js.bool bool
       end
        :> reified_value Js.t)
  | List items ->
      (object%js
         val kind = Js.string "List"
         val value = Js.array (Array.map reify_value (Array.of_list items))
       end
        :> reified_value Js.t)
  | Closure (env_lazy, params, expr) ->
      (object%js
         val kind = Js.string "Closure"
         val value = (env_lazy, params, expr)
       end
        :> reified_value Js.t)
  | Primop name ->
      (object%js
         val kind = Js.string "Primop"
         val value = name
       end
        :> reified_value Js.t)
  | Continuation cont ->
      (object%js
         val kind = Js.string "Continuation"
         val value = cont
       end
        :> reified_value Js.t)

let rec reflect_value : reified_value Js.t -> Syntax.value =
 fun reified ->
  match Js.to_string reified##.kind with
  | "Nil" -> Syntax.Nil
  | "Number" ->
      let reified :
          < kind : Js.js_string Js.t Js.readonly_prop
          ; value : Js.number Js.t Js.readonly_prop >
          Js.t =
        Js.Unsafe.coerce reified
      in
      Syntax.Number (Js.float_of_number reified##.value)
  | "String" ->
      let reified :
          < kind : Js.js_string Js.t Js.readonly_prop
          ; value : Js.js_string Js.t Js.readonly_prop >
          Js.t =
        Js.Unsafe.coerce reified
      in
      Syntax.String (Js.to_string reified##.value)
  | "Bool" ->
      let reified :
          < kind : Js.js_string Js.t Js.readonly_prop
          ; value : bool Js.t Js.readonly_prop >
          Js.t =
        Js.Unsafe.coerce reified
      in
      Syntax.Bool (Js.to_bool reified##.value)
  | "List" ->
      let reified :
          < kind : Js.js_string Js.t Js.readonly_prop
          ; value : reified_value Js.t Js.js_array Js.t Js.readonly_prop >
          Js.t =
        Js.Unsafe.coerce reified
      in
      Syntax.List
        (Array.to_list (Array.map reflect_value (Js.to_array reified##.value)))
  | "Closure" ->
      let reified :
          < kind : Js.js_string Js.t Js.readonly_prop
          ; value :
              (Syntax.env lazy_t * string list * Syntax.expr) Js.readonly_prop >
          Js.t =
        Js.Unsafe.coerce reified
      in
      let env, params, body = reified##.value in
      Syntax.Closure (env, params, body)
  | "Primop" ->
      let reified :
          < kind : Js.js_string Js.t Js.readonly_prop
          ; value : Syntax.primop Js.readonly_prop >
          Js.t =
        Js.Unsafe.coerce reified
      in
      Syntax.Primop reified##.value
  | "Continuation" ->
      let reified :
          < kind : Js.js_string Js.t Js.readonly_prop
          ; value : Syntax.continuation Js.readonly_prop >
          Js.t =
        Js.Unsafe.coerce reified
      in
      Syntax.Continuation reified##.value
  | kind -> raise (Failure ("Invalid flora value kind '" ^ kind ^ "'"))

let encode_eval_result :
    (Syntax.env * Syntax.value) Eval.eval_result -> eval_result_or_error Js.t =
  function
  | Eval.Completed (env, value) ->
      (object%js
         val status = Js.string "Completed"
         val env = env
         val value = reify_value value
       end
        :> eval_result_or_error Js.t)
  | Eval.Suspended (effect, arguments, cont) ->
      (object%js
         val status = Js.string "Suspended"
         val arguments = arguments
         val cont = cont
       end
        :> eval_result_or_error Js.t)

let encode_error : Error.t -> eval_result_or_error Js.t =
 fun err ->
  (object%js
     val status = Js.string "Error"
     val message = Js.string (Error.pretty err)
   end
    :> eval_result_or_error Js.t)

let () =
  Js.export "Flora"
    begin
      object%js
        val emptyEnv = Syntax.empty_env

        method evalString (filename : string) (env : Syntax.env) (str : string)
            : eval_result_or_error Js.t =
          Error.handle
            ~handler:(fun err -> encode_error err)
            begin
              fun () ->
                let result =
                  Driver.eval_string ~filename:(Some filename) env str
                in
                encode_eval_result result
            end

        method prettyValue (reified_value : reified_value Js.t)
            : Js.js_string Js.t =
          Js.string (Syntax.pretty_value (reflect_value reified_value))
      end
    end
