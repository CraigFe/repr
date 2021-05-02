open Ppxlib
open Ppxlib.Ast_builder.Default
open Repr.Unstable.Core

type 'a synth = location -> 'a -> expression

let rec t : type a. a t -> a synth = function
  | Prim x -> prim x
  | List { v = elt; _ } ->
      let elt = t elt in
      fun loc xs -> List.map (fun x -> elt loc x) xs |> elist ~loc
  | Array { v = elt; _ } ->
      let elt = t elt in
      fun loc xs ->
        Array.map (fun x -> elt loc x) xs |> Array.to_list |> pexp_array ~loc
  | Tuple x -> tuple x
  | Option x -> option x
  | Record x -> record x
  | Variant x -> variant x
  | Boxed x -> t x
  | Self x -> fun loc a -> t x.self_fix loc a
  | Var _ -> failwith "Unbound type variable"
  | Map _ -> failwith "Cannot synthesise Map value"
  | Custom _ -> failwith "Cannot synthesise Custom value"

and prim : type a. a prim -> a synth =
  let integer to_string suffix loc x =
    pexp_constant ~loc (Pconst_integer (to_string x, suffix))
  in
  function
  | Unit -> fun loc () -> [%expr ()]
  | Bool -> fun loc b -> if b then [%expr true] else [%expr false]
  | Char -> fun loc x -> pexp_constant ~loc (Pconst_char x)
  | Int -> integer string_of_int None
  | Int32 -> integer Int32.to_string (Some 'l')
  | Int64 -> integer Int64.to_string (Some 'L')
  | Float -> integer Float.to_string (Some '.')
  | String _ -> fun loc x -> estring ~loc x
  | Bytes _ ->
      fun loc x ->
        [%expr Stdlib.Bytes.of_string [%e estring ~loc (Bytes.to_string x)]]

and tuple : type a. a tuple -> a synth = function
  | Pair (a_t, b_t) ->
      let a_t = t a_t and b_t = t b_t in
      fun loc (a, b) -> [%expr [%e a_t loc a], [%e b_t loc b]]
  | Triple (a_t, b_t, c_t) ->
      let a_t = t a_t and b_t = t b_t and c_t = t c_t in
      fun loc (a, b, c) ->
        [%expr [%e a_t loc a], [%e b_t loc b], [%e c_t loc c]]

and option : type a. a t -> a option synth =
 fun elt ->
  let elt = t elt in
  fun loc -> function
    | None -> [%expr None]
    | Some x -> [%expr Some [%e elt loc x]]

and record : type a. a record -> a synth =
 fun r ->
  let field_encoders : (string * a synth) list =
    fields r
    |> List.map @@ fun (Field f) ->
       let field_synth = t f.ftype in
       (f.fname, fun loc r -> field_synth loc (f.fget r))
  in
  fun loc r ->
    let fields =
      List.map
        (fun (name, f) -> (Located.lident ~loc name, f loc r))
        field_encoders
    in
    pexp_record ~loc fields None

and variant : type a. a variant -> a synth =
 fun v loc c (* XXX: unstaged *) ->
  match v.vget c with
  | CV0 c0 -> pexp_construct ~loc (Located.lident ~loc c0.cname0) None
  | CV1 (c1, x) ->
      pexp_construct ~loc
        (Located.lident ~loc c1.cname1)
        (Some (t c1.ctype1 loc x))

let apply_equality : type a b. (a, b) Repr.Witness.eq -> a -> b =
 fun wit a -> match wit with Refl -> a

let static_value : type a. a Repr.t -> location -> a -> expression =
 fun typ ->
  let t = t (apply_equality Repr.Unstable.observe typ) in
  fun loc x -> t loc x
