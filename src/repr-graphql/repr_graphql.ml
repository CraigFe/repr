open Graphql
open Repr.Unstable.Core

type 'a schema =
  | Nullable : (unit, 'a option) Schema.typ -> 'a option schema
  | Non_null : (unit, 'a option) Schema.typ -> 'a schema

let v x = Non_null x

let unwrap (type a) (t : a schema) : (unit, a) Schema.typ =
  match t with Non_null typ -> Schema.non_null typ | Nullable typ -> typ

let partition_map :
    type a b c. a list -> f:(a -> (b, c) Either.t) -> b list * c list =
 fun l ~f ->
  let rec aux left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l -> (
        match f x with
        | Left v -> aux (v :: left) right l
        | Right v -> aux left (v :: right) l)
  in
  aux [] [] l

let rec t : type a. a t -> a schema = function
  | Prim x -> prim x
  | Tuple x -> tuple x
  | Option x -> option x
  | List { v; _ } -> list v
  | Array { v; _ } -> array v
  | Record x -> record x
  | Variant x -> variant x
  | Attributes { attr_type; _ } -> t attr_type
  | Custom _ -> failwith "No GraphQL schema for Custom type"
  | Map _ -> failwith "No GraphQL schema for Map type"
  | Self _ -> failwith "Recursive GraphQL types not supported"
  | Var v -> raise (Unbound_type_variable v)

and prim : type a. a prim -> a schema =
  let open Schema in
  function
  | Unit -> v (scalar "Unit" ~coerce:(fun () -> `Null))
  | Bool -> v bool
  | Char -> v (scalar "Char" ~coerce:(fun c -> `String (Char.escaped c)))
  | Int -> v int
  | Int32 -> v (scalar "Int32" ~coerce:(fun n -> `Int (Int32.to_int n)))
  | Int64 -> v (scalar "Int64" ~coerce:(fun n -> `Int (Int64.to_int n)))
  | Float -> v float
  | String _ -> v string
  | Bytes _ -> v (scalar "Bytes" ~coerce:(fun b -> `String (Bytes.to_string b)))

and tuple : type a. a tuple -> a schema =
  let open Schema in
  let field name typ resolve = field name ~args:[] ~typ:(unwrap typ) ~resolve in
  function
  | Pair (a, b) ->
      v
      @@ obj "Pair" ~fields:(fun _ ->
             [
               field "fst" (t a) (fun _ -> fst);
               field "snd" (t b) (fun _ -> snd);
             ])
  | Triple (a, b, c) ->
      v
      @@ obj "Triple" ~fields:(fun _ ->
             [
               field "fst" (t a) (fun _ (x, _, _) -> x);
               field "snd" (t b) (fun _ (_, x, _) -> x);
               field "trd" (t c) (fun _ (_, _, x) -> x);
             ])

and option : type a. a t -> a option schema =
 fun x ->
  match t x with
  | Nullable _ -> failwith "Nested option types not supported"
  | Non_null x -> Nullable x

and list : type a. a t -> a list schema =
 fun x -> v (Schema.list (unwrap (t x)))

and array : type a. a t -> a array schema =
 fun _ -> failwith "Graphql_mapper: array type not supported"

and record : type a. a record -> a schema =
 fun r ->
  let rec aux : type b. (a, b) fields -> (unit, a) Schema.field list = function
    | F0 -> []
    | F1 ({ fname; ftype; fget }, fs) ->
        Schema.field fname ~typ:(t ftype) ~resolve:(fun _ -> fget) :: aux fs
  in
  let (Fields (fields, constr)) = r.rfields in
  let fields = aux fields in
  Schema.obj r.rname ~fields:(fun _ -> fields)

and variant : type a. a variant -> a schema =
 fun v ->
  let split_cases = partition_map v.vases ~f:(function C0 -> ()) in
  match split_cases with
  | enums, [] ->
      let values =
        List.map (fun (name, value) -> Schema.enum_value name ~value) enums
      in
      Non_null (Schema.enum name ~values)
  | [], objs ->
      let fields =
        List.map
          (fun (O (variant_name, t, f)) ->
            match t with
            | Non_null typ ->
                Schema.field variant_name ~typ ~args:[] ~resolve:(fun _ x ->
                    f x)
            | Nullable typ ->
                failwith "Graphql_mapper: nullable variant type not supported")
          objs
      in
      Non_null (Schema.obj name ~fields:(fun _ -> fields))
  | [], [] -> failwith "empty variant type not supported"
  | _ :: _, _ :: _ ->
      failwith "cannot convert variant type with mixed C0 and C1"
