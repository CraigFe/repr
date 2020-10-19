open Brands
include Type_dsl_intf

module Top_universal = struct
  type 'a t = {
    eval : 'repr. (module Top with type br = 'repr) -> ('a, 'repr) app;
  }
  [@@unboxed] [@@deriving branded]

  let unit =
    { eval = (fun (type r) (module R : Top with type br = r) -> R.(inj unit)) }

  let int =
    { eval = (fun (type r) (module R : Top with type br = r) -> R.(inj int)) }

  let int32 =
    { eval = (fun (type r) (module R : Top with type br = r) -> R.(inj int32)) }

  let int64 =
    { eval = (fun (type r) (module R : Top with type br = r) -> R.(inj int64)) }

  let bool =
    { eval = (fun (type r) (module R : Top with type br = r) -> R.(inj bool)) }

  let char =
    { eval = (fun (type r) (module R : Top with type br = r) -> R.(inj char)) }

  let string =
    {
      eval = (fun (type r) (module R : Top with type br = r) -> R.(inj string));
    }

  let bytes =
    { eval = (fun (type r) (module R : Top with type br = r) -> R.(inj bytes)) }

  let float =
    { eval = (fun (type r) (module R : Top with type br = r) -> R.(inj float)) }

  let option elt =
    let eval (type r) (module R : Top with type br = r) =
      let elt = R.prj (elt.eval (module R)) in
      R.(inj (option elt))
    in
    { eval }

  let list elt =
    let eval (type r) (module R : Top with type br = r) =
      let elt = R.prj (elt.eval (module R)) in
      R.(inj (list elt))
    in
    { eval }

  let array elt =
    let eval (type r) (module R : Top with type br = r) =
      let elt = R.prj (elt.eval (module R)) in
      R.(inj (array elt))
    in
    { eval }

  let result ok err =
    let eval (type r) (module R : Top with type br = r) =
      let ok = R.prj (ok.eval (module R))
      and err = R.prj (err.eval (module R)) in
      R.(inj (result ok err))
    in
    { eval }

  let pair a b =
    let eval (type r) (module R : Top with type br = r) =
      let a = R.prj (a.eval (module R)) and b = R.prj (b.eval (module R)) in
      R.(inj (pair a b))
    in
    { eval }

  let triple a b c =
    let eval (type r) (module R : Top with type br = r) =
      let a = R.prj (a.eval (module R))
      and b = R.prj (b.eval (module R))
      and c = R.prj (c.eval (module R)) in
      R.(inj (triple a b c))
    in
    { eval }

  module Field = struct
    type ('a, 'b) t = {
      eval :
        'repr. (module Top with type Field.br = 'repr) -> ('a, 'b, 'repr) app2;
    }
    [@@deriving branded] [@@unboxed]
  end

  let field a b c =
    let eval (type r) (module R : Top with type Field.br = r) =
      let inner = R.prj (b.eval (module R)) in
      R.(Field.inj (field a inner c))
    in
    { Field.eval }

  module Open_record = struct
    type ('a, 'b, 'c) t = {
      eval :
        'repr. (module Top with type Open_record.br = 'repr) ->
        ('a, 'b, 'c, 'repr) app3;
    }
    [@@deriving branded] [@@unboxed]
  end

  let record a b =
    let eval (type r) (module R : Top with type Open_record.br = r) =
      R.(Open_record.inj (record a b))
    in
    { Open_record.eval }

  let ( |+ ) a b =
    let eval (type r) (module R : Top with type Open_record.br = r) =
      let a = R.Open_record.prj (a.Open_record.eval (module R))
      and b = R.Field.prj (b.Field.eval (module R)) in
      R.(Open_record.inj (a |+ b))
    in
    { Open_record.eval }

  let sealr r =
    let eval (type r) (module R : Top with type br = r) =
      let r = R.Open_record.prj (r.Open_record.eval (module R)) in
      R.(inj (sealr r))
    in
    { eval }

  let mu : 'a. ('a t -> 'a t) -> 'a t =
   fun f ->
    let eval (type r) (module R : Top with type br = r) =
      R.inj @@ R.mu (fun x -> f.eval (module R) x)
    in
    { eval }

  type 'br generic = { generic : 'a. 'a t -> ('a, 'br) app } [@@unboxed]

  let make : type br. (module Top with type br = br) -> br generic =
   fun (module R : Top with type br = br) ->
    { generic = (fun t -> t.eval (module R : Top with type br = br)) }
end

module To_initial : Top with type 'a t = 'a Type_core.t = struct
  open Type_core

  type nonrec 'a t = 'a t [@@deriving branded]

  module Field = struct
    type nonrec ('a, 'b) t = ('a, 'b) field [@@deriving branded]
  end

  module Open_record = struct
    type ('a, 'b, 'c) t = ('a, 'c) fields -> string * 'b * ('a, 'b) fields
    [@@deriving branded]
  end

  let unit = Prim Unit
  let bool = Prim Bool
  let char = Prim Char
  let int = Prim Int
  let int32 = Prim Int32
  let int64 = Prim Int64
  let float = Prim Float
  let string = Prim (String `Int)
  let bytes = Prim (Bytes `Int)
  let list v = List { v; len = `Int }
  let array v = Array { v; len = `Int }
  let pair a b = Tuple (Pair (a, b))
  let triple a b c = Tuple (Triple (a, b, c))
  let option a = Option a

  let field fname ftype fget =
    Utils.check_valid_utf8 fname;
    { fname; ftype; fget }

  let record : string -> 'b -> ('a, 'b, 'b) Open_record.t =
   fun n c fs -> (n, c, fs)

  let ( |+ ) :
      type a b c d.
      (a, b, c -> d) Open_record.t -> (a, c) field -> (a, b, d) Open_record.t =
   fun r f fs ->
    let n, c, fs = r (F1 (f, fs)) in
    (n, c, fs)

  module String_Set = Set.Make (String)

  (** [check_unique f l] checks that all the strings in [l] are unique.
      Otherwise, calls [f dup] with [dup] the first duplicate. *)
  let check_unique f =
    let rec aux set = function
      | [] -> ()
      | x :: xs -> (
          match String_Set.find_opt x set with
          | None -> aux (String_Set.add x set) xs
          | Some _ -> f x)
    in
    aux String_Set.empty

  let check_unique_field_names rname rfields =
    let names = List.map (fun (Field { fname; _ }) -> fname) rfields in
    let failure fname =
      Fmt.invalid_arg
        "The name %s was used for two or more fields in record %s." fname rname
    in
    check_unique failure names

  let sealr : type a b. (a, b, a) Open_record.t -> a t =
   fun r ->
    let rname, c, fs = r F0 in
    let rwit = Witness.make () in
    let sealed = { rwit; rname; rfields = Fields (fs, c) } in
    check_unique_field_names rname (fields sealed);
    Record sealed

  type 'a case_p = 'a case_v
  type ('a, 'b) case = int -> 'a a_case * 'b

  let case1 : type a b. string -> b t -> (b -> a) -> (a, b -> a case_p) case =
   fun cname1 ctype1 c1 ->
    Utils.check_valid_utf8 cname1;
    fun ctag1 ->
      let cwit1 : b Witness.t = Witness.make () in
      let c = { ctag1; cname1; ctype1; cwit1; c1 } in
      (C1 c, fun v -> CV1 (c, v))

  let variant n c vs = (n, c, vs)

  let app v c cs =
    let n, fc, cs = v cs in
    let c, f = c (List.length cs) in
    (n, fc f, c :: cs)

  let check_unique_case_names vname vcases =
    let n0, n1 =
      List.partition (function C0 _ -> true | C1 _ -> false) vcases
    in
    let names0 =
      List.map (function C0 { cname0; _ } -> cname0 | _ -> assert false) n0
    in
    let names1 =
      List.map (function C1 { cname1; _ } -> cname1 | _ -> assert false) n1
    in
    check_unique
      (fun cname ->
        Fmt.invalid_arg
          "The name %s was used for two or more case0 in variant or enum %s."
          cname vname)
      names0;
    check_unique
      (fun cname ->
        Fmt.invalid_arg
          "The name %s was used for two or more case1 in variant or enum %s."
          cname vname)
      names1

  let sealv v =
    let vname, vget, vcases = v [] in
    check_unique_case_names vname vcases;
    let vwit = Witness.make () in
    let vcases = Array.of_list (List.rev vcases) in
    Variant { vwit; vname; vcases; vget }

  let ( |~ ) = app

  let result a b =
    variant "result" (fun ok error -> function
      | Ok x -> ok x | Error x -> error x)
    |~ case1 "ok" a (fun a -> Ok a)
    |~ case1 "error" b (fun b -> Error b)
    |> sealv
end

let initial t =
  To_initial.prj ((Top_universal.make (module To_initial)).generic t)
