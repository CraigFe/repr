open Brands

type empty = |

(** This module defines a set of DSLs for reifying various type universes. *)

module type Bottom = sig
  type 'a t [@@deriving branded]
end

module type Basic = sig
  include Bottom

  (** DSL primitives for the basic OCaml abstract types. *)

  val unit : unit t
  val bool : bool t
  val char : char t
  val string : string t
  val bytes : bytes t
  val int : int t
  val int32 : int32 t
  val int64 : int64 t
  val float : float t

  (** DSL primitives for the standard OCaml container types. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val option : 'a t -> 'a option t
  val result : 'a t -> 'b t -> ('a, 'b) result t
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
end

module type Recursive = sig
  include Bottom

  val mu : ('a t -> 'a t) -> 'a t
  (** [mu f] is the representation [r] such that [r = mu r]. *)

  (* val mu2 : ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t *)
  (** [mu2 f] is the representations [r] and [s] such that [r, s = mu2 r s]. *)
end

module type Variant = sig
  include Bottom

  type ('a, 'b, 'c) open_variant

  val variant : string -> 'b -> ('a, 'b, 'b) open_variant

  type ('a, 'b) case
  type 'a case_p

  val case0 : string -> 'a -> ('a, 'a case_p) case
  val case1 : string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
  val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
end

module type Record = sig
  include Bottom

  module Open_record : sig
    type ('a, 'b, 'c) t [@@deriving branded]
  end

  module Field : sig
    type ('a, 'b) t [@@deriving branded]
  end

  val record : string -> 'b -> ('a, 'b, 'b) Open_record.t
  val field : string -> 'a t -> ('b -> 'a) -> ('b, 'a) Field.t

  val ( |+ ) :
    ('a, 'b, 'c -> 'd) Open_record.t ->
    ('a, 'c) Field.t ->
    ('a, 'b, 'd) Open_record.t

  val sealr : ('a, 'b, 'a) Open_record.t -> 'a t
end

module type Fixed_size = sig
  include Bottom

  type len := [ `Int | `Int8 | `Int16 | `Int32 | `Int64 | `Fixed of int ]

  val string_of : len -> string t
  (** Like {!string} but with a given kind of size. *)

  val bytes_of : len -> bytes t
  (** Like {!bytes} but with a given kind of size. *)

  val boxed : 'a t -> 'a t
  (** [boxed t] is the same as [t] but with a binary representation which is
      always boxed (e.g. top-level values won't be unboxed). This forces
      {!Unboxed} functions to be exactly the same as boxed ones.*)
end

module type Top = sig
  include Bottom
  include Basic with type 'a t := 'a t
  include Recursive with type 'a t := 'a t

  (* include Variant with type 'a t := 'a t *)
  include Record with type 'a t := 'a t

  (* include Fixed_size with type 'a t := 'a t *)
end

module type Type_dsl = sig
  module type Bottom = Bottom
  module type Basic = Basic
  module type Recursive = Recursive
  module type Variant = Variant
  module type Fixed_size = Fixed_size
  module type Top = Top

  module Top_universal : sig
    include Top

    type 'br generic = { generic : 'a. 'a t -> ('a, 'br) app } [@@unboxed]

    val make : (module Top with type br = 'br) -> 'br generic
  end

  val initial : 'a Top_universal.t -> 'a Type_core.t
end
