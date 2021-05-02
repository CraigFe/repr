let print_exp = Ppxlib.Pprintast.expression Fmt.stdout

type basic = {
  int : int;
  int32 : int32;
  int64 : int64;
  float : float;
  string : string;
  bytes : bytes;
}
[@@deriving repr]

let test typ v = print_exp (Repr_ppxlib.static_value typ Ppxlib.Location.none v)

let%expect_test "basic" =
  test basic_t
    {
      int = 1;
      int32 = 2l;
      int64 = 3L;
      float = 4.;
      string = "foo";
      bytes = Bytes.of_string "bar";
    };
  [%expect
    {|
    {
      int = 1;
      int32 = 2l;
      int64 = 3L;
      float = 4..;
      string = "foo";
      bytes = (Stdlib.Bytes.of_string "bar")
    } |}]

let%expect_test "pair" =
  test [%typ: bytes * int64] (Bytes.of_string "a", 1L);
  [%expect {| ((Stdlib.Bytes.of_string "a"), 1L) |}]

let%expect_test "list" =
  test [%typ: int list] [ 1; 2; 3 ];
  [%expect {| [1; 2; 3] |}]

let%expect_test "array" =
  test [%typ: int array] [| 1; 2; 3 |];
  [%expect {| [|1;2;3|] |}]

let%expect_test "variant" =
  test [%typ: [ `nil | `cons of int * 'a ] as 'a] (`cons (1, `cons (2, `nil)));
  [%expect{| cons (1, (cons (2, nil))) |}]
