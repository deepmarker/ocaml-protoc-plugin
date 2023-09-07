open IncludeInclude
module Enum = Enum_testEnum
module Included = IncludeIncluded

let%expect_test _ =
  let module T = I in
  let t = T.{ enum = Enum.Message.E.B;
              m = Some 3;
              o = Some Enum.E1.C;
              c = Some 7;
            } in
  Test_lib.test_encode (module T) t;
  [%expect {|
    enum: B
    m {
      i: 3
    }
    o {
      enum: C
    }
    c {
      i: 7
    } |}]


let%expect_test _ =
  let module T = Z in
  let t = Some Included.N.E.B  in
  Test_lib.test_encode (module T) t;
  [%expect {|
    n {
      e: B
    } |}]

let%expect_test _ =
  let module T = Y in
  let t = Some 42 in
  Test_lib.test_encode (module T) t;
  [%expect {|
    d {
      i: 42
    } |}]
