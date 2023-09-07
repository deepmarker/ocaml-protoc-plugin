open Enum_testEnum

let%expect_test _ =
  let module T = Message in
  let t = Message.E.B in
  Test_lib.test_encode (module T) t;
  [%expect {| enum: B |}]

let%expect_test _ =
  let module T = Outside in
  let t = E1.C in
  Test_lib.test_encode (module T) t;
  [%expect {| enum: C |}]

let%expect_test _ =
  let module T = Aliasing in
  let t = T.Enum.Z in
  Test_lib.test_encode (module T) t;
  (* We do expect the enum to be deserialized as Y. *)
  [%expect {|
    e: Y

    Expect  :Z
    Observed:Y |}]

let%expect_test _ =
  let module T = Negative in
  let t = T.Enum.A3 in
  Test_lib.test_encode (module T) t;
  [%expect {| e: A3 |}]
