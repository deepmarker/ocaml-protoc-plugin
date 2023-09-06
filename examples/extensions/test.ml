open Extensions

(* Set extensions *)
let _ =
  let foo = Foo.{ i = 31; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo_with_bar = Extended__bar.set foo (Some 42) in
  let foo_with_baz = Extended__baz.set foo (Some "Test String") in
  let foo_with_bar_baz = Extended__baz.set foo_with_bar (Some "Test String") in

  (* Get extensions *)
  let open Ocaml_protoc_plugin.Result in
  Extended__bar.get foo_with_bar >>= fun bar ->
  Extended__baz.get foo_with_baz >>= fun baz ->
  assert (bar = Some 42);
  assert (baz = Some "Test String");
  Extended__bar.get foo_with_bar_baz >>= fun bar' ->
  Extended__baz.get foo_with_bar_baz >>= fun baz' ->
  assert (bar' = Some 42);
  assert (baz' = Some "Test String");
  return ()
