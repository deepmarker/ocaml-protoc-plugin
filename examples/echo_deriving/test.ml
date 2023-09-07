open EchoEcho
open Google_types_deriving
open GoogleProtobufTimestamp

let mk_timestamp () =
  let now = Unix.gettimeofday () in
  let seconds = int_of_float now in
  let nanos = ((now -. float seconds) *. 10. ** 12.) |> int_of_float in
  Timestamp.{ seconds; nanos }


let mk_request () =
  Request.{ timestamp = `Ts (mk_timestamp ()); what = `Type Request.Who_.World }


let mk_reply Request.{ timestamp; what } =

  let at =
    match timestamp with
      | `Ts {seconds; nanos = _} ->
          let minutes = seconds / 60 in
          let hours = minutes / 60 in
          Printf.sprintf "%d:%d:%d" (hours mod 24) (minutes mod 60) (seconds mod 60)
      | `not_set ->
          "whenever"
  in

  match what with
    | `Someone person -> Printf.sprintf "%s Hello there, %s" at person
    | `Type Request.Who_.Mum -> Printf.sprintf "%s Hi Mom" at
    | `Type Request.Who_.World -> Printf.sprintf "%s Hello World" at
    | `not_set -> Printf.sprintf "Hello Unknown"

let handle_request proto_request =
  let (decode, encode) = Ocaml_protoc_plugin.Service.make_service_functions Echo.call in
  let request =
    Ocaml_protoc_plugin.Reader.create proto_request
    |> decode
    |> function | Ok v -> v | Error e -> failwith (Printf.sprintf "Could not decode request: %s" (Ocaml_protoc_plugin.Result.show_error e))
  in
  Printf.printf "Got request: %s\n" ([%show: Request.t] request);
  let reply = mk_reply request in
  encode reply
  |> Ocaml_protoc_plugin.Writer.contents

let do_request ~handler request =
  let (encode, decode) = Ocaml_protoc_plugin.Service.make_client_functions Echo.call in
  let proto_request = encode request |> Ocaml_protoc_plugin.Writer.contents in
  let proto_reply = handler proto_request in
  Ocaml_protoc_plugin.Reader.create proto_reply
  |> decode
  |> function | Ok v -> v | Error e -> failwith (Printf.sprintf "Could not reply request: %s" (Ocaml_protoc_plugin.Result.show_error e))

let () =
  let request = mk_request () in
  let reply = do_request ~handler:handle_request request in
  Printf.printf "Reply: %s\n" ([%show: Reply.t] reply)
