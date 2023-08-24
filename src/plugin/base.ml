open StdLabels
open MoreLabels

module StringSet = Set.Make(String)

module StringMap = struct
  include Map.Make(String)

  (** Fail with an error if the key already exists *)
  let add_uniq ~key ~data map =
    update ~key ~f:(function
      | None -> Some data
      | Some _ -> failwith (Printf.sprintf "Key %s already exists" key)
    ) map
end
