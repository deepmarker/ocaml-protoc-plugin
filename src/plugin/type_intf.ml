type t = {
  file_name: string;
  package: string list; (* can be [] = no package *)
  contents: elt list
}

and elt = {
  name: string;
  kind: kind;
}

and kind  =
  | Extension
  | Service of { methods: string list }
  | Enum of { values: string list }
  | Message of {
      types: elt list;
      depends: string list;
      plain_fields: string list;
      oneof_fields: string list list;
    }
