type t = {
  file: string; (* file the element is in *)
  name: string; (* Depends of the kind *)
  chs: t list;
  kind: kind;
}

and kind =
  | Package (* segment of a package name *)
  | Extension (* name is fqn of the message to be extended *)
  | Service
  | ServiceMethod
  | Enum
  | EnumValue
  | Message
  | Field
  | ExtensionField
  | UnionField
  | Oneof

let kind_of_string = function
  | Package -> "package"
  | Extension -> "extension"
  | Service -> "service"
  | ServiceMethod -> "method"
  | Enum -> "enum"
  | EnumValue -> "enum_value"
  | Message -> "message"
  | ExtensionField -> "extension_field"
  | Field -> "field"
  | UnionField -> "union_field"
  | Oneof -> "oneof"

let pp_kind ppf t = Format.pp_print_string ppf (kind_of_string t)

let empty = { file = ""; name = ""; chs = []; kind = Package }
let enum file name chs = { kind = Enum ; file; name; chs }
let enum_value file name = { kind = EnumValue ; file; name; chs = [] }
let message file name chs = { file ; name; chs ; kind = Message }
let service file name chs = { file ; name; chs ; kind = Service }
let field file name = { file ; name; chs = [] ; kind = Field }
let extension_field file name = { file ; name; chs = [] ; kind = ExtensionField }
let union file name = { file ; name; chs = [] ; kind = UnionField }
let oneof file name = { file ; name; chs = [] ; kind = Oneof }
