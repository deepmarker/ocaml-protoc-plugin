(lang dune 3.10)
(name ocaml-protoc-plugin)

(generate_opam_files true)

(source
 (github issuu/ocaml-protoc-plugin))
(authors
  "Anders Fugmann <anders@fugmann.net>")
(maintainers
  "Issuu")
(homepage
  "https://github.com/issuu/ocaml-protoc-plugin")
(bug_reports
  "https://github.com/issuu/ocaml-protoc-plugin/issues")
(license APACHE-2.0)

(documentation https://issuu.github.io/ocaml-protoc-plugin)

(package
 (name ocaml-protoc-plugin)
 (synopsis "Plugin for protoc protobuf compiler to generate ocaml definitions from a .proto file")
 (description "The plugin generates ocaml type definitions,
serialization and deserialization functions from a protobuf file.
The types generated aims to create ocaml idiomatic types;
- messages are mapped into modules
- oneof constructs are mapped to polymorphic variants
- enums are mapped to adt's
- map types are mapped to assoc lists
- all integer types are mapped to int by default (exact mapping is also possible)
- all floating point types are mapped to float.
- packages are mapped to nested modules")
 (depends
   ocaml
   dune
  (base (>= 0.16))
  (sexplib (>= 0.16))
  (ppx_sexp_conv (>= 0.16))
  (ppx_expect :with-test)
  (ppx_inline_test :with-test)
  (ppx_deriving :with-test)
  (conf-pkg-config :build)
 )
 (tags
  (protobuf grpc)))

(package
 (name "conf-protoc")
 (synopsis "Virtual package to install protoc compiler")
 (description
   "This package will install the protoc compiler if invoked via `opam depext`")
 (allow_empty)
)
