open Ppxlib
open Stdlib
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Foo = struct
  type foo = int
end

module FooExtended = struct
  include Foo
end

let () = print_endline "Hello, World!"
