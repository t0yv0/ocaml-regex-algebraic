module OP = Ocamlbuild_plugin
module C = OP.Command

let packages = ["oUnit"]

let ocamlfind cmd =
  C.S [C.A "ocamlfind"; C.A cmd; C.A "-package";
       C.A (String.concat "," packages)]

let _ =
  OP.flag ["ocaml"; "link"] (C.A "-linkpkg")

let plugin hook =
  match hook with
  | OP.After_options ->
    OP.Options.ocamlc := ocamlfind "ocamlc";
    OP.Options.ocamlopt := ocamlfind "ocamlopt"
  | _ -> ()

let _ =
  OP.dispatch plugin
