ocamlopt -I .. -c graphics.cmx ../graphics_utils.ml
ocamlopt -I .. -c ../parser.ml
ocamlopt -I .. -c ../algebra/algebra.ml
ocamlopt -I ../algebra -c algebra.cmx slab_load.ml
pause
