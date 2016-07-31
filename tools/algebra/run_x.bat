ocamlopt -I .. -c graphics.cmx ../graphics_utils.ml
ocamlopt -I .. -c ../parser.ml
ocamlopt -I .. -c algebra.ml
ocamlopt -I .. graphics.cmxa str.cmxa graphics_utils.cmx parser.cmx algebra.cmx algebra_app.ml -o algebra_app.exe
pause
