ocamlopt -I .. -c graphics.cmx ../graphics_utils.ml
ocamlopt -I .. -c ../parser.ml
ocamlopt -I .. -c cmq.ml
ocamlopt -I .. graphics.cmxa str.cmxa graphics_utils.cmx parser.cmx cmq.cmx cmq_app.ml -o cmq_app.exe
rem ocamlc -I .. -i cmq_app.ml > cmq_app.mli
rem ocamlc -I .. -c graphics.cma ../graphics_utils.ml
rem ocamlc -I .. graphics.cma graphics_utils.cmo cmq.ml -o cmq.exe
pause
