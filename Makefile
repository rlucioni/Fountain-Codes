all: operations

FILES=string_droplet.ml string_fountain.ml operations.ml

operations: $(FILES)
	@echo "COMPILING..."
	ocamlc -c pretty_print.ml
	ocamlc -c test_framework.ml
	ocamlc -c string_droplet.ml
	ocamlc -c string_fountain.ml
	ocamlc -c string_goblet.ml
	ocamlc -c droplet.ml
	ocamlc -c fountain.ml
	ocamlc -c goblet.ml
	ocamlc -c distribution.ml
	ocamlc -c probability.ml
	ocamlc -o io_operations pretty_print.cmo test_framework.cmo string_droplet.cmo string_fountain.cmo string_goblet.ml io_operations.ml distribution.cmo probability.cmo

copier: 
	ocamlc -c copier.ml
	ocamlc -o copier copier.cmo

probability:
	ocamlc -c distribution.ml
	ocamlc -c string_droplet.ml
	ocamlc -c string_fountain.ml
	ocamlc -c string_goblet.ml
	ocamlc -o probability distribution.cmo string_droplet.cmo string_fountain.cmo string_goblet.cmo

clean:
	rm -f *.cmo *.cmi operations string_operations io_operations copier probability
