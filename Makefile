all: operations copier probability

operations:
	@echo "COMPILING..."
	ocamlc -c droplet.ml
	ocamlc -c fountain.ml
	ocamlc -c goblet.ml
	ocamlc -c operations.ml
	ocamlc -o operations droplet.cmo fountain.cmo goblet.cmo operations.cmo

probability:
	@echo "COMPILING..."
	ocamlc -c droplet.ml
	ocamlc -c fountain.ml
	ocamlc -c goblet.ml
	ocamlc -c distribution.ml
	ocamlc -c probability.ml
	ocamlc -o probability droplet.cmo fountain.cmo goblet.cmo distribution.cmo probability.cmo

clean:
	rm -f *.cmo *.cmi operations copier probability
