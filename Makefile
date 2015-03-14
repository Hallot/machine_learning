.SUFFIXES: .ml .mli .cmi .cmo .cma

.mli.cmi:
	ocamlc -c $<
.ml.cmo:
	ocamlc -c $<

EXEC= clean main.cmo ml
DEBUG= clean main.cmo bug

all: $(EXEC)

main.cmo: utils.cmi matrix.cmi main.ml

ML_BINS = utils.cmo matrix.cmo main.cmo
ml: $(ML_BINS)
	ocamlc -o $@ str.cma $(ML_BINS)

clean:
	rm -rf *.cm*; rm -f ml	

bug: $(ML_BINS)
	ocamlc -g str.cma $(ML_BINS)
	
debug: $(DEBUG)
	