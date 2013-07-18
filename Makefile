all: src/data.cmo test/test_graph test/test_int test/test_data

src/utils.cmo src/utils.cmi: src/utils.ml
	ocamlc -c $<

src/data.cmo src/data.cmi: src/data.ml src/utils.cmi
	ocamlc -I src -c $<

src/hgraph.cmo src/hgraph.cmi: src/hgraph.ml
	ocamlc -c $<

test/test_graph.cmo test/test_graph.cmi: test/test_graph.ml src/hgraph.cmi
	ocamlc -I src -c $<

test/test_int.cmo test/test_int.cmi: test/test_int.ml src/hgraph.cmi
	ocamlc -I src -c $<

test/test_graph: src/hgraph.cmo test/test_graph.cmo
	ocamlc -o $@ $^

test/test_int: src/hgraph.cmo test/test_int.cmo
	ocamlc -o $@ -I src $^

test/test_data.cmo test/test_data.cmi: test/test_data.ml src/hgraph.cmi src/utils.cmi src/data.cmi
	ocamlc -I src -c $<

test/test_data: src/utils.cmo src/data.cmo src/hgraph.cmo test/test_data.cmo
	ocamlc -o $@ $^

clean:
	rm -f src/*.cm* src/*.o test/*.cm* test/*.o
	rm -f test/test_graph test/test_int

.PHONY: all clean
