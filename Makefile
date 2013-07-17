src/data.cmo: src/data.ml
	ocamlc -c src/data.ml

src/hgraph.cmo: src/hgraph.ml
	ocamlc -c src/hgraph.ml

test/test_graph: src/hgraph.cmo
	ocamlc -o test/test_graph -I src src/hgraph.cmo test/test_graph.ml

test/test_int: src/hgraph.cmo
	ocamlc -o test/test_int -I src src/hgraph.cmo test/test_int.ml