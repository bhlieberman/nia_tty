build:
	dune build

gif: demo.cast
	agg demo.cast assets/demo.gif