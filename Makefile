build:
	dune build

gif: demo.cast
	agg demo.cast assets/demo.gif

watch:
	dune exec nia_tty -w --no-buffer