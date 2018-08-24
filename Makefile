docs:
	stack haddock --open
	stack hoogle -- generate --local
	stack hoogle -- server --local --port=8080
build:
	# For release, remove the --fast option
	stack build --fast --haddock-deps --copy-bins --file-watch
tests:
	stack test --file-watch
run:
	stack exec piquet-exe 8888
