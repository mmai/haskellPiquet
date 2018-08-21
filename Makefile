build:
	# For release, remove the --fast option
	stack build --fast --haddock-deps --copy-bins --file-watch
docs:
	stack haddock --open
	stack hoogle -- generate --local
	stack hoogle -- server --local --port=8080
tests:
	stack test
run:
	stack exec piquet-exe 
