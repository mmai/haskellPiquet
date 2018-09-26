docs:
	stack haddock --open
	stack hoogle -- generate --local
	stack hoogle -- server --local --port=8080
configure:
	# nix-shell --attr env release0.nix --run 'cabal configure'
	nix-shell --run 'cabal configure'
build:
	nix-shell --run 'cabal build'
ghcversion:
	nix-instantiate --eval --expr '(import <nixpkgs> { }).ghc.version'
tests:
	stack test --file-watch
run:
	stack exec piquet-exe 8888
client-run:
	stack exec piquetClient-exe
