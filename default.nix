{ mkDerivation, aeson, aeson-casing, array, base, binary
, bytestring, containers, distributed-process
, distributed-process-simplelocalnet
, distributed-process-supervisor, formatting, hspec, lens
, megaparsec, monad-logger, monad-loops, MonadRandom, mtl, network
, network-transport-inmemory, network-transport-tcp
, ordered-containers, random, resourcet, split, stdenv, stm, text
, time, transformers, wai, wai-app-static, wai-websockets, warp
, websockets, yaml
}:
mkDerivation {
  pname = "piquet";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing array base binary bytestring containers
    distributed-process distributed-process-simplelocalnet
    distributed-process-supervisor formatting lens megaparsec
    monad-logger monad-loops MonadRandom mtl network-transport-inmemory
    network-transport-tcp ordered-containers random resourcet split stm
    text time transformers wai wai-app-static wai-websockets warp
    websockets yaml
  ];
  executableHaskellDepends = [
    aeson aeson-casing array base binary bytestring containers
    distributed-process distributed-process-simplelocalnet
    distributed-process-supervisor formatting lens megaparsec
    monad-logger monad-loops MonadRandom mtl network
    network-transport-inmemory network-transport-tcp ordered-containers
    random resourcet split stm text time transformers wai
    wai-app-static wai-websockets warp websockets yaml
  ];
  testHaskellDepends = [
    aeson aeson-casing array base binary bytestring containers
    distributed-process distributed-process-simplelocalnet
    distributed-process-supervisor formatting hspec lens megaparsec
    monad-logger monad-loops MonadRandom mtl network-transport-inmemory
    network-transport-tcp ordered-containers random resourcet split stm
    text time transformers wai wai-app-static wai-websockets warp
    websockets yaml
  ];
  homepage = "https://github.com/mmai/piquet#readme";
  license = stdenv.lib.licenses.bsd3;
}
