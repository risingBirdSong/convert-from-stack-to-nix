{ mkDerivation, aeson, aeson-casing, base, blaze-builder
, bytestring, case-insensitive, classy-prelude
, classy-prelude-conduit, classy-prelude-yesod, conduit, containers
, data-default, directory, email-validate, esqueleto, fast-logger
, file-embed, foreign-store, forma, hjsmin, hspec, http-client-tls
, http-conduit, http-types, jwt, lib, monad-control, monad-logger
, mtl, persistent, persistent-postgresql, persistent-template, safe
, safe-money, safe-money-aeson, shakespeare, template-haskell, text
, time, transformers, unordered-containers, uuid, vector, wai
, wai-cors, wai-extra, wai-logger, warp, yaml, yesod, yesod-auth
, yesod-core, yesod-form, yesod-static
}:
mkDerivation {
  pname = "bankService";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base blaze-builder bytestring case-insensitive
    classy-prelude classy-prelude-conduit classy-prelude-yesod conduit
    containers data-default directory email-validate esqueleto
    fast-logger file-embed foreign-store forma hjsmin hspec
    http-client-tls http-conduit http-types jwt monad-control
    monad-logger mtl persistent persistent-postgresql
    persistent-template safe safe-money safe-money-aeson shakespeare
    template-haskell text time transformers unordered-containers uuid
    vector wai wai-cors wai-extra wai-logger warp yaml yesod yesod-auth
    yesod-core yesod-form yesod-static
  ];
  executableHaskellDepends = [
    aeson aeson-casing base blaze-builder bytestring case-insensitive
    classy-prelude classy-prelude-conduit classy-prelude-yesod conduit
    containers data-default directory email-validate esqueleto
    fast-logger file-embed foreign-store forma hjsmin http-client-tls
    http-conduit http-types jwt monad-control monad-logger mtl
    persistent persistent-postgresql persistent-template safe
    safe-money safe-money-aeson shakespeare template-haskell text time
    transformers unordered-containers uuid vector wai wai-cors
    wai-extra wai-logger warp yaml yesod yesod-auth yesod-core
    yesod-form yesod-static
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
