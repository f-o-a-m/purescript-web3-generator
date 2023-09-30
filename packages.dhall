let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/921ac217454768ec3f614a9571c109b2ad542c07/purs-0.15.7-web3.dhall
        sha256:818d12df6f7ce455657ff559798e50ec14e098f8d6acc655f674f26c7a007e3d
    with web3 = ../purescript-web3/spago.dhall as Location

in  upstream
