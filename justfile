list:
    just -l

run:
    cabal run srtd
test:
    cabal run srtd-test
build:
    cabal build

docs:
    cabal haddock --haddock-output-dir=./docs/build_haddock/

# Copy prod
copy:
    cp ../srtd_hs_prod/srtd.json ./

# Add a new exposed module below `lib/`
add-module modname:
    #!/usr/bin/env nu
    let modname = "{{modname}}"
    let path = $"lib/($modname | str replace -a '.' '/').hs"
    let dirpath = $path | path dirname 
    mkdir $dirpath
    $"module ($modname) where" | save $path
    cabal run cabal-helper -- add-exposed-module $modname
