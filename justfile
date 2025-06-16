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
