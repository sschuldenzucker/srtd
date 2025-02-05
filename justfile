list:
    just -l

run:
    cabal run srtd
test:
    cabal run srtd-test
build:
    cabal build

# Copy prod
copy:
    cp ../srtd_hs_prod/srtd.json ./
