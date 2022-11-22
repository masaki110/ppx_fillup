# rewriter
dune build ./
target="$1"
find ./"$target" -name "*.ml" |
    while read -r fname; do
        dune exec --no-build ./rewriter.exe "$fname" > /tmp/tmp.ml
        cp /tmp/tmp.ml "$fname"
    done
rm /tmp/tmp.ml

# benchmark
opam install ./"$target" --deps-only;
benchmark(){
    opam exec -- dune build ./"$target"
}
time $benchmark
