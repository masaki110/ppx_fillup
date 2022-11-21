# rewriter
rewritedir="$1"
find ./$rewritedir -name "*.ml" |
    while read -r fname; do
        ./rewriter.exe -o /tmp.ml --impl $fname
        cp /tmp.ml rewritedir2/$fname
    done
rm /tmp.ml

# benchmark
opam install $1 --deps-only;
benchmark(){
    opam exec -- dune build
}
time $benchmark
