# dune build ./rewriter.ml
dune build
CIR=$PWD
target=$1

bench() {
    for i in {1}; do
        rm -rf ..//../_build/default/bench/target
        dune build $1
    done
}

rewrite() {
    find "$1" -name "*.ml" |
        while read -r fname; do
            dune exec --no-build ./rewriter.exe $fname >/tmp/tmp.ml
            cp /tmp/tmp.ml $fname
            echo "file rewrite: $fname\n"
        done
    rm /tmp/tmp.ml

    find "$1" -name "dune" -exec perl -p -i -e 's/\((staged_)?pps(?!.*ppx_fillup.*)/\(staged_pps ppx_fillup/sg' {} \;
    # find "$target" -name "dune" -exec perl -p -i -e 's/executable.*(?!.*preprocess.*)/executable \(preprocess \(staged_pps ppx_fillup\)\)/sg' {} \;
}

cd $target
git pull
opam install . --deps-only -y
cd $CIR

# time $(bench)
rewrite $target
time $(bench)
