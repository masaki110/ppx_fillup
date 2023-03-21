with="./bench_with/bench_with.exe"
without="./bench_without/bench_without.exe"
tynest="./bench_tynest/bench_tynest.exe"
fnnest="./bench_fnnest/bench_fnnest.exe"

bench_build() {
    for i in {1..10}; do
        dune clean --no-print-directory
        dune build --no-print-directory $1
    done
}

bench_exe() {
    for i in {1..100}; do
        dune exec --no-print-directory $1
    done
}

# time (bench_build $with) | echo
# time (bench_build $without) | echo
time (bench_build $tynest) | echo

# time (bench_exe $target) | echo | perl -e 's!.*real(.*)s.*!with fillup : \1!sg'
# time (bench_exe $comp) | echo | perl -e 's!.*real(.*)s.*!without fillup : \1!sg'
time (bench_exe $tynest) | echo

# build 10回
# with : 平均 2.6s
# without : 平均 1.4s
# type nest : 平均 2.1s

# exec 100回
# with : 平均 0.054s
# without : 平均 0.048s
# type nest : 平均 0.049s
