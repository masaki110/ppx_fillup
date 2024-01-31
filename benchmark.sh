for i in {1..10}; do
    dune clean
    gtime -f "%e" -o times.txt -a dune build .
done

cat times.txt | awk '{s = s + $1; n = n + 1;} END{print s/n}'
