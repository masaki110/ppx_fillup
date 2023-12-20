find . -name "dune" -exec perl -p -i -e 's/\((staged_)?pps(?!.*ppx_fillup.*)/\(staged_pps ppx_fillup/sg' {} \;
find . -name "dune" -exec perl -p -i -e 's/\(libraries(?!.*ppx_fillup.*)/\(libraries ppx_fillup/sg' {} \;

ag 'base' -G "\.ml" -l
