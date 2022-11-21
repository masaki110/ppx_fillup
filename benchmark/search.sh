cd opam-repository/packages
# git pull
RECENT="git log --name-status | sed -n -E 's!^[AM]\s+packages/([^/]*).*!\1!gp' | sort -u"
# RECENT="sed -n -E 's!^[AM]\s+packages/([^/]*).*!\1!gp' | sort -u"
for i in *; do
    CNT=$(ag -G .ml "Pprintast." $i | wc -l)
    if [ $CNT -ne 0 ]; then
        echo $CNT $i
    fi
done | sort -n -r
