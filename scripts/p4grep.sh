#!/usr/local/bin/bash
DEPOT="$1"
if [ $DEPOT == "..." ]; then
    DEPOT="*"
fi
for dir in $(p4 -ztag -F%dir% dirs $DEPOT); do
    if [[ $dir == *"vgcommon"* ]]
    then
        echo "Searching in vgcommon: $dir..."
        for vgdir in $(p4 -ztag -F%dir% dirs "$dir/*"); do
            #echo "p4 grep -s -i -e $2 $vgdir/..."
            p4 grep -s -i -e $2 $vgdir/...
        done
    else
        #echo "p4 grep -s -i -e $2 $dir/..."
        p4 grep -s -i -e $2 $dir/...
    fi
done

