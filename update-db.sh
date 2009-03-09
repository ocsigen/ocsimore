#!/bin/bash


if [ -e "database-state" ]
then
    FIRST=`cat database-state`
else
    FIRST=2008-12-31
fi
FIRST=update-$FIRST.sql

UPDATED=0

for A in update-????-??-??.sql;
do
    if [[ $A > $FIRST ]];
    then
        echo psql -U ocsimore ocsimore -f $A;
        export UPDATED=1
    fi
done

if [ "$UPDATED" -eq "1" ];
then
    echo "echo `date +%Y-%m-%d` > database-state"
    echo -n "Your database needs updating. "
    echo "Execute the lines above in a shell"
    exit 1;
fi