#!/bin/sh

createuser -P -d -S ocsimore
createdb -E UTF-8 -O ocsimore ocsimore
# TODO: Remove this. VVV
createlang -U ocsimore plpgsql ocsimore
psql -U ocsimore -f $(dirname $0)/createdb.sql ocsimore
