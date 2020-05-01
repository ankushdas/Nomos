#!/usr/bin/env bash

# An example using transaction files and the definitions in ../test-wallet.nom

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
NOMOS="$DIR/../../"

$NOMOS/_build/default/nomos-bin/nomos.exe -v -1 -w send -o s1.conf "$DIR/t1.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -v -1 -w send -i s1.conf -o s2.conf "$DIR/t2.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -v -1 -w send -i s2.conf -o s3.conf "$DIR/t3.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -v -1 -w send -i s3.conf -o s4.conf "$DIR/t4.nom"
