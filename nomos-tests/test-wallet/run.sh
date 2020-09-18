#!/usr/bin/env bash

# An example using transaction files and the definitions in ../test-wallet.nom

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
NOMOS="$DIR/../../"

$NOMOS/_build/default/nomos-bin/nomos.exe -create -ts ankush -o sa.conf
$NOMOS/_build/default/nomos-bin/nomos.exe -deposit 1000 -ts ankush -i sa.conf -o s0.conf
$NOMOS/_build/default/nomos-bin/nomos.exe -w send -ts ankush -i s0.conf -o s1.conf "$DIR/t1.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -w send -ts ankush -i s1.conf -o s2.conf "$DIR/t2.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -w send -ts ankush -i s2.conf -o s3.conf "$DIR/t3.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -w send -ts ankush -i s3.conf -o s4.conf "$DIR/t4.nom"
