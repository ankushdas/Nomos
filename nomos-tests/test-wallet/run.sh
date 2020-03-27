#!/usr/bin/env bash

# An example using transaction files and the definitions in ../test-wallet.nom

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
NOMOS="$DIR/../../"

$NOMOS/_build/default/nomos-bin/nomos.exe -w send -o s1.conf -t "$DIR/t1.txn" "$DIR/../test-wallet.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -w send -i s1.conf -o s2.conf -t "$DIR/t2.txn" "$DIR/../test-wallet.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -w send -i s2.conf -o s3.conf -t "$DIR/t3.txn" "$DIR/../test-wallet.nom"
