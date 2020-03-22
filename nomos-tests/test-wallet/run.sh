#!/usr/bin/env bash

# An example using transaction files and the definitions in ../test-wallet.nom

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
NOMOS="$DIR/../../"

$NOMOS/_build/default/nomos-bin/nomos.exe -o s1 -t "$DIR/t1" "$DIR/../test-wallet.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -i s1 -o s2 -t "$DIR/t2" "$DIR/../test-wallet.nom"
$NOMOS/_build/default/nomos-bin/nomos.exe -i s2 -o s3 -t "$DIR/t3" "$DIR/../test-wallet.nom"
