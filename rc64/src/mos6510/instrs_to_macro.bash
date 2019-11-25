#!/usr/bin/env bash
set -ex
cd "$(dirname "${BASH_SOURCE[0]}")"
(awk --field-separator='\t' 'BEGIN{OF=FS} { printf("%s => { Imp=%s, Imm=%s, Zpi=%s, ZpX=%s, ZpY=%s, IzX=%s, IzY=%s, Abs=%s, AbX=%s, AbY=%s, Ind=%s, PCr=%s, };\n", $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13) }' | sed -E 's/[A-Za-z]+=\s*,//g' | sed -E 's/[A-Za-z]+=$//g' | sed -E 's/\$/0x/g') < instrs.csv