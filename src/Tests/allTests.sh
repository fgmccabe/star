#!/bin/sh
export CAFE=../Compiler/ccafe

for Test in `ls *.test`; do
  echo $Test
  CAFE=$CAFE sh $Test
done
  