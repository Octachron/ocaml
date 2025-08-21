#!/usr/bin/bash

summary=perf_test/summary
summary2=perf_test/summary_s

for i in $summary
do
rm $i
touch $i
done
rm $summar
for i in perf_test/*.ml
do
./ocamlopt.opt -w -24 -I stdlib ${i}i -o ${i/ml/cmi}
printf "%s " $i >> $summary
PPROF=true ./ocamlopt.opt -w -24 -I stdlib -I perf_test $i -c 2> ${i/ml/log} >> $summary
#printf "%s " $i >> $summary2
#PPROF=true OPREF=S ./ocamlopt.opt -w -24 -I stdlib -I perf_test $i -c 2> ${i/ml/log} >> $summary2

done
# hyperfine -i "./ocamlopt.opt -I stdlib -I perf_test perf_test/test1.ml"
