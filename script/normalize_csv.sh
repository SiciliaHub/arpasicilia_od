#!/bin/bash

# lo script richiede l'installazione di csvkit https://csvkit.readthedocs.org

# rimuovere prime 5 righe
tail -n +6 input.csv > out_01.csv

# rimuovere tutte le virgolette inutili
#rimuovere ultima colonna
#avere "," come separatore campi
# convert in UTF-8
csvcut -C 9 -d ";" out_01.csv > out_02.csv

# convertire la data in datetime
csvsql -u 1 --query "select substr(Field,0,5) || \"-\" || substr(Field, 9,2) || \"-\" || substr(Field,6,2) || substr(Field,11,6) as DateTime,Aver,SWrd,MinT,Min,MaxT,Max,Sigma from out_02" --tables out_02 < out_02.csv > out_03.csv