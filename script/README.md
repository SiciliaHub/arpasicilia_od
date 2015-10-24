# Intro
In questa cartella verranno inseriti script e snippet di codice utili al progetto.

## clean.py
### Intro
Fa le seguenti cose:
* crea per ogni file letto un nuovo file con i dati puliti, cioè senza intestazioni;
* rinomina il file da enna_02092015.CSV a 20150902_enna.csv in modo da avere l'ordinamento alfanumerico uguale a quello cronologico, considerando anche i casi in cui nel nome del file sono presenti spazi;
* prende in input la directory da esplorare e la directory da creare all'interno della quale inserisce i file puliti.

### Utilizzo di clean.py script

``` bash
$  clone https://github.com/SiciliaHub/arpasicilia_od.git
$  cd arpasicilia_od
$  git checkout gh-pages
$  cd data
$  ../clean.py aria ariaweb
```
Qui le cartelle di esempio di input e output: https://github.com/SiciliaHub/arpasicilia_od/tree/master/data

### Requisiti
* Python 2.7

## normalize_csv.sh
### Intro
Al momento è una sorta di piccolo schema di operazioni da fare per la pulizia sul singolo file `CSV`. Fa queste cose:
* rimuove prime 5 righe di intestazione e lascia la sesta come intestazione di colonna;
* rimuove tutte le virgolette inutili, infatti ogni valore di cella è tra `"`;
* rimuove ultima colonna;
* modifica il sepatatore di celle da `;` a `,`;
* converte il valore sul data e ora presente nei CSV da `gg/mm/yyyy hh:mm` in `yyyy-mm-dd hh:mm`.

Questo un file csv di test di [input](https://github.com/SiciliaHub/arpasicilia_od/blob/master/script/normalize_csv_testing/input.csv), e questo l'[output](https://github.com/SiciliaHub/arpasicilia_od/blob/master/script/normalize_csv_testing/out_03.csv).

### Requisiti
* Python 2.7;
* [csvkit](https://csvkit.readthedocs.org/).