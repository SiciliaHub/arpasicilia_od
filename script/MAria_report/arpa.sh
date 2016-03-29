#!/usr/bin/env bash

### requirements ###
# csvkit dev, che si installa con "pip install --upgrade -e git+git://github.com/onyxfish/csvkit.git@master#egg=csvkit"
# agate, che si installa con " pip install agate
### requirements ###

#impostare il nome corretto della cartella di lavoro
cartella="/cartelladilavoro/arpa"

#cartella accessibile via web
web="/var/www/arpa"

mariaDB="http://88.53.168.210/Bollettino2/MAria_report.xls"

# verifico la risposta del server
code=$(curl -s -o /dev/null -w "%{http_code}" $mariaDB)

# se il file è raggiunbile lo script continua, altrimenti si blocca
if [ $code -eq 200 ]
then

#download file 
#curl -s $mariaDB > $cartella/MAria_report.xls

# converto il file da xls a csv
in2csv $cartella/MAria_report.xls -f xls > $cartella/MAria_report.csv

#rimuovo le prime due righe
sed -e '1,2d' $cartella/MAria_report.csv > $cartella/MAria_report_01.csv

#estraggo le sole colonne utili
csvcut -c 25,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222 $cartella/MAria_report_01.csv > $cartella/MAria_report_02.csv

#aggiungo i nomi colonna
sed  '1s/^/data,partinico_SO2_Max_orario,partinico_SO2_Media_24_h,partinico_CO_Max_orario,partinico_CO_Max_media_8h,partinico_NO2_Max_orario,partinico_O3_Max_orario,partinico_O3_Max_media_8h,partinico_PM10_Media_24_h,partinico_Benzene_Max_orario,partinico_PM2.5_Media_24_h,sasol_SO2_Max_orario,sasol_SO2_Media_24_h,sasol_CO_Max_orario,sasol_CO_Max_media_8h,sasol_NO2_Max_orario,sasol_O3_Max_orario,sasol_O3_Max_media_8h,sasol_PM10_Media_24_h,sasol_Benzene_Max_orario,sasol_PM2.5_Media_24_h,enna_SO2_Max_orario,enna_SO2_Media_24_h,enna_CO_Max_orario,enna_CO_Max_media_8h,enna_NO2_Max_orario,enna_O3_Max_orario,enna_O3_Max_media_8h,enna_PM10_Media_24_h,enna_Benzene_Max_orario,enna_PM2.5_Media_24_h,ex_autoparco_SO2_Max_orario,ex_autoparco_SO2_Media_24_h,ex_autoparco_CO_Max_orario,ex_autoparco_CO_Max_media_8h,ex_autoparco_NO2_Max_orario,ex_autoparco_O3_Max_orario,ex_autoparco_O3_Max_media_8h,ex_autoparco_PM10_Media_24_h,ex_autoparco_Benzene_Max_orario,ex_autoparco_PM2.5_Media_24_h,parcheggio_agip_SO2_Max_orario,parcheggio_agip_SO2_Media_24_h,parcheggio_agip_CO_Max_orario,parcheggio_agip_CO_Max_media_8h,parcheggio_agip_NO2_Max_orario,parcheggio_agip_O3_Max_orario,parcheggio_agip_O3_Max_media_8h,parcheggio_agip_PM10_Media_24_h,parcheggio_agip_Benzene_Max_orario,parcheggio_agip_PM2.5_Media_24_h,trapani_SO2_Max_orario,trapani_SO2_Media_24_h,trapani_CO_Max_orario,trapani_CO_Max_media_8h,trapani_NO2_Max_orario,trapani_O3_Max_orario,trapani_O3_Max_media_8h,trapani_PM10_Media_24_h,trapani_Benzene_Max_orario,trapani_PM2.5_Media_24_h,termini_SO2_Max_orario,termini_SO2_Media_24_h,termini_CO_Max_orario,termini_CO_Max_media_8h,termini_NO2_Max_orario,termini_O3_Max_orario,termini_O3_Max_media_8h,termini_PM10_Media_24_h,termini_Benzene_Max_orario,termini_PM2.5_Media_24_h,cda_gabbia_SO2_Max_orario,cda_gabbia_SO2_Media_24_h,cda_gabbia_CO_Max_orario,cda_gabbia_CO_Max_media_8h,cda_gabbia_NO2_Max_orario,cda_gabbia_O3_Max_orario,cda_gabbia_O3_Max_media_8h,cda_gabbia_PM10_Media_24_h,cda_gabbia_Benzene_Max_orario,cda_gabbia_PM2.5_Media_24_h,termica_milazzo_SO2_Max_orario,termica_milazzo_SO2_Media_24_h,termica_milazzo_CO_Max_orario,termica_milazzo_CO_Max_media_8h,termica_milazzo_NO2_Max_orario,termica_milazzo_O3_Max_orario,termica_milazzo_O3_Max_media_8h,termica_milazzo_PM10_Media_24_h,termica_milazzo_Benzene_Max_orario,termica_milazzo_PM2.5_Media_24_h,boccetta_SO2_Max_orario,boccetta_SO2_Media_24_h,boccetta_CO_Max_orario,boccetta_CO_Max_media_8h,boccetta_NO2_Max_orario,boccetta_O3_Max_orario,boccetta_O3_Max_media_8h,boccetta_PM10_Media_24_h,boccetta_Benzene_Max_orario,boccetta_PM2.5_Media_24_h,misterbianco_SO2_Max_orario,misterbianco_SO2_Media_24_h,misterbianco_CO_Max_orario,misterbianco_CO_Max_media_8h,misterbianco_NO2_Max_orario,misterbianco_O3_Max_orario,misterbianco_O3_Max_media_8h,misterbianco_PM10_Media_24_h,misterbianco_Benzene_Max_orario,misterbianco_PM2.5_Media_24_h,megara_SO2_Max_orario,megara_SO2_Media_24_h,megara_CO_Max_orario,megara_CO_Max_media_8h,megara_NO2_Max_orario,megara_O3_Max_orario,megara_O3_Max_media_8h,megara_PM10_Media_24_h,megara_Benzene_Max_orario,megara_PM2.5_Media_24_h,lab_mobile_1_so2__Max_orario,lab_mobile_1_so2__Media_24_h,lab_mobile1_CO_Max_orario,lab_mobile1_CO_Max_media_8h,lab_mobile1_NO2_Max_orario,lab_mobile1_O3_Max_orario,lab_mobile1_O3_Max_media_8h,lab_mobile1_PM10_Media_24_h,lab_mobile1_Benzene_Max_orario,lab_mobile1_PM2.5_Media_24_h,porto_empedocle_SO2_Max_orario,porto_empedocle_SO2_Media_24_h,porto_empedocle_CO_Max_orario,porto_empedocle_CO_Max_media_8h,porto_empedocle_NO2_Max_orario,porto_empedocle_O3_Max_orario,porto_empedocle_O3_Max_media_8h,porto_empedocle_PM10_Media_24_h,porto_empedocle_Benzene_Max_orario,porto_empedocle_PM2.5_Media_24_h,villa_augusta_SO2_Max_orario,villa_augusta_SO2_Media_24_h,villa_augusta_CO_Max_orario,villa_augusta_CO_Max_media_8h,villa_augusta_NO2_Max_orario,villa_augusta_O3_Max_orario,villa_augusta_O3_Max_media_8h,villa_augusta_PM10_Media_24_h,villa_augusta_Benzene_Max_orario,villa_augusta_PM2.5_Media_24_h,lab_mobile_4_so2__Max_orario,lab_mobile_4_SO2_Media_24_h,lab_mobile_4_CO_Max_orario,lab_mobile_4_CO_Max_media_8h,lab_mobile_4_NO2_Max_orario,lab_mobile_4_O3_Max_orario,lab_mobile_4_O3_Max_media_8h,lab_mobile_4_PM10_Media_24_h,lab_mobile_4_Benzene_Max_orario,lab_mobile_4_PM2.5_Media_24_h,partinico_O3,partinico_NO2,partinico_SO2,sasol_O3,sasol_NO2,sasol_SO2,enna_O3,enna_NO2,enna_SO2,ex_autoparco_O3,ex_autoparco_NO2,ex_autoparco_SO2,parcheggio_agip_O3,parcheggio_agip_NO2,parcheggio_agip_SO2,trapani_O3,trapani_NO2,trapani_SO2,termini_O3,termini_NO2,termini_SO2,cda_gabbia_O3,cda_gabbia_NO2,cda_gabbia_SO2,termica_milazzo_O3,termica_milazzo_NO2,termica_milazzo_SO2,boccetta_O3,boccetta_NO2,boccetta_SO2,misterbianco_O3,misterbianco_NO2,misterbianco_SO2,megara_O3,megara_NO2,megara_SO2\n/' $cartella/MAria_report_02.csv > $cartella/MAria_report_03.csv

# estraggo un file con le metainformazioni sulle colonne (MAria_report_03.txt) e genero un csv con i nomi campi e tipi campi dedotti (campi.csv)
csvstat $cartella/MAria_report_03.csv | tee $cartella/MAria_report_03.txt | awk --posix '/^ *[0-9]{1,3}/,/>/' | sed ':a;N;$!ba;s/\n\t/,/g' | sed -re 's/\. /,/g' | sed -re 's/<type '\''//g' | sed -re 's/'\''>//g' | sed  '1s/^/id,nome,tipo\n/' | sed 's/^ *//g' > $cartella/campi.csv

# creo una variabile che contiene il numero di colonne che sono vuote
colonnevuote=$(cat $cartella/campi.csv | grep 'NoneType' | sed 's/,.*$//g' | tr '\n' ',' | sed s/,$//g)

# estraggo un csv che contiene le sole colonne che non sono vuote
csvcut -C $colonnevuote $cartella/MAria_report_03.csv > $cartella/MAria_report_temp.csv

# rimuovi tutti i record con data precendente a oggi
csvsql --query "select * from MAria_report_temp where data  <  date('$(date '+%Y-%m-%d')')" $cartella/MAria_report_temp.csv > $web/MAria_report.csv

fi
