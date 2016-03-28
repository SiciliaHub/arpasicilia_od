Maria\_report
================
Patrick Hausmann
28/03/2016

-   [Import the data](#import-the-data)
-   [Export csv file (gzipped)](#export-csv-file-gzipped)
-   [PM10 - Enna e Trapani](#pm10---enna-e-trapani)
-   [Highcharter](#highcharter)
-   [Vegalite](#vegalite)
-   [Days per year with a PM10 value over 50 mg/m3 (max. 35 excedances allowed per year)](#days-per-year-with-a-pm10-value-over-50-mgm3-max.-35-excedances-allowed-per-year)

``` r
library('stringr')
library('reshape2')
library('ggplot2')
library('dplyr')
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library('tidyr')
library('highcharter')
library('vegalite')
```

    ## 
    ## Attaching package: 'vegalite'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     scale_shape, scale_x_sqrt, scale_y_sqrt

``` r
options(stringsAsFactors = FALSE)
```

``` r
split_names <- function(m) {
   stazione <- zz$stazione[which(str_detect(m, zz$stazione))]
   inq      <- zz$inq[which(str_detect(m, zz$inq))]
   tdm      <- zz$tdm[which(str_detect(m, zz$tdm))]
   out <- paste(stazione, inq, tdm, sep = "__")
   out
}

# tdm -> tipo di misura

zz <- read.table(header = TRUE, sep = ";", text = "
stazione;inq;tdm
partinico;SO2;Max_orario
sasol;CO;Media_24_h
enna;NO2;Max_media_8h
ex_autoparco;O3;NA
trapani;Benzene;NA
termini;PM10;NA
cda_gabbia;PM2.5;NA
termica_milazzo;NA;NA
boccetta;NA;NA
misterbianco;NA;NA
megara;NA;NA
lab_mobile1;NA;NA
porto_empedocle;NA;NA
villa_augusta;NA;NA
parcheggio_agip;NA;NA
")

zz %>% knitr::kable()
```

| stazione         | inq     | tdm            |
|:-----------------|:--------|:---------------|
| partinico        | SO2     | Max\_orario    |
| sasol            | CO      | Media\_24\_h   |
| enna             | NO2     | Max\_media\_8h |
| ex\_autoparco    | O3      | NA             |
| trapani          | Benzene | NA             |
| termini          | PM10    | NA             |
| cda\_gabbia      | PM2.5   | NA             |
| termica\_milazzo | NA      | NA             |
| boccetta         | NA      | NA             |
| misterbianco     | NA      | NA             |
| megara           | NA      | NA             |
| lab\_mobile1     | NA      | NA             |
| porto\_empedocle | NA      | NA             |
| villa\_augusta   | NA      | NA             |
| parcheggio\_agip | NA      | NA             |

### Import the data

``` r
x <- read.csv("http://dev.opendatasicilia.it/arpa/MAria_report.csv")

colnames(x) <- c("date", unlist(lapply(colnames(x), split_names)))

xm <- melt(x, id = "date")
xm$variable <- as.character(xm$variable)

u <- data.frame(do.call("rbind", strsplit(xm$variable, "__")), stringsAsFactors = FALSE)
colnames(u) <- c("stazione", "inq", "tdm")
fin <- data.frame(u, date = xm$date, value = xm$value)

head(fin) %>% knitr::kable()
```

| stazione  | inq | tdm         | date       |     value|
|:----------|:----|:------------|:-----------|---------:|
| partinico | SO2 | Max\_orario | 2013-01-01 |  2.227614|
| partinico | SO2 | Max\_orario | 2013-01-02 |  1.331977|
| partinico | SO2 | Max\_orario | 2013-01-03 |  4.150291|
| partinico | SO2 | Max\_orario | 2013-01-04 |  7.960566|
| partinico | SO2 | Max\_orario | 2013-01-05 |  3.602461|
| partinico | SO2 | Max\_orario | 2013-01-06 |  1.674221|

``` r
# most recent day
max(as.Date(fin[!is.na(fin$value), "date"]))
```

    ## [1] "2016-03-24"

``` r
addmargins(table(fin$stazione, !is.na(fin$value), useNA= "always")) %>% knitr::kable()
```

|                  |  FALSE|   TRUE|   NA|     Sum|
|------------------|------:|------:|----:|-------:|
| boccetta         |   8034|    732|    0|    8766|
| cda\_gabbia      |   1717|   4127|    0|    5844|
| enna             |   3700|   9449|    0|   13149|
| ex\_autoparco    |    602|    859|    0|    1461|
| lab\_mobile1     |   5609|    235|    0|    5844|
| megara           |    527|    934|    0|    1461|
| misterbianco     |   3267|   9882|    0|   13149|
| parcheggio\_agip |    587|    874|    0|    1461|
| partinico        |   3053|  10096|    0|   13149|
| porto\_empedocle |  13964|    646|    0|   14610|
| sasol            |    791|    670|    0|    1461|
| termica\_milazzo |   3275|   6952|    0|   10227|
| termini          |   3374|   9775|    0|   13149|
| trapani          |   3695|   9454|    0|   13149|
| villa\_augusta   |   1284|    177|    0|    1461|
| NA               |      0|      0|    0|       0|
| Sum              |  53479|  64862|    0|  118341|

``` r
addmargins(table(fin$stazione, fin$inq, useNA= "always")) %>% knitr::kable()
```

|                  |  Benzene|     CO|    NO2|     O3|   PM10|  PM2.5|    SO2|   NA|     Sum|
|------------------|--------:|------:|------:|------:|------:|------:|------:|----:|-------:|
| boccetta         |        0|      0|   1461|   2922|   1461|      0|   2922|    0|    8766|
| cda\_gabbia      |     1461|      0|   1461|      0|      0|      0|   2922|    0|    5844|
| enna             |     1461|   2922|   1461|   2922|   1461|      0|   2922|    0|   13149|
| ex\_autoparco    |     1461|      0|      0|      0|      0|      0|      0|    0|    1461|
| lab\_mobile1     |     1461|      0|   1461|   2922|      0|      0|      0|    0|    5844|
| megara           |     1461|      0|      0|      0|      0|      0|      0|    0|    1461|
| misterbianco     |     1461|   2922|   1461|   2922|   1461|      0|   2922|    0|   13149|
| parcheggio\_agip |     1461|      0|      0|      0|      0|      0|      0|    0|    1461|
| partinico        |     1461|   2922|   1461|   2922|   1461|      0|   2922|    0|   13149|
| porto\_empedocle |     1461|   2922|   1461|   2922|   1461|   1461|   2922|    0|   14610|
| sasol            |     1461|      0|      0|      0|      0|      0|      0|    0|    1461|
| termica\_milazzo |     1461|   2922|   1461|   2922|   1461|      0|      0|    0|   10227|
| termini          |     1461|   2922|   1461|   2922|   1461|      0|   2922|    0|   13149|
| trapani          |     1461|   2922|   1461|   2922|   1461|      0|   2922|    0|   13149|
| villa\_augusta   |     1461|      0|      0|      0|      0|      0|      0|    0|    1461|
| NA               |        0|      0|      0|      0|      0|      0|      0|    0|       0|
| Sum              |    20454|  20454|  14610|  26298|  11688|   1461|  23376|    0|  118341|

### Export csv file (gzipped)

``` r
con_out <- gzfile("maria_report_04_long.csv.gz", open = "wb")
   write.csv(fin, file = con_out, row.names = FALSE)
close(con_out)
```

### PM10 - Enna e Trapani

``` r
fin_enna_pm10 <- subset(fin, subset = stazione %in% c("enna", "trapani") & inq == "PM10" & tdm == "Media_24_h")
p1 <- ggplot(fin_enna_pm10, aes(x=as.Date(date), y=value, col= stazione)) + geom_line()
p1 <- p1 + facet_wrap( ~ stazione)
p1 <- p1 + geom_hline(yintercept = 50, col = "blue")
p1 <- p1 + geom_smooth()
p1
```

    ## Warning: Removed 797 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 565 rows containing missing values (geom_path).

![](MAria_report_files/figure-markdown_github/Plot_PM10_Enna_Trapani-1.png)

### Highcharter

``` r
fin_hc <- tbl_df(fin) %>% 
          filter(stazione %in% c("enna", "trapani"), inq == "PM10", tdm == "Media_24_h") %>%
          mutate(year_month = format(as.Date(date), "%Y-%m")) %>%
          group_by(stazione, year_month) %>%
          summarise(mean_val = round(mean(value, na.rm = TRUE), 2)) %>%
          ungroup()

fin_hc <- data.frame(fin_hc)

hc1 <- highchart() %>% 
        hc_xAxis(categories = fin_hc$year_month) %>% 
        hc_add_series(name = "Enna", data = fin_hc[fin_hc$stazione=="enna", "mean_val"]) %>%
        hc_add_series(name = "Trapani", data = fin_hc[fin_hc$stazione=="trapani", "mean_val"])
hc1
```

![](MAria_report_files/figure-markdown_github/highcharter-1.png)

### Vegalite

``` r
vegalite(export=TRUE) %>%
  cell_size(400, 400) %>% 
  add_data(fin_hc) %>%
  encode_x("year_month", "temporal") %>%
  encode_y("mean_val", "quantitative") %>%
  encode_color("stazione", "nominal") %>% 
  axis_x(axisWidth=0, format="%Y", labelAngle=0) %>%
  mark_line() -> vl

vl
```

![](MAria_report_files/figure-markdown_github/Vegalite-1.png)

### Days per year with a PM10 value over 50 mg/m3 (max. 35 excedances allowed per year)

``` r
m <- tbl_df(fin) %>%
       filter(inq == "PM10") %>%
       mutate(year = format(as.Date(date), "%Y")) %>%
       group_by(stazione, year) %>%
       mutate(days_gt50 = cumsum(ifelse(!is.na(value) & value > 50, 1, 0))) %>%
       select(stazione, year, days_gt50) %>%
       top_n(1, days_gt50) %>%
       distinct(stazione, year) %>%
       ungroup() %>%
       arrange(stazione, year)

spread(m, year, days_gt50) %>% knitr::kable()
```

| stazione         |  2013|  2014|  2015|  2016|
|:-----------------|-----:|-----:|-----:|-----:|
| boccetta         |     0|     0|     0|     0|
| enna             |     4|     9|     6|     2|
| misterbianco     |     4|    16|     5|     2|
| partinico        |     4|    21|     9|     3|
| porto\_empedocle |     0|     0|    29|     0|
| termica\_milazzo |     5|    18|     8|     4|
| termini          |     2|     9|     6|     3|
| trapani          |     2|    12|     1|     2|

``` r
devtools::session_info()
```

    ## Session info --------------------------------------------------------------

    ##  setting  value                                      
    ##  version  R version 3.2.3 Patched (2015-12-19 r69791)
    ##  system   i386, mingw32                              
    ##  ui       RTerm                                      
    ##  language (EN)                                       
    ##  collate  German_Germany.1252                        
    ##  tz       Europe/Berlin                              
    ##  date     2016-03-28

    ## Packages ------------------------------------------------------------------

    ##  package     * version    date       source                            
    ##  assertthat    0.1        2013-12-06 CRAN (R 3.2.3)                    
    ##  base64        1.1        2011-12-03 CRAN (R 3.2.3)                    
    ##  chron         2.3-47     2015-06-24 CRAN (R 3.2.3)                    
    ##  clipr         0.2.0      2015-10-06 CRAN (R 3.2.3)                    
    ##  colorspace    1.2-6      2015-03-11 CRAN (R 3.2.3)                    
    ##  data.table    1.9.6      2015-09-19 CRAN (R 3.2.3)                    
    ##  DBI           0.3.1      2014-09-24 CRAN (R 3.2.3)                    
    ##  devtools      1.10.0     2016-01-23 CRAN (R 3.2.4)                    
    ##  digest        0.6.9      2016-01-08 CRAN (R 3.2.3)                    
    ##  dplyr       * 0.4.3      2015-09-01 CRAN (R 3.2.3)                    
    ##  evaluate      0.8.3      2016-03-05 CRAN (R 3.2.3)                    
    ##  formatR       1.3        2016-03-05 CRAN (R 3.2.3)                    
    ##  ggplot2     * 2.1.0      2016-03-01 CRAN (R 3.2.3)                    
    ##  gtable        0.2.0      2016-02-26 CRAN (R 3.2.3)                    
    ##  highcharter * 0.2.0      2016-02-25 CRAN (R 3.2.3)                    
    ##  highr         0.5.1      2015-09-18 CRAN (R 3.2.3)                    
    ##  htmltools     0.3.5      2016-03-21 CRAN (R 3.2.4)                    
    ##  htmlwidgets   0.6        2016-02-25 CRAN (R 3.2.3)                    
    ##  jsonlite      0.9.19     2015-11-28 CRAN (R 3.2.3)                    
    ##  knitr         1.12.22    2016-03-25 Github (yihui/knitr@c308522)      
    ##  labeling      0.3        2014-08-23 CRAN (R 3.2.3)                    
    ##  lattice       0.20-33    2015-07-14 CRAN (R 3.2.3)                    
    ##  lazyeval      0.1.10     2015-01-02 CRAN (R 3.2.3)                    
    ##  magrittr      1.5        2014-11-22 CRAN (R 3.2.3)                    
    ##  Matrix        1.2-3      2015-11-28 CRAN (R 3.2.3)                    
    ##  memoise       1.0.0      2016-01-29 CRAN (R 3.2.3)                    
    ##  mgcv          1.8-10     2015-12-12 CRAN (R 3.2.3)                    
    ##  munsell       0.4.3      2016-02-13 CRAN (R 3.2.3)                    
    ##  nlme          3.1-124    2016-01-20 CRAN (R 3.2.3)                    
    ##  plyr          1.8.3      2015-06-12 CRAN (R 3.2.3)                    
    ##  purrr         0.2.1      2016-02-17 Github (hadley/purrr@da96161)     
    ##  quantmod      0.4-5      2015-07-24 CRAN (R 3.2.3)                    
    ##  R6            2.1.2      2016-01-26 CRAN (R 3.2.3)                    
    ##  Rcpp          0.12.3     2016-01-10 CRAN (R 3.2.3)                    
    ##  reshape2    * 1.4.1      2014-12-06 CRAN (R 3.2.3)                    
    ##  rlist         0.4.6      2016-03-06 CRAN (R 3.2.3)                    
    ##  rmarkdown     0.9.5.3    2016-03-25 Github (rstudio/rmarkdown@2733ef0)
    ##  scales        0.4.0      2016-02-26 CRAN (R 3.2.3)                    
    ##  stringi       1.0-1      2015-10-22 CRAN (R 3.2.3)                    
    ##  stringr     * 1.0.0      2015-04-30 CRAN (R 3.2.3)                    
    ##  tidyr       * 0.4.1      2016-02-05 CRAN (R 3.2.3)                    
    ##  TTR           0.23-0     2015-07-10 CRAN (R 3.2.3)                    
    ##  vegalite    * 0.6.1      2016-03-27 Github (hrbrmstr/vegalite@8c31c1b)
    ##  viridisLite   0.1.3      2016-03-12 CRAN (R 3.2.3)                    
    ##  webshot       0.3.0.9000 2016-03-28 Github (wch/webshot@bd66652)      
    ##  xts           0.9-7      2014-01-02 CRAN (R 3.2.3)                    
    ##  yaml          2.1.13     2014-06-12 CRAN (R 3.2.3)                    
    ##  zoo           1.7-12     2015-03-16 CRAN (R 3.2.3)
