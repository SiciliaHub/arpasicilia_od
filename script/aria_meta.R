# uno script R creato da Patrick Hausmann (https://gist.github.com/patperu) per estrarre metadati sui file presenti in questa cartella

options(stringsAsFactors = FALSE)

get_data <- function(m) {
 z <- read.csv2(file.path(datadir, m), nrows = 6, stringsAsFactors = FALSE, head = FALSE)
 stazione <- z[2, 2]
 parm <- unique(unlist(z[3, ]))
 c(stazione, parm)
 }

wd <- getwd()
datadir <- file.path(wd, "data/aria")

files <- list.files(datadir, recursive = TRUE)

zz <- lapply(files, get_data)
# http://r.789695.n4.nabble.com/How-to-rbind-list-of-vectors-with-unequal-vector-lengths-tp3032489p3032496.html
zz <- data.frame(t(sapply(zz, '[', 1:max(sapply(zz, length)))))
zz$date <- unlist(regmatches(files, gregexpr("[0-9]+", files)))
zz <- zz[order(zz$X1, zz$date), ]
rownames(zz) <- NULL
names(zz)[1] <- "stazione"
zz

#
# FINE
#
