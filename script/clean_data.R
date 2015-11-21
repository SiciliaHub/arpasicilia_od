#
# Mail di Giovanni Vacante 15.09.2015 da Opendatasicilia
#
# Source:
# http://opendatasicilia.65952.x6.nabble.com/file/n1737/20b_file_csv_Qualit%C3%A0_Aria_Set2015.rar
#

library('dplyr')
library('data.table')
library('ggplot2')
library('zoo')

get_data <- function(x) {

  # data
  w <- read.csv2(file.path(datadir, x), skip = 6,
                 stringsAsFactors = FALSE,
                 head = FALSE,
                 dec=".")
  # Data header
  y <- read.csv2(file.path(datadir, x),
                 nrows = 6,
                 stringsAsFactors = FALSE,
                 head = FALSE)

  xtime     <- w[, 1]
  Stazione  <- y[2, 2]
  parm      <- unique(as.character(y[3, ]))[-1]
  xcolnames <- unique(as.character(y[6, ]))[-1]

  parm_match <- lapply(parm, function(m) y[3, ] == m)
  res  <- lapply(parm_match, function(m) w[, m])

  xtime <- rep(xtime, length(res))
  xtime <- strptime(xtime, "%d/%m/%Y %H:%M")
  hour  <- hour(xtime)

  res <- as.data.frame(data.table::rbindlist(res))
  colnames(res) <- xcolnames

  parm_vector <- toupper(rep(parm, each = nrow(w)))

  res  <- data.frame(Stazione, xtime, hour,
                     Parm = parm_vector,
                     res, stringsAsFactors = FALSE)

  # Only data from which is equal to file date
  tday <- unlist(regmatches(x, regexec('\\d{8}', x)))
  tday <- strptime(tday, "%d%m%Y")
  idx <- format(res$xtime, "%Y-%m-%d") %in% format(tday, "%Y-%m-%d")
  res <- res[idx, ]

  rownames(res) <- NULL

  res

}

count_hours <- function(x){
  m <- rle(x)
  rep(m$lengths, m$lengths)
}

export_data <- function(x, outdir = "data/stazione_by_day") {
  m <- split(x, f=paste(x$Stazione, format(x$xtime, "%Y%m%d"), sep = "_"))
  files <- paste0(names(m), ".csv")
  invisible(
    mapply(write.csv2, m, file = file.path(wd, outdir, files),
           MoreArgs = list(quote = FALSE, row.names = FALSE))
  )}

################################################################
#                   Filling missing dates
################################################################

fill_missing_days <- function(z1) {

  z1 <- tbl_df(z1) %>%
    arrange(Stazione, Parm, xtime)

  z1 <- data.frame(z1)

  z1s <- split(z1, list(z1$Stazione, z1$Parm))

  # Only data with at least one row
  zz  <- sapply(z1s, function(m) nrow(m) > 0)
  z1s <- z1s[zz]

  date_seq  <- lapply(z1s, function(x) {
                               data.frame(xtime = seq(x$xtime[1],
                                          x$xtime[nrow(x)],
                                          by = "hour") )
                                       })

  x <- mapply(function(date_seq, z1s)
    left_join(date_seq, z1s, by="xtime"),
    date_seq, z1s, SIMPLIFY=FALSE)

  x <- do.call("rbind", x)
  rownames(x) <- NULL

  # 'na.locf' replacing each NA with the most recent non-NA prior to it
  x$Stazione <- zoo::na.locf(x$Stazione)
  x$Parm     <- zoo::na.locf(x$Parm)

  x$hour     <- hour(x$xtime)

  x

}

###############################################################################

wd <- getwd()

datadir <- file.path(wd, "data/aria")

m1 <- list.files(datadir, recursive = TRUE)
m1 <- grep('\\d{8}', m1, value = TRUE)

z <- lapply(m1, get_data)
z <- do.call("rbind", z)

z1 <- tbl_df(z) %>%
  arrange(Stazione, Parm, xtime) %>%
    group_by(Stazione, Parm) %>%
      mutate(nhour = count_hours(hour))

z2 <- fill_missing_days(z1)
addmargins( table(z2$hour, z2$Parm, useNA = "always") )

z2$is_dup = duplicated(z2)

z2 <- z2[z2$is_dup == FALSE, ]

z2 <- tbl_df(z2) %>%
          arrange(Stazione, Parm, xtime) %>%
          group_by(Stazione, Parm) %>%
          mutate(nhour = count_hours(hour))

table(z2$nhour, z2$hour)

write.csv2(z2, file = "data/clean_data/clean_data.csv")

export_data(z2)

#
# FINI
#
