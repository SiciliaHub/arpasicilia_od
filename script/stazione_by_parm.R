#
# Mail di Giovanni Vacante 15.09.2015 da Opendatasicilia
#
# Source:
# http://opendatasicilia.65952.x6.nabble.com/file/n1737/20b_file_csv_Qualit%C3%A0_Aria_Set2015.rar
#

library("dplyr")
library("data.table")

get_data <- function(x) {
  # res = final dataset

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
  res <- lapply(parm_match, function(m) w[, m])

  xtime <- rep(xtime, length(res))
  xtime <- strptime(xtime, "%d/%m/%Y %H:%M")
  hour  <- hour(xtime)

  res <- as.data.frame(data.table::rbindlist(res))
  colnames(res) <- xcolnames

  parm_vector <- toupper(rep(parm, each = nrow(w)))

  res  <- data.frame(Stazione, xtime, hour,
                     Parm = parm_vector,
                     x_df, stringsAsFactors = FALSE)
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

table(z1$nhour, z1$hour)

z1$is_dup = duplicated(z1)

export_data(z1)

#
# FINI
#
