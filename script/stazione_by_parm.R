#
# Mail di Giovanni Vacante 15.09.2015 da Opendatasicilia
#
# Source:
# http://opendatasicilia.65952.x6.nabble.com/file/n1737/20b_file_csv_Qualit%C3%A0_Aria_Set2015.rar
#

library("dplyr")
library("data.table")

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

  x_df <- lapply(parm, function(m) y[3, ] == m)
  x_df <- lapply(x_df, function(m) w[, m])

  xtime <- rep(xtime, length(x_df))
  xtime <- strptime(xtime, "%d/%m/%Y %H:%M")
  hour  <- hour(xtime)

  x_df <- as.data.frame(data.table::rbindlist(x_df))
  colnames(x_df) <- xcolnames

  parm_vector <- rep(parm, each = nrow(w))

  x_df <- data.frame(Stazione, xtime, hour,
                     Parm = parm_vector,
                     x_df, stringsAsFactors = FALSE)
  x_df

}

export_data <- function(x, outdir = "data/stazione_by_parm") {
  m <- split(x, f=paste(x$Stazione, x$Parm, sep = "_"))
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
  arrange(Stazione, Parm, xtime)

z1$is_dup = duplicated(z1)

export_data(z1)

#
# FINI
#
