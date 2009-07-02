## 1. manually download overviews for each season
##    (as linked from, e.g., http://www.dfb.de/index.php?id=82918)
## 2. convert to UTF-8 (on UTF-8 systems only??)
## 3. source Bundesliga-TH1.R which downloads each webpage for each
##    round within a season
## 4. source Bundesliga-TH2.R which extracts the data and does the
##    formatting

files <- list.files(pattern="html")

for (fi in files) {

   x <- readLines(fi)
   year <- gsub(".html", "", fi)

spiel <- grep("Spieltag anzeigen", x)

foo <- function(x) {
    a <- strsplit(x, "http")[[1]]
    i <- ifelse(length(a) == 2, 2, 3)
    strsplit(a[i], "\"")[[1]][[1]]
}

f <- sapply(x[spiel], foo)
f <- paste("http", f, sep = "")
f <- gsub("amp;", "", f)

for (i in 1:length(f)) {

    dfile <- paste(year, "Spieltag", i, ".html", sep = "_")
    download.file(f[i], dfile)
    system(paste("recode latin1..utf8 ", dfile)) ## FIXME: only on UTF-8 systems?
}

}

