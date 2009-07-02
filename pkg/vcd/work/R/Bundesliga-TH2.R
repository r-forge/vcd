
files <- list.files(pattern = "Spieltag")
files <- files[grep("html", files)]

allreg <- c()

for (f in files) {

    x <- readLines(f)

    nobr <- grep("nobr", x)

    year <- strsplit(f, "_")[[1]][1]
    tag <- strsplit(f, "_")[[1]][3]

    a <- c(strsplit(x[nobr[1]], "td")[[1]][c(6, 8,10,12)],
      sapply(x[nobr[-1]], function(x) strsplit(x, "td")[[1]][c(2, 4,6,8)]))


    reg <- sapply(strsplit(a, ">"), function(x) {
        if (length(x)==3) return(gsub("[-</]", "", x[3]))
        return(substr(x[4], 1,4))
    })

    reg <- matrix(reg, ncol = 4, byrow = TRUE)
    reg <- cbind(reg, year, tag)
    allreg <- rbind(allreg, reg)
    write.csv(reg, file = gsub("html", "txt", f))
}

save(allreg, file = "allreg.Rda")

home <- allreg[,2]

home <- gsub(" $", "", home)
home <- gsub("ö", "oe", home)
home <- gsub("ü", "ue", home)
home <- gsub("ä", "ae", home)

away <- allreg[,3]

away <- gsub(" $", "", away)

away <- gsub("ö", "oe", away)
away <- gsub("ü", "ue", away)
away <- gsub("ä", "ae", away)

home[home == "Bor. Moenchengladbach"] <- "Borussia Moenchengladbach"
away[away == "Bor. Moenchengladbach"] <- "Borussia Moenchengladbach"

homes <- as.integer(sapply(strsplit(allreg[,4], ":"), function(x) x[1]))
aways <- as.integer(sapply(strsplit(allreg[,4], ":"), function(x) x[2]))
round <- as.integer(allreg[,6])
year <- as.integer(allreg[,5])

data("Bundesliga", package = "vcd")

levels(Bundesliga$HomeTeam) <- c(levels(Bundesliga$HomeTeam), "1899 Hoffenheim")
levels(Bundesliga$AwayTeam) <- c(levels(Bundesliga$AwayTeam), "1899 Hoffenheim")

dfb <- data.frame(HomeTeam = factor(home, levels = levels(Bundesliga$HomeTeam)),
                  AwayTeam = factor(away, levels = levels(Bundesliga$AwayTeam)),
                  HomeGoals = homes, AwayGoals = aways, Round = round, Year = year,
                  Date = as.POSIXct(strptime(reg[,1], "%d.%m.%Y, %H.%M")))

save(dfb, file = "Bundesliga_2005-2008.Rda")

summary(subset(dfb, Year == 2005))
summary(subset(Bundesliga, Year == 2005))	

Bundesliga2 <- rbind(Bundesliga, subset(dfb, Year > 2005))

save(Bundesliga2, file = "Bundesliga2.Rda")

