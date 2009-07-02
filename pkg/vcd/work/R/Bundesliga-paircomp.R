## turn Bundesliga results into a data frame
## with a paircomp column

## packages and data
library("prefmod2")
data("Bundesliga", package = "vcd")

## desired teams and seasons
teams <- c("Bayern Muenchen", "Werder Bremen", "VfL Wolfsburg", "VfB Stuttgart", "Borussia Dortmund")
years <- 1992:2005

## set up return value
nobj <- length(teams)
pc <- which(upper.tri(diag(nobj)), arr.ind = TRUE)

## compute paircomp for each season (= first and second half)
compute_pc_for_one_year <- function(y) {

  rval <- matrix(0, ncol = nrow(pc), nrow = 2)

  bl <- subset(Bundesliga, Year == y & HomeTeam %in% teams & AwayTeam %in% teams)
  bl1 <- subset(bl, Round <= max(bl$Round)/2)
  bl2 <- subset(bl, Round >  max(bl$Round)/2)

  for(i in 1:ncol(rval)) {
    wi1 <- which(bl1$HomeTeam == teams[pc[i,1]] & bl1$AwayTeam == teams[pc[i,2]])
    wi2 <- which(bl1$HomeTeam == teams[pc[i,2]] & bl1$AwayTeam == teams[pc[i,1]])
    rval[1,i] <- if(length(wi1) > 0) sign(bl1$HomeGoals[wi1] - bl1$AwayGoals[wi1])
      else if(length(wi2) > 0) sign(bl1$AwayGoals[wi2] - bl1$HomeGoals[wi2])
      else NA

    wi1 <- which(bl2$HomeTeam == teams[pc[i,1]] & bl2$AwayTeam == teams[pc[i,2]])
    wi2 <- which(bl2$HomeTeam == teams[pc[i,2]] & bl2$AwayTeam == teams[pc[i,1]])
    rval[2,i] <- if(length(wi1) > 0) sign(bl2$HomeGoals[wi1] - bl2$AwayGoals[wi1])
      else if(length(wi2) > 0) sign(bl2$AwayGoals[wi2] - bl2$HomeGoals[wi2])
      else NA
  }

  rval <- as.data.frame(rval)
  rval$year <- rep(y, 2)
  rval$round <- factor(1:2, labels = c("Hinrunde", "Rueckrunde"))
  return(rval)  
}

## formatting
BL <- do.call("rbind", lapply(years, compute_pc_for_one_year))
BL$result <- paircomp(as.matrix(BL[,1:nrow(pc)]), labels = teams)
BL <- BL[, -c(1:nrow(pc))]

## check results
summary(BL$result)
plot(BL$result)
