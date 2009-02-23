###################################################
### chunk number 1: preliminaries
###################################################
## random seed
myseed <- 1071
set.seed(myseed)

## packages
library("vcd")
library("scatterplot3d")
library("xtable")

## data
hospital <- as.table(matrix(c(43,16,3,6,11,10,9,18,16), c(3,3), byrow = TRUE, 
  dimnames = list("Visit frequency" = c("Regular","Less than monthly","Never"), 
                  "Length of stay" = c("2-9","10-19","20+"))))

data("Arthritis")
art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female")
names(dimnames(art))[2] <- "Improvement"

data("UCBAdmissions")
names(dimnames(UCBAdmissions)) <- c("Admission", "Gender", "Department")

data("Punishment")
punish <- xtabs(Freq ~ memory + attitude + age + education, data = Punishment)
dimnames(punish)[[3]][3] <- "40+"

## inference
set.seed(myseed)
art_max <- coindep_test(art, n = 5000)

set.seed(myseed)
ucb_max <- coindep_test(aperm(UCBAdmissions, c(3,2,1)), margin = "Department", n = 5000)

set.seed(myseed)
art_chisq <- coindep_test(art, n = 5000, indepfun = function(x) sum(x^2))

set.seed(myseed)
hos_chisq <- coindep_test(hospital, n = 5000, indepfun = function(x) sum(x^2))


