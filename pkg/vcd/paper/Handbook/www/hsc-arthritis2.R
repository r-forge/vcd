###########################################################################
### Figure 12.14: Mosaic plot for the arthritis data, using the maximum test
### and data-driven cut-off points for the residuals
###########################################################################
library(vcd)

data(Arthritis)
art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female")
names(dimnames(art))[2] <- "Improvement"

set.seed(1071)
mosaic(art, gp = shading_max, gp_args = list(n = 5000))


