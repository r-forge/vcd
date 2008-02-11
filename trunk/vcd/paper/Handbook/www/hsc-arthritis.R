##########################################################################
### Figure 12.13: Mosaic plot for the arthritis data, using the chi-squared
### test and fixed cut-off points for the shading
##########################################################################
library(vcd)

data(Arthritis)
art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female")
names(dimnames(art))[2] <- "Improvement"

set.seed(1071)
art_chisq <- coindep_test(art, n = 5000, indepfun = function(x) sum(x^2))

mosaic(art, shade = TRUE, gp_args = list(p.value = art_chisq$p.value))


