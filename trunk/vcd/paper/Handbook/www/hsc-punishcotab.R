#################################################################
### Figure 12.21: Conditional mosaic plot for the punishment data
#################################################################
library("vcd")

data("Punishment")
punish <- xtabs(Freq ~ memory + attitude + age + education, data = Punishment)
dimnames(punish)[[3]][3] <- "40+"

set.seed(123)
cotabplot(punish, panel = cotab_coindep, panel_args = list(varnames = FALSE, margins = c(2, 1, 1, 2)))


