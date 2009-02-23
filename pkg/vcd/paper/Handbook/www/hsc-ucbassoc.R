##############################################################
### Figure 12.18: Association plot for the UCB admissions data
##############################################################
library(vcd)

data(UCBAdmissions)
names(dimnames(UCBAdmissions)) <- c("Admission", "Gender", "Department")

set.seed(1071)
ucb_max <- coindep_test(aperm(UCBAdmissions, c(3,2,1)), margin = "Department", n = 5000)

assoc(aperm(UCBAdmissions, c(3,2,1)), split = c(TRUE,FALSE,TRUE), shade = TRUE,
      residuals_type = "Pearson", residuals = ucb_max$residuals,
      gp_args = list(p.value = ucb_max$p.value), rep = c(Admission = FALSE))


