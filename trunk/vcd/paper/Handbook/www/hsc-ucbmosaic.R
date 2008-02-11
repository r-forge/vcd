########################################################
### Figure 12.17: Mosic plot for the UCB admissions data
########################################################
library(vcd)

data(UCBAdmissions)
names(dimnames(UCBAdmissions)) <- c("Admission", "Gender", "Department")

set.seed(1071)
ucb_max <- coindep_test(aperm(UCBAdmissions, c(3,2,1)), margin = "Department", n = 5000)

mosaic(aperm(UCBAdmissions, c(3,2,1)), data = UCBAdmissions, split = TRUE,
       shade = TRUE, keep = FALSE, residuals = ucb_max$residuals,
       gp_args = list(p.value = ucb_max$p.value), rep = c(Admission = FALSE))


