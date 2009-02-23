##########################################################################
### Figure 12.19: Conditional association plot for the UCB admissions data
##########################################################################
library(vcd)

data(UCBAdmissions)
names(dimnames(UCBAdmissions)) <- c("Admission", "Gender", "Department")

set.seed(1071)
cotabplot(aperm(UCBAdmissions, c(2,1,3)), panel = cotab_coindep, shade = TRUE,
          legend = FALSE, panel_args = list(type = "assoc", margins = c(2,1,1,2), varnames = FALSE))


