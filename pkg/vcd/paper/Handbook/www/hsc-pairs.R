#######################################################
### Figure 12.15: Pairs-plot for the UCB admissions data
#######################################################
library(vcd)

data(UCBAdmissions)
names(dimnames(UCBAdmissions)) <- c("Admission", "Gender", "Department")

pairs(UCBAdmissions)


