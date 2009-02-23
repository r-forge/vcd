##############################################################
### Figure 12.16: Doubledecker plot for the UCB admissions data
##############################################################
library(vcd)

data(UCBAdmissions)
names(dimnames(UCBAdmissions)) <- c("Admission", "Gender", "Department")

doubledecker(aperm(aperm(UCBAdmissions, c(1,3,2))[2:1,,], c(2,3,1)),
             margins = c(left = 0, right = 5),col = rev(grey.colors(2)))


