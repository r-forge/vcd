##############################################################
### Figure 12.7: Association plot for the hospital data
##############################################################
library("vcd")
data("Hospital")

assoc(t(Hospital), split = TRUE)


