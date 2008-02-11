########################################################
### Figure 12.1: Grouped bar chart for the hospital data
########################################################
library("vcd")
data("Hospital")
barplot(Hospital, legend = rownames(Hospital), beside = TRUE,
        xlab = "Length of stay", ylab = "Number of patients")


