############################################################################
### Figure 12.3: Construction of a mosaic plot for the hospital data, step 1
############################################################################
library("vcd")
data("Hospital")

mosaic(margin.table(Hospital, 2), split = TRUE)


