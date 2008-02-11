############################################################################
### Figure 12.4: Construction of a mosaic plot for the hospital data, step 2
############################################################################
library("vcd")
data("Hospital")

mosaic(t(Hospital), split = TRUE, mar = c(left = 3.5),
       labeling_args = list(offset_labels = c(left = 0.5),
         offset_varnames = c(left = 1, top = 0.5), set_labels =
         list("Visit frequency" = c("Regular","Less than\nmonthly","Never"))))


