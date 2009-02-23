####################################################################
### Figure 12.10: Spine plot with highlighting for the hospital data
####################################################################
library(vcd)
data(Hospital)

mycol <- rep(grey.colors(2)[2:1], 1:2)
mosaic(t(Hospital),
       mar = c(left = 3.5),
       labeling_args = list(offset_labels = c(left = 0.5),
         offset_varnames = c(left = 1, top = 0.5), set_labels =
         list("Visit frequency" = c("Regular","Less than\nmonthly","Never"))),
       split = TRUE, highlighting = 2, gp = gpar(fill = mycol, col = mycol))


