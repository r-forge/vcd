###################################################
### Figure 12.6: Sieve plot for the hospital data
###################################################
library("vcd")
data("Hospital")

sieve(t(Hospital), split = TRUE, pop = FALSE, gp = gpar(lty = "dotted", col = "black"))
labeling_cells(text = t(Hospital), clip = FALSE, gp = gpar(fontface = 2, fontsize = 15))(t(Hospital))


