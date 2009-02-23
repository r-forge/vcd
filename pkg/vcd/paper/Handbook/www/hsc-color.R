################################################################
### Figure 12.8: Qualitative color palette for HSV and HCL space
################################################################
library(vcd)

par(mfrow = c(1,2), mar = c(1,1,1,1), oma = c(0,0,0,0))
pie(rep(1,9), radius = 1, col = rainbow(9), labels = 360 * 0:8/9)
pie(rep(1,9), radius = 1, col = rainbow_hcl(9), labels = 360 * 0:8/9)


