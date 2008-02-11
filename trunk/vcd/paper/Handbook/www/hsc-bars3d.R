###################################################
### Figure 12.2: 3D bar chart for the hospital data
###################################################
library("vcd")
library("scatterplot3d")
data("Hospital")

myHospital = t(Hospital)[,3:1]
mydat = data.frame("Length of stay" = as.vector(row(myHospital)),
                   "Visit frequency" = as.vector(col(myHospital)),
                   "Number of patients" = as.vector(myHospital))

scatterplot3d(mydat, type = "h", pch = " ", lwd = 10,
              x.ticklabs = c("2-9","","10-19","","20+"),
              y.ticklabs = c("Never","","Less than monthly","","Regular"),
              xlab = "Length of stay", ylab = "Visit frequency", zlab = "Number of patients",
              y.margin.add = 0.2,
              color = "black", box = F)


