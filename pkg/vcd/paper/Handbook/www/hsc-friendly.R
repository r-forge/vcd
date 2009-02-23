########################################################################
### Figure 12.11: Mosaic display of the hospital data with Friendly-like
### color coding of the residuals
########################################################################
library(vcd)
data(Hospital)

set.seed(1071)
hos_chisq <- coindep_test(Hospital, n = 5000, indepfun = function(x) sum(x^2))

mosaic(t(Hospital), split = TRUE, shade = TRUE,
       mar = c(left = 3.5),
       gp_args = list(p.value = hos_chisq$p.value),
       labeling_args = list(offset_labels = c(left = 0.5),
         offset_varnames = c(left = 1, top = 0.5), set_labels =
         list("Visit frequency" = c("Regular","Less than\nmonthly","Never"))))


