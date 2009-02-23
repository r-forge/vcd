#########################################################################
### Figure 12.12: Association plot of the hospital data with Friendly-like
### color coding of the residuals
#########################################################################
library(vcd)
data(Hospital)

set.seed(1071)
hos_chisq <- coindep_test(Hospital, n = 5000, indepfun = function(x) sum(x^2))

assoc(t(Hospital), split = TRUE, shade = TRUE, keep = TRUE,
      gp_args = list(p.value = hos_chisq$p.value))


