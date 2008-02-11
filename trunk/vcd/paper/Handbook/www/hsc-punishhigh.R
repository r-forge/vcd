######################################################################
### Figure 12.20: Mosic plot with highlighting for the punishment data
######################################################################
library("vcd")

data("Punishment")
punish <- xtabs(Freq ~ memory + attitude + age + education, data = Punishment)
dimnames(punish)[[3]][3] <- "40+"

mosaic(~ age + education + memory + attitude, data = punish, keep = FALSE,
       gp = gpar(fill = grey.colors(2)), spacing = spacing_highlighting,
       rep = c(attitude = FALSE))


