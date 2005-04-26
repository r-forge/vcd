mosaic.loglm <- plot.loglm <- function(x,
                       panel = mosaic,
                       type = c("observed", "expected"),
                       residuals_type = c("pearson", "deviance"),
                       ...)
{
  residuals_type <- match.arg(residuals_type)
  if(is.null(x$fitted)) x <- update(x, fitted = TRUE)
  expected <- fitted(x)
  residuals <- residuals(x, type = "pearson")
  observed <- residuals * sqrt(expected) + expected
  if(residuals_type == "deviance") residuals <- residuals(x, type = "deviance")
  panel(observed, residuals = residuals, expected = expected, df = x$df,
        type = type, ...)
}

#Z# example:
#Z# 
#Z# data(PreSex)
#Z# tab <- xtabs(Freq ~ PremaritalSex + ExtramaritalSex + Gender + MaritalStatus,
#Z#   data = as.data.frame(PreSex))
#Z# mosaic(tab)
#Z# fm <- loglm(~ PremaritalSex * ExtramaritalSex * (Gender + MaritalStatus),
#Z#   data = tab)
#Z# fm
#Z# plot(fm)
#Z# plot(fm, split = TRUE)
#Z# plot(fm, split = TRUE, type = "expected")
#Z# plot(fm, residuals = "deviance")
