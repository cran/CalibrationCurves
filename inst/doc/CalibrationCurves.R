## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----logo, echo=FALSE, out.width="25%"----------------------------------------
knitr::include_graphics("./CalibrationCurves.png")

## ----PerfectCalibration, fig.align = 'center', fig.cap = "Example of a perfectly calibrated model", fig.topcaption = TRUE, echo = FALSE, out.width="100%"----
knitr::include_graphics("PerfectCalibration.png")

## ----Overfitted, fig.align = 'center', fig.cap = "Example of a miscalibrated model due to overfitting", fig.topcaption = TRUE, echo = FALSE, out.width="100%"----
knitr::include_graphics("Overfitted.png")

## ----Underfitted, fig.align = 'center', fig.cap = "Example of a miscalibrated model due to underfitting", fig.topcaption = TRUE, echo = FALSE, out.width="100%"----
knitr::include_graphics("Underfitted.png")

## -----------------------------------------------------------------------------
library(CalibrationCurves)
data("traindata")

## -----------------------------------------------------------------------------
head(traindata)

## -----------------------------------------------------------------------------
glmFit = glm(y ~ . , data = traindata, family = binomial)
summary(glmFit)

## -----------------------------------------------------------------------------
data("testdata")
pHat = predict(glmFit, newdata = testdata, type = "response")

## -----------------------------------------------------------------------------
yTest = testdata$y

## ----out.width="100%"---------------------------------------------------------
calPerf = val.prob.ci.2(pHat, yTest)

## -----------------------------------------------------------------------------
calPerf

## -----------------------------------------------------------------------------
str(calPerf)

## -----------------------------------------------------------------------------
flexCal = calPerf$CalibrationCurves$FlexibleCalibration
plot(flexCal[, 1:2], type = "l", xlab = "Predicted probability", ylab = "Observed proportion", lwd = 2, xlim = 0:1, ylim = 0:1)
polygon(
  x = c(flexCal$x, rev(flexCal$x)),
  y = c(
    flexCal$ymax,
    rev(flexCal$ymin)
  ),
  col = rgb(177, 177, 177, 177, maxColorValue = 255),
  border = NA
)

## ----rcsFit,  out.width="100%"------------------------------------------------
rcsFit = tryCatch(val.prob.ci.2(pHat, yTest, smooth = "rcs"),
                  error = function(e) TRUE)
if(is.logical(rcsFit)) {
  plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10))
  text(x = 5, y = 5, labels = paste0("There was a problem estimating\n",
                                     "the calibration curve using rcs"), cex = 2)
} else {
  rcsFit
}

## ----out.width="100%"---------------------------------------------------------
invisible(val.prob.ci.2(pHat, yTest, logistic.cal = TRUE, smooth = "none"))

## ----out.width="100%"---------------------------------------------------------
invisible(val.prob.ci.2(pHat, yTest, logistic.cal = TRUE, col.log = "orange"))

## ----out.width="100%"---------------------------------------------------------
invisible(val.prob.ci.2(pHat, yTest, col.ideal = "black", col.smooth = "red", CL.smooth = TRUE,
              legendloc = c(0, 1), statloc = c(0.6, 0.25)))

## ----out.width="100%"---------------------------------------------------------
invisible(val.prob.ci.2(pHat, yTest, dostats = c("C (ROC)", "Intercept", "Slope", "ECI")))

## ----out.width="100%"---------------------------------------------------------
valProbggplot(pHat, yTest)

## -----------------------------------------------------------------------------
data("poissontraindata")

## -----------------------------------------------------------------------------
head(traindata)

## -----------------------------------------------------------------------------
glmFit = glm(Y ~ . , data = poissontraindata, family = poisson)
summary(glmFit)

## -----------------------------------------------------------------------------
data("poissontestdata")
yHat = predict(glmFit, newdata = poissontestdata, type = "response")

## -----------------------------------------------------------------------------
yTest = poissontestdata$Y

## ----out.width="100%"---------------------------------------------------------
calPerf = genCalCurve(yTest, yHat, family = poisson)

## -----------------------------------------------------------------------------
calPerf

## -----------------------------------------------------------------------------
str(calPerf)

## -----------------------------------------------------------------------------
GLMCal = calPerf$CalibrationCurves$GLMCalibration
plot(GLMCal[, 1:2], type = "l", xlab = "Predicted value", ylab = "Empirical average", lwd = 2, xlim = 0:1, ylim = 0:1,
     col = "red", lty = 2)
abline(0, 1, lty = 1)

## ----out.width="100%"---------------------------------------------------------
set.seed(1)
yTest = testdata$y
pHat[sample(1:length(pHat), 5, FALSE)] = sample(0:1, 5, TRUE)
x = val.prob.ci.2(pHat, yTest, allowPerfectPredictions = TRUE)

