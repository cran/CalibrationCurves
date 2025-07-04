#' Print function for a CalibrationCurve object
#'
#' Prints the call, confidence level and values for the performance measures.
#'
#' @param x an object of type CalibrationCurve, resulting from \code{\link{val.prob.ci.2}}.
#' @param ... arguments passed to \code{\link{print}}
#'
#' @seealso \code{\link{val.prob.ci.2}}
#' @return The original \code{CalibrationCurve} object is returned.
print.CalibrationCurve <- function(x, ...) {
  cat("Call:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat(
    paste(
      "A ",
      x$cl.level * 100,
      "% confidence interval is given for the calibration intercept, calibration slope and c-statistic. \n\n",
      sep = ""
    )
  )
  print(x$stats, ...)
  if(!is.null(x$warningMessages))
    for(w in x$warningMessages)
      warning(paste0(w, "\n"), immediate. = TRUE)
  invisible(x)
}

#' Print function for a ggplotCalibrationCurve object
#'
#' Prints the ggplot, call, confidence level and values for the performance measures.
#'
#' @param x an object of type ggplotCalibrationCurve, resulting from \code{\link{valProbggplot}}.
#' @param ... arguments passed to \code{\link{print}}
#'
#' @seealso \code{\link{valProbggplot}}
#' @return The original \code{ggplotCalibrationCurve} object is returned.
print.ggplotCalibrationCurve <- function(x, ...) {
  print(x$ggPlot)
  cat("Call:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat(
    paste(
      "A ",
      x$cl.level * 100,
      "% confidence interval is given for the calibration intercept, calibration slope and c-statistic. \n\n",
      sep = ""
    )
  )
  print(x$stats, ...)
  if(!is.null(x$warningMessages))
    for(w in x$warningMessages)
      warning(paste0(w, "\n"), immediate. = TRUE)
  invisible(x)
}


#' Print function for a GeneralizedCalibrationCurve object
#'
#' Prints the call, confidence level and values for the performance measures.
#'
#' @param x an object of type GeneralizedCalibrationCurve, resulting from \code{\link{genCalCurve}}.
#' @param ... arguments passed to \code{\link{print}}
#'
#' @seealso \code{\link{genCalCurve}}
#' @return The original \code{GeneralizedCalibrationCurve} object is returned.
print.GeneralizedCalibrationCurve <- function(x, ...) {
  cat("Call:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat(
    paste(
      "A ",
      x$cl.level * 100,
      "% confidence interval is given for the calibration intercept and calibration slope. \n\n",
      sep = ""
    )
  )
  print(x$stats, ...)
  if(!is.null(x$warningMessages))
    for(w in x$warningMessages)
      warning(paste0(w, "\n"), immediate. = TRUE)
  invisible(x)
}


#' Print function for a SurvivalCalibrationCurve object
#'
#' @param x an object of type SurvivalCalibrationCurve, resulting from \code{\link{valProbSurvival}}.
#' @param ... arguments passed to \code{\link{print}}
#'
#' @seealso \code{\link{valProbSurvival}}
#' @return The original \code{SurvivalCalibrationCurve} object is returned.
print.SurvivalCalibrationCurve <- function(x, ...) {
  cat("Call:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat(
    paste(
      "A ",
      (1 - x$alpha) * 100,
      "% confidence interval is given for the statistics. \n\n",
      sep = ""
    )
  )
  cat("Calibration performance:\n")
  cat("------------------------\n\n")
  cat("In the large\n\n")
  print(x$stats$Calibration$InTheLarge, ...)
  cat("\nSlope\n\n")
  print(x$stats$Calibration$Slope, ...)
  cat("\nAdditional statistics\n\n")
  print(x$stats$Calibration$Statistics, ...)
  print(x$stats$Calibration$BrierScore, ...)

  cat("\n\nDiscrimination performance:\n")
  cat("-------------------------------\n\n")
  cat("Concordance statistic\n\n")
  print(x$stats$Concordance, ...)
  cat("\n\nTime-dependent AUC\n\n")
  print(x$stats$TimeDependentAUC)
  invisible(x)
}
