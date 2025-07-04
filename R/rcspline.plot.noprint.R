#' Internal function
#'
#' Adjusted version of the \code{\link[Hmisc]{rcspline.plot}} function where only the output is returned and no plot is made
#'
#'
#' @param x a numeric predictor
#' @param y a numeric response. For binary logistic regression, \code{y} should be either 0 or 1.
#' @param model \code{"logistic"} or \code{"cox"}. For \code{"cox"}, uses the \code{coxph.fit} function with \code{method="efron"} argument set.
#' @param xrange range for evaluating \code{x}, default is \eqn{f} and \eqn{1 - f} quantiles of \code{x},
#' where \eqn{f = \frac{10}{\max{(n, 200)}}}{f = 10/max(\code{n}, 200)} and \eqn{n} the number of observations
#' @param event event/censoring indicator if \code{model="cox"}. If \code{event} is present, \code{model} is assumed to be \code{"cox"}
#' @param nk number of knots
#' @param knots knot locations, default based on quantiles of \code{x} (by \code{\link[Hmisc]{rcspline.eval}})
#' @param show \code{"xbeta"} or \code{"prob"} - what is plotted on \verb{y}-axis
#' @param adj optional matrix of adjustment variables
#' @param xlab \verb{x}-axis label, default is the \dQuote{label} attribute of \code{x}
#' @param ylab \verb{y}-axis label, default is the \dQuote{label} attribute of \code{y}
#' @param ylim \verb{y}-axis limits for logit or log hazard
#' @param plim \verb{y}-axis limits for probability scale
#' @param plotcl plot confidence limits
#' @param showknots show knot locations with arrows
#' @param add add this plot to an already existing plot
#' @param plot logical to indicate whether a plot has to be made. \code{FALSE} suppresses the plot.
#' @param subset subset of observations to process, e.g. \code{sex == "male"}
#' @param lty line type for plotting estimated spline function
#' @param noprint suppress printing regression coefficients and standard errors
#' @param m for \code{model="logistic"}, plot grouped estimates with triangles. Each group contains \code{m} ordered observations on \code{x}.
#' @param smooth plot nonparametric estimate if \code{model="logistic"} and \code{adj} is not specified
#' @param bass smoothing parameter (see \code{supsmu})
#' @param main main title, default is \code{"Estimated Spline Transformation"}
#' @param statloc location of summary statistics. Default positioning by clicking left mouse button where upper left corner of statistics should appear.
#'  Alternative is \code{"ll"} to place below the graph on the lower left, or the actual \code{x} and \code{y} coordinates. Use \code{"none"} to suppress statistics.
#'
#' @return list with components (\samp{knots}, \samp{x}, \samp{xbeta}, \samp{lower}, \samp{upper}) which are respectively the knot locations, design matrix,
#' linear predictor, and lower and upper confidence limits
#' @seealso   \code{\link[rms]{lrm}}, \code{\link[rms]{cph}}, \code{\link[Hmisc]{rcspline.eval}}, \code{\link[graphics]{plot}}, \code{\link[stats]{supsmu}},
#' \code{\link[survival:survival-internal]{coxph.fit}}, \code{\link[rms]{lrm.fit}}
.rcspline.plot <- function(x, y, model=c("logistic","cox","ols"), xrange,
                          event, nk=5, knots=NULL, show=c("xbeta", "prob"),
                          adj=NULL, xlab, ylab, ylim, plim=c(0,1),
                          plotcl=TRUE, showknots=TRUE, add=FALSE, plot = TRUE, subset,
                          lty=1, noprint=FALSE, m, smooth=FALSE, bass=1,
                          main="auto", statloc)
{
  model <- match.arg(model)
  show  <- match.arg(show)
  if(plot) {
    oldpar = par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }

  if(! missing(event))
    model<-"cox"

  if(model == "cox" & missing(event))
    stop('event must be given for model="cox"')

  if(show == "prob" & ! missing(adj))
    stop('show="prob" cannot be used with adj')

  if(show == "prob" & model != "logistic")
    stop('show="prob" can only be used with model="logistic"')

  if(length(x) != length(y))
    stop('x and y must have the same length')

  if(! missing(event) && length(event) != length(y))
    stop('y and event must have the same length')

  if(! missing(adj)) {
    if(! is.matrix(adj)) adj <- as.matrix(adj)
    if(dim(adj)[1] != length(x))
      stop('x and adj must have the same length')
  }

  if(missing(xlab))
    xlab <- label(x)

  if(missing(ylab))
    ylab <- label(y)

  isna <- is.na(x) | is.na(y)
  if(! missing(event))
    isna <- isna | is.na(event)

  nadj <- 0
  if(! missing(adj)) {
    nadj <- ncol(adj)
    isna <- isna | apply(is.na(adj), 1, sum) > 0
  }

  if(! missing(subset))
    isna <- isna | (! subset)

  x <- x[! isna]
  y <- y[! isna]
  if(! missing(event))
    event <- event[! isna]

  if(! missing(adj))
    adj <- adj[! isna, ]

  n <- length(x)
  if(n<6)
    stop('fewer than 6 non-missing observations')

  if(missing(xrange)) {
    frac<-10./max(n, 200)
    xrange<-quantile(x, c(frac, 1.-frac))
  }

  if(missing(knots))
    xx <- rcspline.eval(x, nk=nk)
  else xx <- rcspline.eval(x, knots)

  knots <- attr(xx, "knots")
  nk <- length(knots)

  df1 <- nk-2
  if(model == "logistic") {
    b    <- rms::lrm.fit(cbind(x, xx, adj),  y)
    beta <- b$coef
    cov  <- vcov(b)
    model.lr <- b$stats["Model L.R."]
    offset <- 1 	#to skip over intercept parameter
    ylabl <-
      if(show == "prob")
        "Probability"
    else "log Odds"

    sampled <- paste("Logistic Regression Model,  n=", n," d=", sum(y), sep="")
  }

  if(model == "cox") {
    if(! existsFunction('coxph.fit'))
      coxph.fit <- getFromNamespace('coxph.fit', 'survival')
    ##11mar04

    ## added coxph.control around iter.max, eps  11mar04
    lllin <- coxph.fit(cbind(x, adj), cbind(y, event), strata=NULL,
                       offset=NULL, init=NULL,
                       control=coxph.control(iter.max=10, eps=.0001),
                       method="efron", rownames=NULL)$loglik[2]
    b <- coxph.fit(cbind(x, xx, adj), cbind(y, event), strata=NULL,
                   offset=NULL, init=NULL,
                   control=coxph.control(iter.max=10, eps=.0001),
                   method="efron", rownames=NULL)
    beta <- b$coef
    if(! noprint) {
      print(beta);
      print(b$loglik)
    }

    beta     <- b$coef
    cov      <- vcov(b)
    model.lr <- 2*(b$loglik[2]-b$loglik[1])
    offset   <- 0
    ylabl    <- "log Relative Hazard"
    sampled  <- paste("Cox Regression Model, n=",n," events=",sum(event),
                     sep="")
  }

  if(model == "logistic"|model == "cox") {
    model.df <- nk - 1 + nadj
    model.aic <- model.lr-2.*model.df
    v <- solve(cov[(1 + offset) : (nk + offset - 1), (1 + offset) : (nk + offset - 1)])
    assoc.chi <- beta[(1 + offset) : (nk + offset - 1)] %*% v %*%
      beta[(1 + offset) : (nk + offset - 1)]
    assoc.df <- nk - 1   #attr(v,"rank")
    assoc.p <- 1.-pchisq(assoc.chi, nk - 1)
    v <- solve(cov[(2 + offset) : (nk + offset - 1), (2 + offset) : (nk + offset - 1)])
    linear.chi <- beta[(2 + offset) : (nk + offset - 1)] %*% v %*%
      beta[(2 + offset) : (nk + offset - 1)]
    linear.df <- nk - 2   #attr(v,"rank")
    linear.p <- 1. - pchisq(linear.chi, linear.df)
    if(nadj > 0) {
      ntot <- offset + nk - 1 + nadj
      v <- solve(cov[(nk + offset) : ntot, (nk + offset) : ntot])
      adj.chi <- beta[(nk + offset) : ntot] %*% v %*%
        beta[(nk + offset) : ntot]
      adj.df <- ncol(v)   #attr(v,"rank")
      adj.p <- 1. - pchisq(adj.chi, adj.df)
    } else {
      adj.chi <- 0
      adj.p <- 0
    }
  }

  ## Evaluate xbeta for expanded x at desired range
  xe <- seq(xrange[1], xrange[2], length=600)
  if(model == "cox")
    xx <- rcspline.eval(xe, knots, inclx=TRUE)
  else
    xx<- cbind(rep(1, length(xe)), rcspline.eval(xe, knots, inclx=TRUE))

  xbeta <- xx %*% beta[1 : (nk - 1 + offset)]
  var <- drop(((xx %*% cov[1 : (nk - 1 + offset), 1 : (nk - 1 + offset)])*xx) %*%
                rep(1, ncol(xx)))
  lower <- xbeta - 1.96*sqrt(var)
  upper <- xbeta + 1.96*sqrt(var)
  if(show == "prob") {
    xbeta <- 1./(1. + exp(-xbeta))
    lower <- 1./(1. + exp(-lower))
    upper <- 1./(1. + exp(-upper))
  }

  xlim <- range(pretty(xe))
  if(missing(ylim))
    ylim <- range(pretty(c(xbeta, if(plotcl) lower, if(plotcl) upper)))

  if(main == "auto") {
    if(show == "xbeta")
      main <- "Estimated Spline Transformation"
    else main <- "Spline Estimate of Prob{Y=1}"
  }

  if(! interactive() & missing(statloc))
    statloc<-"ll"

  if(plot) {
    if(! add) {
      oldmar<-par("mar")
      if(! missing(statloc) && statloc[1] == "ll")
        oldmar[1]<- 11

      plot(xe, xbeta, type="n", main=main, xlab=xlab, ylab=ylabl,
           xlim=xlim, ylim=ylim)
      lines(xe, xbeta, lty=lty)
      ltext<-function(z, line, label, cex=.8, adj=0)
      {
        zz<-z
        zz$y<-z$y-(line - 1)*1.2*cex*par("csi")*(par("usr")[4]-par("usr")[3])/
          (par("fin")[2])   #was 1.85
        text(zz, label, cex=cex, adj=adj)
      }

      sl<-0
      if(missing(statloc)) {
        message("Click left mouse button at upper left corner for statistics\n")
        z<-locator(1)
        statloc<-"l"
      } else if(statloc[1] != "none") {
        if(statloc[1] == "ll") {
          z<-list(x=par("usr")[1], y=par("usr")[3])
          sl<-3
        } else z<-list(x=statloc[1], y=statloc[2])
      }

      if(statloc[1] != "none" & (model == "logistic" | model == "cox"))	{
        rnd <- function(x, r=2) as.single(round(x, r))

        ltext(z, 1 + sl, sampled)
        ltext(z, 2 + sl, "    Statistic        X2  df")
        chistats<-format(as.single(round(c(model.lr, model.aic,
                                           assoc.chi, linear.chi, adj.chi), 2)))
        pvals<-format(as.single(round(c(assoc.p, linear.p, adj.p), 4)))
        ltext(z, 3 + sl, paste("Model        L.R. ", chistats[1], model.df,
                               " AIC=", chistats[2]))
        ltext(z, 4 + sl, paste("Association  Wald ", chistats[3], assoc.df,
                               " p= ", pvals[1]))
        ltext(z, 5 + sl, paste("Linearity    Wald ", chistats[4], linear.df,
                               " p= ", pvals[2]))
        if(nadj > 0)ltext(z, 6 + sl, paste("Adjustment   Wald " , chistats[5],
                                           adj.df, " p= ", pvals[3]))}
    } else lines(xe, xbeta, lty=lty)

    if(plotcl) {
      #prn(cbind(xe, lower, upper))
      lines(xe, lower, lty=2)
      lines(xe, upper, lty=2)
    }

    if(showknots) {
      bot.arrow <- par("usr")[3]
      top.arrow <- bot.arrow + .05 * (par("usr")[4]-par("usr")[3])
      for(i in 1 : nk)
        arrows(knots[i], top.arrow, knots[i], bot.arrow, length=.1)
    }

    if(model == "logistic" & nadj == 0) {
      if(smooth) {
        z<-supsmu(x, y, bass=bass)
        if(show == "xbeta") z$y <- logb(z$y/(1.-z$y))
        points(z, cex=.4)
      }

      if(! missing(m)) {
        z<-groupn(x, y, m=m)
        if(show == "xbeta") z$y <- logb(z$y/(1.-z$y))

        points(z, pch=2, mkh=.05)}
    }
  }

  invisible(list(
    knots = knots,
    x = xe,
    xbeta = xbeta,
    lower = lower,
    upper = upper
  ))
}
