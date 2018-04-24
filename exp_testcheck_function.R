#' This function generates test results that can be used for assumption check.
#'
#' @param data A data set.
#' @param x A explanatory variable.
#' @param y A dependent variable.
#' @return The selected test result and note.
#' @examples
#' exp_testcheck(esoph, esoph$agegp, esoph$ncontrols, test = "Score")
#' @export

exp_testcheck <- function (data,x,y,test=c("Score", "Shapiro", "Bartlett", "DurbinWatson")) {

  data <- data %>% filter(complete.cases(data))

  xvar <- as.character(deparse(substitute(x)))
  yvar <- as.character(deparse(substitute(y)))

  test <- match.arg(test)


  pround <- function(p.val) {
    if(p.val < 0.001) p.txt <- "p < 0.001"
    else if(p.val < 0.01) p.txt <- paste("p =",format(p.val, digits=2))
    else p.txt <- paste("p =",format(p.val, digits=3))
    p.txt
  }

  data <- data %>%
    filter(complete.cases(data))

  #heteroscedasticity
  mod1 <- lm(y ~ x, data=data)
  summ1 <- summary(mod1)
  res <- resid(mod1)
  n <- nrow(data)

  #nonconstant variance, hypothesis of constant error variance
  if (test=="Score") {
    ncv <- ncvTest(mod1)$p
    if (ncv > 0.05) {
      ncv <- pround(ncv)
      d <- "Assumption holds on the 5% significance level."
    }
    else {
      ncv <- pround(ncv)
      d <- "Assumption does not hold on the 5% significance level."
    }
    t <- cbind(ncv, d)
    colnames(t) <- c("p.Score Test for Non-Constant Error Variance", "Assumption Check Result")
    knitr::kable(t,align='c',caption="Non-constant variance Test")
  }

  #test of homogeneity of variances (bartlett test)
  else if (test=="Bartlett") {
    if (n < 10) {
      suppressWarnings("Sample size is too small.")
    }
    else {
      bt <- bartlett.test(y~x, data)$p.value
      if (bt > 0.05) {
        bt <- pround(bt)
        d <- "Assumption holds on the 5% significance level."
      }
      else {
        bt <- pround(bt)
        d <- "Assumption does not hold on the 5% significance level."
      }
      t <- cbind(bt, d)
      colnames(t) <- c("p.Bartlett", "Assumption Check Result")
      knitr::kable(t,align='c',caption="Bartlett's Test")
    }
  }

  #nonnormality (shapiro test)
  else if (test=="Shapiro") {
    if (n < 10) {
      suppressWarnings("Sample size is too small.")
    }
    else {
      sp <- shapiro.test(mod1$residuals)$p.value
      if (sp > 0.05) {
        sp <- pround(sp)
        d <- "Assumption holds on the 5% significance level."
      }
      else {
        sp <- pround(sp)
        d <- "Assumption does not hold on the 5% significance level."
      }
      t <- cbind(sp, d)
      colnames(t) <- c("p.Shapiro", "Assumption Check Result")
      knitr::kable(t,align='c',caption="Shapiro's Test")
    }
  }

  #nonindependent error
  else if (test=="DurbinWatson") {
    dw <- durbinWatsonTest(mod1)$p
    if (dw < 0.05) {
      dw <- pround(dw)
      d <- "Assumption holds on the 5% significance level."
    }
    else {
      dw <- pround(dw)
      d <- "Assumption does not hold on the 5% significance level."
    }
    t <- cbind(dw, d)
    colnames(t) <- c("p.DurbinWatson", "Assumption Check Result")
    knitr::kable(t,align='c',caption="DurbinWatson Test")
  }

}
