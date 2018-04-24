#' This function generates plots that can be used for assumption check.
#'
#' @param data A data set.
#' @param x A explanatory variable.
#' @param y A dependent variable.
#' @param bio A string of x.
#' @return The plot selected.
#' @examples
#' exp_plotcheck(esoph, esoph$agegp, esoph$ncontrols, plot = "ip", bio="Agegp")
#' @export

exp_plotcheck <- function (data,x,y,bio=bio_def,plot=c("normality", "res","rc", "ip")) {
  data <- data %>% filter(complete.cases(data))

  bio_def <- as.character(deparse(substitute(title)))
  plot <- match.arg(plot)

  pround <- function(p.val) {
    if(p.val < 0.001) p.txt <- "p<0.001"
    else if(p.val < 0.01) p.txt <- paste("p=",format(p.val, digits=2))
    else p.txt <- paste("p=",format(p.val, digits=3))
    p.txt
  }

  data <- data %>%
    filter(complete.cases(data))

  #heteroscedasticity
  mod1 <- lm(y ~ x, data=data)
  summ1 <- summary(mod1)
  res <- resid(mod1)
  s.res <- studres(mod1)
  n <- nrow(data)

  if (plot=="normality") {
  par(mfrow = c(3,1))
  norm.p <- qqnorm(mod1$res, pch = 19, cex = 0.5,main = paste0("QQ-plot of\n",bio));qqline(mod1$res)
  hist <- hist(s.res, prob=TRUE, las=2, col="grey",main = paste0("Histogram of\n",bio))
  box <- boxplot(x, main = paste0("Boxplot of\n",bio))
  }

  else if (plot=="ip") {
    par(mfrow=c(3,1))
    cutoff <- 4/(nrow(data)-1-length(mod1$coefficients)-1)
    cd <- plot(mod1, which = 4, cook.levels = cutoff, main = paste0("Cook's D of ",bio))
    i <- influencePlot(mod1, id.method = "identity",main = paste0("Influencital plot of\n",bio))
    df <- plot(dfbetas(mod1)[,2], ylim=c(-0.5,0.5), pch=19, cex=0.5,main = paste0("DFBETA plot of\n",bio));abline(h=0, col="red");abline(h=0.2, col="red");abline(h=-0.2, col="red")
  }

  else if (plot=="rc") {
    plot(x, res, main = paste0('Residuals vs ',bio))
    abline(0, 0)
    lines(loess.smooth(x, res), col = 'red')
  }

  else if (plot=="res") {
    par(mfrow = c(2, 1))
    res.p <- plot(mod1$fit, mod1$res, xlab = "Fitted Values", ylab = "Residuals", pch = 19, cex = 0.5,main = paste0("Residuals' plot of\n",bio))
    s.res.p <- plot(mod1$fit, mod1$s.res, xlab = "Fitted Values", ylab = "Studentized Residuals", pch = 19, cex = 0.5, main = paste0("Studentized Residuals' plot of\n",bio))
  }
}

