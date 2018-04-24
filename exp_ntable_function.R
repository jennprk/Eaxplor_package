#' This function generates a numerical summary table of a variable for each levels of a factor variable in knitr format.
#'
#' @param x A factor variable.
#' @param y A numerical variable.
#' @return A numerical summary table of \code{y} for each levels of \code{x}.
#' @examples
#' exp_ntable(esoph, esoph$agegp, esoph$ncontrols)
#' exp_ntable(esoph, esoph$agegp, esoph$ncontrols, pval="both")
#' @export

exp_ntable <- function(data,x,y,pval=c("none","anova","kw","both")) {
  pval <- match.arg(pval)


  if (is.null(levels(x))) {
    warning("x should be a factor")
  }

  else{
    pround <- function(p.val) {
      if(p.val < 0.001) p.txt <- "p<0.001"
      else if(p.val < 0.01) p.txt <- paste("p=",format(p.val, digits=2))
      else p.txt <- paste("p=",format(p.val, digits=3))
      p.txt
    }

    xvar <- as.character(deparse(substitute(x)))
    yvar <- as.character(deparse(substitute(y)))
    bio_x <- gsub(".*\\$","",xvar)
    bio_y <- gsub(".*\\$","",yvar)

    fsmry_y <- function(y){
      out <- data.frame(n = length(y),
                        n.complete = length(y[!is.na(y)]),
                        mean = mean(y, na.rm=T),
                        sd = sd(y, na.rm=T),
                        median = median(y,na.rm=T),
                        Q1 = quantile(y,0.25),
                        Q3 = quantile(y,0.75))
      return(out)
    }

    fsmry_by_grp <- function(x, y){

      x <- factor(x)

      smry <- tapply(y, x, fsmry_y)
      smry <- do.call(rbind, smry)
      return(smry)
    }


    summ <- fsmry_by_grp(x, y)
    summ$group <- levels(x)

    kw <- function(x,y){
      k <- kruskal.test(y~x)$p.value
      pround(k)
    }


    lmp <- function (x,y) {
      model <- lm(y~x)
      p <- anova(model)$`Pr(>F)`[1]
      pround(p)
    }

    kw <- function(x,y){
      k <- kruskal.test(y~x)$p.value
      pround(k)
    }


    lmp <- function (x,y) {
      model <- lm(y~x)
      p <- anova(model)$`Pr(>F)`[1]
      pround(p)
    }

    if (pval == "none"){
      out <- data.frame(summ[,1:2],
                        mean.sd = paste0(format(summ$mean, digits=3),
                                         " +/- ",
                                         format(summ$sd, digits=3)),
                        median.iqr = paste0(format(summ$median,digits=3),
                                            "(",
                                            format(summ$Q1,digits=3),
                                            ",",
                                            format(summ$Q3,digits=3),
                                            ")"))
      names(out)[3] <- "mean +/- sd"
      names(out)[4] <- "median (IQR)"
      knitr::kable(out,align="c",caption=paste0("Summary of ", bio_y ," for ", bio_x))
    }

    else if (pval == "both")  {
      out <- data.frame(summ[,1:2],
                        mean.sd = paste0(format(summ$mean, digits=3),
                                         " +/- ",
                                         format(summ$sd, digits=3)),
                        p.anova=c(replicate(nrow(summ)-1,""), lmp(x,y)),
                        median.iqr = paste0(format(summ$median,digits=3),
                                            "(",
                                            format(summ$Q1,digits=3),
                                            ",",
                                            format(summ$Q3,digits=3),
                                            ")"),
                        p.kw=c(replicate(nrow(summ)-1,""),kw(x,y)))

      names(out)[3] <- "mean +/- sd"
      names(out)[5] <- "median (IQR)"
      knitr::kable(out,align="c",caption=paste0("Summary of ", bio_y ," for ", bio_x))
    }

    else if (pval == "anova") {
      out <- data.frame(summ[,1:2],
                        mean.sd = paste0(format(summ$mean, digits=3),
                                         " +/- ",
                                         format(summ$sd, digits=3)),
                        p.anova=c(replicate(nrow(summ)-1,""), lmp(x,y)),
                        median.iqr = paste0(format(summ$median,digits=3),
                                            "(",
                                            format(summ$Q1,digits=3),
                                            ",",
                                            format(summ$Q3,digits=3),
                                            ")"))
      names(out)[3] <- "mean +/- sd"
      names(out)[5] <- "median (IQR)"
      knitr::kable(out,align="c",caption=paste0("Summary of ", bio_y ," for ", bio_x))
    }

    else if (pval == "kw"){
      out <- data.frame(summ[,1:2],
                        mean.sd = paste0(format(summ$mean, digits=3),
                                         " +/- ",
                                         format(summ$sd, digits=3)),
                        median.iqr = paste0(format(summ$median,digits=3),
                                            "(",
                                            format(summ$Q1,digits=3),
                                            ",",
                                            format(summ$Q3,digits=3),
                                            ")"),
                        p.kw=c(replicate(nrow(summ)-1,""),kw(x,y)))


      names(out)[3] <- "mean +/- sd"
      names(out)[4] <- "median (IQR)"
      knitr::kable(out,align="c",caption=paste0("Summary of ", bio_y ," for ", bio_x))
    }
  }
}
