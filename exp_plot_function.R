#' This function generates a numerical summary table of a variable for each levels of a factor variable in knitr format.
#'
#' @param x A numeric variable.
#' @param y A variable.
#' @return A categorical summary table of \code{x} and \code{y}.
#' @examples
#' exp_plots(esoph, esoph$agegp, esoph$ncontrols,alternative="both",color="color")
#' exp_plots(esoph, esoph$agegp, esoph$ncontrols,alternative="barplot",color="mono",bio="number of controls",legend_title = "age")
#' @export

exp_plots <- function(data,x,y,alternative=c("both","barplot","boxplot"),colors=c("color","mono"),bio=bio_y,legend_title=bio_x,legend_labels=label_default,...) {
  alternative <- match.arg(alternative)
  colors <- match.arg(colors)

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


  smry <- fsmry_by_grp(x, y)

  kw <- function(x,y){
    k <- kruskal.test(y~x)$p.value
    pround(k)
  }


  lmp <- function (x,y) {
    model <- lm(y~x)
    p <- anova(model)$`Pr(>F)`[1]
    pround(p)
  }

  smry$factor <-levels(x)
  as.data.frame(smry)


  label_default <- levels(x)

  if (alternative=="both" & colors == "color") {
    barplot <-  ggplot(smry,aes(x = factor, y= mean,group=factor,fill=factor)) +
      geom_col() +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd,width = 0.2)) +
      theme_bw() +
      labs(title = paste0("Errorbar plots of\n",bio), y= bio) +
      annotate("text",  x=Inf, y = Inf, label = lmp(x,y) , vjust=1, hjust=1) +
      scale_fill_discrete(name = legend_title,breaks=label_default,labels=legend_labels)

    boxplot <-ggplot(data,aes(x=x, y=y,fill = x)) +
      geom_boxplot() +
      theme_bw() +
      labs(title = paste0("Boxplots of\n",bio), y= bio) +
      annotate("text",  x=Inf, y = Inf, label = kw(x,y), vjust=1, hjust=1) +
      scale_fill_discrete(name = legend_title,breaks=label_default,labels=legend_labels)

    plot_grid(barplot, boxplot, ncol = 2, align = 'v')
  }

  else if (alternative=="both" & colors == "mono") {
    barplot <- ggplot(smry,aes(x = factor, y= mean,group=factor,fill=factor)) +
      geom_col() +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd,width = 0.2))+
      theme_bw() +
      labs(title = paste0("Errorbar plots of\n",bio), y= bio) +
      annotate("text",  x=Inf, y = Inf, label = lmp(x,y) , vjust=1, hjust=1) +
      scale_fill_manual(name = legend_title,values=rep("gray56",length(label_default)),breaks=label_default,labels=legend_labels)

    boxplot <-ggplot(data,aes(x=x, y=y,fill = x)) +
      geom_boxplot() +
      theme_bw() +
      labs(title = paste0("Boxplots of\n",bio), y= bio) +
      annotate("text",  x=Inf, y = Inf, label = kw(x,y), vjust=1, hjust=1) +
      scale_fill_manual(name = legend_title,values=rep("gray56",length(label_default)),breaks=label_default,labels=legend_labels)

    plot_grid(barplot, boxplot, ncol = 2, align = 'v')
  }


  else if (alternative=="barplot" & colors == "color") {
    ggplot(smry,aes(x = factor, y= mean,group=factor,fill=factor)) +
      geom_col() +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd,width = 0.2))+
      theme_bw() +
      labs(title = paste0("Errorbar plots of\n",bio), y= bio) +
      annotate("text",  x=Inf, y = Inf, label = lmp(x,y) , vjust=1, hjust=1)+
      scale_fill_discrete(name = legend_title,breaks=label_default,labels=legend_labels)
  }

  else if (alternative=="barplot" & colors == "mono"){
    ggplot(smry,aes(x = factor, y= mean,group=factor,fill=factor)) +
      geom_col() +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd,width = 0.2))+
      theme_bw() +
      labs(title = paste0("Errorbar plots of\n",bio), y= bio) +
      annotate("text",  x=Inf, y = Inf, label = lmp(x,y) , vjust=1, hjust=1)+
      scale_fill_manual(name = legend_title,values=rep("gray56",length(label_default)),breaks=label_default,labels=legend_labels)
  }

  else if (alternative=="boxplot" & colors == "color"){
    ggplot(data,aes(x=x, y=y,fill=x)) +
      geom_boxplot() +
      theme_bw() +
      labs(title = paste0("Boxplots of\n",bio), y= bio) +
      annotate("text",  x=Inf, y = Inf, label = kw(x,y), vjust=1, hjust=1)+
      scale_fill_discrete(name = legend_title,breaks=label_default,labels=legend_labels)
  }

  else {
    ggplot(data,aes(x=x, y=y,fill=x)) +
      geom_boxplot() +
      theme_bw() +
      labs(title = paste0("Boxplots of\n",bio), y= bio) +
      annotate("text",  x=Inf, y = Inf, label = kw(x,y), vjust=1, hjust=1)+
      scale_fill_manual(name = legend_title,values=rep("gray56",length(label_default)),breaks=label_default,labels=legend_labels)
  }
}


