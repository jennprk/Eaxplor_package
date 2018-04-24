#' This function generates a numerical summary table of a variable for each levels of a factor variable in knitr format.
#'
#' @param x A factor variable.
#' @param y A factor variable.
#' @return A categorical summary table of \code{x} and \code{y}.
#' @examples
#' exp_ctable(esoph, esoph$agegp, esoph$alcgp)
#' @export

exp_ctable <- function(data,x,y){
  if (!is.null(levels(x)) & !is.null(levels(y))) {
  # Generate cell count
  mytable <- table(x,y)

  ## Generate cell proportion of row
  CPR <- format(prop.table(mytable, 1),digits=1)

  ## Generate cell proportion of col
  CPC <- format(prop.table(mytable, 2),digits=1)

  ## Generate cell proportion of total
  CPT <- format(prop.table(mytable),digits=1)

  ## Number of unique levels of x
  nx <-as.numeric(length(unique(x)))
  ny <- as.numeric(length(unique(y)))
  ## Generate a matrix with NAs with needed nrow*ncol
  result <- matrix(nrow=nx,ncol=ny)

  ## Add values in each cell

  for(i in 1:nx) {
    for(j in 1:ny) {
      result[i,j] <- paste0(mytable[i,j],"(",CPR[i,j],",",CPC[i,j],",",CPT[i,j],")")
    }
  }

  result <- as.data.frame(result)
  ## Change colnames, change rownames
  for (i in 1:ncol(mytable)) {
    colnames(result)[i] <- colnames(mytable)[i]
  }

  for (i in 1:nrow(mytable)) {
    rownames(result)[i] <- rownames(mytable)[i]
  }

  ## Add footnote explaining the cell values mean cellcount(rowperc,colperc,totalperc)
  kablet <- knitr::kable(result)
  add_footnote(kablet,c("Cell count(row proportion,column proportion,overall proportion)"))
  }
  else {
    warning("both variables should be factors")
  }
}
