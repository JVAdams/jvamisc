#' Primary Key
#'
#' Look for "primary keys" of a data frame.  The primary key is a variable
#' (or combination of variables) that uniquely identify all rows.
#' @param df
#'   A data frame in which to find the primary key.
#' @param varz
#'   A numeric or character vector representing the candidate columns or
#'   variable names to consider for membership in the primary key, default
#'   is to include all variables, \code{1:dim(df)[2]}.
#' @return
#'   A list with two elements, \code{npk} an integer giving the number
#'   of variables that make up the primary key, and \code{k} a list of character
#'   vectors) identifying the variable names that make up the primary key.
#' @export
#' @examples
#' mydat1 <- data.frame(
#'  group=c("a", "a", "a", "b", "b", "c", "c", "d", "d"),
#'  id=1:9,
#'  value=rnorm(9))
#' mydat2 <- data.frame(
#'  group=c("a", "a", "a", "b", "b", "c", "c", "d", "d"),
#'  id=c(1:3, 1:2, 1:2, 1:2),
#'  value=rnorm(9))
#' mydat3 <- data.frame(
#'  group=c("a", "a", "a", "b", "b", "c", "c", "d", "d"),
#'  name=c("ax", "ax", "ax", "be", "be", "cat", "cat", "dog", "dog"),
#'  id=c(1:3, 1:2, 1:2, 1:2),
#'  value=rnorm(9))
#' # primary key made up of one variable
#' primKey(mydat1[, c("group", "id")])
#' # primary key made up of two variables
#' primKey(mydat2[, c("group", "id")])
#' # two primary keys, each made up of two variables
#' primKey(mydat3[, c("group", "name", "id")])

primKey <- function(df, varz=1:dim(df)[2]) {
  df2 <- df[, varz]
  if(anyDuplicated(df2)>0) {
    print(df2[duplicated(df2) | duplicated(df2, fromLast=TRUE), ])
    stop("There are duplicate rows in the data frame, no primary key exists.")
  }
  target <- dim(df2)[1]
  fields <- names(df2)
  nfields <- length(fields)
  # 1 primary key
  k1 <- apply(df2, 2, function(x) length(unique(x)))
  out <- list(npk="all", k=nfields)
  if(sum(k1==target)>0) {
    out$npk <- 1
    out$k <- names(k1)[k1==target]
  } else {
    for(i in 2:nfields) {
      aci <- allcombs(nfields, i, i)
      ki <- apply(aci, 1, function(v)
        length(unique(do.call(paste, df2[, v==1]))))
      if(sum(ki==target)>0) {
        out$npk <- i
        out$k <- lapply(as.data.frame(t(aci[ki==target, , drop=FALSE]==1)), function(x) fields[x])
        break()
      }
    }
  }
  return(out)
}
