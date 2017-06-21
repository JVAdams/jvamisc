#' Assign Reviewers to Papers
#'
#' Given a data frame of papers, in which each paper is associated with a
#' Unit, assign a reviewing Unit to each paper such that each reviewing unit
#' has approximately the same number of papers to review and no reviewing unit
#' is assigned one of its own papers to review.
#' @param data
#'   Data frame of papers to be reviewed, each row represents a single paper.
#' @param Unitvar
#'   Character scalar, name of variable in \code{data} identifying the Unit with
#'   which each paper is associated.
#' @param prefOrd
#'   Character vector, preferred ordering of Units, default geographical,
#'   \code{c("LSBS", "HBBS", "LMERS", "Coastal", "LMichigan", "LHuron",
#'   "LEBS", "LOBS", "TLAS")}.
#' @return
#'   A data frame, the same as \code{data} with one new column, \code{Reviewer},
#'   and sorted by Reviewer.
#' @details
#'   If a vector of length 1 is provided for \code{round2}, \code{ndec},
#'   \code{comma}, or \code{align}, the same rounding, decimals, commas, or
#'   alignment is applied to all specified columns.
#' @seealso
#'   \code{\link{format}}.
#' @export
#' @import plyr
#' @examples
#' test <- data.frame(Source=rep(paste("Group", 1:5), c(2, 3, 1, 6, 4)),
#'  Paper=LETTERS[1:16])
#' test2 <- assignReviewer(data=test, Unitvar="Source",
#'  prefOrd=paste("Group", 1:5))
#' test
#' test2
#' with(test2, addmargins(table(Reviewer, Source)))

assignReviewer <- function(data, Unitvar,
    prefOrd=c("LSBS", "HBBS", "LMERS", "Coastal", "LMichigan", "LHuron",
    "LEBS", "LOBS", "TLAS")) {

  orig.names <- names(data)
  data$orig.ord. <- 1:dim(data)[1]
  data$Unit. <- data[, Unitvar]

  nU <- length(unique(data$Unit.))
  # cycle through columns with biggest producers first
  npapers <- table(data$Unit.)[order(-table(data$Unit.))]
  m <- matrix(rep(npapers %/% (nU - 1), rep(nU-1, nU)), ncol=nU)
  extra <- npapers %% (nU - 1)
  m <- m + sapply(extra, function(x) rep(1:0, c(x, nU-1-x)))

  # fill up matrix with "even" dist'n of numbers
  # calculate matrix starting point at lowest row total so far
  # and fill with biggest numbers first in each column
  M <- matrix(0, nrow=nU, ncol=nU)
  for(i in 1:nU) {
    # load up the next column's zero first
    ord <- order((1:8)!=(i+1), apply(M, 1, sum))
    ord <- ord[ord!=i]
    M[ord, i] <- m[, i]
  }

  paperOrd <- names(npapers)

  df <- data %>%
    mutate(
      Unit. = factor(Unit., levels=paperOrd),
      RandPaper = sample(dim(data)[1])
    ) %>%
    arrange(Unit., RandPaper) %>%
    mutate(
      Reviewer = factor(paperOrd[rep(as.vector(row(M)), as.vector(M))],
        levels=prefOrd),
      Unit. = factor(Unit., levels=prefOrd)
    ) %>%
    arrange(Reviewer, orig.ord.)

  df[, Unitvar] <- df$Unit.
  df[, c("Reviewer", orig.names)]
}
