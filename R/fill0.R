#' Fill in Missing Rows with Zeroes
#'
#' Add rows to a data frame for all combinations of the recorded effort
#' variables and the recorded group variables.
#' @param df
#'   A data frame containing the effort, group, and measurement variables.
#'   See details.
#' @param effvars
#'   A character vector with the names of the effort variables, typically
#'   identifying the design space and/or time.
#' @param grpvars
#'   A character vector with the names of the group variables, typically
#'   identifying the categories of things measured.
#' @param measvars
#'   A character vector with the names of the measurement variables.
#' @param fillwith
#'   A scalar, assigned to the measurement variables for the added rows.
#' @return
#'   A data frame containing the same variables as \code{df} with additional
#'   rows for any combinations of effort and group excluded from \code{df}.
#'   See details.
#' @details
#'   The need for this function arises when measurements are recorded
#'   only for the subset of possible grouping observed.  For example, when
#'   surveying trees, measurements for oak and maple may be recorded for a given
#'   plot, while the "missing" measurements for many other species of interest
#'   are not recorded.
#'
#'   Additional variables may also be included in \code{df}.  These will be
#'   carried through to the returned data frame, and will have missing values
#'   for any rows added by the function.
#'
#'   Note that only the observed combinations of the effort variables
#'   and only the observed combinations of the group variables are used to
#'   determine missing effort-group combinations.  In other words, if only
#'   large oaks and small hickories are observed, then no rows are added for
#'   small oaks or large hickories.  See example.
#' @export
#' @examples
#' mydat <- data.frame(
#'   year = c(1990, 1991, 1991, 1992, 1992),
#'   plot = c(1, 1, 2, 1, 2),
#'   tree = c("oak", "oak", "maple", "maple", "hickory"),
#'   size = c("large", "large", "small", "large", "small"),
#'   number = c(3, 4, 5, 8, 4),
#'   condition = c(6, 8, 5, 16, 4))
#' fill0(df=mydat, effvars=c("year", "plot"), grpvars=c("tree", "size"),
#'   measvars="number")

fill0 <- function(df, effvars, grpvars, measvars, fillwith=0) {
  # data frame with unique effort data
  eff.df <- df[, effvars, drop=FALSE]
  eff.df$ef <- interaction(eff.df, drop=TRUE)
  eff.df <- eff.df[match(levels(eff.df$ef), eff.df$ef), ]
  # data frame with unique group data
  grp.df <- df[, grpvars, drop=FALSE]
  grp.df$gf <- interaction(grp.df, drop=TRUE)
  grp.df <- grp.df[match(levels(grp.df$gf), grp.df$gf), ]
  # put it all together
  full <- expand.grid(ef=eff.df$ef, gf=grp.df$gf)
  full2 <- merge(full, eff.df, all.x=TRUE)
  full3 <- merge(full2, grp.df, all.x=TRUE)
  full4 <- merge(full3, df[, c(effvars, grpvars, measvars)], all=TRUE)
  full4 <- full4[, names(full4)[names(full4) %in% names(df)]]
  x <- full4[, measvars]
  x[is.na(x)] <- fillwith
  full4[, measvars] <- x
  full4
}
