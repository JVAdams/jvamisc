#' Tweet Headlines
#'
#' Tweet the latest headlines from the specified website.
#' \pkg{twitteR} and \pkg{RCurl} packages required.
#' @param posttweet
#'   A logical scalar indicating if tweets should be posted, default TRUE.
#' @param username
#'   A character scalar, giving the name of the twitter user. The default, NULL,
#'   uses information stored in local .Renviron file (see Details).
#' @param website
#'   A character scalar, giving the name of the website, from which to pull
#'   headlines. The default, NULL, uses information stored in local .Renviron
#'   file (see Details).
#' @param credOrPath
#'   Either a character scalar giving the path of the environment where the
#'   credentials are stored or a character vector of length four, giving the
#'   credentials themselves: twitter_api_key, twitter_api_secret,
#'   twitter_access_token, and twitter_access_token_secret.  The default is
#'   "C:/JVA/R/Working Directory/.Renviron" (see Details).
#' @return
#'   A list of length 3.  The first element is a data frame of old tweets
#'   with text, favoriteCount, retweetCount, and createdUTC (date/time) as
#'   columns.  The second and third elements are character strings of the
#'   current headlines (currentheads) and the latests items to tweet (totweet).
#' @details
#'   This function is customized to work on a particular website.
#'   It's not for general use. To store information in local .Renviron file,
#'   use \code{writeLines(c("username=xxx", "website=xxx",
#'   "twitter_api_key=xxx", "twitter_api_secret=xxx",
#'   "twitter_access_token=xxx", "twitter_access_token_secret=xxx"),
#'   file.path(getwd(), ".Renviron"))}.
#' @export
#' @references
#'
#' Simon Munzert.  19 Jan 2015.
#' Programming a Twitter bot - and the rescue from procrastination.
#' \emph{http://www.r-datacollection.com/blog/Programming-a-Twitter-bot/}
#'
#' Simon Munzert.  21 Dec 2014.
#' How to conduct a tombola with R.
#' \emph{www.r-datacollection.com/blog/How-to-conduct-a-tombola-with-R/}
#'
#' @examples
#' \dontrun{
#' tweethead(FALSE)
#' tweethead()
#' }

tweethead <- function(posttweet=TRUE, username=NULL, website=NULL,
  credOrPath="C:/JVA/R/Working Directory/.Renviron") {
  if (!requireNamespace("twitteR", quietly=TRUE)) {
    stop("twitteR must be installed.", call.=FALSE)
  }
  if (!requireNamespace("RCurl", quietly=TRUE)) {
    stop("RCurl must be installed.", call.=FALSE)
  }
  if (length(credOrPath)==1) {
    readRenviron(credOrPath)
    api_key <- Sys.getenv("twitter_api_key")
    api_secret <- Sys.getenv("twitter_api_secret")
    access_token <- Sys.getenv("twitter_access_token")
    access_token_secret <- Sys.getenv("twitter_access_token_secret")
  } else {
    api_key <- credOrPath[1]
    api_secret <- credOrPath[2]
    access_token <- credOrPath[3]
    access_token_secret <- credOrPath[4]
  }

  # connect to Twitter
  origop <- options("httr_oauth_cache")
  options(httr_oauth_cache=TRUE)
  twitteR::setup_twitter_oauth(api_key, api_secret, access_token,
    access_token_secret)
  options(httr_oauth_cache=origop)

  # grab headlines from website
  # read in html source code
  base.url <- Sys.getenv("website")
  base.html <- RCurl::getURLContent(base.url)[[1]]

  # pull off links that say "More"
  links <- strsplit(base.html, "ID=")[[1]]
  links2 <- sapply(strsplit(links, "</a>"), "[", 1)[-1]
  more.codes <- substring(stringin("more", links2), 1, 5)
  more.urls <- paste0(base.url, "index.php?ID=", more.codes)

  # pull off headline, photo url, photo caption
  pull <- function(thisurl) {
    thishtml <- RCurl::getURLContent(thisurl)[[1]]
    # headline
    headlong <- strsplit(thishtml, "<font size=6><b>")[[1]][2]
    head <- strsplit(headlong, "</b>")[[1]][1]
    # photo url
    photolong <- strsplit(thishtml, "<img src='./Photos/")[[1]]
    if (length(photolong)>1) {
      photolong <- photolong[2]
      photo <- strsplit(photolong, "'><br>")[[1]][1]
      photo.url <- paste0(base.url, "Photos/", photo)
    } else {
      photo.url <- ""
    }
    # photo caption
    caplong <- strsplit(thishtml, "<br><font size=2><b>")[[1]]
    if (length(caplong)>1) {
      caplong <- caplong[2]
      cap <- strsplit(caplong, "</b>")[[1]][1]
    } else {
      cap <- ""
    }
    c(article.url=thisurl, headline=head, photo.url=photo.url,
      photo.caption=cap)
  }

  # get new tweets ready
  m <- do.call(rbind, lapply(more.urls, pull))
  currentheads <- apply(m[, 2:1], 1, paste, collapse=". ")
  currentheads <- gsub("&#39;", "'", currentheads)

  ### grab latest tweets
  adj <- twitteR::getUser(Sys.getenv("username"))
  oldtweets <- twitteR::twListToDF(
    twitteR::userTimeline(adj, n=15, excludeReplies=TRUE))[,
    c("text", "favoriteCount", "retweetCount", "created")]
  names(oldtweets)[names(oldtweets)=="created"] <- "createdUTC"
  oldtweets$text <- gsub("&#39;", "'", oldtweets$text)

  ### tweet all new tweets that haven't been tweeted before
  totweet <- currentheads[!(substring(currentheads, 1, 30) %in%
      substring(oldtweets$text, 1, 30))]

  if (length(totweet) > 0) {
    if (posttweet) {
      lapply(rev(totweet), updateStatus, lat=45.141473, long=-89.152339)
    } else {
      cat(paste("\n\n***  This is what would be posted if posttweet=TRUE.\n\n"))
      print(totweet)
      cat("\n\n")
    }
  } else {
    cat(paste0("\n\n***  No new headlines since last tweet, ",
      format(max(oldtweets$createdUTC), "%a %b %e %I:%M %p", tz=Sys.timezone()),
      ".\n\n\n"))
  }

  list(oldtweets=oldtweets, currentheads=currentheads, totweet=totweet)
}
