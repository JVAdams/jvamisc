#' Tweet Headlines
#'
#' Tweet the latest headlines from the specified website.
#' \pkg{rtweet} and \pkg{RCurl} packages required.
#' @param posttweet
#'   A logical scalar indicating if tweets should be posted, default TRUE.
#' @param username
#'   A character scalar, giving the name of the twitter user. The default, NULL,
#'   uses information stored in local .Renviron file.
#' @param website
#'   A character scalar, giving the name of the website, from which to pull
#'   headlines. The default, NULL, uses information stored in local .Renviron
#'   file.
#' @return
#'   A list of length 3.  The first element is a data frame of old tweets
#'   with text, favoriteCount, retweetCount, and createdUTC (date/time) as
#'   columns.  The second and third elements are character strings of the
#'   current headlines (currentheads) and the latests items to tweet (totweet).
#' @details
#'   This function is customized to work on a particular website.
#'   It's not for general use.
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

tweethead <- function(posttweet=TRUE, username=NULL, website=NULL, save=FALSE) {
  if (!requireNamespace("rtweet", quietly=TRUE)) {
    stop("rtweet must be installed.", call.=FALSE)
  }
  if (!requireNamespace("RCurl", quietly=TRUE)) {
    stop("RCurl must be installed.", call.=FALSE)
  }
  if(is.null(username)) username <- Sys.getenv("username")
  if(is.null(website)) website <- Sys.getenv("website")

  # grab headlines from website
  # read in html source code
  base.html <- RCurl::getURLContent(website)[[1]]

  # pull off links that say "More"
  links <- strsplit(base.html, "ID=")[[1]]
  links2 <- sapply(strsplit(links, "</a>"), "[", 1)[-1]
  more.codes <- substring(jvamisc::stringin("more", links2), 1, 5)
  more.urls <- paste0(website, "index.php?ID=", more.codes)

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
      photo.url <- paste0(website, "Photos/", photo)
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
  df <- as.data.frame(do.call(rbind, lapply(more.urls, pull)),
    stringsAsFactors=FALSE)
  df$currentheads <- gsub("&#39;", "'",
    paste(df$headline, df$article.url, sep=". "))

  ### grab latest tweets
  adj <- rtweet::get_timeline(username, n=15, include_rts=FALSE)
  oldtweets <- adj[1:15,
    c("text", "favorite_count", "retweet_count", "created_at")]
  names(oldtweets)[names(oldtweets)=="created_at"] <- "createdUTC"
  oldtweets$text <- gsub("&#39;", "'", oldtweets$text)

  ### tweet all new tweets that haven't been tweeted before
  totweeti <- rev((1:dim(df)[1])[!(substring(df$currentheads, 1, 30) %in%
      substring(oldtweets$text, 1, 30))])

  if (length(totweeti) > 0) {
    if (posttweet) {
      for(i in totweeti) {
        if(df$photo.url[i]=="") {
          rtweet::post_tweet(status=df$currenthead[i])
        } else {
          download.file(df$photo.url[i], "Image.jpg", mode="wb")
          rtweet::post_tweet(status=df$currenthead[i], media="Image.jpg")
        }
      }
    } else {
      cat(paste("\n\n***  This is what would be posted if posttweet=TRUE.\n\n"))
      print(df$currenthead[totweeti])
      cat("\n\n")
    }
  } else {
    cat(paste0("\n\n***  No new headlines since last tweet, ",
      format(max(oldtweets$createdUTC), "%a %b %e %I:%M %p", tz=Sys.timezone()),
      ".\n\n\n"))
  }

  if(save) {
    return(list(oldtweets=oldtweets, currentheads=df$currentheads,
    totweet=df$currenthead[totweeti]))
  } else {
    invisible()
  }
}
