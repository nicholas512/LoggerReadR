#' @title regexcapturedmatches
#' @noRd
#' @details https://gist.github.com/MrFlick/10413321
#' regcapturedmatches.R: extracts captured matches from match data obtained by regexpr, gregexpr or regexec
regcapturedmatches<-function(x,m) {

  if (length(x) != length(m))
    stop(gettextf("%s and %s must have the same length",
                  sQuote("x"), sQuote("m")), domain = NA)

  ili <- is.list(m)
  useBytes <- if (ili) {
    any(unlist(lapply(m, attr, "useBytes")))
  } else {
    any(attr(m, "useBytes"))
  }
  if (useBytes) {
    asc <- iconv(x, "latin1", "ASCII")
    ind <- is.na(asc) | (asc != x)
    if (any(ind))
      Encoding(x[ind]) <- "bytes"
  }
  if (ili) {
    if (any(sapply(m, function(x) {is.null(attr(x,"capture.start"))})==T)) {
      stop("No capture data found (did you use perl=T?)")
    }
    starts<-lapply(m, function(x) {attr(x, "capture.start")})
    lengths<-lapply(m, function(x) {attr(x, "capture.length")})
  } else {
    if (is.null(attr(m,"capture.start"))) {
      stop("No capture data found (did you use perl=T?)")
    }
    x<-list(x)
    starts<-list(attr(m, "capture.start"))
    lengths<-list(attr(m, "capture.length"))
  }

  cleannames <- function(x) {
    if (!is.null(colnames(x))) {
      colnames(x) <- make.unique(make.names(colnames(x)))
    }
    x
  }
  starts <- lapply(starts, cleannames)
  lengths <- lapply(lengths, cleannames)


  Substring<-function(x,starts,lens) {
    if(all(starts<0)) {
      return(character())
    } else {
      x <- t(
        mapply(function(x,st,ln) substring(x,st,st+ln-1),
               x, data.frame(t(starts)), data.frame(t(lens)),
               USE.NAMES=F)
      )
      if (!is.null(colnames(starts))) {
        colnames(x)<-colnames(starts)
      }
      x
    }
  }

  y<-Map(
    function(x, sos, mls) {
      Substring(x,sos,mls)
    },
    x,
    starts,
    lengths,
    USE.NAMES = FALSE
  )
  if (ili) {
    y
  } else {
    y[[1]]
  }
}

#' Helper function to get mode
#' https://stackoverflow.com/a/8189441
#' @noRd
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
