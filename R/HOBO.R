# Configuration ===========================================================


HOBO_MAXLINES <- 400
HOBO_MAX_HEADER_LINES <- 40  # How many possible lines before data (a guess).
HOBO_DATA_HEADERS <- c(  # Taken from HOBOware help manual. Not Complete.
  "x accel", "y accel", "z accel",
  "watt-hours", "kilowatt-hours",
  "watts",
  "rh", "temp", "wind speed", "wind dir", "soil moisture", "amps", "volts"
)
HOBO_TZ_REGEX = "GMT\\s?[-+]\\d\\d:\\d\\d"

HOBO_DETAILS_KEYWORDS <- c("First Sample Time", "Battery at Launch", "Device Info", "Deployment Info")
HOBO_DETAILS_HEADERS <- c("Series", "Event Type")
HOBO_DETAILS_SUBHEADERS <- c("Devices", "Device Info", "Deployment Info", "Series Statistics")
HOBO_DATE_FORMATS <- c("MDY", "YMD", "DMY")
HOBO_POS_N_FMT <- c(1,2,3,4)
HOBO_NEG_N_FMT <- c(1,2,3)


# Public functions ======================================================


#' @title Read Onset HOBOWare file
#'
#' @author Nick Brown
#'
#' @description Read Onset HOBOWare file no matter how it is formatted
#'
#' @param filename character, path to the file
#'
#' @param configuration A named list of file configuration properties according to \code{\link{detect_hoboware_configuration}}.
#' If missing, LoggerReadR will attempt to auto-detect configuration properties.
#'
#' @param simplify logical, whether LoggerReadR should simplify the returned dataframe (see details)
#'
#' @param encoding The encoding of the textfile. Defaults to "UTF-8"
#'
#' @return dataframe, a dataframe
#'
#' @details
#'
#' @export
read_hoboware <- function(filename, configuration, simplify=TRUE, encoding = "UTF-8"){

  if (missing(configuration)){
    warning("No configuration properties were provided. LoggerReadR autodetected the configuration.")
    configuration <- detect_hoboware_configuration(filename)
  }

  lines <- readLines(filename, encoding=encoding)
  headerline_i <- which(hbw_is_header_line(lines, configuration$separate_date_time,
                                       configuration$no_quotes_or_commas,
                                       configuration$separator))
  headerline <- lines[headerline_i]

  df <- utils::read.table(textConnection(lines), sep=configuration$separator,
                    header=F, skip=headerline_i, fill=T, stringsAsFactors = F)

  if (configuration$include_plot_details){
    df <- df[,-ncol(df)]
  }

  colnames(df) <- colnames(utils::read.table(textConnection(removeBOM(headerline)),
                                    sep=configuration$separator,
                                    header=T, stringsAsFactors = F, comment.char=""))

  df[, hbw_is_data_header(colnames(df))] <- lapply(df[, hbw_is_data_header(colnames(df))],
                                                   function(x) hbw_convert_number_format(x, configuration$positive_number_format))


  if (!simplify){
    return(df)

  }else{
    details <- hbw_extract_hoboware_details(lines)
    tz <- NULL
    tz <- hbw_detect_time_zone_from_header_line(headerline)

    if (is.null(tz)
        & configuration$include_plot_details
        & configuration$no_quotes_or_commas
        & length(details) > 1){
      tz <- hbw_detect_time_zone_from_details(details)
    }

    data_columns <- df[, hbw_is_data_header(colnames(df))]
    data_columns$TIME <- create_datetime_column(df, tz, configuration)


    output <- data_columns[, c(ncol(data_columns), 1:(ncol(data_columns) - 1))]  # Reorder
    return(output)
  }

}


#' @title Detect HOBOWare file properties
#'
#' @description Auto-detect the various HOBOWare text file configuration options that were set for a file
#'
#' @param filename character, path to the file
#'
#' @return A named list of HOBOWare configuration properties (see details for names)
#'
#' @details The name keywords to configure a HOBOWare file are:
#'
#' \itemize{
#' \item \code{include_line_number}: whether the line number is included as a column. Chosen from: {\code{TRUE}, \code{FALSE}}
#' \item \code{include_plot_title_in_header}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' \item \code{always_show_fractional_seconds}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' \item \code{separate_date_time}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' \item \code{no_quotes_or_commas}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' \item \code{include_logger_serial}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' \item \code{include_sensor_serial}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' \item \code{date_format}. Chosen from: \code{"MDY", "YMD", "DMY"}
#' \item \code{date_separator}. Chosen from: `-`, `\`
#' \item \code{time_format_24hr}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' \item \code{positive_number_format}. Chosen from: \code{1,2,3,4} (corresponding to \code{"1,234.56", "1 234,56", "1.234,56", "1.234 56"})
#' \item \code{negative_number_format}. Chosen from: \code{1,2,3} (corresponding to \code{"-123", "123-", , "(123)"})
#' \item \code{include_plot_details}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' }
#'
#' @export
detect_hoboware_configuration <- function(filename, encoding="UTF-8"){
  lines <- readLines(filename, encoding=encoding, n=HOBO_MAXLINES)
  config <- hobo.config.empty()

  number_info <- hbw_parse_number_format(lines)
  date_info <- hbw_parse_time_format(lines)

  config$separator <- number_info$sep
  config$include_line_number <- !(date_info$lino == "")
  config$include_plot_title_in_header <- NULL
  config$always_show_fractional_seconds  <- !(date_info$deci == "")
  config$separate_date_time  <- date_info$dtdelim != " "
  config$no_quotes_or_commas  <- hbw_detect_no_quotes_commas(lines)
  config$include_logger_serial  <- NULL
  config$include_sensor_serial  <- NULL
  config$date_format <- hbw_detect_date_format(lines)
  config$date_separator <- date_info$dsep
  config$time_format_24hr <- !(toupper(date_info$ampm) %in% c("AM", "PM"))
  config$positive_number_format  <- hbw_evaluate_positive_number_format(number_info$thou, number_info$decimal)
  config$negative_number_format   <- hbw_evaluate_negative_number_format(number_info$neg1, number_info$neg2)
  config$include_plot_details   <- hbw_detect_include_plot_details(lines)

  config
}

#' @title Read hoboware details
#' @description Read details from a Hoboware export file if they were included
#' @param filename character, path to the file
#' @param configuration A named list of file configuration properties according to \code{\link{detect_hoboware_configuration}}.
#' If missing, LoggerReadR will attempt to auto-detect configuration properties.
#' @param encoding The encoding of the textfile. Defaults to "UTF-8"
#' @export
read_hoboware_details <- function(filename, configuration, encoding = "UTF-8"){
  if (missing(configuration)){
    warning("No configuration properties provided. Attempting to detect.")
    configuration <- detect_hoboware_configuration(filename)
  }

  lines <- readLines(filename, encoding=encoding, n=500)

  hbw_extract_hoboware_details(lines)
}

# Hidden functions =================================================================


hbw_extract_hoboware_details <- function(lines){
  matches <- regexpr("(?P<key>\\b[A-Za-z ]+\\b):(?P<value>.*)$", lines, perl=TRUE)
  match.table <- regcapturedmatches(lines, matches)
  match.table[match.table == ""] <- NA


  details <- data.frame(stats::na.omit(match.table), stringsAsFactors = F)


  if (nrow(details) > 2){  # If it only gets the header row
    details[,2] <- trimws(details[,2])
    return(details)
  }else{
    return(NA)
  }
}


#' @description Given a list of lines from a hoboware file, detect
#'  certain properties about the textfile configuration
#' @noRd
hbw_parse_number_format <- function(lines){

  pattern <- paste0(
    "(?P<sep>[\\t,;])",       # Column separator
    "(",                      # Group for one data column
      "(?P<neg1>[-\\(])?",      # Possible opening negative sign {'-', '(')}
      "(\\d{1,3}",              # millions, billions or more, etc.
        "(?P<thou>",
          "[ ,\\.]",                # Separated by a thousands delimiter
        ")",
      ")?",                     # Zero or one times
      "(\\d{3}(?P=thou))*",     # 'Sandwiched' digit triplets using same thousands separator
      "\\d{1,3}",               # Hundreds, tens, ones
      "(?P<decimal>",
        "[\\., ]",                # Possibly separated by a decimal delimiter
      ")",
      "\\d+",                   # Decimal digits (TODO: these might actually not be present)
      "(?P<neg2>[-\\)])?",      # Possible terminating negative sign {')'}
      "(?P=sep)",               # The same column separator
    ")+"                      # Repeated for each data column
  )

  matches <- regexpr(pattern, lines, perl=TRUE)
  match.table <- regcapturedmatches(lines, matches) # LoggerReadR::

  as.list(apply(match.table, 2, Mode)) # LoggerReadR::
}

#' @title remove byte order mark (BOM) from dataframe headers
#' @param  headers character vector of dataframe headers
#'
#' @return character vector of dataframe headers with BOM (<U+FEFF>) removed
#' @noRd
removeBOM <- function(headers){
  gsub("\\xef\\xbb\\xbf", "", headers, useBytes = T)
}


#' @title create datetime column from a dataframe
#' @param df dataframe with datetime column or date & time columns from HOBOWare export
#' @param tz string of GMT offset e.g. "-0400"
#'
#' @noRd
create_datetime_column <- function(df, tz, configuration){  # TODO: make input date_col and time_col instead of df // values instead of full config
  tzfmt <- ifelse(missing(tz), "", "%z")
  tz <- ifelse(missing(tz), "", tz)

  if (missing(tz)){
    warning("Created datetime column with no timezone. Assumed UTC.")
  }

  if (configuration$separate_date_time){

    date_column <- df[, grepl("Date", colnames(df))]
    time_column <- df[, grepl("Time", colnames(df))]

    full_date <- paste(date_column, time_column, tz)

  }else{
    datetime_column <- df[, grepl("Date.Time", colnames(df))]
    full_date <- paste(datetime_column, tz)

  }

  date_fmt <- paste(hbw_date_fmt(configuration$date_format,
                                  configuration$date_separator),
                     hbw_time_fmt(configuration$time_format_24hr,
                                  configuration$always_show_fractional_seconds),
                     tzfmt)

  full_date <- gsub("  ", " ", full_date)  # strip double-spaces

  date <- as.POSIXct(full_date, format=date_fmt)

  date
}


#' @title HoboWare time string format
#' @details Return the appropriate time format string
#' @noRd
hbw_time_fmt <- function(time_format_24hr, always_show_fractional_seconds){

  if (time_format_24hr){
    fmt = "%H:%M:%S"
  }else{
    fmt =  "%I:%M:%S %p"
  }

  if (always_show_fractional_seconds){
    fmt = gsub("S", "OS", fmt)
  }
  fmt
}

hbw_date_fmt <- function(date_format, date_separator){
  #if (! date_format %in% HOBO_DATE_FORMATS){
  #  stop(paste0("Incorrect date pattern. Choose from", HOBO_DATE_FORMATS))
 # }
  switch(date_format,
         "YMD" = gsub("-", date_separator, "%y-%m-%d"),
         "MDY" = gsub("-", date_separator, "%m-%d-%y"),
         "DMY" = gsub("-", date_separator, "%d-%m-%y"),
         stop(paste0("Incorrect date pattern. Choose from: ",
                     paste0(HOBO_DATE_FORMATS, collapse=", "))))
}

hbw_parse_time_format <- function(lines){
  pattern = paste0("^",     #  Start of line
                   "(\\d*(?P<lino>[\\t,;]))?",  # maybe a line number with separator
                   "(?P<D1>",  # First date component
                     "\\d{2,4}",  # digits
                   ")",
                   "(?P<dsep>[/-])",  # Date Separator
                   "(?P<D2>",         # Second date component
                     "\\d{2,4}",      # digits
                   ")",
                   "(?P=dsep)",  # Date Separator (as before)
                   "(?P<D3>",  # Third date component
                     "\\d{2,4}",  # digits
                   ")",
                   "(?P<dtdelim>([\\t,;]|[ ]))",  # Possibly a date-time separator or a space
                   "\\d{2}:\\d{2}:\\d{2}",  # H:M:S
                   "(?P<deci>[\\.,])?",   # Possibly decimal seconds
                     "\\d*",  # The decimal seconds themselves
                   "\\s*",     # some whitespace
                   "(?P<ampm>",
                     "(AM|PM)",  # possibly an AM or PM designator
                   ")?",
                   "[\\t,;]"  # Final separator
                   )
  matches <- regexpr(pattern, lines, perl=TRUE)
  match.table <- regcapturedmatches(lines, matches)
  as.list(apply(match.table, 2, Mode))
}


#' @title Detect whether date and time are separate columns
#' @noRd
hbw_detect_separate_date_time <- function(lines){
  separate <- "Date[^ ].*Time"
  combined <- "Date Time"

  separate_matches <- sum(grepl(separate, lines))
  combined_matches <- sum(grepl(combined, lines))

  if (separate_matches + combined_matches > 1){
    stop("Duplicate Date or Time headers")

  }else if (separate_matches == 1){
    TRUE  # True, they are separate

  }else if (combined_matches == 1){
      FALSE  # False, they are not separate

  }else{
        stop("Could not find Date, Time headers")
  }}


#' @title Detect the value of the parameter "include plot details"
#' @noRd
hbw_detect_include_plot_details <- function(lines){
  options <- paste0(HOBO_DETAILS_KEYWORDS, collapse="|")
  pattern <- paste0("(", options, ")")
  if (sum(grepl(pattern, lines)) >= 1){
    TRUE
  }else{
    FALSE
  }
}


hbw_decimal_separator <- function(positive_number_format){
  switch(as.character(positive_number_format),
         "1"=".",
         "2"=",",
         "3"=",",
         "4"=" ", NA)
}


hbw_thousands_separator <- function(positive_number_format){
  switch(as.character(positive_number_format),
         "1"=",",
         "2"=" ",
         "3"=".",
         "4"=".", NA)
}


#' @title  convert nonstandard number formats
#' @param df vector of nonstandard numeric data
#' @param positive_number_format integer according to \code{\link{hbw_evaluate_positive_number_format}
#' @param negative_number_format integer \code{\link{hbw_evaluate_negative_number_format}
#' @return vector of converted (numeric) data
#' @noRd
hbw_convert_number_format <- function(data, positive_number_format, negative_number_format){
  converted <- gsub(hbw_thousands_separator(positive_number_format), "", data, fixed=T)
  converted <- gsub(hbw_decimal_separator(positive_number_format), ".", converted, fixed=T)
  converted <- gsub("^\\(([0-9\\.]*)\\)$", "-\\1", converted)
  converted <- gsub("^([0-9\\.]*)-$", "-\\1", converted)

  as.numeric(converted)
}


#' @title Evaluate the positive number format
#' @details Evaluate the positive number format based on detected separators.
#' Positive number formats are as follows:
#'   |1| 1,234.56 | comma, period |
#'   |2| 1 234,56 | space, comma  |
#'   |3| 1.234,56 | period, comma |
#'   |4| 1.234 56 | period, space |
#' @noRd
hbw_evaluate_positive_number_format <- function(thou_sep, deci_sep){

  if ((thou_sep == ",") & (deci_sep == ".")){
     return(1)
  }else if ((thou_sep == " ") & (deci_sep == ",")){
    return(2)
  }else if ((thou_sep == ".") & (deci_sep == ",")){
    return(3)
  }else if ((thou_sep == " ") & (deci_sep == ".")){
    return(4)
  }else if (thou_sep == ""){
    if (deci_sep == "."){
      return(1)
    }else if (deci_sep == " "){
      return(4)
    }
  }else if (deci_sep == ""){
    if (thou_sep == ","){
      return(1)
    }else if (thou_sep == " "){
      return(2)
    }}

  return(NA)
}

#' @title Evaluate negative number format
#' @details Return negative number format
#'         |1| -123 | -, None |
#'         |2| 123- | None, -  |
#'         |3| (123) | (, ) |
#' @noRd
hbw_evaluate_negative_number_format <- function(negative_open, negative_terminator){
  if ((negative_open == "-") & (negative_terminator == "")){
    return(1)
  }else if ((negative_open == "") & (negative_terminator == "-")){
    return(2)
  }else if ((negative_open == "(") & (negative_terminator == ")")){
    return(3)
  }else{
    return(NA)
  }}

#' @title Detect whether dates are MDY, YMD, or DMY.
#' @details This is based on heuristics and the assumption of evenly distributed sampling at
#' a frequency greater than monthly (by looking at which set of numbers is more 'diverse' and
#' assuming that corresponds to days rather than months)
#' @noRd
hbw_detect_date_format <- function(lines){
  matches <- regexpr("(?P<p1>\\d{2}).(?P<p2>\\d{2}).(?P<p3>\\d{2}).\\d{2}:\\d{2}:\\d{2}", lines, perl=TRUE)
  match.table <- regcapturedmatches(lines, matches)
  match.table[match.table==""] <- NA
  match.table <- data.frame(stats::na.omit(match.table), stringsAsFactors = F)

  if (max(match.table$p2) > 12){
    fmt = "MDY"
  }else{
    if (length(unique(match.table$p1)) > length(unique(match.table$p3))){  # Which is more 'diverse'
      fmt = "DMY"
    }else{
      fmt = "YMD"
    }
  }
  return(fmt)
}


#' @title Determine whether a string represents a column name with data
#' @noRd
hbw_is_data_header <- function(text){

    pattern = paste0("(", paste0(HOBO_DATA_HEADERS, collapse='|'), ")")

  grepl(pattern, tolower(text), ignore.case=TRUE)
}


hbw_is_datetime_header <- function(text){
  grepl("(Date.Time|Date|Time)(,\\s*GMT.\\d\\d.\\d\\d)?$", text)
}

#' @title Detect the value of the parameter "no quotes or commas"
#' @noRd
hbw_detect_no_quotes_commas <- function(lines){
  !any(grepl('"Date', lines))
}


#' @title create a configuration list for 'default' hoboware settings (2021)
#' @noRd
hobo.config.defaults <- function(){
  config <- list(separator= ",",
                 include_line_number= TRUE,
                 include_plot_title_in_header= TRUE,
                 always_show_fractional_seconds= FALSE,
                 separate_date_time= FALSE,
                 no_quotes_or_commas= FALSE,
                 include_logger_serial= TRUE,
                 include_sensor_serial= TRUE,
                 date_format= "MDY",
                 date_separator= "/",
                 time_format_24hr= FALSE,
                 positive_number_format= 1,
                 negative_number_format= 1,
                 include_plot_details= FALSE)

  config
}


hbw_header_regex <- function(separate_date_time, no_quotes_or_commas, separator){
  if (separate_date_time){

    if (no_quotes_or_commas){
      return(paste0("Date", separator, "Time"))
    }else{
      return(paste0('Date"', separator, '"Time'))
    }

  }else{
    return("Date Time")
  }
}




hbw_is_header_line <- function(lines, separate_date_time, no_quotes_or_commas, separator){
  pattern <- hbw_header_regex(separate_date_time,
                          no_quotes_or_commas,
                          separator)
  grepl(pattern, lines)
}


hbw_detect_time_zone_from_header_line <- function(headerline){
  tz_match <- regmatches(headerline, regexpr(HOBO_TZ_REGEX, headerline))
  tz <- gsub(":", "", tz_match)
  tz <- gsub("GMT", "", tz)

  if (length(tz) == 0){
    return(NULL)
  }
  tz
}


#' @title detect HoboWare time zone from details
#' @param details dataframe of details as returned by read.hobo.details
#' @noRd
hbw_detect_time_zone_from_details <- function(details){
  tz_match <- regmatches(details[,2], regexpr(HOBO_TZ_REGEX, details[,2]))
  tz <- gsub(":", "", tz_match)
  tz <- gsub("GMT", "", tz)

  if (!length(unique(tz)) == 1){
    warning("Several possible time zones were found in the file details.")
  }

  tz <- rev(tz)[1]

  if (!class(tz) == "character" || length(tz) == 0){
    return(NULL)
  }

  return(tz)
}


#' @title Configuration 'default' hoboware settings (2021)
#' @noRd
hobo.config.defaults <- function(){
  config <- list(separator= ",",
                 include_line_number= TRUE,
                 include_plot_title_in_header= TRUE,
                 always_show_fractional_seconds= FALSE,
                 separate_date_time= FALSE,
                 no_quotes_or_commas= FALSE,
                 include_logger_serial= TRUE,
                 include_sensor_serial= TRUE,
                 date_format= "MDY",
                 date_separator= "/",
                 time_format_24hr= FALSE,
                 positive_number_format= 1,
                 negative_number_format= 1,
                 include_plot_details= NA)

  config
}


#' @title Create an empty Hoboware configuration
#' @return list
#' @noRd
hobo.config.empty <- function(){
  config <- hobo.config.defaults()
  config[TRUE] <- NA
  config
}



hobo.config.classic <- function(){
  config <- list(separator= "\t",
                 include_line_number= FALSE,
                 include_plot_title_in_header= FALSE,
                 always_show_fractional_seconds= TRUE,
                 separate_date_time= FALSE,
                 no_quotes_or_commas= TRUE,
                 include_logger_serial= FALSE,
                 include_sensor_serial= TRUE,
                 date_format= "MDY",
                 date_separator= "/",
                 time_format_24hr= TRUE,
                 positive_number_format= 1,
                 negative_number_format= 1,
                 include_plot_details= NA)

  config

}
