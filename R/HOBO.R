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
#' @description Read Onset HOBOWare file. Optionally provide file configuration properties.
#'
#' @param filename character, path to the file
#'
#' @param configuration A named list of file configuration properties according to \code{\link{detect_hoboware_configuration}}.
#' If missing, LoggerReadR will attempt to auto-detect configuration properties.
#'
#' @param simplify logical, whether LoggerReadR should simplify the returned dataframe (see details)
#'
#' @return dataframe, a dataframe
#'
#' @details
#'
#' @export
read_hoboware <- function(filename, configuration, simplify=FALSE, encoding = "UTF-8"){

  if (missing(configuration)){
    warning("No configuration properties provided. Attempting to detect.")
    configuration <- detect_hoboware_configuration(filename)
  }

  lines <- readLines(filename, encoding=encoding)
  headerline_i <- which(is_header_line(lines, configuration$separate_date_time,
                                       configuration$no_quotes_or_commas,
                                       configuration$separator))
  print(headerline_i)
  headerline <- lines[headerline_i]

  # get time zone

  df <- utils::read.table(textConnection(lines), sep=configuration$separator,
                    header=F, skip=headerline_i, fill=T)
  if (configuration$include_plot_details){
    df <- df[,-ncol(df)]
  }

  colnames(df) <- headerline

  df
}


#' @title Detect HOBOWare file properties
#'
#' @description Auto-detect the various HOBOWare text file configuration options that were set for a file
#'
#' @param filename character, path to the file
#'
#' @return named list of HOBOWare configuration properties (see details for names)
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
#' \item \code{date_separator}. Chosen from: {}
#' \item \code{time_format_24hr}. Chosen from:  {\code{TRUE}, \code{FALSE}}
#' \item \code{positive_number_format}. Chosen from: \code{1,2,3,4}
#' \item \code{negative_number_format}. Chosen from: \code{1,2,3} (corresponding to )
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
  config$time_format_24hr <- toupper(date_info$ampm) %in% c("AM", "PM")
  config$positive_number_format  <- hbw_evaluate_positive_number_format(number_info$thou, number_info$decimal)
  config$negative_number_format   <- hbw_evaluate_negative_number_format(number_info$neg1, number_info$neg2)
  config$include_plot_details   <- hbw_detect_include_plot_details(lines)

  config
}


read_hoboware_details <- function(filename, configuration, encoding = "UTF-8"){
  if (missing(configuration)){
    warning("No configuration properties provided. Attempting to detect.")
    configuration <- detect_hoboware_configuration(filename)
  }

  lines <- readLines(filename, encoding=encoding, n=500)
  matches <- regexpr("(?P<key>\\b[A-Za-z ]+\\b):(?P<value>.*)$", lines, perl=TRUE)
  match.table <- regcapturedmatches(lines, matches)
  match.table[match.table == ""] <- NA

  data.frame(na.omit(match.table), stringsAsFactors = F)
}

# Hidden functions =================================================================


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
        "[\\., ]",                # Separated by a decimal delimiter
      ")",
      "\\d+",                   # Decimal digits (assume at least 1)
      "(?P<neg2>[-\\)])?",      # Possible terminating negative sign {')'}
      "(?P=sep)",               # The same column separator
    ")+"                      # Repeated for each data column
  )

  matches <- regexpr(pattern, lines, perl=TRUE)
  match.table <- regcapturedmatches(lines, matches) # LoggerReadR::

  as.list(apply(match.table, 2, Mode)) # LoggerReadR::
}

#' @details Return the appropriate time format string
hbw_time_fmt <- function(time_format_24hr, always_show_fractional_seconds){

  if (time_format_24hr){
    fmt = "%H:%M:%S"
  }else{
    fmt =  "%I:%M:%S %p"
  }

  if (always_show_fractional_seconds){
    fmt = gsub("S", "S.%f", fmt)
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


#' @details Detect whether date and time are separate columns
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


#' @details Detect the value of the parameter "include plot details"
hbw_detect_include_plot_details <- function(lines){
  options <- paste0(HOBO_DETAILS_KEYWORDS, collapse="|")
  pattern <- paste0("(", options, ")")
  if (length(grepl(pattern, lines)) > 3){
    TRUE
  }else{
    FALSE
  }
}

#'
#' @details Evaluate the positive number format based on detected separators.
#' Positive number formats are as follows:
#'   |1| 1,234.56 | comma, period |
#'   |2| 1 234,56 | space, comma  |
#'   |3| 1.234,56 | period, comma |
#'   |4| 1.234 56 | period, space |
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


#' @details Return negative number format
#'         |1| -123 | -, None |
#'         |2| 123- | None, -  |
#'         |3| (123) | (, ) |
#'
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

#' @details Detect whether dates are MDY, YMD, or DMY.
#' This is based on heuristics and the assumption of evenly distributed sampling at
#' a frequency greater than monthly (by looking at which set of numbers is more 'diverse' and
#' assuming that corresponds to days rather than months)
hbw_detect_date_format <- function(lines){
  matches <- regexpr("(?P<p1>\\d{2}).(?P<p2>\\d{2}).(?P<p3>\\d{2}).\\d{2}:\\d{2}:\\d{2}", lines, perl=TRUE)
  match.table <- regcapturedmatches(lines, matches)
  match.table[match.table==""] <- NA
  match.table <- data.frame(na.omit(match.table), stringsAsFactors = F)

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


#' @details Determine whether a string represents a column name with data
is_data_header <- function(text, no_quotes_or_commas){
  if (no_quotes_or_commas){
    pattern = paste0("(", paste0(HOBO_DATA_HEADERS, collapse='|'), ") \\(.{1,5}\\)")
  }else{
    pattern = paste0("(", paste0(HOBO_DATA_HEADERS, collapse='|'), ")")
  }
  grepl(pattern, text, ignore.case=TRUE)
}

#' @details Detect the value of the parameter "no quotes or commas"
hbw_detect_no_quotes_commas <- function(lines){
  !any(grepl('"Date', lines))
}


#' @details create a configuration list for 'default' hoboware settings (2021)
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


header_regex <- function(separate_date_time, no_quotes_or_commas, separator){
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


is_header_line <- function(lines, separate_date_time, no_quotes_or_commas, separator){
  pattern <- header_regex(separate_date_time,
                          no_quotes_or_commas,
                          separator)
  grepl(pattern, lines)
}


detect_time_zone_from_header_line <- function(headerline){
  tz_match <- regmatches(headerline, regexpr(HOBO_TZ_REGEX, headerline))
  tz <- gsub(":", "", tz_match)
  gsub("GMT", "", tz)
}


#' @details Create an empty configuration list
hobo.config.empty <- function(){
  config <- hobo.config.defaults()
  config[TRUE] <- NA
  config
}

