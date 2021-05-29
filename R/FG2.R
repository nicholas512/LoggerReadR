FG2_DATEFMT = "%d.%m.%Y %H:%M:%S"
FG2_DELIMITER = ","

#' @title Read a FG2 file
#'
#' @description Read data from a Geoprecision FG2 file
#'
#' @param filename character, path to the file
#'
#' @return dataframe, a dataframe
#'
#' @export
read_fg2 <- function(filename){
  lines <- readLines(filename)

  header <- get_fg2_header(lines)
  obs <- get_fg2_observations(lines)

  ncols <- length(gregexpr(",", header)[[1]]) + 1
  obs <- pad_missing_separators(obs, ncols, FG2_DELIMITER)

  df <- utils::read.table(textConnection(obs), sep=',', header=F)
  colnames(df) <- strsplit(header, ",")[[1]]

  df$TIME <- as.POSIXct(df$TIME, format=FG2_DATEFMT)
  return(df)
}

read_fg2_metadata <- function(filename){
  stop("Not Implemented")
}

get_fg2_header <- function(lines){
  index <- which(is_fg2_header(lines))

  if (length(index) == 0){
    stop("Could not find a header line.")
  }else if (length(index) > 1){
    stop("Found multiple header rows.")
  }else{
    return(trimws(lines[index]))
  }
}

get_fg2_observations <- function(lines){
  index <- which(is_fg2_observation(lines))

  if (length(index) == 0){
    stop("No data lines found.")
  }else{
    return(trimws(lines[index]))
  }
}

is_fg2_header <- function(line){
  grepl(paste0("NO", FG2_DELIMITER, "TIME"), line)
}

is_fg2_observation <- function(line){
  grepl(paste0("^\\d*", FG2_DELIMITER, "\\d\\d.\\d\\d"), line)
}

is_fg2_info <- function(line){
  grepl("^<.*>$", line)
}




