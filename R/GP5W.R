#' @title Read GP5W file
#'
#' @description Read data from a Geoprecision GP5W file
#'
#' @param filename character, path to the file
#'
#' @return dataframe, a dataframe
#'
#' @export
#' @include

DATEFMT = "%d.%m.%Y %H:%M:%S"

read_GP5W <- function(filename){
  lines <- readLines(filename)

  header <- get_gp5w_header(lines)
  obs <- get_gp5w_observations(lines)

  ncols <- length(gregexpr(",", header)[[1]])
  obs <- pad_missing_separators(obs, ncols)


  df <- read.table(textConnection(obs), sep=',', header=T)
  colnames(df) <- strsplit(header, ",")[[1]]

  df$Time <- as.POSIXct(df$Time, format=DATEFMT)
  return(df)
}

read_GP5W_metadata <- function(filename){
  stop("Not Implemented")
}

get_gp5w_header <- function(lines){
  index <- which(is_gp5w_header(lines))

  if (length(index) == 0){
    stop("Could not find a header line.")
  }else if (length(index) > 1){
    stop("Found multiple header rows.")
  }else{
    return(trimws(lines[index]))
  }
}

get_gp5w_observations <- function(lines){
  index <- which(is_gp5w_observation(lines))

  if (length(index) == 0){
    stop("No data lines found.")
  }else{
    return(trimws(lines[index]))
  }
}

is_gp5w_header <- function(line){
  grepl("No,Time", line)
}

is_gp5w_observation <- function(line){
  grepl("\\d*,\\d{2}\\.\\d{2}\\.\\d{4}", line)
}

pad_missing_separators <- function(lines, ncols){
  n_separators = sapply(gregexpr(",", lines), length)
  n_missing_separators = ncols - n_separators

  padding <- as.character(sapply(n_missing_separators, rep, x=","))
  padding[padding=="character(0)"] = ""

  paste0(lines, padding)
}

