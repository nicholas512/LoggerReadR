# Configuration ===========================================================


GP5_DATEFMT = "%d.%m.%Y %H:%M:%S"
GP5_DELIMITER = ","


# Public functions ======================================================


#' @title Read Geoprecision (GP5W)
#'
#' @description Read data from a Geoprecision GP5W file
#'
#' @param filename character, path to the file
#' @param simplify logical, whether LoggerReadR should simplify the returned dataframe (see details)
#' @return dataframe, a dataframe
#'
#' @details If simplify is `TRUE`, the dataframe will be standardized somewhat: row numbers and non-data columns
#' will be removed; time and date columns will be combined into a single column called `TIME`.
#' @export
read_gp5w <- function(filename, simplify=FALSE){
  lines <- readLines(filename)

  header <- get_gp5w_header(lines)
  obs <- get_gp5w_observations(lines)

  ncols <- length(gregexpr(",", header)[[1]]) + 1  # length(strsplit(header, ","))
  obs <- pad_missing_separators(obs, ncols, GP5_DELIMITER)


  df <- utils::read.table(textConnection(obs), sep=',', header=F)
  colnames(df) <- strsplit(header, ",")[[1]]

  df$Time <- as.POSIXct(df$Time, format=GP5_DATEFMT)
  return(df)
}

read_GP5W_metadata <- function(filename){
  stop("Not Implemented")
}


# Hidden functions =================================================================


#' @noRd
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


#' @noRd
get_gp5w_observations <- function(lines){
  index <- which(is_gp5w_observation(lines))

  if (length(index) == 0){
    stop("No data lines found.")
  }else{
    return(trimws(lines[index]))
  }
}


is_gp5w_header <- function(line){grepl("No,Time", line)}


is_gp5w_observation <- function(line){grepl("\\d*,\\d{2}\\.\\d{2}\\.\\d{4}", line)}


pad_missing_separators <- function(lines, ncols, separator){
  n_separators = sapply(gregexpr(",", lines), length)
  n_missing_separators = pmax(0, ((ncols - 1) - n_separators))

  padding <- sapply(lapply(n_missing_separators, rep, x=separator), paste0, collapse="")
  #  padding[padding == "character(0)"] = ""

  paste0(lines, padding)
}


simplify_gp5w <- function(df){
  stop("not implemented")
}
