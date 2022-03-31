# Public functions =================================================================


#' @title Read Geoprecision file
#'
#' @description Read geoprecision file (autodetect GP5W / FG2)
#' @param filename path to geoprecision file
#' @param simplify logical whether LoggerReadR should simplify the returned dataframe (see details)
#' @export
read_geoprecision <- function(filename, simplify=FALSE){
  info_line <- readLines(filename, n=1)
  reader <- get_reader(info_line)

  if (is(reader, "function")){
    return(reader(filename))
  }else{
    warning("Could not autodetect file type (GP5W / FG2) from first line. Trying to read anyways.")
    tryCatch({  # try them all
      return(LoggerReadR::read_fg2(filename, simplify))
    })
    tryCatch({
      return(LoggerReadR::read_gp5w(filename, simplify))
    },
    error={
      stop("Could not read file. Tried reading as GP5W and FG2.")
    })
  }

}


# Hidden functions =================================================================


#' @description return appropriate geoprecision reader based on info line
#' @noRd
#'
get_reader <- function(info_line){
  if (grepl("GP5W", info_line)){
    return(LoggerReadR::read_gp5w)
  }else if (grepl("FG2", info_line)){
    return(LoggerReadR::read_fg2)
  } else{
    return(NULL)
  }
}
