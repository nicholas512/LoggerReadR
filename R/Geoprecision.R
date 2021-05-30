#' @title Read Geoprecision file
#'
#' @description Read geoprecision file (autodetect GP5W / FG2)
#' @param filename path to geoprecision file
#' @export
read_geoprecision <- function(filename){
  info_line <- readLines(filename, n=1)
  reader <- get_reader(info_line)

  if (!is.na(reader)){
    return(reader(filename))
  }else{
    warning("Could not autodetect file type (GP5W / FG2) from first line. Trying to read anyways.")
    tryCatch({  # try them all
      return(LoggerReadR::read_fg2(filename))
    })
    tryCatch({
      return(LoggerReadR::read_gp5w(filename))
    })
  }

}


#' @description return appropriate geoprecision reader based on info line
#' @noRd
#'
get_reader <- function(info_line){
  if (grepl("FG2", info_line)){
    return(LoggerReadR::read_gp5w)
  }else if (grepl("GP5W", info_line)){
    return(LoggerReadR::read_fg2)
  } else{
    return(NA)
  }
}
