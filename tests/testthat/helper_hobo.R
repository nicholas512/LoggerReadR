files_dir <- "test_files"


hobo_header_1 <- '"Date Time, GMT-07:00";"Temp, °F";"Temp, °F";"Coupler Detached";"Coupler Attached";"Host Connected";"End Of File"'
hobo_header_2 <- 'Date Time\tTemp (°C)  #9724974\tTemp (°C)  #9724974\tCoupler Detached\tCoupler Attached\tHost Connected\tEnd Of File'

hobo_file_2 <- file.path(files_dir, "hobo_classic.csv")
hobo_file_3 <- file.path(files_dir, "hobo_defaults.csv")

hobo_lines_1 <- readLines(file.path(files_dir, "hobo_minimal.txt"), encoding = "UTF-8")
hobo_lines_2 <- readLines(hobo_file_2, encoding = "UTF-8")
hobo_lines_3 <- readLines(hobo_file_3, encoding = "UTF-8")

