arguments <- commandArgs(trailingOnly = TRUE)
file_where <- arguments[1]
my_ip <- arguments[2]
port <- as.numeric(arguments[3])
browser <- as.logical(arguments[4])
polling <- as.numeric(arguments[5])

shiny::runApp(system.file("board/xgboard.R", package = "Laurae"), port = port, launch.browser = browser, host = my_ip)
