#load packages
library(tictoc) #read the tictoc package

#define function for printing tictoc results
printTicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

#define the paths for the 3 R scripts where I want to measure the time
script1 <- "scripts/Assignment_8_1.R"
script2 <- "scripts/Assignment_8_2.R"
script3 <- "scripts/Assignment_8_3.R"

#write a function to measure the time and display in a table
measure_time <- function(path) {
  tic.clearlog()
  tic()
  source(path)
  toc(log=TRUE)
  printTicTocLog() %>%
    knitr::kable()
}

measure_time(script1)
measure_time(script2)
measure_time(script3)
