
# Define the packages to be installed and loaded
pack <- c('gtrendsR','dplyr','ggplot2','showtext','sysfonts','gganimate','tidyquant','tidyr','ggchicklet','lubridate','ggimage','magick')

# Check for packages not yet installed and install them
vars <- pack[!(pack %in% installed.packages()[, "Package"])]
if (length(vars) != 0) {
  install.packages(vars, dependencies = TRUE)
}

# Load the packages
sapply(pack, require, character.only = TRUE)

# Define the function to fetch Google Trends data with retry logic
fetch_trends_data <- function(companies, start_date, end_date) {
  trends_data <- NULL
  retry_count <- 0
  max_retries <- 5
  
  while (is.null(trends_data) && retry_count < max_retries) {
    try({
      trends_data <- gtrends(companies, time = paste(start_date, end_date), gprop = "web")
    }, silent = TRUE)
    
    if (is.null(trends_data)) {
      retry_count <- retry_count + 1
      Sys.sleep(10)  # Wait for 10 seconds before retrying
    }
  }
  
  if (is.null(trends_data)) {
    stop("Failed to fetch Yahoo Trends data after multiple retries")
  }
  
  return(trends_data)
}