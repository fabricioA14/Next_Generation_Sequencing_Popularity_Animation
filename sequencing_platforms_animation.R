
# Load the dependencies (packages and custom function)
source("src/dependencies.R")

### FIRST PLOT ###

# Define the companies of interest
companies <- c("Illumina", "PacBio", "Oxford Nanopore")

# Function to fetch Google Trends data with retry logic
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

# Define the date ranges for the 10-year period
date_ranges <- list(
  c("2010-01-02", "2011-01-01"),
  c("2011-01-02", "2012-01-01"),
  c("2012-01-02", "2013-01-01"),
  c("2013-01-02", "2014-01-01"),
  c("2014-01-02", "2015-01-01"),
  c("2015-01-02", "2016-01-01"),
  c("2016-01-02", "2017-01-01"),
  c("2017-01-02", "2018-01-01"),
  c("2018-01-02", "2019-01-01"),
  c("2019-01-02", "2020-01-01"),
  c("2020-01-02", "2021-01-01"),
  c("2021-01-02", "2022-01-01"),
  c("2022-01-02", "2023-01-01"),
  c("2023-01-02", "2024-01-01")
)

# Fetch Yahoo Trends data for each date range
trends_data_list <- lapply(date_ranges, function(dates) {
  fetch_trends_data(companies, dates[1], dates[2])
})

# Combine the data
trends_over_time <- do.call(bind_rows, lapply(trends_data_list, function(x) x$interest_over_time))

# Convert the date column to Date type
trends_over_time$date <- as.Date(trends_over_time$date)

# Filter relevant columns and convert hits to numeric
trends_over_time <- trends_over_time %>%
  select(date, hits, keyword) %>%
  mutate(hits = as.numeric(hits))

# Normalize hits for each keyword
trends_over_time <- trends_over_time %>%
  group_by(keyword) %>%
  mutate(hits_normalized = ifelse(hits > 0, ceiling(((hits - min(hits)) / (max(hits) - min(hits))) * 99 + 1), 0)) %>%
  ungroup()

# Select one date per month for each keyword
trends_over_time <- trends_over_time %>%
  group_by(keyword, year = format(date, "%Y"), month = format(date, "%m")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-year, -month)

# Add Comfortaa font from Google Fonts
font_add_google(name = "Comfortaa", family = "Comfortaa")
showtext_auto()

# Compute smoothed data with SE
smoothed_data <- trends_over_time %>%
  group_by(keyword) %>%
  do({
    model <- loess(hits_normalized ~ as.numeric(date), data = ., span = 0.1)
    data.frame(
      date = .$date,
      hits_normalized = .$hits_normalized,
      keyword = .$keyword,
      smoothed_hits = predict(model),
      se = predict(model, se = TRUE)$se.fit
    )
  }) %>%
  ungroup() %>%
  mutate(lower = smoothed_hits - se, upper = smoothed_hits + se)

# Add a column to check if the points are close to the edges of the plot with category-specific thresholds
smoothed_data <- smoothed_data %>%
  mutate(vjust = ifelse((keyword == "Illumina" & date > as.Date("2022-01-01")), -1,
                        ifelse(keyword == "Oxford Nanopore" & date > as.Date("2020-01-01"), 1.5,
                               ifelse(keyword == "PacBio" & date > as.Date("2022-01-01"), -1, 0))),
         hjust = ifelse((keyword == "Illumina" & date > as.Date("2022-01-01")) |
                          (keyword == "Oxford Nanopore" & date > as.Date("2020-01-01")) |
                          (keyword == "PacBio" & date > as.Date("2022-01-01")), 1, 
                        ifelse(keyword == "Illumina", -0.2, 
                               ifelse(keyword == "Oxford Nanopore", -0.07, -0.2))))

p <- ggplot(smoothed_data, aes(x = date, y = smoothed_hits, color = keyword, fill = keyword)) +  # Create ggplot object with date on x-axis and smoothed_hits on y-axis, colored and filled by keyword
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +  # Add a ribbon (shaded area) representing the SE shadow with transparency
  geom_line(size = 1) +  # Add a line plot with line width of 1
  geom_point(shape = 21, size = 4, alpha = 0.5) +  # Add points with specific shape, size, and transparency
  geom_text(aes(label = keyword,
                hjust = hjust,
                vjust = vjust,
                size = 4,
                family = "Comfortaa"),
            color = "white") +  # Add text labels for the keyword next to points with specified aesthetics
  labs(title = "Popularity Trends - Next Generation Sequencing Platforms",  # Add title to the plot
       x = "Date",  # Label for x-axis
       y = "Search Interest (Normalized)") +  # Label for y-axis
  scale_x_date(expand = c(0, 0), date_breaks = "2 years", date_labels = "%Y") +  # Customize x-axis: remove space at start and set breaks every 2 years with year format
  scale_y_continuous(limits = c(-10, 110), breaks = seq(0, 100, by = 20)) +  # Customize y-axis: set limits and intervals
  theme_minimal() +  # Use minimal theme
  theme(
    plot.background = element_rect(fill = "black"),  # Set plot background color to black
    panel.background = element_rect(fill = "black"),  # Set panel background color to black
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_text(color = "white", family = "Comfortaa", face = "bold"),  # Customize axis text: color, font, and style
    axis.title = element_text(color = "white", family = "Comfortaa", face = "bold"),  # Customize axis titles: color, font, and style
    axis.line = element_line(color = "white"),  # Set axis lines to white
    plot.title = element_text(color = "white", size = 14, face = "bold", hjust = 0.5, family = "Comfortaa"),  # Customize plot title: color, size, style, alignment, and font
    legend.position = "none",  # Exclude the legend
    plot.margin = margin(t = 30, r = 30, b = 30, l = 30),  # Set plot margins
    axis.title.y = element_text(margin = margin(r = 10))  # Adjust margin for y-axis title
  ) +
  transition_reveal(date)  # Animate the plot over the date variable


# Render the animation
animation_a <- animate(p, nframes = 400, fps = 20, width = 710, height = 400, end_pause = 50)


### SECOND PLOT ###

# Add the Comfortaa font from Google Fonts
font_add_google(name = "Comfortaa", family = "Comfortaa")
showtext_auto()

# Define stock symbols and date range
symbols <- c("ILMN", "PACB", "ONT.L")
start_date <- "2021-01-01"
end_date <- Sys.Date()

# Get stock prices for the specified symbols and date range
stock_prices <- tq_get(symbols, from = start_date, to = end_date, get = "stock.prices")

# Select relevant columns and filter out rows with NA values in the adjusted column
stock_prices <- stock_prices %>%
  select(symbol, date, adjusted) %>%
  filter(!is.na(adjusted))

# Aggregate data every 15 days and calculate the mean adjusted prices
stock_prices <- stock_prices %>%
  mutate(date = floor_date(date, unit = "15 days")) %>%
  group_by(symbol, date) %>%
  summarise(adjusted = mean(adjusted, na.rm = TRUE)) %>%
  ungroup()

# Prepare data for visualization by calculating ranks and relative values
stock_prices <- stock_prices %>%
  group_by(date) %>%
  mutate(rank = rank(-adjusted),
         Value_rel = adjusted / adjusted[rank == 1],
         Value_lbl = paste0(" ", round(adjusted))) %>%
  ungroup()

# Filter data for dates starting from October 2021 and arrange by date
data <- stock_prices %>%
  filter(date >= as.Date("2021-10-01")) %>%
  arrange(date) %>%
  group_by(date) %>%
  mutate(rank = rank(-adjusted),
         Value_lbl = paste0(" ", round(adjusted))) %>%
  ungroup()

# Adjust the symbol names for better display
data$symbol[data$symbol == "ILMN"] <- "  Illumina"
data$symbol[data$symbol == "PACB"] <- "  PacBio"
data$symbol[data$symbol == "ONT.L"] <- " Oxford Nanopore"

# Create the visualization
p1 <- ggplot(data, aes(rank, group = symbol, fill = as.factor(symbol), color = as.factor(symbol))) +
  geom_chicklet(aes(y = adjusted), radius = grid::unit(2, 'mm'), alpha = 0.8, color = "white", size = 0.5, show.legend = FALSE) +  # Create bar plots with rounded corners
  # geom_image(aes(y = adjusted/2, image = image), size = 0.05) +  # Adding images inside bars (commented out)
  geom_text(aes(y = 0.0, label = ifelse(symbol == " Oxford Nanopore", "     Oxford \n Nanopore", symbol)),  # Add symbol names
            vjust = 0.5, hjust = 1.1, color = "white", size = 5, family = "Comfortaa") +
  geom_text(aes(y = adjusted, label = Value_lbl), hjust = -0.1, color = "white", size = 5, family = "Comfortaa") +  # Add value labels
  coord_flip(clip = "off", expand = FALSE) +  # Flip coordinates
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels
  scale_x_reverse() +  # Reverse x-axis
  scale_fill_manual(values = c("  Illumina" = "#F8766D", "  PacBio" = "#4E7DCC", " Oxford Nanopore" = "#00952D")) +  # Set fill colors
  scale_color_manual(values = c("  Illumina" = "#F8766D", "  PacBio" = "#4E7DCC", " Oxford Nanopore" = "#00952D")) +  # Set line colors
  guides(color = "none", fill = "none") +  # Remove legends
  theme(
    axis.line = element_blank(),  # Remove axis lines
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    legend.position = "none",  # Remove legend
    panel.background = element_blank(),  # Remove panel background
    panel.border = element_blank(),  # Remove panel border
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", colour = "white", vjust = -1, family = "Comfortaa"),  # Style plot title
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic", color = "grey", family = "Comfortaa"),  # Style plot subtitle
    plot.caption = element_text(size = 14, hjust = 0.5, face = "italic", color = "grey", family = "Comfortaa"),  # Style plot caption
    plot.background = element_rect(fill = "black", color = "black"),  # Set plot background color
    plot.margin = margin(1, 5, 1, 5, "cm")  # Set plot margins
  ) +
  transition_states(date, transition_length = 8, state_length = 1) +  # Add animation transitions based on date
  view_follow(fixed_x = TRUE) +  # Ensure x-axis remains fixed
  labs(title = 'Date: {closest_state}',  # Add dynamic title with date
       subtitle = "Post Pandemic - Adjusted Stock Prices Over Time ($)",  # Add subtitle
       caption = "Data Source: Yahoo Finance")  # Add caption

# Render the animation with increased duration
animation_b <- animate(p1, nframes = 400, fps = 20, width = 710, height = 300, end_pause = 50)  # Create animation with specified parameters

### MERGED ANIMATIONS ###

a_mgif <- image_read(animation_a)
b_mgif <- image_read(animation_b)

length_a <- length(a_mgif)
length_b <- length(b_mgif)

# Determine the target length
target_length <- max(length_a, length_b)

# Repeat the last frame for the shorter GIF
if (length_a < target_length) {
  last_frame_a <- a_mgif[length_a]
  repeated_frames_a <- rep(last_frame_a, target_length - length_a)
  a_mgif <- c(a_mgif, repeated_frames_a)
} else if (length_b < target_length) {
  last_frame_b <- b_mgif[length_b-6]
  repeated_frames_b <- rep(last_frame_b, target_length - length_b)
  b_mgif <- c(b_mgif, repeated_frames_b)
}

# Combine the frames
new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:length(a_mgif)){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

# Export the new_gif object
image_write(new_gif, path = "combined_gif.gif")

new_gif

