# Degovx
# 2018-04-24
# Processing Data from a Tilt Hydometer

# Loading Packages --------------------------------------------------------
# These are the packages needed to run this script. To make things easier we
# check if packages are installed and loaded as we initialize them so simply
# running this block will make things work if you aren't super familiar with R.
# Essentially, we only need the googlesheets package to directly access the data
# from the Tilt Google Sheets document. However, I like to use the dplyr package
# in cases like this where I am aiming for readability and accessability in my
# code. Lubridate allows us to easily handle dates. Finally, ggplot2 and scales
# are two packages that really take the R graphics capabilities up to 11.
# First we have loading in data directly from Google Sheets
if(require("googlesheets")){
  print("googlesheets is loaded correctly")
} else {
  print("trying to install googlesheets")
  install.packages("googlesheets")
  if(require("googlesheets")){
    print("googlesheets installed and loaded")
  } else {
    stop("could not install googlesheets")
  }
}

# dplyr for processing data
if(require("dplyr")){
  print("dplyr is loaded correctly")
} else {
  print("trying to install dplyr")
  install.packages("dplyr")
  if(require("dplyr")){
    print("dplyr installed and loaded")
  } else {
    stop("could not install dplyr")
  }
}

# ggplot2 for graphing and scales for easy scaling
if(require("ggplot2")){
  print("ggplot2 is loaded correctly")
} else {
  print("trying to install ggplot2")
  install.packages("ggplot2")
  if(require("ggplot2")){
    print("ggplot2 installed and loaded")
  } else {
    stop("could not install ggplot2")
  }
}
if(require("scales")){
  print("scales is loaded correctly")
} else {
  print("trying to install scales")
  install.packages("scales")
  if(require("scales")){
    print("scales installed and loaded")
  } else {
    stop("could not install scales")
  }
}

# Lubridate for handling date formats easily
if(require("lubridate")){
  print("lubridate is loaded correctly")
} else {
  print("trying to install lubridate")
  install.packages("lubridate")
  if(require("lubridate")){
    print("lubridate installed and loaded")
  } else {
    stop("could not install lubridate")
  }
}

# Loading Data ------------------------------------------------------------
# Set a working directory. We do this to give a location for the oauth token to
# be stored (see below). It also allows us to easily save any of the work we are
# doing to a known location easily.
setwd("~/Google Drive/Beer Things")

# If you have never run this script before or on loading the data you get a
# failed authentication error run the commented out code below. This will open a
# browser window and ask for you to log in to Google. You need to log into
# whichever account you have the Tilt database stored. You can check out the
# googlesheets package on github to see that it won't accsess your data without
# you interacting. Finally, it creates an oauth token in your working directory
# which it will use for future access. So you can delete that token after
# running to require re-authenticaion.
# gs_auth()

# Then you need to find the key for your sheet. copy the entire URL of your
# sheet out of your browser and Paste it in between the ("") in the section
# below. Once you run that code line you will see a string in the console. Put
# that key in the gs_key() below
# extract_key_from_url("")

# We download the metadata for the database and then use that to access the
# data. I am going straight for sheet 2 which is the raw data. If you keep your
# raw data on a different sheet change ws = 2 to be ws = [number of sheet raw
# data is in]. I like to use the raw data because it requires less fidding about
# in the sheet before starting (like using the filtered worksheet on page 1)
# This does require us to select which beer and timeframe we are interested in
# viewing. I do both time and beer name since if you have brewed the same beer
# multiple times you would get screwy data. I also usually export the data into
# a csv format after a beer is finished brewing and delete those rows from the
# spreadsheet so for me it's a relatively clean database.
metadata <- gs_key("")
raw_data <- gs_read(metadata, ws = 2, check.names = TRUE)

# Input User Variables ----------------------------------------------------
# Now we need to specify which beer we are going to use. Run the commented out
# code below to see which beers are in the sheet. Paste that name between the
# "" below to select that beer for analysis.
# levels(factor(raw_data$Beer))
selected_beer <- "Munich Helles"

# If you have that name in the dataset multiple times for different batches
# (this is the second or more time you have brewed that beer without deleting
# any of the raw data) you will need to narrow down the time range you were
# brewing. If you want to use all data for that name leave as NULL.
timeframe <- NULL
# Let's say you wanted to only see data from April  01, 2018 to April 10, 2018. 
# We specify the dates as yyyy-mm-dd hh:mm:ss. c("Start", "End")
# timeframe <- c("2018-04-01 00:00:00", "2018-04-10 00:00:00")

# Now we need to specify the timezone data was recorded in which is the timezone
# we will use for all dates. Unfortunatly, time in general for computing is a
# rough concept. Generally, we can just use the timezone your computer is set to
# as it's likely you are processing the data near where you brewed your data.
# However, if this isn't the case you will need to replace Sys.timezone() with
# an actual timezone value that corresponds with the timezone set on the device
# which uploaded the data to the cloud. See help("timezones") for more
# information.
timezone <- Sys.timezone()

# Data Processing ---------------------------------------------------------
# Keep only the beer we are interested in.
filter(raw_data, Beer == selected_beer) -> beer_data

# Remove "undefined" rows from the data.
beer_data <- data.frame(lapply(beer_data, gsub, pattern = "undefined", replacement = NA, fixed = TRUE))

# Convert Timepoint into an R readable time format.
# Tilt stores dates as Julian Dates in raw (object oriented programming). That
# means the origin point is 1899-12-30 00:00:00 and the timezone is system time
# of the device used to upload Tilt data to the cloud. Finally, just to make
# sure that everything converts correctly we wrap the raw timepoint data in
# as.character and as.numeric. This ensures if the data reads strings as factors
# we get the actual values.
beer_data$Timepoint <- 
  ymd_hms("1899-12-30 00:00:00", tz = timezone) + # Google Sheets Origin date/time
  as.numeric(as.character(beer_data$Timepoint)) * 86400 # Conversion from decimal day to second

# filter to the timeframe specified above unless timeframe is set to NULL.
# We use the lubridate interval argument combined with dplyr filter to create a
# subset of the data confined to the start and end times specified in the user
# input section.
if(!is.null(timeframe[1])){
  beer_data <-
    filter(beer_data,
           Timepoint %within% interval(
             start = ymd_hms(timeframe[1], tz = timezone),
             end = ymd_hms(timeframe[2], tz = timezone),
             tzone = timezone
           ))
}

# Convert Color and Beer name to Factor.
beer_data$Color <- factor(beer_data$Color)
beer_data$Beer <- factor(beer_data$Beer)

# Convert SG and Temp to Numeric.
beer_data$SG <- as.numeric(as.character(beer_data$SG))
beer_data$Temp <- as.numeric(as.character(beer_data$Temp))

# Drop NA rows. These usually crop up as "undefined" above where there is some
# communication issues between the Tilt and the recording device. Dropping these
# rows makes the loess model below more clean.
beer_data <- beer_data[complete.cases(beer_data[, 1:5]), ]

# Sort by date. This way our graphs will look correct.
beer_data <- arrange(beer_data, Timepoint)

# Drop any Gravity readings that are jumps off the local mean. This is to get
# rid of those readings where the Tilt jumps due to a bubble or being stirred.
# First, we fit a loess model to the data for a comparison.
loessMod <- loess(SG ~ as.numeric(Timepoint), data = beer_data, span = 0.20)
beer_data$SG_smooth <- as.vector(loessMod$fitted)

# Then, we replace extreme values with the smoothed mean to normalize the data.
beer_data$SG <- ifelse(beer_data$SG >= beer_data$SG_smooth + .005, beer_data$SG_smooth, beer_data$SG)
beer_data$SG <- ifelse(beer_data$SG <= beer_data$SG_smooth - .005, beer_data$SG_smooth, beer_data$SG)
# As a note never do this with actual data. Since we are making pretty graphics
# for a floating hydrometer it doesn't matter, but in an actual data set please
# don't do this.

# Beer Statistics ---------------------------------------------------------
# Calculate the Original Gravity (OG) of the bear from a mean of the first
# 12 measurement points (first 3 hours). Can be extended/contracted as much as
# desired. In my experience this yeilds a fairly stable estimate of the gravity.
OG <- select(beer_data, "SG") %>%
  slice(1:12) %>%
  colMeans()

# Convert OG into degrees Plato. I used the standard formula for this
# conversion. If anyone knows of a more accurate method please let me know.
Original_Plato <- (-1 * 616.868) + (1111.14 * OG) - (630.272 * OG ^ 2) + (135.997 * OG ^ 3)

# Repeat for FG except take the last 12 measurement points.
FG <- select(beer_data, "SG") %>%
  slice((nrow(beer_data) - 11):nrow(beer_data)) %>%
  colMeans()
Final_Plato <- (-1 * 616.868) + (1111.14 * FG) - (630.272 * FG ^ 2) + (135.997 * FG ^ 3)

# Calculate ABV and ABW. We use both the approximation formula which is fine for
# low alcohol beers. But, as alcohol content rises the accuracy of the fomula
# degrades. So, I also have the more specific formula. I don't know a ton about
# this formula and its accuracy but it seems to always show a higher alcohol
# content for bigger beers and is closer to the approximate ABV in smaller
# beers. I generally use these two values to represent a range of possible
# alcohol content values. We also have the Alcohol by Weight formula here for
# our European friends.
ABV_approximate <- (OG - FG) * 131.25
ABV_specific <- 76.08 * (OG - FG) / (1.775 - OG) * (FG / 0.794)
ABW <- 76.08 * (OG - FG) / (1.775 - OG)

# Calculate the apparent degree of fermentation (apparent attentuation). Once
# again, this seems to be the formula everyone uses. If anyone knows a better,
# more accurate, or even an addition method for this please send it my way.
ADF <- ((OG - FG) / (OG - 1)) * 100

# Calculate the real extract.
Real_Extract <- 0.188 * Original_Plato + 0.8192 * Final_Plato

# And the real attentuation. 
RA <- ((Original_Plato - Real_Extract) / Original_Plato) * 100

# Caloric content in 12oz and 22oz bottles
Calories_12oz <- (6.9 * ABW + 4.0 * (Real_Extract - 0.1)) * 3.54882 * FG 
Calories_22oz <- (6.9 * ABW + 4.0 * (Real_Extract - 0.1)) * 6.50618 * FG 

# Graph -------------------------------------------------------------------
# We use ggplot2 for our graphics as it produces some nice looking graphs.
# Mainly, my goal with these was to create plots without much user
# interactivity. That comes at the cost of making things highly specific for
# each beer. For example, if your beer only fermented for 1 week the 20
# breakpoints won't look as great. If your beer fermented for 30 days it will
# look excellent. Similarly, gravity specificity in the graph is based on
# starting and ending values. If you are brewing a barleywine vs a session ale
# you will lose fidelity on that barelywine. You can adjust the scaling fairly
# easily to match your desire. I also have it set to graph both the raw data and
# a fitted local regression (loess) line to the data. The color of the raw data
# changes as a function of being close to the max or the min to add a bit of
# color. You can adjust the labels in time to include something other than just
# month and day by changing date_format. However, you will also need to rotate
# the labels to be either horizontal or at a 45 degree angle. See the commented
# code below and replace the "labels =" argument with that to accomplish this.

# More specific Time labels
# labels = date_format("%b %d, %I:%M")) +
# theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

# Temperature by Time
ggplot(beer_data, aes(x = Timepoint, y = Temp)) +
  geom_line(aes(color = Temp)) +
  geom_smooth(color = "steelblue", se = FALSE, method = loess) +
  scale_color_gradient(low = "blue", high = "red", guide = "none") +
  scale_x_datetime(breaks = seq(
    from = min(beer_data$Timepoint),
    to = max(beer_data$Timepoint),
    by = (max(beer_data$Timepoint) - min(beer_data$Timepoint)) / 20
  ),
  labels = date_format("%b, %d")) +
  scale_y_continuous(breaks = seq(
    from = round(min(beer_data$Temp), 0) - 2,
    to = round(max(beer_data$Temp), 0) + 2,
    by = round((
      max(beer_data$Temp) - min(beer_data$Temp)
    ) / 20, 1)
  )) +
  xlab("Time") +
  ylab("Temperature (F)") +
  ggtitle(paste(as.character(beer_data$Beer)[nrow(beer_data)], "Temperature by Time Linegraph"))

# Gravity by Time
ggplot(beer_data, aes(x = Timepoint, y = SG)) +
  geom_line(aes(color = SG)) +
  geom_smooth(color = "steelblue", se = FALSE, method = loess) +
  scale_color_gradient(low = "darkgreen", high = "lightgreen", guide = "none") +
  scale_x_datetime(breaks = seq(
    from = min(beer_data$Timepoint),
    to = max(beer_data$Timepoint),
    by = (max(beer_data$Timepoint) - min(beer_data$Timepoint)) / 20
  ),
  labels = date_format("%b, %d")) +
  scale_y_continuous(breaks = seq(
    from = min(beer_data$SG) - .1,
    to = max(beer_data$SG) + .1,
    by = round((max(beer_data$SG) - min(beer_data$SG)) / 20, 3)
  )) +
  xlab("Time") +
  ylab("Specific Gravity") +
  ggtitle(paste(
    as.character(beer_data$Beer)[nrow(beer_data)],
    "Specific Gravity by Time Linegraph"
  ))

# Printing ----------------------------------------------------------------
# Print everything out into the console to view.
cat(paste(as.character(beer_data$Beer)[nrow(beer_data)], "Statisitics"))
cat("Original Gravity = ", round(OG, 3), "; Original Plato = ", round(Original_Plato, 3), sep = "")
cat("Final Gravity = ", round(FG, 3), "; Final Plato = ", round(Final_Plato, 3), sep = "")
cat("ABV = ", round(ABV_approximate, 2), "% to ", round(ABV_specific, 2), "%", sep = "")
cat("ABW = ", round(ABW, 2), "%", sep = "")
cat("Apparent Attenuation = ", round(ADF, 2), "%; Real Attentuation = ", round(RA, 2), "%", sep = "")
cat("Calories in 12oz Bottle = ", round(Calories_12oz, 0), sep = "")
cat("Calories in 22oz Bottle = ", round(Calories_22oz, 0), sep = "")
