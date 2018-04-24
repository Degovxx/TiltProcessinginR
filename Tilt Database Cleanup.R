# Degovx
# 2018-04-24
# Clean up the Tilt database

# Google Sheets can become full over time. We also might want to brew the same
# beer lots of time. Either way, it can be cumbersome to have all your archival
# data stored in an inaccessable format. Here what we will do is pull in the
# Sheet 2 (raw data). Archive it by Beer name. And clean out sheet 2.

# When I am done monitoring a brew with the tilt I use the app to set the name
# to "test." That way logging can be seperated from the intended data points and
# the unintended data points. This also has the added benefit that if you
# accidently recoded data (like the tilt tipped over in storage) it won't get
# used. It also means that when you turn everything back on to tare it out on
# your next brew day all that test data would be filed appropriately. Below in
# this script I drop all "test" beer rows before doing an archive.

# Set Storage Folder ------------------------------------------------------
# Set the base folder for storing Tilt Archives
directory <- "~/Google Drive/Beer Things/Tilt Archive/"

# Load Packages -----------------------------------------------------------
# googlesheets for loading and managing a GSheet
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

# dplyr for data manipulation
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

# Load Data ---------------------------------------------------------------
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
raw_data <- gs_read(metadata, ws = "Sheet1", check.names = TRUE)

# Data Processing ---------------------------------------------------------
# Remove "undefined." I don't know what causes the Tilt to sometimes report and
# record undefined but those rows are of course not very interesting. Here we
# will replace all undefined with NA. Then, in the next step when we drop rows
# that are not complete they will be filtered out.
beer_data <- data.frame(lapply(raw_data, gsub, pattern = "undefined", replacement = NA, fixed = TRUE))

# Drop NA rows. We exclude the comment field as that is likely empty.
beer_data <- beer_data[complete.cases(beer_data[, 1:5]), ]

# Drop test beer rows. We use tolower here to capture all capitalizations of test.
beer_data <- filter(beer_data, !(tolower(Beer) == "test"))

# Convert the Timepoint into an R readable time format
# Replace PST with the time zone the data was recorded in.
# Tilt stores dates as Julian Dates in raw (object oriented programming)
beer_data$Timepoint <- 
  ymd_hms("1899-12-30 00:00:00", tz = "PST") + # Google Sheets Origin date/time
  as.numeric(as.character(beer_data$Timepoint)) * 86400 # Conversion from decimal day to second

# Convert Color and Beer name to Factor
beer_data$Color <- factor(beer_data$Color)
beer_data$Beer <- factor(beer_data$Beer)

# Convert SG/Temp to numeric
beer_data$SG <- as.numeric(as.character(beer_data$SG))
beer_data$Temp <- as.numeric(as.character(beer_data$Temp))

# Split Dataframes --------------------------------------------------------
# Create a list of dataframes by factor.
# If you have brewed the same name beer multiple times all brews will get
# stored in the same output file. If you have an extensive backlog of data I
# suggest downloading it all and starting fresh. Alternatively, use this to
# parse the data into beer name groups and manually divide those groups into
# their own datasets. I experimented with methods of trying to do this
# automatically but it is dependent on too many individual factors to make the
# process reliable.
beer_split <- split(beer_data, beer_data$Beer)

# Export Dataframes -------------------------------------------------------
for(i in 1:length(beer_split)){
  write.csv(beer_split[i], 
            file = paste0(directory, format(beer_split[[i]][1, 1], format = "%Y-%m"), "_", names(beer_split[i])),
            row.names = FALSE)
}

# Clean up GSheets --------------------------------------------------------
# Now we will dump all the rows in GSheets into the garbage bin. I generally
# suggest opening the csv file(s) created above and inspecting them before
# wholesale slaughter of raw data. Basically what this does is replace the
# second row with an empty string. Then it trims everything that isn't that row
# and below out of the sheet. This leave the top row intact so the Tilt app can
# still find and propogate the table. The first row will stay empty but that
# isn't really a problem. This is wrapped in an if statement with menu so there
# is a console prompt to make sure people really want to perform this action.
if(menu(c("Delete all Tilt data!", "Leave my data alone!")) == 1){
  gs_edit_cells(metadata, ws = "Sheet1", input = matrix("", ncol = length(names(beer_data)), nrow = 1), anchor = "A2", trim = TRUE, verbose = TRUE)
  print("Tilt data cleared.")
} else {
  print("Tilt data unmodified.")
  }
