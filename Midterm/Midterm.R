# Make sure to restart your R session before running this script
# Click on Session -> Restart R

# Set seed for reproducibility
set.seed(123)

# Load the MASS library
library(MASS)
# Load the tidyverse library
library(tidyverse)
# Load the boot library
library(boot)
# Load the leaps library
library(leaps)

# Read the dataset from the URL
df <- read.csv("https://raw.githubusercontent.com/JaySquare87/DTA395-1Rollins/main/Midterm/songs_normalize.csv", header = TRUE, sep = ",")

# Create a data frame called dfNumeric
# This data frame contains all the columns of type numeric
dfNumeric <- df |>
  select(where(is.numeric))

# Create a data frame called dfArtist
# This data frame contains all rows where the artist is either
# Eminem or Britney Spears
dfArtist <- df |> filter(artist %in% c("Eminem", "Britney Spears"))
dfArtist$artist <- factor(dfArtist$artist)

# Your code starts here...


