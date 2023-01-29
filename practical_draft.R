
# Install packages ------------------------------------

# Start with (installing and) 
# loading the packages that I will use

# TO DO: find the if(require)...install ...

library(tidyverse) # see in console that it loads a number of packages (and warns for conflicts)
library(here) # To work with relative (to the script location) paths (no more setwd() ... !)

here() # prints path to current working directory


# Import data --------------------------------

# Note that:
#  - "./in/" for raw/original data ONLY
#  - after manipulations, save to "./out/"
#  - we're using a data set from {palmerpenguins} that I modified for use 
#    in this workshop (more on this later) 
#     -> technically, it's not the original data (-> should not be in "in")
#    but let's pretend that it raw data for the sake of the demonstration
raw <- read_csv("./in/XXXXXX.csv")

# Note that it shows wha type of data we have (variable date type)

meta # <- think of/make up something 



# Explore the data set -------------------------------
summary(raw)
structure(raw)
view(raw) 
raw # prints in Console (or Ctrl + right-click to open in View)
    # see col type (chr, dbl, date, ... )
head(raw)

# mention {janitor} but let's leave it out for now...

# Check 





