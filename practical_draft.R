
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
raw <- read_csv("./palmer_rawdata.csv")
      # Note that it shows what type of data we have (variable date type)

meta # <- think of/make up something 



# Explore the data set -------------------------------

# first we want to just look at our data and check if everything is alright (it never is ... !)

## Quick look ------------------------------

raw # prints in Console (or Ctrl + right-click to open in View)
    # see col type (chr, dbl, date, ... )
view(raw) # can scroll, but not ideal to have to scroll 100s of lines ...
head(raw)
summary(raw)
dplyr::glimpse(raw) # (tidyverse) to see glimpse (head) of var "horizontally"

# mention {janitor} but let's leave it out for now...


## Check replication ---------------------------

# This implies a previous knowledge of the data set and the structure
# (how many species, how many observations per study, nr of individuals, ...)

# Nr of studies
raw$studyName %>% unique() # ok correct "PAL0708" "PAL0809" "PAL0910" 

# Nr of species 
raw$Species %>% unique()
  # Something wrong here ... !

# Find what's wrong 
raw %>% 
  filter(Species == "Gentoo") # just to view

# Easy fix 
raw$Species <- recode(raw$Species, "Gentoo" = "Gentoo penguin (Pygoscelis papua)")
raw$Species %>% unique() # all good now


# Check if sample number matches with 





