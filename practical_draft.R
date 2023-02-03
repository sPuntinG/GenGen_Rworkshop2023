
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
raw <- read_csv("./in/palmer_rawdata.csv")
      # Note that it shows what type of data we have (variable date type)


# library(readxl)
# meta <- readxl::read_excel("./comments.xlsx") # {readxl} not part of tidyverse



# Explore the data set -------------------------------

# first we want to just look at our data and check if everything is alright (it never is ... !)

## Quick look ------------------------------

raw # prints in Console (or Ctrl + right-click to open in View)
    # see col type (chr, dbl, date, ... )
view(raw) # can scroll, but not ideal to have to scroll 100s of lines ...
head(raw)
summary(raw)
dplyr::glimpse(raw) # (tidyverse) to see glimpse (head) of var "horizontally"


names(raw) 
# "studyName" is not intuitive for me (!= format) -> I want to rename this variable



# Rename variable -------------------------------
raw <- raw %>% # don't forget to assign to (same) object! (common mistake)
  rename(Study_Name = studyName) %>% view()



## Check replication ---------------------------

# This implies a previous knowledge of the data set and the structure
# (how many species, how many observations per study, nr of individuals, ...)

## Nr of studies -----------------
raw$Study_Name %>% unique() # ok correct "PAL0708" "PAL0809" "PAL0910" 

## Nr of species -----------------
raw$Species %>% unique()
  # Something wrong here ... !
  # Needs to be fixed (see next section)

# Inspect wrong spp value
raw %>% 
  filter(Species == "Gentoo") # just to view

# Fix (obvious) mistake in entry -----------------------

## Easy way (base R) ---------------
raw$Species <- recode(raw$Species, "Gentoo" = "Gentoo penguin (Pygoscelis papua)")

# Check again 
raw$Species %>% unique() # all good

# # Unsafe because if we have more than one "Gentoo" it will replace all 
# # But we might want to make this more specific ...
# 
# # NOT SURE I WANT TO GO THROUGH THIS ROAD



## Check nr of replicates per study (use group_by() + summarize() ) ------------------------

# Data was collected in 3 separate studies
# I want to see how many samples ("Sample_Number") where taken per study ("studyName")
raw %>% 
  group_by(Study_Name) %>% 
  summarise(nr_of_samples = length(Sample_Number)) %>% 
  view()
 # see that there's much less samples from the last study (PAL0910)

# Now check nr of samples by penguin spp 
raw %>% 
  group_by(Study_Name, Species) %>% 
  summarise(nr_of_samples = length(Sample_Number)) %>% 
  view()
  # last study (PAL0910) also lacks one spp ...
  # we can also directly check nr of spp per study
raw %>% 
  group_by(Study_Name) %>% 
  summarise(nr_of_species = length(unique(Species))) %>% # note `unique()`
  view()
  # now it's clear that one species is missing from last study 

# Also: check if any study has < 3 spp 
raw %>% 
  group_by(Study_Name) %>% 
  summarise(nr_of_species = length(unique(Species))) %>% # note `unique()`
  filter(nr_of_species < 3) %>%  # add this filter
  view()

  
# looks like last study is incomplete ... 
# This is because (in our hypothetical scenario) we are working on the data 
#  was available, but now our colleagues sent us an updated version of the 
#  data table for the last study, which is called "PAL0910.csv"

# Join data from different file --------------------------

## Import new data ----------------
PAL0910 <- read_csv("./in/PAL0910.csv")

## Join ---------------------

raw2 <- left_join(raw, PAL0910)

# Note that "raw2" has the same nr of rows as "raw", but one extra col
#  let's check why is that 
#  (answer: non-matching col name studyName != Study_Name)

# What to do with "studyName"
# Could remove, but let's see how to quickly chack if
#  values from two variables match (in this case "Study_Name" with "studyName")
raw2 %>% 
  mutate(matching = ifelse(studyName == Study_Name, "Yes", "Nope")) %>% 
  relocate(studyName, .after = Study_Name) %>% 
  relocate(matching, .after = studyName) %>% 
  view()
           
# Ok, can drop raw$studyName as it doesn't have any "new" info
raw2 <- raw2 %>% 
  select(-studyName)


rm(raw)



## Individual ID --------------------
raw2$Individual_ID %>% unique() # roughly see what it looks like
raw2$Individual_ID %>% unique() %>% length() # how many unique values? 155

# How long are these ID names? (number of characters of each ID)?
raw2$Individual_ID %>% nchar() # see al lot of 4's and 5's, but hard to spot other numbers ...
raw2$Individual_ID %>% nchar() %>% unique() # 4 5 6
raw2$Individual_ID %>% nchar() %>% range() # 4 6

# Look for patterns in character strings  with {stringr} ------------------------

# Use {stringr} to extract (and check) patterns in the ID names

## View (in "Viewer" pane) ------------------
str_view_all(raw2$Individual_ID, "[:alpha:]")
   # good but not great for many entries (doesn't show all)

raw2 %>% 
  mutate(Individual_ID_letters = str_extract_all(Individual_ID, "[:alpha:]")) %>% 
  view()
  
# raw2 %>% 
#   mutate(Individual_ID_letters = str_match_all(Individual_ID, "[:alpha:]")) %>% 
#   view()

# Looks like all "Individual_ID contain N and A,
#  but let's check if all the same

raw2 %>% 
  mutate(Individual_ID_letters = str_extract_all(Individual_ID, "[:alpha:]")) %>% 
  pull(Individual_ID_letters) %>% unique() 
  # Looks like one valus has an extra A ...









# RESUME FROM HERE (since 03 Feb) ==================================



# We could (and should) check every variable but for the sake of time we stop here ...



### Easy fix ------------------
raw$Species <- recode(raw$Species, "Gentoo" = "Gentoo penguin (Pygoscelis papua)")
raw$Species %>% unique() # all good now


# More robust fix (based on conditions) 












# Add info from Excel table "Comments.xlsx"  ------------------------


## Import excel table -----------------------------------

library(readxl)
# readxl::excel_format("./comments.xlsx")
comments <- readxl::read_excel("./in/Palmer_comments.xlsx") # {readxl} not part of tidyverse





# Merge to our data --------------------------
data <- left_join(raw, comments, by = c("studyName", "Sample_Number", "Species", "Individual_ID"))

# NOTE: data is wide! WHich is annoying ... so make long!


# Pivot comments to make "long" (instead of "wide") --------------------------

comments_long <- comments %>% 
  pivot_longer(
    cols = -c(1:4),
    names_to = "Comments",
    values_to = "Comments_value"
  ) %>% 
  # filter(!is.na(Comments_value)) %>%  # doesn't work because is character!
  filter(Comments_value != "NA") %>% 
  select(-Comments_value) %>% view()

# Remove "comments" since we don't need it anymore
rm(comments)


