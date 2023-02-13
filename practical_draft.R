
# Install packages ------------------------------------

# Start with (installing and) 
# loading the packages that I will use

# TO DO: find the if(require)...install ...

library(tidyverse) # see in console that it loads a number of packages (and warns for conflicts)
library(here) # To work with relative (to the script location) paths (no more setwd() ... !)

here() # prints path to current working directory


# Import data (palmer_rawdata) --------------------------------

# Note that:
#  - "./in/" for raw/original data ONLY
#  - after manipulations, save to "./out/"
#  - we're using a data set from {palmerpenguins} that I modified for use 
#    in this workshop (more on this later) 
#     -> technically, it's not the original data (-> should not be in "in")
#    but let's pretend that it raw data for the sake of the demonstration ;)
raw <- read_csv("./in/palmer_rawdata.csv")
      # Note that it shows what type of data we have (variable date type)



# Explore the data set -------------------------------

# first we want to just look at our data and check if everything is alright (it never is ... !)

## Quick look ------------------------------

raw # prints in Console (or Ctrl + right-click to open in View)
    # see col type (chr, dbl, date, ... )
head(raw)
summary(raw)
dplyr::glimpse(raw) # (tidyverse) to see glimpse (head) of var "horizontally"
view(raw) # can scroll, but not ideal to have to scroll 100s of lines ...
          # can also directly filter!

names(raw) # get all var names (handy for copy pasting)
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

# TO DO MAYBE Think of replacing 2 missing values, of which we known that one is one species and the other is another species



## Check nr of replicates per study (use group_by() + summarize() ) ------------------------

# Data was collected in 3 separate studies
# I want to see how many samples ("Sample_Number") where taken per study ("studyName")
raw %>% 
  group_by(Study_Name) %>% 
  summarise(nr_of_samples = length(Sample_Number)) %>% 
  view()
 # see that there's much less samples from the last study (PAL0910)

# Now check nr of samples by study and penguin spp 
raw %>% 
  group_by(Study_Name, Species) %>%  # Note that order defines hierarchy
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
# This is because (in our hypothetical scenario) we are working on the data that
#  was available, but now our colleagues sent us an updated version of the 
#  data table for the last study, which is called "PAL0910.csv"


# Join data from separate file --------------------------

## Import new data ----------------
PAL0910 <- read_csv("./in/PAL0910.csv")

nrow(PAL0910) # 120 rows (also see from 'Environment')
ncol(PAL0910) # 13 cols 


## Before joining: Check -----------

### if same nr of cols ------------
ncol(PAL0910) == ncol(raw) # TRUE

### if variable have the same names ------------

names(PAL0910) == names(raw) # looks like name of 1st var of PAL0910 doesn't match w/ same in raw

names(PAL0910)[1] 
names(raw)[1]

# Let's rename it
PAL0910 <- PAL0910 %>% 
  rename(Study_Name = studyName)

names(PAL0910) == names(raw) # now we can join
# Note: You don't need ALL variables to be found in both tibbles! 
#  But in this case is convenient ...
 


## Join ---------------------

raw2 <- full_join(raw, PAL0910) 
# Note that full_join() automatically identifies cols with same name (if none it wil tell you and you can specify it manually)
# Note that "raw2" has 104 more rows than "raw" (344 - 240)



## Other uses of `*_join()`: keep only "new" (non-matching) values --------------

# Rows of PAL0910 that were NOT present in raw
# (a way to double check that the merging is correct: e.g. it didn't create duplicated values)
new_data <- anti_join(PAL0910, raw) #%>% view()

nrow(new_data) # 104 (matches with previous comment)


rm(raw)



# Work with "character" data (string manipulation) -------------------------------

# Let's focus on the variable "Individual_ID" (unique penguin id code)

## Some (base R) functions that I find useful ---------------------

# overview of values in this var
raw2$Individual_ID %>% unique() # roughly see what it looks like
raw2$Individual_ID %>% unique() %>% length() # how many unique values? 191

# How long are these ID names? (number of characters of each ID)?
raw2$Individual_ID %>% nchar() # see al lot of 4's and 5's, but hard to spot other numbers ...
raw2$Individual_ID %>% nchar() %>% unique() # 4 5 6
raw2$Individual_ID %>% nchar() %>% range() # 4 6


## Look for patterns in character strings with {stringr} ------------------------

raw2$Individual_ID %>% unique()
# From this ^ we could sense that names have and alphanumeric pattern: "N*A*"
#  so let's explore this systematically ...

# Use {stringr} to extract (and check) patterns in the ID names

## View (in "Viewer" pane): great to just see (without manipulating) ------------------
# I also like this a lot to test out my regex (did I formulate the query correctly?)
#                 (this example is easy but regex can get complicated)
str_view_all(raw2$Individual_ID, "[:alpha:]")
   # good but not great for many entries (doesn't show all)

raw2 %>% 
  mutate(Individual_ID_letters = str_extract_all(Individual_ID, "[:alpha:]")) %>% 
  view()

# Looks like all "Individual_ID contain N and A,
#  but let's check if they are all the same
raw2 %>% 
  mutate(Individual_ID_letters = str_extract_all(Individual_ID, "[:alpha:]")) %>% 
  pull(Individual_ID_letters) %>% unique()  # creates a list of 2 vectors
  # Looks like one values has an extra A ...

# Inspect: filter() & str_detect()
raw2 %>% 
  filter(stringr::str_detect(Individual_ID, "AA"))

# Look at it in the context of the table (see with previous and following values)
# use "x" to flag the rows that I want to inspect (make scrolling easier)
raw2 %>% 
  mutate(ID_tocheck = ifelse(stringr::str_detect(Individual_ID, "AA"), "x", "")) %>% 
  view() # Looks like someone accidentally pressed "A" twice ...
   

## Fix typo in character string with `str_replace()` -------------------------------

# Note: we could have also used recode() as we did before:
# `raw2$Individual_ID <- recode(raw2$Individual_ID, "N11AA1" = "N11A1")` 

raw3 <- raw2 %>% 
  mutate(
    Individual_ID = stringr::str_replace(Individual_ID, "AA", "A")
  ) 

# Check if "AA" still present in tibble:
raw3 %>% 
  filter(stringr::str_detect(Individual_ID, "AA")) # A tibble: 0 x 13 ... empty = good!



# Create new (character) variables ---------------------------

# Extract info from Individual_ID as "N_nr" and "A_nr" (keep only nr that follow N and A)

# Use str_view() to test regex
stringr::str_view_all(raw3$Individual_ID[1], "^[:alpha:]{1}[:digit:]{1,2}") # keep N + nr
stringr::str_view_all(raw3$Individual_ID[1], "N[:digit:]{1,2}")         # same as above
stringr::str_view_all(raw3$Individual_ID[111], "(?<=N)[:digit:]{1,}") # >=1 digits preceded by "N"

raw3 <- raw3 %>% 
  mutate(
    N_nr = str_extract(Individual_ID, "(?<=N)[:digit:]{1,}"),
    A_nr = str_extract(Individual_ID, "(?<=A)[:digit:]{1,}")
  ) %>% 
  relocate(N_nr, .after = Individual_ID) %>% 
  relocate(A_nr, .after = N_nr) # %>% view()



# Filter() with %in% -----------------

# Let's say we want to only keep observations where N_nr matches a string of values
#  e.g., only when = to 1, 21, 47, 96 and 99 (for some hypothetical reason)

N_to_keep = c(1, 21, 47, 96, 99)

raw3_Ntokeep <- raw3 %>% 
  filter(N_nr %in% N_to_keep)



# Mutate() + case_when() paste() & {stringr} -------------------

# Current "Species' var is a bit long and unpractical ... let's extract info as
#  new cols:
#  - "Vernacular" = colloquial spp names
#  - "Scientific" = just the scientific name

## use `case_when()` to add Vernacular name var --------------------
raw3 <- raw3 %>% 
  mutate(
    Vernacular = case_when(
      str_detect(Species, "Gen") ~ "Gentoo",
      str_detect(Species, "Chin") ~ "Chinstrap",
      str_detect(Species, "Ade") ~ "Adelie")
      ) %>% 
  relocate(Vernacular, .after = Species) %>% 
  view()

## use `str_extract()` to extract text between parenthesis to create scientific name var ---------------------
stringr::str_view_all(raw3$Species[1], "(?<=\\()[:alpha:]+\\s[:alpha:]+") # after "(", letters separated by space 

raw3 <- raw3 %>% 
  mutate(
    Scientific = str_extract_all(Species, "(?<=\\()[:alpha:]+\\s[:alpha:]+")
  ) %>% 
  relocate(Scientific, .after = Vernacular) %>% 
  view()


# Add short name var
raw3 %>% 
  mutate(
    Ver_short = str_extract(Vernacular, "^\\w")
  ) %>% view()



## use `paste()` to append spp info to Individual_ID -------------------

# Note that Individual_ID is not unique: same ID given tp != spp!
# see:
raw3 %>% 
  group_by(Individual_ID) %>% 
  summarise(spp = unique(Species))

raw3 %>% 
  group_by(Individual_ID) %>% 
  summarise(spp = length(unique(Species)))

# => Let's make it unique by appending the first 3 letters of "Vernacular" to "Individual_ID"
raw3 <- raw3 %>% 
  mutate(Individual_ID_spp = paste(Individual_ID, str_extract(Vernacular, "^[:alpha:]{3}"), sep = "_")) %>% 
  relocate(Individual_ID_spp, .after = Individual_ID) # %>% view()

# Check if "Individual_ID_spp" has only unique values
raw3 %>% 
  group_by(Individual_ID_spp) %>% 
  summarise(spp = length(unique(Species))) %>% 
  filter(spp > 1) # good :)




# Add info from Excel table: "Comments.xlsx"  ------------------------


## Import excel table -----------------------------------

library(readxl)
comments <- readxl::read_excel("./in/Palmer_comments.xlsx") # {readxl} not part of tidyverse


# RESUME FROM HERE (since 03 Feb) ==================================




# Merge to our data --------------------------
data <- left_join(raw, comments, by = c("studyName", "Sample_Number", "Species", "Individual_ID"))

# NOTE: data is wide! Which is annoying ... so make long!


# Pivot "long" (instead of "wide" format of "comments") --------------------------

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





# ggplot() --------------------------------------------------

## Works with layers -------------------

# Build up plot ... (basic plot is easier in base R but ggplot blows minds for complex designs)



## Exploratory data viz: geom_point() to check recorded observations -----------------------



## Explanatory data viz: scatter plot with correlation, box plots ... --------------------------



## Explanatory data viz2: small multiples with overall comparison --------------------------





