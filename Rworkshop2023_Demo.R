
# Install packages ------------------------------------

# Start with #(installing and) 
# loading the packages that I will use

# if (!require("tidyverse")) install.packages("tidyverse")
# if (!require("here")) install.packages("here")
library(tidyverse) # see in console that it loads a number of packages (and warns for conflicts)
library(here) # To work with relative (to the script location) paths (no more setwd() ... !)

here() # prints path to current working directory


# Create directory for output files --------------------
#  To keep output separated from the original/raw data
dir.create(here("out"))


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

# Note: be careful with this: it will replace all "Gentoo" 



## Check nr of replicates per study (use `group_by()` + `summarize()` ) ------------------------

# Data was collected in 3 separate studies
# I want to see how many samples ("Sample_Number") where taken per study ("Study_Name")
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
  # summarise(nr_of_species = length(unique(Species))) %>% # note `unique()` 
  summarise(nr_of_species = n_distinct(Species)) %>%
  view()
  # now it's clear that one species is missing from last study 

# Also: check if any study has < 3 spp 
raw %>% 
  group_by(Study_Name) %>% 
  summarise(nr_of_species = n_distinct(Species)) %>% # note `unique()`
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

raw %>% 
  filter(Study_Name == "PAL0910") %>% 
  nrow() # 16 rows 


## Before joining: Check -----------

### if same nr of cols ------------
ncol(PAL0910) == ncol(raw) # TRUE

### if variable have the same names ------------

names(PAL0910) == names(raw) # looks like name of 1st var of PAL0910 doesn't match w/ same in raw

names(PAL0910)[1] 
names(raw)[1]



## full_join: keep everything ---------------------

raw2 <- full_join(raw, PAL0910, by = c("Study_Name" = "studyName", # since differnt var name we need to manually tell whic one corresponds to which
                                       "Sample_Number", "Species", "Island", "Individual_ID", "Date_Egg", "Culmen_Length_mm",
                                         "Culmen_Depth_mm", "Flipper_Length_mm", "Body_Mass_g", "Sex", "Delta_15_N", "Delta_13_C")) 
# Note that full_join() automatically identifies cols with same name (if none it wil tell you and you can specify it manually)
# Note that "raw2" has 104 more rows than "raw" (344 - 240)



## anti_join(): keep only "new" (non-matching) values --------------

# Rows of PAL0910 that were NOT present in raw
# (a way to double check that the merging is correct: e.g. it didn't create duplicated values)
latest_data <- anti_join(PAL0910, raw) #%>% view()

nrow(latest_data) # 104 (matches with previous comment)


rm(raw)



# Work with "character" data (string manipulation) -------------------------------

# Let's focus on the variable "Individual_ID" (unique penguin id code)


## Some (base R) functions that I find useful ---------------------

# overview of values in this var
raw2$Individual_ID %>% unique() # roughly see what it looks like
raw2$Individual_ID %>% unique() %>% length() # how many unique values? 191
raw2$Individual_ID %>% n_distinct() 

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



## Extract info from string -------------------------------

# Extract info from Individual_ID as "N_nr" and "A_nr" (keep only nr that follow N and A)

# Use str_view() to test regex
stringr::str_view_all(raw3$Individual_ID[111], "^[:alpha:]{1}[:digit:]{1,2}") # keep N + nr
# stringr::str_view_all(raw3$Individual_ID[111], "N[:digit:]{1,2}")         # same as above, keep N + nr
stringr::str_view_all(raw3$Individual_ID[111], "(?<=N)[:digit:]{1,}") # >=1 digits preceded by "N"

raw3 <- raw3 %>% 
  mutate(
    N_nr = str_extract(Individual_ID, "(?<=N)[:digit:]{1,}"),
    A_nr = str_extract(Individual_ID, "(?<=A)[:digit:]{1,}")
  ) %>% 
  relocate(N_nr, .after = Individual_ID) %>% 
  relocate(A_nr, .after = N_nr)  #%>% view()



# Filter() with %in% -----------------

# Can filter using boolean operators (multiple conditions)
raw3 %>% 
  filter(N_nr == 1 & A_nr == 1 | A_nr == 2) %>% 
  view()

# But in some cases I want to filter based on values of just one variable 
#  but using a long list of values, for example,
#  let's say we want to only keep observations where N_nr matches any element of
#  a list/string of values
#  such as 1, 21, 37, 47, 96 and 99 (for some hypothetical reason)

N_to_keep <- c(1, 21, 37, 47, 96, 99)

raw3_Ntokeep <- raw3 %>% 
  filter(N_nr %in% N_to_keep)



# Mutate() + case_when() paste() & {stringr} -------------------

# Current "Species" var is a bit long and unpractical ... let's extract info as
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



## use `paste()` to append spp info to Individual_ID -------------------

# Note that Individual_ID is not unique: same ID given tp != spp!
# see:
raw3 %>% 
  group_by(Individual_ID) %>% 
  summarise(spp = unique(Species))

raw3 %>% 
  group_by(Individual_ID) %>% 
  summarise(spp = n_distinct(Species))

# => Let's make it unique by appending the first 3 letters of "Vernacular" to "Individual_ID"
raw3 <- raw3 %>% 
  mutate(Individual_ID_spp = paste(Individual_ID, str_extract(Vernacular, "^[:alpha:]{3}"), sep = "_")) %>% 
  relocate(Individual_ID_spp, .after = Individual_ID) # %>% view()

# Check if "Individual_ID_spp" has only unique values
raw3 %>% 
  group_by(Individual_ID_spp) %>% 
  summarise(spp = n_distinct(Species)) %>% 
  filter(spp > 1) # good :)


# Split cells (genus-/-species) ---------------------------
raw3 %>% 
  tidyr::separate(Scientific, 
                  into = c("Scientific_Genus", "Scientific_species"), # names of new vars
                  sep = " ", # define separator
                  remove = F) %>% # to keep var "Species"
  view()
  



# Add info from Excel table: "Comments.xlsx"  ------------------------

# This is an excel sheet that contains comments about individual penguins
#  about sample collection or behavior, where info was recorded by "ticking"
#  the appropriate box (mark with an x when comment applies to penguin)

# Open in excel to see ...
# ... then import in R


## Import excel table -----------------------------------

library(readxl)

comments <- readxl::read_excel("./in/Palmer_comments.xlsx") # {readxl} not part of tidyverse

# Note that the table has a "wide" format, which is unpractical
#  -> make it "long"




# Pivot "long" (instead of "wide" format of "comments") --------------------------

# First pivot long
comments_long <- comments %>% 
  pivot_longer(
    cols = -c(1:4),
    names_to = "Comments",
    values_to = "Comments_value"
  ) 

# Now see that we have more rows than we need: 
#    NA's can be removed (just say that the comment doesn't apply to the respective row)
comments_long <- comments_long %>% 
  filter(!is.na(Comments_value)) %>% #view() # Note: filter with negation ("!") to leave out rows with NA
  select(-Comments_value) #%>% view() # No need this col anymore




# Merge to our data --------------------------
raw4 <- left_join(raw3, comments_long, 
                  by = c("Study_Name" = "studyName", # cols with different names (but same type of data)
                         "Sample_Number", "Species", "Individual_ID"))

# Remove "comments" since we don't need it anymore
rm(comments)
rm(raw2, raw3, raw3_Ntokeep)



# Export summary table -----------------------------------------------

# Now we're done with edits and checks on our data and we want to export it
# ... as it is
write_csv(raw4, "./out/raw4.csv")

# ... as summary of certain parameters: i.e.,
#  mean of each numeric var, grouped by Species, Sex and Island
raw4 %>% 
  group_by(Species, Sex, Island) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  select(-Sample_Number) %>%   # exclude because doesn't meke sense to calc. mean of this
  arrange(desc(Species)) %>% # use to arrange display (in this case Male/Female) 
  view() %>% 
  write_csv(., "./out/raw4_means.csv")
  




# Plot with `ggplot()` --------------------------------------------------


## Comparison with base R plots --------------

#  Basic plots are easier in base R ...  

# Base R
plot(raw4$Culmen_Length_mm, raw4$Culmen_Depth_mm) # I don't know why I get this error ...

# GGplot 
ggplot(data = raw4, aes(x = Culmen_Length_mm,
                        y = Culmen_Depth_mm)) +
  geom_point()

# ... but ggplot blows minds for complex designs


### Building up a ggplot step by step (use layers) ------------------

# Canvas only
ggplot(data = raw4, aes(x = Culmen_Length_mm,
                        y = Culmen_Depth_mm)) 

# Canvas + points (scatter plot) 
ggplot(data = raw4, aes(x = Culmen_Length_mm,
                        y = Culmen_Depth_mm)) +
  geom_point()

# Canvas + points (scatter plot) + color by factor
ggplot(data = raw4, aes(x = Culmen_Length_mm,
                        y = Culmen_Depth_mm)) +
  geom_point(aes(color = Species))

# Add correlation lines
ggplot(data = raw4, aes(x = Culmen_Length_mm,
                        y = Culmen_Depth_mm)) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species),
              method = "lm",
              ormula = 'y ~ x')

# Change theme to look nicer
ggplot(data = raw4, aes(x = Culmen_Length_mm,
                        y = Culmen_Depth_mm)) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species),
              method = "lm",
              formula = 'y ~ x',
              se = F) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

# Faceting (small multiples)
ggplot(data = raw4, aes(x = Culmen_Length_mm,
                        y = Culmen_Depth_mm)) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species),
              method = "lm",
              formula = 'y ~ x',
              se = F) +
  theme_bw() + 
  theme(
    legend.position = "bottom"
  ) +
  facet_wrap(~Species)


# Faceting (small multiples) with overall 
#  (cool example of power of working with layers!)
# Note that we need a 2nd data (without factor)
raw4_nofactor <- raw4 %>% 
  select(Culmen_Length_mm, Culmen_Depth_mm)

ggplot() +
  geom_point(data = raw4_nofactor, 
             aes(x = Culmen_Length_mm,
                 y = Culmen_Depth_mm),
             color = "grey85") +
  geom_point(data = raw4, 
             aes(x = Culmen_Length_mm,
                 y = Culmen_Depth_mm,
                 color = Species)) +
  geom_smooth(data = raw4,
              aes(x = Culmen_Length_mm,
                  y = Culmen_Depth_mm,
                  color = Species),
              method = "lm",
              formula = 'y ~ x',
              # formula = 'y ~ poly(x, 2)', # for 2nd degree polynomial regression (just FYI)
              se = F) +
  # coord_fixed(ratio = 1) + # for x and y to have same ratio
  theme_bw() + 
  theme(
    legend.position = "bottom"
    # aspect.ratio = 1 # dimension of the plot (as ration of y and x axis size)
  ) +
  facet_wrap(~Species)



## Exporting figures with `ggsave()` ------------------------------

# Best way to ensure reproducible plots in terms of size
#  (you can also use "Export" from the Plots pane but then need to note down the size used)

ggsave(# _plot name_ if you have it, else it will save the last one printed 
       filename = "./out/pengiuins_smallmultiples.png", 
       dpi = 300, # for publications  
       units = "cm", 
       width = 23, height = 13,
       bg = "white")




## Stats directly in ggplot with `stat_summary()` ------------------------------------------

# Median, mean, CI ... can also be calculated and plotted in one-go 
#  (as opposed to first calculate and then inject into ggplot)
#  (e.g., from before: `geom_smooth()` but can also do more!)

ggplot(data = raw4, aes(x = Sex,
                        y = Body_Mass_g,
                        color = Species)) +
  geom_jitter(width = 0.2) +
  stat_summary(
    aes(group = Species),
    fun = mean,
    fun.min = function(x) mean(x) - sd(x), 
    fun.max = function(x) mean(x) + sd(x), 
    geom = "errorbar",
    width = 0,
    size = 1,
    color = "black") +
  stat_summary(
    aes(fill = Species),
    fun = mean,
    geom = "point",
    shape = 21, # 23
    size = 5,
    stroke = 1.5,
    color = "black") +
  theme_bw() + 
  theme(
    legend.position = "bottom"
  ) +
  facet_wrap(~Species)












