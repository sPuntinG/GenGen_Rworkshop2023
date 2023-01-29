library(tidyverse)
library(here)


# {palmerpenguins} ====================================

# install.packages("palmerpenguins")
# library(palmerpenguins) # to load, BUT no need if you use namespace ;)

palmerpenguins::penguins
raw <- palmerpenguins::penguins_raw


# To do on this data set: 

# Make col names easier to work with (replace space and non alpha num characters) --------------------------------

# Use to test regex
names(raw) %>% str_view_all(., "o/oo")


names(raw) <- str_replace_all(names(raw), " ", "_")
names(raw) <- str_replace_all(names(raw), "\\(|\\)", "")
names(raw) <- str_replace_all(names(raw), "_o/oo", "")
names(raw)


# Explore 

## studyName -----------------------

raw$studyName %>% length() # 344
raw$studyName %>% unique() #  "PAL0708" "PAL0809" "PAL0910" Data comes from 3 studies
raw$studyName %>% unique() %>% length() # 3

raw %>% 
  group_by(studyName) %>% 
  summarise(spp = unique(Species)) #%>% view()
# 1 PAL0708   Adelie Penguin (Pygoscelis adeliae)      
# 2 PAL0708   Gentoo penguin (Pygoscelis papua)        
# 3 PAL0708   Chinstrap penguin (Pygoscelis antarctica)
# 4 PAL0809   Adelie Penguin (Pygoscelis adeliae)      
# 5 PAL0809   Gentoo penguin (Pygoscelis papua)        
# 6 PAL0809   Chinstrap penguin (Pygoscelis antarctica)
# 7 PAL0910   Adelie Penguin (Pygoscelis adeliae)      
# 8 PAL0910   Gentoo penguin (Pygoscelis papua)        
# 9 PAL0910   Chinstrap penguin (Pygoscelis antarctica)

# Each study looked at all 3 spp


## Region --------------------------------------

# Most boring info

raw$Region %>% length() # 344
raw$Region %>% unique() #  "Anvers"
raw$Region %>% unique() %>% length() # 1 



## Island --------------------------------------

# this is more interesting

raw$Island %>% length() # 344
raw$Island %>% unique() #  "Torgersen" "Biscoe"    "Dream"
raw$Island %>% unique() %>% length() # 3


## Stage --------------------------------------

# also very boring, only one unique value

raw$Stage %>% length() # 344
raw$Stage %>% unique() #  "Adult, 1 Egg Stage"
raw$Stage %>% unique() %>% length() # 1


# Individual_ID ----------------------

# More interesting! 190 individuals

raw$Individual_ID %>% length() # 344
raw$Individual_ID %>% unique() # many!
raw$Individual_ID %>% unique() %>% length() # 190

raw$Individual_ID %>% unique() %>% nchar() %>% range() # 4 6
                                                   # names vary from 4 to 6 characters

# Look for patterns
uniqueID <- raw$Individual_ID %>% unique() 
str_view_all(uniqueID, "[:alpha:]") # good but not great for many entries (doesn't show all)
uniqueID %>% str_extract_all(., "[:alpha:]") %>% unique() %>% .[[1]]
uniqueID %>% str_extract_all(., "[:alpha:]") # not what I want (this is a selector function)
uniqueID %>% str_match_all(., "[:alpha:]") %>% unique()

# N and A must have a meaning ... 
# TO DO: use this to practice extracting info from single string!