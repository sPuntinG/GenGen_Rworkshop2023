library(tidyverse)
library(here)


# {palmerpenguins} ====================================

# install.packages("palmerpenguins")
# library(palmerpenguins) # to load, BUT no need if you use namespace ;)
raw <- palmerpenguins::penguins_raw


# Get info about "penguins_raw" --------------------------------------------

?penguins_raw
# A tibble with 344 rows and 17 variables:






# Make dummy date set ---------------------------

# Make col names easier to work with (replace space and non alpha num characters) --------------------------------

names(raw) <- str_replace_all(names(raw), " ", "_")
names(raw) <- str_replace_all(names(raw), "\\(|\\)", "")
names(raw) <- str_replace_all(names(raw), "_o/oo", "")
names(raw)




# Make comments wide and save as excel and remove from "raw" --------------------------
raw$Comments %>% unique() # only 11 values 

comments <- raw %>% 
  pivot_wider(
    names_from = Comments,
    values_from = Comments
    # values_fn = ~ mean(.x, na.rm = TRUE)
    # values_fn = ~if_else(is.na(), NA, "x")
  ) %>% 
  mutate_at(c(17:27), ~ifelse(is.na(.), "NA", "x")) %>% 
  select(-"NA") %>% 
    select(studyName, Sample_Number, Species, Individual_ID, c(17:26)) %>% 
  # mutate_at(c(5:14), ~ifelse(. == "NA", 0, 1)) %>% 
  view() 


# install.packages("writexl")
writexl::write_xlsx(comments, "./Palmer_comments.xlsx")




# Remove useless vars ------------------
raw <- raw %>% 
  select(-c(Region, Stage, Clutch_Completion, Comments))


# Simulate incomplete data set from last study (PAL0910) ------------------
raw$studyName %>% unique()

# Subset so I can work on this ...
PAL0910 <- raw %>% 
  filter(studyName == "PAL0910") %>% 
  # rename(Study_Name = studyName) %>% 
  write_csv("./PAL0910.csv")

# ... to drop a big part of the readings (so that it looks incomplete)
to_remove <- PAL0910 %>% slice_max(Date_Egg, n = 99)

# ... and then later show how to add this again
raw_small <- anti_join(raw, to_remove, 
                       by = c("studyName", "Species", "Island", "Individual_ID")) # return all x without match in y


# Add typos ------------------------------

## Error 1: obvious misspelling (species) ------------------------------
raw_small$Species[133] <- "Gentoo"
raw_small$Species %>% unique()

# DROP THIS ONE FOR NOW Error 2: missing info (Species) -> correct based on Individual_ID ---------------
# find individual that has more than 2 readings so I can be sure about the spp
raw_small %>% 
  group_by(Individual_ID) %>% 
  summarise(n = n()) %>% 
  filter(n > 2)  # N28A1 and others ...

raw_small %>% 
  filter(Individual_ID == "N28A1") # 2 spp ... wtf

raw %>% 
  group_by(Individual_ID) %>% 
  summarise(spp_nr = length(unique(Species))) %>% 
  filter(spp_nr > 1) %>% 
  view()


# Plot to see spp to indivdual nr ...
ggplot(raw, aes(x = Species,
                      y = Individual_ID)) +
  geom_point()
  


# Error 2: tricky (Individual_ID, double "A" in name) --------------------
raw_small$Individual_ID[21] #  "N11A1"
raw_small$Individual_ID[21] <- "N11AA1"

# # Error 3: tricky (Date wrong year 2090 instead of 2009) ----------------------
# raw$Date_Egg[127] # "2009-11-21"
# raw$Date_Egg[127] <- "2090-11-21"
# raw$Date_Egg[127]




# Then export as CSV
write_csv(raw_small, "./palmer_rawdata.csv")


# XXXXXXXXXXXXX STOPS HERE XXXXXXXXXXXXXXXXXXXXX -----------------------------



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


## Sample Number -----------------------

raw$Sample_Number %>% length() # 344
raw$Sample_Number %>% unique() #  1 to 152 (not sure if any nr missing in the middle ... )
raw$Sample_Number %>% unique() %>% length() # 152 

raw$Sample_Number %>% unique() == 1:152 # all TRUE


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

raw %>%  group_by(studyName) %>% 
  summarise(length(unique(Island)))

raw %>% 
  # filter(studyName == studyName[1]) %>% 
  group_by(studyName, Island) %>% 
  summarise(obs_island = length(Island) ) %>% 
  view()
  
# There are observation from each island in for each of the 3 studies, 
# bur != nr of observations 




## Stage --------------------------------------

# also very boring, only one unique value

raw$Stage %>% length() # 344
raw$Stage %>% unique() #  "Adult, 1 Egg Stage"
raw$Stage %>% unique() %>% length() # 1



## Individual_ID ----------------------

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

raw %>% 
  filter(Individual_ID == Individual_ID[111]) %>% 
  view()

### Plot to visualize distribution of observations (Indiv, per island per study) ------------
ggplot(data = raw,
       aes(x = Island,
           y = Individual_ID)) +
  geom_point(aes(color = studyName))

# Subset to only a few Individuals for easy of quick lpot viz
raw %>% 
  filter(Individual_ID %in% c(raw$Individual_ID[1:10])) %>% 
  ggplot(data = .,
         aes(x = Island,
             y = Individual_ID)) +
  geom_point(aes(color = studyName))



### Individual ID vs Sample Number -------------------
ggplot(raw, 
       aes(x = Individual_ID, 
           y = Sample_Number,
           color = studyName)) +
  geom_point() +
  facet_wrap(studyName~Species) +
  theme_classic()


raw %>% 
  filter(Sample_Number == 151) %>% 
  select(Individual_ID)

# Not clear what corresponds to what with samples nr and indiv ids ...
# Looks like samples are not unique ... ???




## Clutch Completion --------------------------

# Just says if cluthc was compledted or not (yes if 2 eggs)

raw$Clutch_Completion %>% length() # 344
raw$Clutch_Completion %>% unique() # "Yes" "No"
raw$Clutch_Completion %>% unique() %>% length() # 2



## Date Egg --------------------------

# Date when eg observed - good for demo {lubridate}

raw$Date_Egg %>% length() # 344
raw$Date_Egg %>% unique() #  "2007-11-11" "2007-11-16" "2007-11-15" ...
raw$Date_Egg %>% unique() %>% length() # 50



## ... ------------------


## Comments -------------------------

raw$Comments
raw$Date_Egg %>% length() # 344
raw$Date_Egg %>% unique() #  "2007-11-11" "2007-11-16" "2007-11-15" ...
raw$Date_Egg %>% unique() %>% length() # 50



