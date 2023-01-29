library(tidyverse)


# str view is super cool to visualize on the viewer panel ---------
x <- c("apple", "banana", "pear")
stringr::str_view(x, "an")

x %>% str_view( ".a.")
str_view(x, "^a")

y <- "MDCCCLXXXVIII"

str_view(y, "V{1,}")

#, ignore_case = TRUE 



# Sample character vectors
words
fruit

# Locate
str_detect(words, "ph") # returns logical vector (T/F)
str_detect(words, "ph") %>% sum() # 3 (T = 1, F = 0)

# Actually extract
str_subset(words, "ph") # "paragraph"  "photograph" "telephone" 


# Combine str_detect() with filter() ------------------------
df <- tibble(
  word = words,
  i = seq_along(words)
)

df %>% 
  filter(str_detect(word, "x$"))
#   word      i
# <chr> <int>
# 1 box     108
# 2 sex     747
# 3 six     772
# 4 tax     841



words[5] %>% nchar()



# Split 
"a|b|c|d" %>% 
  str_split("\\|") %>%  # , simplify = TRUE) returns a matrix!
  .[[1]] # this is to kind of unnest it





