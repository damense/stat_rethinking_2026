library(tidyverse)
library(gtools)
n_people <- 10
values <- c(T,F)
res <-as.data.frame(
  permutations(n=length(values),
               r=n_people,
               v=values,
               repeats.allowed = T)
  )
# Using the “garden of forking data” approach, how many ways are there to realize
# this sample (8 out of 10), if all participants are honest?
res %>%
  mutate(sum = as.factor(rowSums(., na.rm=TRUE))) %>% 
  group_by(sum) %>% 
  summarise(n=n()) %>% 
  mutate(freq=n/sum(n))

### There are 45 out of 1024 ways = 0.0439 to realize (8 out of 10)


# How many ways, if 5 of the participants are honest and the other 3 lie if the have a False?
###If 5 participants are honest and the other five say they got "heads"


    v_cols <- paste0("V", seq_len(5))
    
    res %>%
      mutate(across(all_of(v_cols), ~ TRUE)) %>%
      mutate(sum = rowSums(across(starts_with("V")), na.rm = TRUE)) %>%
      count(sum, name = "n") %>%
      mutate(freq = n / sum(n)) %>%
      filter(sum == 8) %>%
      pull(n) %>%
      { if (length(.) == 0) 0 else . }

### The amount is 320/1024=0.3125
# Can you figure out the number of honest participants that maximizes the number of ways to realize the observed sample
#(8 out of 10)
map_dbl(
  seq_len(10),
  function(k) {
    v_cols <- paste0("V", seq_len(k))
    
    res %>%
      mutate(across(all_of(v_cols), ~ TRUE)) %>%
      mutate(sum = rowSums(across(starts_with("V")), na.rm = TRUE)) %>%
      count(sum, name = "n") %>%
      mutate(freq = n / sum(n)) %>%
      filter(sum == 8) %>%
      pull(n) %>%
      { if (length(.) == 0) 0 else . }
  }
)
### The amount is 6