## data 0 - convert files from https://zenodo.org/record/8059616 to single csv per measure

library(tidyverse)
library(fs)
library(archive)
library(janitor)

f <- dir_ls(path = "./data-raw-22-06-2023/")
unzip(zipfile = f[[1]], list = TRUE)

# create directories
f0 <- f %>%
  str_sub(start = 23, end = -9) %>%
  unique() %>%
  make_clean_names() %>%
  str_replace_all(pattern = "_", replacement = "-") %>%
  paste0("./data/", .) 

dir_create(path = f0)

# population data
f1 <- f %>%
  str_subset("_pop.zip")

for(i in 1:length(f1)){
  f2 <- f1[[i]] %>%
    unzip(list = TRUE) %>%
    as_tibble() %>%
    slice(-1) %>%
    clean_names()
    
  
  f3 <- f2 %>%
    filter(str_detect(name, pattern = "EU")) %>%
    # filter(str_detect(name, pattern = "_migout.csv")) %>%
    mutate(data = map(.x = name, .f = ~read_csv(archive_read(archive = f1[[i]], file = .x))))
  
  d <- f3 %>%
    select(data) %>%
    unnest() %>%
    select(-area) %>%
    rename(birth = CoB,
           year = period) %>%
    mutate(edu = str_sub(edu, start = 2),
           sex = str_sub(sex, end = 1))
  
  write_csv(d, paste0(f0[i], "/pop.csv"))
}


# migration data .. ddnt finish
f1 <- f %>%
  str_subset("_mig.zip")

for(i in 1:length(f1)){
  f2 <- f1[[i]] %>%
    unzip(list = TRUE) %>%
    as_tibble() %>%
    slice(-1) %>%
    clean_names()
  
  
  f3 <- f2 %>%
    # filter(str_detect(name, pattern = "_migout.csv")) %>%
    mutate(data = map(.x = name, .f = ~read_csv(archive_read(archive = f1[[i]], file = .x))))
  
  d <- f3 %>%
    select(data) %>%
    unnest() %>%
    select(-area) %>%
    rename(birth = CoB,
           year = period) %>%
    mutate(edu = str_sub(edu, start = 2),
           sex = str_sub(sex, end = 1))
  
  write_csv(d, paste0(f0[i], "/pop.csv"))
}
