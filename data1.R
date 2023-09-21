## data 0 - convert files from https://zenodo.org/record/8059616 to single csv per measure
## data 1 - create measure files

library(tidyverse)
library(fs)

d <- dir_ls("./data", recurse = TRUE, type = "file") %>%
  as_tibble() %>%
  rename(file = 1) %>%
  mutate(data = map(.x = file, .f = ~read_csv(.x))) 
  

eu = unique(d$data[[1]]$dest)

# add nativity column
d <- d %>%
  mutate(data = map(.x = data, 
                    .f = function(x = .x){
                      x %>%
                        mutate(nativity = case_when(
                          birth == dest ~ "native",
                          birth %in% eu ~ "eu",
                          TRUE ~ "non-eu"
                        ))
                    }
                    )) 


basic <- function(x){
  x %>%
    group_by(year, dest) %>%
    summarise(pop_total = sum(pop),
              pop_female = sum(pop[sex == "f"]), 
              pop_male = sum(pop[sex == "m"]), 
              sex_ratio = pop_male/pop_female * 100, 
              # median_age = matrixStats::weightedMedian(x = age, w = pop),
              # mys = 
              no_edu = sum(pop[edu == 1]) / pop_total * 100,
              some_primary = sum(pop[edu == 2]) / pop_total * 100,
              primary = sum(pop[edu == 3]) / pop_total * 100,
              low_second = sum(pop[edu == 4]) / pop_total * 100,
              upp_second = sum(pop[edu == 5]) / pop_total * 100,
              post_second = sum(pop[edu == 6]) / pop_total * 100,
              
              native = sum(pop[nativity == "native"]) / pop_total * 100,
              foriegn = sum(pop[nativity != "native"]) / pop_total * 100,
              foriegn_non_eu = sum(pop[nativity == "non-eu"]) / pop_total * 100,
              foriegn_eu = sum(pop[nativity == "eu"]) / pop_total * 100)
}

d <- d %>%
  mutate(
    basic = map(.x = data, .f = ~ basic(.x)),
    by_age = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, age) %>%
        summarise(pop = sum(pop))
    ),
    by_sex = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, sex) %>%
        summarise(pop = sum(pop))
    ),
    by_edattain = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, edu) %>%
        summarise(pop = sum(pop))
    ),
    by_nativity = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, nativity) %>%
        summarise(pop = sum(pop))
    ),
    by_birth = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, birth) %>%
        summarise(pop = sum(pop))
    ),
    by_age_sex = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, age, sex) %>%
        summarise(pop = sum(pop))
    ),
    by_age_edattain = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, age, edu) %>%
        summarise(pop = sum(pop))
    ),
    by_age_nativity = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, age, nativity) %>%
        summarise(pop = sum(pop))
    ),
    by_sex_edattain = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, sex, edu) %>%
        summarise(pop = sum(pop))
    ),
    by_sex_nativity = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, sex, nativity) %>%
        summarise(pop = sum(pop))
    ),
    by_edattain_nativity = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, edu, nativity) %>%
        summarise(pop = sum(pop))
    ),
    by_age_sex_edattain = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, age, sex, edu) %>%
        summarise(pop = sum(pop))
    ),
    by_age_sex_nativity = map(
      .x = data,
      .f = function(x)
        x %>%
        group_by(dest, year, age, sex, nativity) %>%
        summarise(pop = sum(pop))
    )
  )

d <- d %>%
  mutate(scenario = file %>% 
           path_dir() %>%
           str_sub(start = 8))


d %>%
  select(scenario, basic) %>%
  unnest() %>%
  write_csv("./data/basic.csv")


for(i in names(d)[4:16]){
  d %>%
    select(scenario, !!i) %>%
    unnest() %>%
    write_csv(paste0("./data/",i,".csv"))
}
  





         