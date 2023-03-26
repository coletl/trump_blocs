## Verify numbers that are in the main text of the ethnoracial voting blocs
## section
library(tidyverse)


## Main results using ANES for turnout + vote choice, CPS for composition
res = readRDS("data/vbdf_race_caa.rds")

round3 <- function(x) mutate(x, across(where(is.numeric), round, 3))


## pr_voterep_mean and pr_votedem_mean should add up tto 1 - minus
## share voting 3rd party.
## instead it adds up to the share voting for D or R (unconditional on turnout).
# res_update = res %>% 
#   mutate(pr_votethird = pr_turnout_mean - pr_voterep_mean - pr_votedem_mean,
#          net_r_revised = prob_mean * (pr_voterep_mean - pr_votedem_mean))



## citizen voting age population from census in 2020
## https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/cvap.html
cvap = read.csv("data/cvap/CVAP_2017-2021_ACS_csv_files/Nation.csv")
cvap2020 = cvap %>% 
  filter(lntitle == "Total") %>% 
  pluck("cvap_est")


# net votes from Black americans ------------------------------------------


# Net votes from Blacks in 2008-2020
res %>% 
  filter(year %in% c(2008,2012,2016, 2020),
         race == "black") %>% 
  round3() %>% 
  select(-contains("median")) %>% 
  glimpse()




# Average net votes from Blacks, 1976-2004
res %>% 
  filter(year %in% seq(1976, 2004, 4),
         race == "black") %>% 
  summarise(mean_net_votes = mean(net_rep_mean)) %>% 
  round3() 

res %>% 
  filter(year %in% seq(2008, 2020, 4),
         race == "black") %>% 
  summarise(mean_net_votes = mean(net_rep_mean)) %>% 
  round3() 


# Total net votes from blacks, 2020
cat("Net votes from Black citizens in 2020:", 
    (cvap2020 * res %>% 
      filter(year == 2020,
             race == "black") %>% 
      pluck("net_rep_mean")) %>% 
      prettyNum(big.mark=","))

# Change in composition
res %>% 
  filter(race == "black", 
         year %in% c(1976, 2020)) %>% 
  round3() %>% 
  pluck("prob_mean")


# turnout
res %>% 
  filter(race == "black", year %in% c(1976, 2020)) %>% 
  select(year, contains("turnout"), prob_mean) 
  


# Net votes from Latinos --------------------------------------------------

# Net votes from Latinos in 2000-2020
res %>% 
    filter(year %in% c(2000, 2008, 2012,2016, 2020),
           race == "hispanic") %>% 
    round3() %>% 
  select(-contains("median")) %>% 
    glimpse()

# pre-Obama
res %>% 
    filter(year %in% seq(1976, 2004, 4),
           race == "hispanic") %>% 
    summarise(mean_net_votes = mean(net_rep_mean),
              mean_net_votes_low = mean(net_rep_low),
              mean_net_votes_high = mean(net_rep_high)) %>% 
    round3()


# composition
res %>% 
  filter(year %in% c(1976, 2000, 2020),
         race == "hispanic") %>% 
  round3() %>% 
  pluck("prob_mean")


# vote choice
res %>% 
  filter(year %in% c(2020, 2016, 2012, 2008, 2004, 2000), 
         race == "hispanic") %>% 
    round3() %>%
    select(year, starts_with("pr_vote"), -contains("median")) %>% 
    glimpse()

res %>% filter(year %in% c(2000, 2004), race == "hispanic") %>% 
    select(starts_with("pr_vote")) %>% 
    colMeans()


# Net votes from Whites ---------------------------------------------------
# Net votes from Latinos in 2000-2020
res %>% 
    filter(year %in% c(2000, 2004, 2008, 2012,2016, 2020),
           race == "white") %>% 
    round3() %>% 
    glimpse()
