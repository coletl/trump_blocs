# quick code to clean side surveys

library(readstata13)
library(tidyverse)
library(questionr)

gen_std_scale = function(x){
    x_stdize = apply(x, 2, function(z){
        (z - mean(z,na.rm=TRUE)) / sd(z,na.rm=TRUE)
    })
    # use the first column as the directionality reference
    for (i in 2:ncol(x_stdize)) {
        if (cor(x_stdize[, 1], x_stdize[, i], use = "pa") < 0){
            x_stdize[, i] = x_stdize[, i] * -1
        }
    }
    if (any(sign(cor(x_stdize, use="pa")) == -1)) warning("Not all items are positively correlated")
    num_nonmiss = apply(x_stdize, 1, function(x) sum(!is.na(x)))
    out = apply(x_stdize, 1, mean, na.rm=TRUE)
    out[num_nonmiss < 3] = NA
    out = (out - mean(out, na.rm=TRUE)) / sd(out, na.rm=TRUE)
    
    # generate reliability stats - avg. interitem corr and chronbach's alpha
    std_cor = cor(x_stdize, use = "pa")
    std_cov = cov(x_stdize, use = "pa")
    avg_cor = mean(as.numeric(std_cor)[1:(length(std_cor)/2)])
    message(paste0("Average interitem correlation after standardizing columns is ", round(avg_cor, 3)))
    
    # chronbach alpha - formula from wikipedia
    cbar = mean(std_cov[upper.tri(std_cov, diag = FALSE)])
    alpha = (nrow(std_cov) * cbar) / (1 + (nrow(std_cov) - 1) * cbar)
    message(paste0("\nChronbach's alpha is ", round(alpha, 3)))
    
    return(out)
}

########################### VOTER PANEL ###########################

d<- read.csv('~/Dropbox/trump_voting/replication/data/OutputData_2016.csv')

pvote_16_recode<- case_when(d$presvote16post_2016=='Hillary Clinton'~-1,
                            d$presvote16post_2016=='Donald Trump'~ 1,
                            TRUE~ 0)


d$pvote_16_recode<- pvote_16_recode

pvote_12_recode<- case_when(d$post_presvote12_2012=='Barack Obama'~-1,
                            d$post_presvote12_2012=='Mitt Romney'~ 1,
                            TRUE~ 0)
d$pvote_12_recode<- pvote_12_recode



rr_recode = "'Strongly Agree' = 1;
'Agree' = 2;
'Dont Know' = 3;
'Disagree'=4;
'Strongly Disagree' = 5;
else = NA"



race_slave_baseline<- gsub("\\'", '', as.character(d$race_slave_baseline))
race_tryharder_baseline<- gsub("\\'", '', as.character(d$race_tryharder_baseline))
race_overcome_baseline<- gsub("\\'", '', as.character(d$race_overcome_baseline))
race_deservemore_baseline<- gsub("\\'", '', as.character(d$race_deservemore_baseline))


d$resent_num_slave = as.numeric(as.character(car::recode(race_slave_baseline, rr_recode)))
d$resent_num_try = as.numeric(as.character(car::recode(race_tryharder_baseline, rr_recode)))
d$resent_num_overcome     = as.numeric(as.character(car::recode(race_overcome_baseline, rr_recode)))
d$resent_num_deserve = as.numeric(as.character(car::recode(race_deservemore_baseline, rr_recode)))


resent_var<- dplyr::select(d, starts_with('resent_num'))
test<- gen_std_scale(resent_var)
d$rr<- test

##doign this with 2016 measured rr

race_slave_16<- gsub("\\'", '', as.character(d$race_slave_2016))
race_tryharder_16<- gsub("\\'", '', as.character(d$race_tryharder_2016))
race_overcome_16<- gsub("\\'", '', as.character(d$race_overcome_2016))
race_deservemore_16<- gsub("\\'", '', as.character(d$race_deservemore_2016))





d$resent_2016_num_slave = as.numeric(as.character(car::recode(race_slave_16, rr_recode)))
d$resent_2016_num_try = as.numeric(as.character(car::recode(race_tryharder_16, rr_recode)))
d$resent_2016_num_overcome     = as.numeric(as.character(car::recode(race_overcome_16, rr_recode)))
d$resent_2016_num_deserve = as.numeric(as.character(car::recode(race_deservemore_16, rr_recode)))


resent_var_16<- select(d, starts_with('resent_2016_num'))
test<- gen_std_scale(resent_var_16)
d$rr_16<- test


d <- 
    select(d, case_identifier,
           matches("^pvote_"), matches("^rr"), 
           matches("FT11"), matches("race"), matches("weight"))

saveRDS(d, "~/Dropbox/trump_voting/data/voter_clean.rds")


########################### VOTER UPDATE
d<- read.csv('~/Dropbox/trump_voting/data/UpdatePanel/voter_panel.csv')

pvote_16_recode<- case_when(d$presvote_2016==1~-1,
                            d$presvote_2016==2~ 1,
                            d$presvote_2016> 2~ 0 )

pvote_20_recode<- case_when(d$presvote_2020Nov==2~-1,
                            d$presvote_2020Nov==1~ 1,
                            d$presvote_2020Nov>2~ 0)

pvote_12_recode<- case_when(d$presvote_2012==1~-1,
                            d$presvote_2012==2~ 1,
                            d$presvote_2012>2 ~ 0 )

d$pvote_16_recode<- pvote_16_recode
d$pvote_12_recode<- pvote_12_recode
d$pvote_20_recode<- pvote_20_recode




rr_recode = "'Strongly Agree' = 1;
'Agree' = 2;
'Dont Know' = 3;
'Disagree'=4;
'Strongly Disagree' = 5;
else = NA"



race_slave_baseline<- gsub("\\'", '', as.character(d$race_slave_2011))
race_tryharder_baseline<- gsub("\\'", '', as.character(d$race_tryharder_2011))
race_overcome_baseline<- gsub("\\'", '', as.character(d$race_overcome_2011))
race_deservemore_baseline<- gsub("\\'", '', as.character(d$race_deservemore_2011))


d$resent_num_slave = as.numeric(race_slave_baseline)
d$resent_num_try = as.numeric(race_tryharder_baseline)
d$resent_num_overcome     = as.numeric(race_overcome_baseline)
d$resent_num_deserve = as.numeric(race_deservemore_baseline)


resent_var<- dplyr::select(d, starts_with('resent_num'))
test<- gen_std_scale(resent_var)
d$rr<- test

d$white<- ifelse(d$race_2011==1, 1, 0)


d <- 
    mutate(d, respid = paste0("resp", 1:nrow(d))) %>% 
    select(respid,
           matches("^pvote_"), matches("^rr"), matches("white"),
           matches("FT11"), matches("race"), matches("weight"))


saveRDS(d, "~/Dropbox/trump_voting/data/voter_update_clean.rds")

########################       GSS         ###########################
d<- read.dta13('~/Dropbox/trump_voting/data/GSS/gss7221_r1a.dta')


d %>% select(pres72:pres16) %>% summary()
table(d$pres72)
#https://gss.norc.org/documents/codebook/GSS_Codebook_mainbody.pdf
#codebook

d$vote_pres3_72<- case_when(d$pres72==1~  -1,
                            d$pres72==2~   1,
                            d$pres72 %in% c(3, 4, 5, 8, 9, 0)~ 0,
                            d$vote72 %in% c(2, 3, 4, 6) ~ 0  )


d$vote_pres3_76<- case_when(d$pres76==1 ~ -1,
                            d$pres76==2~1,
                            d$pres76 >2 ~ 0 ,
                            d$vote76 > 1 ~ 0  )


d$vote_pres3_80<- case_when(d$pres80==1 ~ -1,
                            d$pres80==2~1,
                            d$pres80 >2 ~ 0 ,
                            d$vote80 > 1 ~ 0  )


d$vote_pres3_84<- case_when(d$pres84==1 ~ -1,
                            d$pres84==2~1,
                            d$pres84 >2 ~ 0 ,
                            d$vote84 >1 ~ 0  )



d$vote_pres3_88<- case_when(d$pres88==1 ~ -1,
                            d$pres88==2~1,
                            d$pres88  > 2 ~ 0 ,
                            d$vote88 >1  ~ 0  )

d$vote_pres3_92<- case_when(d$pres92==1 ~ -1,
                            d$pres92==2~1,
                            d$pres92 >2 ~ 0 ,
                            d$vote92 >1  ~ 0  )


d$vote_pres3_96<- case_when(d$pres96==1 ~ -1,
                            d$pres96==2~1,
                            d$pres96 >2 ~ 0 ,
                            d$vote96 >1  ~ 0  )

d$vote_pres3_00<- case_when(d$pres00==1 ~ -1,
                            d$pres00==2~1,
                            d$pres00 >2 ~ 0 ,
                            d$vote00 >1  ~ 0  )


d$vote_pres3_04<- case_when(d$pres04==1 ~ -1,
                            d$pres04==2~1,
                            d$pres04 >2 ~ 0 ,
                            d$vote04 >1  ~ 0  )

d$vote_pres3_08<- case_when(d$pres08==1 ~ -1,
                            d$pres08==2~1,
                            d$pres08 >2 ~ 0 ,
                            d$vote08 >1  ~ 0  )

d$vote_pres3_12<- case_when(d$pres12=='obama' ~ -1,
                            d$pres12=='romney' ~1,
                            d$pres12!= 'obama' & d$pres12!= 'romney' ~ 0 ,
                            d$vote12 =='did not vote' | d$vote12=='ineligible'  ~ 0  )

d$vote_pres3_16<- case_when(d$pres16=='clinton' ~ -1,
                            d$pres16=='trump' ~1,
                            d$pres16!= 'clinton' & d$pres16!= 'trump' ~ 0 ,
                            d$vote16 =='did not vote' | d$vote16=='ineligible'  ~ 0  )

vote_pres3<- rep(NA, nrow(d))
vote_pres3[which(d$year %in% c(1973:1976))]<- d$vote_pres3_72[which(d$year %in% c(1973:1976))]
vote_pres3[which(d$year %in% c(1977:1980))]<- d$vote_pres3_76[which(d$year %in% c(1977:1980))]
vote_pres3[which(d$year %in% c(1981:1984))]<- d$vote_pres3_80[which(d$year %in% c(1981:1984))]
vote_pres3[which(d$year %in% c(1985:1988))]<- d$vote_pres3_84[which(d$year %in% c(1985:1988))]
vote_pres3[which(d$year %in% c(1989:1992))]<- d$vote_pres3_88[which(d$year %in% c(1989:1992))]
vote_pres3[which(d$year %in% c(1993:1996))]<- d$vote_pres3_92[which(d$year %in% c(1993:1996))]
vote_pres3[which(d$year %in% c(1997:2000))]<- d$vote_pres3_96[which(d$year %in% c(1997:2000))]
vote_pres3[which(d$year %in% c(2001:2004))]<- d$vote_pres3_00[which(d$year %in% c(2001:2004))]
vote_pres3[which(d$year %in% c(2005:2008))]<- d$vote_pres3_04[which(d$year %in% c(2005:2008))]
vote_pres3[which(d$year %in% c(2009:2012))]<- d$vote_pres3_08[which(d$year %in% c(2009:2012))]
vote_pres3[which(d$year %in% c(2013:2016))]<- d$vote_pres3_12[which(d$year %in% c(2013:2016))]
vote_pres3[which(d$year %in% c(2017:2020))]<- d$vote_pres3_16[which(d$year %in% c(2017:2020))]




d$vote_pres3<- vote_pres3

##FEPOL is one variable

##NATRACEY, RACDIF{1, 2, 3, 4, }
d$racdif1_clean<- case_when(d$racdif1=='yes'~ 1,
                            d$racdif1=='no'~ 0 )
d$racdif2_clean<- case_when(d$racdif2=='yes'~ 1,
                            d$racdif2=='no'~ 0 )
d$racdif3_clean<- case_when(d$racdif3=='yes'~ 1,
                            d$racdif3=='no'~ 0 )
d$racdif4_clean<- case_when(d$racdif4=='yes'~ 1,
                            d$racdif4=='no'~ 0 )

d$natrace_clean<- case_when(d$natrace=='too little'~ 1,
                            d$natrace=='about right'~ 0,
                            d$natrace=='too much'~ -1 )

##assistance to blacks
d$natracey_clean<- case_when(d$natracey=='too little'~ 1,
                             d$natracey=='about right'~ 0,
                             d$natracey=='too much'~ -1 )

d$fepol_clean<- case_when(d$fepol=='agree'~ 1,
                          d$fepol=='disagree'~ 0 )


##hardworking
##WORKWHTS,
##WORKBLKS,
## INTLWHTS]

d$workblks_clean<- ifelse(d$workblks>7, NA, d$workblks)
d$workwhts_clean<- ifelse(d$workwhts>7, NA, d$workwhts)

d$work_diff<- d$workwhts_clean - d$workblks_clean

d$intlwhts_clean<- ifelse(d$intlwhts>7, NA, d$intlwhts)
d$intlblks_clean<- ifelse(d$intlblks>7, NA, d$intlblks)


d$liveblks_clean<- case_when(d$liveblks=='strongly favor'~ 1,
                             d$liveblks=='favor' ~ 2,
                             d$liveblks=='neither favor nor oppose'~ 3,
                             d$liveblks=='oppose'~ 4,
                             d$liveblks=='strongly oppose'~ 5)
d$marblks_clean<- case_when(d$marblk=='strongly favor'~ 1,
                            d$marblk=='favor' ~ 2,
                            d$marblk=='neither favor nor oppose'~ 3,
                            d$marblk=='oppose'~ 4,
                            d$marblk=='strongly oppose'~ 5)
d$marhisp_clean<- case_when(d$marhisp=='strongly favor'~ 1,
                            d$marhisp=='favor' ~ 2,
                            d$marhisp=='neither favor nor oppose'~ 3,
                            d$marhisp=='oppose'~ 4,
                            d$marhisp=='strongly oppose'~ 5)
d$discaff_clean<- case_when(d$discaff=='very likely' ~ 1,
                            d$discaff=='somewhat likely'~ 0 ,
                            d$discaff=='not very likely'~ -1)

d$affrmact_clean<- case_when(d$affrmact=='strongly favors'~1,
                             d$affrmact=='not strongly favors'~2,
                             d$affrmact=='not strongly opposes'~3,
                             d$affrmact=='strongly opposes'~4)

d$wrkwayup_clean<- case_when(d$wrkwayup=='agree strongly'~1,
                             d$wrkwayup=='agree somewhat'~2,
                             d$wrkwayup=='neither agree nor disagree'~3,
                             d$wrkwayup=='disagree somewhat'~4,
                             d$wrkwayup=='disagree strongly'~5)


ee<- d %>% group_by(year, vote_pres3) %>% summarise(means = wtd.mean(wrkwayup_clean, weight = wtssall))
ee %>% subset(vote_pres3==1) %>% ggplot(aes(x = year, y = means)) + geom_point() + geom_line() +
    geom_vline(xintercept = c(2008, 2012, 2016))

saveRDS(d, "~/Dropbox/trump_voting/data/gss_clean.rds")