####################################################################
################### COPIED FROM 01prepareANES.R ####################
####################################################################

## The cumulative data file only has income percentile brackets, but the 
## codebook doesn't have the dollar ranges for anything past 2004. So
## get the income from the yearly files. 
library(readstata13)
library(assertthat)


incvars = list(# extending
               "anes1956" = "V560190",
               "anes1960" = "V600189",
               "anes1964" = "V640269",
               "anes1968" = "V680261",
               "anes1972" = "V720420",
               "anes1976" = "V763507",
               "anes1980" = "V800686",
               # will
               "anes1984" = "V840680",
               "anes1986" = "V860733",
               "anes1988" = "V880520",
               "anes1990" = "V900663",
               "anes1992" = "V924104",
               "anes1994" = "V941404",
               "anes1996" = "V960701",
               "anes1998" = "V980652",
               "anes2000" = "V000994",
               "anes2002" = "V023149",
               "anes2004" = "V043293x",
               "anes2008" = "V083248x",
               "anes2012" = "incgroup_prepost_x",
               "anes2016" = c("V161361x","V162309x"),
               "anes2020" = "V201617x")
idvars = list(# extending
              "anes1956" = "V560002",
              "anes1960" = "V600002",
              "anes1964" = "V640002",
              "anes1968" = "V680002",
              "anes1972" = "V720002",
              "anes1976" = "V763002",
              "anes1980" = "V800004",
              
              # will
              "anes1984" =  "V840004",
              "anes1986" =  "V860004",
              "anes1988" =  "V880004",
              "anes1990" =  "V900004",
              "anes1992" =  "V923004",
              "anes1994" =  "V940001",
              "anes1996" =  "V960001",
              "anes1998" =  "V980001",
              "anes2000" =  "V000001",
              "anes2002" =  "V020001",
              "anes2004" =  "V040001",
              "anes2008" =  "V080001",
              "anes2012" =  "caseid",
              "anes2016" =  "V160001",
              
              
              "anes2020" =  "V200001"
              )


for (d in c("anes1984", "anes1986", "anes1988", "anes1990", "anes1992", 
            "anes1994", "anes1996", "anes1998", "anes2000", "anes2002",
            "anes2004", "anes2008", "anes2012", "anes2016", "anes2020")) {
    
    y = as.numeric(gsub("anes", "", d))
    
    if (y == 2008)  {
        dta = read.dta13("data/anes_waves/anes2008/anes_timeseries_2008_stata12.dta")
    } else if (y == 2012){
        dta = read.dta13("data/anes_waves/anes2012/anes_timeseries_2012_Stata12.dta")
    } else if (y == 2016){
        dta = read.dta13("data/anes_waves/anes2016/anes_timeseries_2016_Stata13.dta")
    } else if (y == 2020){
        dta = read.dta13("data/anes_waves/anes_timeseries_2020_stata_20210719/anes_timeseries_2020_stata_20210719.dta")
    } else {
        f = list.files(file.path("data/anes_waves", d), pattern = "dta$")
        
        if (length(f) > 1) {
            warning(paste0("more than one file for ", d), immediate. = TRUE)
            next
        }
        dta = read.dta13(file.path("data/anes_waves", d, f))
    }
    
    len_uid <- pull(dta, {idvars[[d]]}) %>% unique() %>% length()
    stopifnot(len_uid == nrow(dta))
    
    
    if (y == 1996) {
        dta = dta %>% 
            select(respid2 = idvars[[d]], code = incvars[[d]]) %>% 
            mutate(income = case_when(
                code ==  1 ~ "None or less than $2,999",
                code ==  2 ~ "$3,000-$4,999",
                code ==  3 ~ "$5,000-$6,999",
                code ==  4 ~ "$7,000-$8,999",
                code ==  5 ~ "$9,000-$9,999",
                code ==  6 ~ "$10,000-$10,999",
                code ==  7 ~ "$11,000-$11,999",
                code ==  8 ~ "$12,000-$12,999",
                code ==  9 ~ "$13,000-$13,999",
                code == 10 ~ "$14,000-$14.999",
                code == 11 ~ "$15,000-$16,999",
                code == 12 ~ "$17,000-$19,999",
                code == 13 ~ "$20,000-$21,999",
                code == 14 ~ "$22,000-$24,999",
                code == 15 ~ "$25,000-$29,999",
                code == 16 ~ "$30,000-$34,999",
                code == 17 ~ "$35,000-$39,999",
                code == 18 ~ "$40,000-$44,999",
                code == 19 ~ "$45,000-$49,999",
                code == 20 ~ "$50,000-$59,999",
                code == 21 ~ "$60,000-$74,999",
                code == 22 ~ "$75,000-89,999",
                code == 23 ~ "$90,000-$104,999",
                code == 24 ~ "$105,000 and over",
                TRUE ~ NA_character_
            ))
    } else if (y == 1994) {
        dta = dta %>% 
            select(respid2 = idvars[[d]], code = incvars[[d]]) %>% 
            mutate(income = case_when(
                code == 01 ~ "NONE OR LESS THAN $2,999",
                code == 02 ~ "$3,000-$4,999",
                code == 03 ~ "$5,000-$6,999",
                code == 04 ~ "$7,000-$8,999",
                code == 05 ~ "$9,000-$9,999",
                code == 06 ~ "$10,000-$10,999",
                code == 07 ~ "$11,000-$11,999",
                code == 08 ~ "$12,000-$12,999",
                code == 09 ~ "$13,000-$13,999",
                code == 10 ~ "$14,000-$14,999",
                code == 11 ~ "$15,000-$16,999",
                code == 12 ~ "$17,000-$19,999",
                code == 13 ~ "$20,000-$21,999",
                code == 14 ~ "$22,000-$24,999",
                code == 15 ~ "$25,000-$29,999",
                code == 16 ~ "$30,000-$34,999",
                code == 17 ~ "$35,000-$39,999",
                code == 18 ~ "$40,000-$44,999",
                code == 19 ~ "$45,000-$49,999",
                code == 20 ~ "$50,000-$59,999",
                code == 21 ~ "$60,000-$74,999",
                code == 22 ~ "$75,000-$89,999",
                code == 23 ~ "$90,000-$104,999",
                code == 24 ~ "$105,000 AND OVER",
                TRUE ~ NA_character_
            ))
    } else if (y == 2016){
        
        labs = attr(dta, "label.table")
        labs1 = labs[which(names(labs) == incvars[[d]][1])]
        labs2 = labs[which(names(labs) == incvars[[d]][2])]
        
        labdf1 = data.frame(code1 = labs1[[1]], inc1 = gsub("^[0-9]+\\.", "", names(labs1[[1]])))
        labdf2 = data.frame(code2 = labs2[[1]], inc2 = gsub("^[0-9]+\\.", "", names(labs2[[1]])))
        
        dta = dta %>% 
            select(respid2 = idvars[[d]], 
                   code1 = incvars[[d]][1],
                   code2 = incvars[[d]][2]) 
        dta = left_join(dta, labdf1, by = "code1")
        dta = left_join(dta, labdf2, by = "code2")
        dta = dta %>% 
            mutate(income = case_when(
                !is.na(inc1) ~ inc1,
                is.na(inc1) & !is.na(inc2) ~ inc2,
                TRUE ~ NA_character_
            )) %>% 
            select(respid2, income)
    } else {
        
        # get value labels
        labs = attr(dta, "label.table")
        labs = labs[which(names(labs) == paste0(incvars[[d]], "_"))]
        if (length(labs) == 0){
            labs = attr(dta, "label.table")
            labs = labs[grep(incvars[[d]], names(labs))]
            stopifnot(length(labs) == 1)
        }
        
        lab_recode = data.frame(code = labs[[1]], income = names(labs[[1]])) %>% 
            mutate(income = gsub("^[0-9]+\\.", "", income))
        
        
        # subset data down
        dta = dta %>% 
            select(all_of(c(idvars[[d]], incvars[[d]])))
        assert_that(ncol(dta) %in% c(2, 3))
        names(dta)[names(dta) == idvars[[d]]] = "respid2"
        names(dta)[names(dta) == incvars[[d]]] = "code"
        
        # add value label
        dta = left_join(dta, lab_recode, by = "code")
    }
    
    
    # merge with CDF. use this merge tracker to maek sure i'm not losing rows
    dta$code = NULL
    dta$year = as.numeric(y)
    dta$merge = 1
    
    
    # split the year from the full anes cdf to avoid duplicate columns
    anes_thisyear = anes %>% 
        filter(year == y) 
    anes_thisyear$income = NULL
    anes_other    = anes %>% filter(year != y)  
    
    # merge with income variable
    t1 = nrow(anes_thisyear)
    t2 = nrow(dta)
    anes_thisyear = left_join(anes_thisyear, dta, by = c("respid2", "year"))
    assert_that(t1 == nrow(anes_thisyear), 
                msg = paste0("lost rows from CDF in the merge for ", d))
    assert_that(sum(anes_thisyear$merge, na.rm=TRUE) == t2,
                msg = paste0("lost rows from yearly file in the merge for ", d))
    
    anes_thisyear$merge = NULL
    
    # put the cdf back together
    anes = bind_rows(
        anes_other,
        anes_thisyear
    )
    
}
anes = anes %>% 
    arrange(year, respid2)



## now clean up income data
inc2 = trimws(anes$income)
inc2 = gsub(",", "", inc2)
inc2[grep("ref", inc2, ignore.case = TRUE)] = NA
inc2 = trimws(gsub( "^[A-Z]+\\.", "", inc2))
inc2 = case_when(
    inc2 %in% c("NA", "NA (9 in Y26)", "NA (89 in Y20)") ~ NA_character_,
    inc2 %in% c("DON-T KNOW", "Don't know", "-8. Don't know", "DK", "NOT ASCERTAINED") ~ NA_character_,
    grepl("Interview breakoff", inc2) ~ NA_character_,
    inc2 == "Don't know household income (8 in Z6" ~ NA_character_,
    TRUE ~ inc2
)

int_bottom = NA_character_
int_top = NA_character_
which_bottomcode = grepl("less than|<", inc2, ignore.case = TRUE)
int_bottom[which_bottomcode] = 0
int_top = case_when(
    grepl("2999", inc2) & which_bottomcode ~ "2999",
    grepl("4999", inc2) & which_bottomcode ~ "4999",
    grepl("50000", inc2) & which_bottomcode ~ "50000",
    inc2 == "NONE - LESS THAN" ~ "2999", # from 1986-88
    TRUE ~ NA_character_
)

recodes = distinct(
    data.frame(income = anes$income, inc2 = inc2,
               int_bottom = int_bottom, int_top = int_top) 
)
split = strsplit(recodes$inc2, "-")
two = which(lapply(split, length) == 2)
recodes$int_bottom[two] = unlist(lapply(split[two], function(x) x[1]))
recodes$int_top[two] = unlist(lapply(split[two], function(x) x[2]))

recodes = recodes %>% 
    mutate(int_bottom = case_when(
        inc2 == "$75000 AND OVER" ~ "75000",
        inc2 == "$90000 AND OVER" ~ "90000",
        inc2 == "$90000 AND OVER" ~ "90000",
        inc2 == "$105000 AND OVER" ~ "105000",
        inc2 == "$105000 AND OVER" ~ "105000",
        inc2 == "$105000 and over" ~ "105000",
        inc2 == "$105000 and over" ~ "105000",
        inc2 == "$200000 and over" ~ "200000",
        inc2 == "$120000 and over" ~ "120000",
        inc2 == "$150000 and over" ~ "150000",
        inc2 == "$250000 or more" ~ "250000",
        inc2 == "More than $84999" ~ "84999",
        inc2 == "Under $5000" ~ "0",
        inc2 == "Just about $50000 [VOL]" ~ "50000",
        inc2 == "Below $24999 but NA category" ~ "0",
        TRUE ~ int_bottom
    )) %>% 
    mutate(int_top = case_when(
        inc2 == "Just about $50000 [VOL]" ~ "50000",
        inc2 == "Under $5000" ~ "5000",
        inc2 == "Below $24999 but NA category" ~ "24999",
        grepl("more than|and over|or more", inc2, ignore.case = TRUE) ~ "Inf",
        TRUE ~ int_top
    )) %>% 
    mutate(int_bottom = ifelse(inc2 == "Less than $50000 - DK/NA/RF additio", NA, int_bottom),
           int_top = ifelse(inc2 == "Less than $50000 - DK/NA/RF additio", NA, int_top))

recodes = recodes %>% 
    rename(inc_top = int_top, inc_bottom = int_bottom) %>% 
    mutate(inc_top = trimws(gsub("\\$|\\.", "", inc_top)),
           inc_bottom = trimws(gsub("\\$|\\.", "", inc_bottom))) %>% 
    mutate(inc_top = as.numeric(inc_top),
           inc_bottom = as.numeric(inc_bottom)) %>% 
    mutate(inc_midpoint = case_when(
        inc_top == Inf ~ inc_bottom,
        TRUE ~ (inc_top + inc_bottom) / 2
    ))


# join with anes data
anes = left_join(
    anes, 
    recodes %>% filter(!is.na(income)) %>% select(!inc2),
    by = "income")



# get table of income categories. this is in the codebook for years up to 2004,
# but not since.
inc_table = expand.grid(faminc = na.omit(unique(anes$faminc)), 
                        year = unique(anes$year[!is.na(anes$income)]))
inc_table$faminc_bottom = NA_real_
inc_table$faminc_top = NA_real_
for (i in 1:nrow(inc_table)){
    grp = inc_table$faminc[i]
    yr  = inc_table$year[i]
    inc_table$faminc_bottom[i] = min(anes$inc_bottom[anes$year == yr & anes$faminc == grp], na.rm=TRUE)
    inc_table$faminc_top[i] = max(anes$inc_top[anes$year == yr & anes$faminc == grp], na.rm=TRUE)
}
inc_table$faminc_bottom[inc_table$faminc == "1. 0 to 16 percentile"] = 0
inc_table$faminc_midpoint = (inc_table$faminc_bottom + inc_table$faminc_top) / 2
inc_table$faminc_midpoint[inc_table$faminc == "5. 96 to 100 percentile"] = inc_table$faminc_bottom[inc_table$faminc == "5. 96 to 100 percentile"]


# merge back to anes
anes = left_join(anes, inc_table, by = c("faminc", "year"))

################################################################################
# Using within-year quintiles, so no need for inflation adjustment
################################################################################
# # deflate using CPI - Urban (https://fred.stlouisfed.org/series/CPIAUCSL). 
# # Put all income in 2016 dollars. 
# cpi = read.csv("data/CPIAUCSL.csv")
# cpi$year = as.integer(substr(cpi$DATE, 1, 4))
# cpi$deflator = as.numeric(cpi$CPIAUCSL) / 100
# cpi = cpi %>% 
#     filter(year %in% anes$year) %>% 
#     select(year, deflator)
# 
# # replace deflator w/ 2016 dollars
# cpi16 = cpi$deflator[cpi$year == 2016]
# cpi$deflator = cpi$deflator / cpi16
# 
# anes = left_join(anes, cpi, by = "year")
# anes = anes %>% 
#     mutate(inc_midpoint_defl = inc_midpoint / deflator,
#            inc_top_defl = inc_top / deflator,
#            inc_bottom_defl = inc_bottom / deflator)
# 
