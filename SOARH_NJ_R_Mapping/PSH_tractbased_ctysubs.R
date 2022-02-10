library(haven)
library(tidyverse)
library(caret)
library(leaps)
library(base)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(tidycensus)
library(USAboundaries)
library(tigris)
library(ggplot2)
library(maptools)
library(stringr)


#OPEN PSH

PSH_trct_2020<-read_excel("C:\\Users\\laurenenolan\\Box\\NJSOARH\\Data\\Picture of Subsidized Households\\TRACT_NJ_2020_copy.xlsx")

#will's code part 1
PSH_trct_2020[PSH_trct_2020==-1]<-0 # replace -1 with 0 throughout (missing data)
PSH_trct_2020[PSH_trct_2020==-4]<-NA # replace -4 with NA throughout (suppressed demographic data)
PSH_trct_2020[PSH_trct_2020==-5]<-NA # replace -5 with NA throughout (reporting under 50% demographic data)
prefixes <- unique(PSH_trct_2020$program) # get all the unique program names used in the dataset
PSH_trct_2020 <- pivot_wider( # pivot them to one row per cdp/cousub/remainder
  PSH_trct_2020,
  id_cols = NULL,
  names_from = program,
  names_prefix = "",
  names_sep = "_",
  names_glue = "Pg{program}_{.value}",
  values_from = c(program_label,total_units,pct_occupied,number_reported,pct_reported,months_since_report,months_since_report,pct_movein,people_per_unit,people_total,rent_per_month,spending_per_month,hh_income,person_income,pct_lt5k,pct_5k_lt10k,pct_10k_lt15k,pct_15k_lt20k,pct_ge20k,pct_wage_major,pct_welfare_major,pct_other_major,pct_median,pct_lt50_median,pct_lt30_median,pct_2adults,pct_1adult,pct_female_head,pct_female_head_child,pct_disabled_lt62,pct_disabled_ge62,pct_disabled_all,pct_lt24_head,pct_age25_50,pct_age51_61,pct_age62plus,pct_age85plus,pct_minority,pct_black_nonhsp,pct_native_american_nonhsp,pct_asian_pacific_nonhsp,pct_white_nothsp,pct_black_hsp,pct_wht_hsp,pct_oth_hsp,pct_hispanic,pct_multi,months_waiting,months_from_movein,pct_utility_allow,ave_util_allow,pct_bed1,pct_bed2,pct_bed3,pct_overhoused,tpoverty,tminority,tpct_ownsfd),
  names_sort = TRUE
)
# sort the dataframe so that each set of unit counts and demographic variables are together by program
names_to_order <- map(prefixes, ~ names(PSH_trct_2020)[grep(paste0(.x,"_"), names(PSH_trct_2020))]) %>% unlist
names_id <- setdiff(names(PSH_trct_2020), names_to_order)
PSH_trct_2020 <- PSH_trct_2020 %>%
  select(names_id, names_to_order)
rm(names_id, names_to_order,prefixes) # remove intermediate outputs

# sum occupied units for the programs that have any reporting (not 6 or 7) to make formulas easier later
PSH_trct_2020$pg1_occupied_units <- PSH_trct_2020$Pg1_total_units * PSH_trct_2020$Pg1_pct_occupied / 100
PSH_trct_2020$pg2_occupied_units <- PSH_trct_2020$Pg2_total_units * PSH_trct_2020$Pg2_pct_occupied / 100
PSH_trct_2020$pg3_occupied_units <- PSH_trct_2020$Pg3_total_units * PSH_trct_2020$Pg3_pct_occupied / 100
PSH_trct_2020$pg4_occupied_units <- PSH_trct_2020$Pg4_total_units * PSH_trct_2020$Pg4_pct_occupied / 100
PSH_trct_2020$pg5_occupied_units <- PSH_trct_2020$Pg5_total_units * PSH_trct_2020$Pg5_pct_occupied / 100
PSH_trct_2020$pg8_occupied_units <- PSH_trct_2020$Pg8_total_units * PSH_trct_2020$Pg8_pct_occupied / 100
PSH_trct_2020$pg9_occupied_units <- PSH_trct_2020$Pg9_total_units * PSH_trct_2020$Pg9_pct_occupied / 100

###my code

#Using LODES crosswalk

NJ_xwalk=gzfile('C:\\Users\\laurenenolan\\Box\\NJSOARH\\Data\\LODES\\OD\\nj_xwalk.csv.gz','rt') 
NJ_xwalk=read.csv(NJ_xwalk,header=T) 

NJ_xwalk_trct<-NJ_xwalk %>%
  select(ctycsub, ctycsubname, trct,trctname)

NJ_xwalk_trct<-NJ_xwalk_trct[!duplicated(NJ_xwalk_trct$trct),]

NJ_xwalk_trct$trct<-as.character(NJ_xwalk_trct$trct)


#append xwalk
PSH_trct_appended <- left_join(PSH_trct_2020, NJ_xwalk_trct, by=c("code"="trct"))

#select ctysubs of interest

PSH_trct_appended <- subset(PSH_trct_appended, ctycsub=='3402970320'|ctycsub=='3402903050'|ctycsub=='3401304695'
                            |ctycsub=='3401306260'|ctycsub=='3401974420'|ctycsub=='3400569990'|ctycsub=='3400129280'
                            |ctycsub=='3402129310'|ctycsub=='3401133120'|ctycsub=='3402133180'|ctycsub=='3401139450'
                            |ctycsub=='3402139510'|ctycsub=='3401547250'|ctycsub=='3402347280'|ctycsub=='3402554270'
                            |ctycsub=='3402954300'|ctycsub=='3401577180'|ctycsub=='3402777240'|ctycsub=='3404177300')
##will's code part 2

###{r the final aggregation}
place_PSH_trcts_Final <- PSH_trct_appended %>%
  dplyr::group_by(ctycsub) %>%
  summarise(count = n(), 
            name = ifelse(is.na(ctycsubname),
                          MUNI_MUN_LABEL,
                          substr(ctycsubname,1,nchar(ctycsubname))),
            #county = str_to_title(COUNTY),
            ## Program 1 - Summary of all HUD Programs (634 PLACE entries with units, 422 reporting demographics) ##
            total_units = sum(Pg1_total_units, na.rm=TRUE), 
            total_pct_occupied = (sum(pg1_occupied_units, na.rm=TRUE)/sum(Pg1_total_units, na.rm=TRUE)*100),
            total_occupied = sum(pg1_occupied_units, na.rm=TRUE),
            total_pct_reported = (sum(Pg1_number_reported, na.rm=TRUE)/sum(Pg1_total_units, na.rm=TRUE)*100),
            total_reported = sum(Pg1_number_reported, na.rm=TRUE),
            total_months_since_report = (sum(Pg1_number_reported * Pg1_months_since_report, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_people_reporting = sum(Pg1_people_total, na.rm=TRUE),
            total_people_per_unit = (sum(Pg1_people_total, na.rm=TRUE)/sum(Pg1_number_reported, na.rm=TRUE)),
            # Rent and Income #
            total_rent_per_month = (sum(Pg1_rent_per_month * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_spending_per_month = (sum(Pg1_spending_per_month * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            # Household Income Variables #
            total_hh_income = (sum(Pg1_hh_income * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_person_income = (sum(Pg1_person_income * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_lt5k = (sum(Pg1_pct_lt5k * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_5k_lt10k = (sum(Pg1_pct_5k_lt10k * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_10k_lt15k = (sum(Pg1_pct_10k_lt15k * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_15k_lt20k = (sum(Pg1_pct_15k_lt20k * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_ge20k = (sum(Pg1_pct_ge20k * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            # Major Source of Income #
            total_pct_wage_major = (sum(Pg1_pct_wage_major * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_welfare_major = (sum(Pg1_pct_welfare_major * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_other_major = (sum(Pg1_pct_other_major * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_median = (sum(Pg1_pct_median * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_lt50_median = (sum(Pg1_pct_lt50_median * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_lt30_median = (sum(Pg1_pct_lt30_median * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            # Presence of Children #
            total_pct_2adults = (sum(Pg1_pct_2adults * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_1adult = (sum(Pg1_pct_1adult * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_female_head = (sum(Pg1_pct_female_head * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_female_head_child = (sum(Pg1_pct_female_head_child * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            # Head or Spouse or Co-Head Has a Disability #
            total_pct_disabled_lt62 = (sum(Pg1_pct_disabled_lt62 * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_disabled_ge62 = (sum(Pg1_pct_disabled_ge62 * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_disabled_all = (sum(Pg1_pct_disabled_all * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            # Age of Head/Spouse (Whoever is Older) #
            total_pct_lt24_head = (sum(Pg1_pct_lt24_head * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_age25_50 = (sum(Pg1_pct_age25_50 * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_age51_61 = (sum(Pg1_pct_age51_61 * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_age62plus = (sum(Pg1_pct_age62plus * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_age85plus = (sum(Pg1_pct_age85plus * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            # Race and Ethnicity #
            total_pct_minority = (sum(Pg1_pct_minority * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_black_nonhsp = (sum(Pg1_pct_black_nonhsp * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_native_american_nonhsp = (sum(Pg1_pct_native_american_nonhsp * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_asian_pacific_nonhsp = (sum(Pg1_pct_asian_pacific_nonhsp * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_white_nothsp = (sum(Pg1_pct_white_nothsp * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_black_hsp = (sum(Pg1_pct_black_hsp * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_wht_hsp = (sum(Pg1_pct_wht_hsp * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_oth_hsp = (sum(Pg1_pct_oth_hsp * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_hispanic = (sum(Pg1_pct_hispanic * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_multi = (sum(Pg1_pct_multi * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            # Social Conditions #
            total_months_waiting = (sum(Pg1_months_waiting * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_months_from_movein = (sum(Pg1_months_from_movein * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_utility_allow = (sum(Pg1_pct_utility_allow * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            total_pct_lt5kave_util_allow = (sum(Pg1_ave_util_allow * Pg1_number_reported, na.rm=TRUE))/sum(Pg1_number_reported, na.rm=TRUE),
            ## Program 2 - Public Housing (72 PLACE entries with units, 65 reporting demographics) ##
            pg2_units = sum(Pg2_total_units, na.rm=TRUE), 
            pg2_pct_occupied = ((sum(pg2_occupied_units, na.rm=TRUE))/sum(Pg2_total_units, na.rm=TRUE)*100),
            pg2_occupied_units = sum(pg2_occupied_units, na.rm=TRUE),
            pg2_pct_reported = (sum(Pg2_number_reported, na.rm=TRUE)/sum(pg2_occupied_units, na.rm=TRUE)*100),
            pg2_reported_units = sum(Pg2_number_reported, na.rm=TRUE), 
            pg2_months_since_report = (sum(Pg2_number_reported * Pg2_months_since_report, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_people_reporting = sum(Pg2_people_total, na.rm=TRUE),
            pg2_people_per_unit = (sum(Pg2_people_total, na.rm=TRUE)/sum(Pg2_number_reported, na.rm=TRUE)),
            # Rent and Income #
            pg2_rent_per_month = (sum(Pg2_rent_per_month * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_spending_per_month = (sum(Pg2_spending_per_month * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Household Income Variables #
            pg2_hh_income = (sum(Pg2_hh_income * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_person_income = (sum(Pg2_person_income * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_lt5k = (sum(Pg2_pct_lt5k * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_5k_lt10k = (sum(Pg2_pct_5k_lt10k * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_10k_lt15k = (sum(Pg2_pct_10k_lt15k * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_15k_lt20k = (sum(Pg2_pct_15k_lt20k * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_ge20k = (sum(Pg2_pct_ge20k * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Major Source of Income #
            pg2_pct_wage_major = (sum(Pg2_pct_wage_major * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_welfare_major = (sum(Pg2_pct_welfare_major * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_other_major = (sum(Pg2_pct_other_major * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_median = (sum(Pg2_pct_median * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_lt50_median = (sum(Pg2_pct_lt50_median * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_lt30_median = (sum(Pg2_pct_lt30_median * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Presence of Children #
            pg2_pct_2adults = (sum(Pg2_pct_2adults * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_1adult = (sum(Pg2_pct_1adult * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_female_head = (sum(Pg2_pct_female_head * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_female_head_child = (sum(Pg2_pct_female_head_child * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Head or Spouse or Co-Head Has a Disability #
            pg2_pct_disabled_lt62 = (sum(Pg2_pct_disabled_lt62 * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_disabled_ge62 = (sum(Pg2_pct_disabled_ge62 * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_disabled_all = (sum(Pg2_pct_disabled_all * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Age of Head/Spouse (Whoever is Older) #
            pg2_pct_lt24_head = (sum(Pg2_pct_lt24_head * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_age25_50 = (sum(Pg2_pct_age25_50 * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_age51_61 = (sum(Pg2_pct_age51_61 * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_age62plus = (sum(Pg2_pct_age62plus * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_age85plus = (sum(Pg2_pct_age85plus * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Race and Ethnicity #
            pg2_pct_minority = (sum(Pg2_pct_minority * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_black_nonhsp = (sum(Pg2_pct_black_nonhsp * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_native_american_nonhsp = (sum(Pg2_pct_native_american_nonhsp * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_asian_pacific_nonhsp = (sum(Pg2_pct_asian_pacific_nonhsp * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_white_nothsp = (sum(Pg2_pct_white_nothsp * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_black_hsp = (sum(Pg2_pct_black_hsp * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_wht_hsp = (sum(Pg2_pct_wht_hsp * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_oth_hsp = (sum(Pg2_pct_oth_hsp * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_hispanic = (sum(Pg2_pct_hispanic * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_multi = (sum(Pg2_pct_multi * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Social Conditions #
            pg2_months_waiting = (sum(Pg2_months_waiting * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_months_from_movein = (sum(Pg2_months_from_movein * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_utility_allow = (sum(Pg2_pct_utility_allow * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_lt5kave_util_allow = (sum(Pg2_ave_util_allow * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Number of Bedrooms #
            pg2_pct_bed1 = (sum(Pg2_pct_bed1 * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_bed2 = (sum(Pg2_pct_bed2 * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_bed3 = (sum(Pg2_pct_bed3 * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_pct_overhoused = (sum(Pg2_pct_overhoused * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            # Surrounding Census Tract #
            pg2_tpoverty = (sum(Pg2_tpoverty * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_tminority = (sum(Pg2_tminority * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            pg2_tpct_ownsfd = (sum(Pg2_tpct_ownsfd * Pg2_number_reported, na.rm=TRUE))/sum(Pg2_number_reported, na.rm=TRUE),
            ## Program 3 - Housing Choice Vouchers (627 PLACE entries with units, 368 reporting demographics) ##
            pg3_units = sum(Pg3_total_units, na.rm=TRUE), 
            pg3_pct_occupied = (sum(pg3_occupied_units, na.rm=TRUE))/sum(Pg3_total_units, na.rm=TRUE),
            pg3_occupied_units = sum(pg3_occupied_units, na.rm=TRUE),
            pg3_pct_reported = (sum(Pg3_number_reported, na.rm=TRUE)/sum(pg3_occupied_units, na.rm=TRUE)*100),
            pg3_reported_units = sum(Pg3_number_reported, na.rm=TRUE), 
            pg3_months_since_report = (sum(pg3_occupied_units * Pg3_months_since_report, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_people_reporting = sum(Pg3_people_total, na.rm=TRUE),
            pg3_people_per_unit = (sum(Pg3_people_total, na.rm=TRUE)/sum(pg3_occupied_units, na.rm=TRUE)),
            pg3_months_since_report = (sum(Pg3_number_reported * Pg3_months_since_report, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_people_reporting = sum(Pg3_people_total, na.rm=TRUE),
            pg3_people_per_unit = (sum(Pg3_people_total, na.rm=TRUE)/sum(Pg3_number_reported, na.rm=TRUE)),
            # Rent and Income #
            pg3_rent_per_month = (sum(Pg3_rent_per_month * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_spending_per_month = (sum(Pg3_spending_per_month * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            # Household Income Variables #
            pg3_hh_income = (sum(Pg3_hh_income * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_person_income = (sum(Pg3_person_income * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_lt5k = (sum(Pg3_pct_lt5k * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_5k_lt10k = (sum(Pg3_pct_5k_lt10k * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_10k_lt15k = (sum(Pg3_pct_10k_lt15k * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_15k_lt20k = (sum(Pg3_pct_15k_lt20k * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_ge20k = (sum(Pg3_pct_ge20k * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            # Major Source of Income #
            pg3_pct_wage_major = (sum(Pg3_pct_wage_major * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_welfare_major = (sum(Pg3_pct_welfare_major * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_other_major = (sum(Pg3_pct_other_major * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_median = (sum(Pg3_pct_median * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_lt50_median = (sum(Pg3_pct_lt50_median * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_lt30_median = (sum(Pg3_pct_lt30_median * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            # Presence of Children #
            pg3_pct_2adults = (sum(Pg3_pct_2adults * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_1adult = (sum(Pg3_pct_1adult * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_female_head = (sum(Pg3_pct_female_head * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_female_head_child = (sum(Pg3_pct_female_head_child * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            # Head or Spouse or Co-Head Has a Disability #
            pg3_pct_disabled_lt62 = (sum(Pg3_pct_disabled_lt62 * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_disabled_ge62 = (sum(Pg3_pct_disabled_ge62 * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_disabled_all = (sum(Pg3_pct_disabled_all * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            # Age of Head/Spouse (Whoever is Older) #
            pg3_pct_lt24_head = (sum(Pg3_pct_lt24_head * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_age25_50 = (sum(Pg3_pct_age25_50 * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_age51_61 = (sum(Pg3_pct_age51_61 * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_age62plus = (sum(Pg3_pct_age62plus * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_age85plus = (sum(Pg3_pct_age85plus * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            # Race and Ethnicity #
            pg3_pct_minority = (sum(Pg3_pct_minority * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_black_nonhsp = (sum(Pg3_pct_black_nonhsp * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_native_american_nonhsp = (sum(Pg3_pct_native_american_nonhsp * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_asian_pacific_nonhsp = (sum(Pg3_pct_asian_pacific_nonhsp * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_white_nothsp = (sum(Pg3_pct_white_nothsp * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_black_hsp = (sum(Pg3_pct_black_hsp * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_wht_hsp = (sum(Pg3_pct_wht_hsp * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_oth_hsp = (sum(Pg3_pct_oth_hsp * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_hispanic = (sum(Pg3_pct_hispanic * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_multi = (sum(Pg3_pct_multi * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            # Social Conditions #
            pg3_months_waiting = (sum(Pg3_months_waiting * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_months_from_movein = (sum(Pg3_months_from_movein * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_utility_allow = (sum(Pg3_pct_utility_allow * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            pg3_pct_lt5kave_util_allow = (sum(Pg3_ave_util_allow * Pg3_number_reported, na.rm=TRUE))/sum(Pg3_number_reported, na.rm=TRUE),
            ## Program 4 - Moderate Rehabilitation (13 PLACE entries with units, 8 reporting demographics) ##
            pg4_units = sum(Pg4_total_units, na.rm=TRUE), 
            pg4_pct_occupied = (sum(pg4_occupied_units, na.rm=TRUE))/sum(Pg4_total_units, na.rm=TRUE),
            pg4_occupied_units = sum(pg4_occupied_units, na.rm=TRUE),
            pg4_pct_reported = (sum(Pg4_number_reported, na.rm=TRUE)/sum(pg4_occupied_units, na.rm=TRUE)*100),
            pg4_reported_units = sum(Pg4_number_reported, na.rm=TRUE), 
            pg4_months_since_report = (sum(pg4_occupied_units * Pg4_months_since_report, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_people_reporting = sum(Pg4_people_total, na.rm=TRUE),
            pg4_people_per_unit = (sum(Pg4_people_total, na.rm=TRUE)/sum(pg4_occupied_units, na.rm=TRUE)),
            pg4_months_since_report = (sum(Pg4_number_reported * Pg4_months_since_report, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_people_reporting = sum(Pg4_people_total, na.rm=TRUE),
            pg4_people_per_unit = (sum(Pg4_people_total, na.rm=TRUE)/sum(Pg4_number_reported, na.rm=TRUE)),
            # Rent and Income #
            pg4_rent_per_month = (sum(Pg4_rent_per_month * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_spending_per_month = (sum(Pg4_spending_per_month * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            # Household Income Variables #
            pg4_hh_income = (sum(Pg4_hh_income * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_person_income = (sum(Pg4_person_income * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_lt5k = (sum(Pg4_pct_lt5k * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_5k_lt10k = (sum(Pg4_pct_5k_lt10k * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_10k_lt15k = (sum(Pg4_pct_10k_lt15k * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_15k_lt20k = (sum(Pg4_pct_15k_lt20k * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_ge20k = (sum(Pg4_pct_ge20k * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            # Major Source of Income #
            pg4_pct_wage_major = (sum(Pg4_pct_wage_major * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_welfare_major = (sum(Pg4_pct_welfare_major * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_other_major = (sum(Pg4_pct_other_major * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_median = (sum(Pg4_pct_median * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_lt50_median = (sum(Pg4_pct_lt50_median * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_lt30_median = (sum(Pg4_pct_lt30_median * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            # Presence of Children #
            pg4_pct_2adults = (sum(Pg4_pct_2adults * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_1adult = (sum(Pg4_pct_1adult * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_female_head = (sum(Pg4_pct_female_head * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_female_head_child = (sum(Pg4_pct_female_head_child * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            # Head or Spouse or Co-Head Has a Disability #
            pg4_pct_disabled_lt62 = (sum(Pg4_pct_disabled_lt62 * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_disabled_ge62 = (sum(Pg4_pct_disabled_ge62 * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_disabled_all = (sum(Pg4_pct_disabled_all * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            # Age of Head/Spouse (Whoever is Older) #
            pg4_pct_lt24_head = (sum(Pg4_pct_lt24_head * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_age25_50 = (sum(Pg4_pct_age25_50 * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_age51_61 = (sum(Pg4_pct_age51_61 * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_age62plus = (sum(Pg4_pct_age62plus * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_age85plus = (sum(Pg4_pct_age85plus * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            # Race and Ethnicity #
            pg4_pct_minority = (sum(Pg4_pct_minority * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_black_nonhsp = (sum(Pg4_pct_black_nonhsp * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_native_american_nonhsp = (sum(Pg4_pct_native_american_nonhsp * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_asian_pacific_nonhsp = (sum(Pg4_pct_asian_pacific_nonhsp * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_white_nothsp = (sum(Pg4_pct_white_nothsp * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_black_hsp = (sum(Pg4_pct_black_hsp * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_wht_hsp = (sum(Pg4_pct_wht_hsp * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_oth_hsp = (sum(Pg4_pct_oth_hsp * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_hispanic = (sum(Pg4_pct_hispanic * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_multi = (sum(Pg4_pct_multi * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            # Social Conditions #
            pg4_months_waiting = (sum(Pg4_months_waiting * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_months_from_movein = (sum(Pg4_months_from_movein * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_utility_allow = (sum(Pg4_pct_utility_allow * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            pg4_pct_lt5kave_util_allow = (sum(Pg4_ave_util_allow * Pg4_number_reported, na.rm=TRUE))/sum(Pg4_number_reported, na.rm=TRUE),
            ## Program 5 - Project Based Section 8 (224 PLACE entries with units, 155 reporting demographics) ##
            pg5_units = sum(Pg5_total_units, na.rm=TRUE), 
            pg5_pct_occupied = (sum(pg5_occupied_units, na.rm=TRUE))/sum(Pg5_total_units, na.rm=TRUE),
            pg5_occupied_units = sum(pg5_occupied_units, na.rm=TRUE),
            pg5_pct_reported = (sum(Pg5_number_reported, na.rm=TRUE)/sum(pg5_occupied_units, na.rm=TRUE)*100),
            pg5_reported_units = sum(Pg5_number_reported, na.rm=TRUE), 
            pg5_months_since_report = (sum(pg5_occupied_units * Pg5_months_since_report, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_people_reporting = sum(Pg5_people_total, na.rm=TRUE),
            pg5_people_per_unit = (sum(Pg5_people_total, na.rm=TRUE)/sum(pg5_occupied_units, na.rm=TRUE)),
            pg5_months_since_report = (sum(Pg5_number_reported * Pg5_months_since_report, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_people_reporting = sum(Pg5_people_total, na.rm=TRUE),
            pg5_people_per_unit = (sum(Pg5_people_total, na.rm=TRUE)/sum(Pg5_number_reported, na.rm=TRUE)),
            # Rent and Income #
            pg5_rent_per_month = (sum(Pg5_rent_per_month * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_spending_per_month = (sum(Pg5_spending_per_month * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            # Household Income Variables #
            pg5_hh_income = (sum(Pg5_hh_income * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_person_income = (sum(Pg5_person_income * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_lt5k = (sum(Pg5_pct_lt5k * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_5k_lt10k = (sum(Pg5_pct_5k_lt10k * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_10k_lt15k = (sum(Pg5_pct_10k_lt15k * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_15k_lt20k = (sum(Pg5_pct_15k_lt20k * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_ge20k = (sum(Pg5_pct_ge20k * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            # Major Source of Income #
            pg5_pct_wage_major = (sum(Pg5_pct_wage_major * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_welfare_major = (sum(Pg5_pct_welfare_major * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_other_major = (sum(Pg5_pct_other_major * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_median = (sum(Pg5_pct_median * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_lt50_median = (sum(Pg5_pct_lt50_median * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_lt30_median = (sum(Pg5_pct_lt30_median * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            # Presence of Children #
            pg5_pct_2adults = (sum(Pg5_pct_2adults * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_1adult = (sum(Pg5_pct_1adult * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_female_head = (sum(Pg5_pct_female_head * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_female_head_child = (sum(Pg5_pct_female_head_child * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            # Head or Spouse or Co-Head Has a Disability #
            pg5_pct_disabled_lt62 = (sum(Pg5_pct_disabled_lt62 * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_disabled_ge62 = (sum(Pg5_pct_disabled_ge62 * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_disabled_all = (sum(Pg5_pct_disabled_all * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            # Age of Head/Spouse (Whoever is Older) #
            pg5_pct_lt24_head = (sum(Pg5_pct_lt24_head * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_age25_50 = (sum(Pg5_pct_age25_50 * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_age51_61 = (sum(Pg5_pct_age51_61 * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_age62plus = (sum(Pg5_pct_age62plus * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_age85plus = (sum(Pg5_pct_age85plus * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            # Race and Ethnicity #
            pg5_pct_minority = (sum(Pg5_pct_minority * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_black_nonhsp = (sum(Pg5_pct_black_nonhsp * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_native_american_nonhsp = (sum(Pg5_pct_native_american_nonhsp * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_asian_pacific_nonhsp = (sum(Pg5_pct_asian_pacific_nonhsp * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_white_nothsp = (sum(Pg5_pct_white_nothsp * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_black_hsp = (sum(Pg5_pct_black_hsp * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_wht_hsp = (sum(Pg5_pct_wht_hsp * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_oth_hsp = (sum(Pg5_pct_oth_hsp * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_hispanic = (sum(Pg5_pct_hispanic * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_multi = (sum(Pg5_pct_multi * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            # Social Conditions #
            pg5_months_waiting = (sum(Pg5_months_waiting * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_months_from_movein = (sum(Pg5_months_from_movein * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_utility_allow = (sum(Pg5_pct_utility_allow * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            pg5_pct_lt5kave_util_allow = (sum(Pg5_ave_util_allow * Pg5_number_reported, na.rm=TRUE))/sum(Pg5_number_reported, na.rm=TRUE),
            ## Program 6 - RentSup/RAP (1 PLACE entry with units, 0 reporting demographics) ##
            pg6_units = sum(Pg6_total_units, na.rm=TRUE), # nothing more to say since no reporting (all NAs)
            ## Program 7 - S236/BMIR (12 PLACE entries with units, 0 reporting demographics) ##
            pg7_units = sum(Pg7_total_units, na.rm=TRUE), # nothing more to say since no demographic reporting
            pg7_reported = sum(Pg7_number_reported, na.rm=TRUE), 
            pg7_pct_reported = (sum(Pg7_number_reported, na.rm=TRUE)/sum(Pg7_total_units, na.rm=TRUE)*100),
            ## Program 8 - 202/PRAC (60 PLACE entries with units, 50 reporting demographics) ##
            pg8_units = sum(Pg8_total_units, na.rm=TRUE), 
            pg8_pct_occupied = (sum(pg8_occupied_units, na.rm=TRUE))/sum(Pg8_total_units, na.rm=TRUE),
            pg8_occupied_units = sum(pg8_occupied_units, na.rm=TRUE),
            pg8_pct_reported = (sum(Pg8_number_reported, na.rm=TRUE)/sum(pg8_occupied_units, na.rm=TRUE)*100),
            pg8_reported_units = sum(Pg8_number_reported, na.rm=TRUE), 
            pg8_months_since_report = (sum(pg8_occupied_units * Pg8_months_since_report, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_people_reporting = sum(Pg8_people_total, na.rm=TRUE),
            pg8_people_per_unit = (sum(Pg8_people_total, na.rm=TRUE)/sum(pg8_occupied_units, na.rm=TRUE)),
            pg8_months_since_report = (sum(Pg8_number_reported * Pg8_months_since_report, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_people_reporting = sum(Pg8_people_total, na.rm=TRUE),
            pg8_people_per_unit = (sum(Pg8_people_total, na.rm=TRUE)/sum(Pg8_number_reported, na.rm=TRUE)),
            # Rent and Income #
            pg8_rent_per_month = (sum(Pg8_rent_per_month * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_spending_per_month = (sum(Pg8_spending_per_month * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            # Household Income Variables #
            pg8_hh_income = (sum(Pg8_hh_income * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_person_income = (sum(Pg8_person_income * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_lt5k = (sum(Pg8_pct_lt5k * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_5k_lt10k = (sum(Pg8_pct_5k_lt10k * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_10k_lt15k = (sum(Pg8_pct_10k_lt15k * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_15k_lt20k = (sum(Pg8_pct_15k_lt20k * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_ge20k = (sum(Pg8_pct_ge20k * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            # Major Source of Income #
            pg8_pct_wage_major = (sum(Pg8_pct_wage_major * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_welfare_major = (sum(Pg8_pct_welfare_major * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_other_major = (sum(Pg8_pct_other_major * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_median = (sum(Pg8_pct_median * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_lt50_median = (sum(Pg8_pct_lt50_median * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_lt30_median = (sum(Pg8_pct_lt30_median * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            # Presence of Children #
            pg8_pct_2adults = (sum(Pg8_pct_2adults * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_1adult = (sum(Pg8_pct_1adult * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_female_head = (sum(Pg8_pct_female_head * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_female_head_child = (sum(Pg8_pct_female_head_child * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            # Head or Spouse or Co-Head Has a Disability #
            pg8_pct_disabled_lt62 = (sum(Pg8_pct_disabled_lt62 * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_disabled_ge62 = (sum(Pg8_pct_disabled_ge62 * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_disabled_all = (sum(Pg8_pct_disabled_all * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            # Age of Head/Spouse (Whoever is Older) #
            pg8_pct_lt24_head = (sum(Pg8_pct_lt24_head * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_age25_50 = (sum(Pg8_pct_age25_50 * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_age51_61 = (sum(Pg8_pct_age51_61 * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_age62plus = (sum(Pg8_pct_age62plus * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_age85plus = (sum(Pg8_pct_age85plus * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            # Race and Ethnicity #
            pg8_pct_minority = (sum(Pg8_pct_minority * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_black_nonhsp = (sum(Pg8_pct_black_nonhsp * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_native_american_nonhsp = (sum(Pg8_pct_native_american_nonhsp * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_asian_pacific_nonhsp = (sum(Pg8_pct_asian_pacific_nonhsp * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_white_nothsp = (sum(Pg8_pct_white_nothsp * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_black_hsp = (sum(Pg8_pct_black_hsp * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_wht_hsp = (sum(Pg8_pct_wht_hsp * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_oth_hsp = (sum(Pg8_pct_oth_hsp * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_hispanic = (sum(Pg8_pct_hispanic * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_multi = (sum(Pg8_pct_multi * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            # Social Conditions #
            pg8_months_waiting = (sum(Pg8_months_waiting * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_months_from_movein = (sum(Pg8_months_from_movein * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_utility_allow = (sum(Pg8_pct_utility_allow * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),
            pg8_pct_lt5kave_util_allow = (sum(Pg8_ave_util_allow * Pg8_number_reported, na.rm=TRUE))/sum(Pg8_number_reported, na.rm=TRUE),        
            ## Program 9 - 811/PRACs (120 PLACE entries with units, 24 reporting demographics) ##
            pg9_units = sum(Pg9_total_units, na.rm=TRUE), 
            pg9_pct_occupied = (sum(pg9_occupied_units, na.rm=TRUE))/sum(Pg9_total_units, na.rm=TRUE),
            pg9_occupied_units = sum(pg9_occupied_units, na.rm=TRUE),
            pg9_pct_reported = (sum(Pg9_number_reported, na.rm=TRUE)/sum(pg9_occupied_units, na.rm=TRUE)*100),
            pg9_reported_units = sum(Pg9_number_reported, na.rm=TRUE), 
            pg9_months_since_report = (sum(pg9_occupied_units * Pg9_months_since_report, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_people_reporting = sum(Pg9_people_total, na.rm=TRUE),
            pg9_people_per_unit = (sum(Pg9_people_total, na.rm=TRUE)/sum(pg9_occupied_units, na.rm=TRUE)),
            pg9_months_since_report = (sum(Pg9_number_reported * Pg9_months_since_report, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_people_reporting = sum(Pg9_people_total, na.rm=TRUE),
            pg9_people_per_unit = (sum(Pg9_people_total, na.rm=TRUE)/sum(Pg9_number_reported, na.rm=TRUE)),
            # Rent and Income #
            pg9_rent_per_month = (sum(Pg9_rent_per_month * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_spending_per_month = (sum(Pg9_spending_per_month * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            # Household Income Variables #
            pg9_hh_income = (sum(Pg9_hh_income * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_person_income = (sum(Pg9_person_income * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_lt5k = (sum(Pg9_pct_lt5k * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_5k_lt10k = (sum(Pg9_pct_5k_lt10k * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_10k_lt15k = (sum(Pg9_pct_10k_lt15k * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_15k_lt20k = (sum(Pg9_pct_15k_lt20k * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_ge20k = (sum(Pg9_pct_ge20k * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            # Major Source of Income #
            pg9_pct_wage_major = (sum(Pg9_pct_wage_major * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_welfare_major = (sum(Pg9_pct_welfare_major * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_other_major = (sum(Pg9_pct_other_major * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_median = (sum(Pg9_pct_median * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_lt50_median = (sum(Pg9_pct_lt50_median * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_lt30_median = (sum(Pg9_pct_lt30_median * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            # Presence of Children #
            pg9_pct_2adults = (sum(Pg9_pct_2adults * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_1adult = (sum(Pg9_pct_1adult * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_female_head = (sum(Pg9_pct_female_head * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_female_head_child = (sum(Pg9_pct_female_head_child * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            # Head or Spouse or Co-Head Has a Disability #
            pg9_pct_disabled_lt62 = (sum(Pg9_pct_disabled_lt62 * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_disabled_ge62 = (sum(Pg9_pct_disabled_ge62 * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_disabled_all = (sum(Pg9_pct_disabled_all * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            # Age of Head/Spouse (Whoever is Older) #
            pg9_pct_lt24_head = (sum(Pg9_pct_lt24_head * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_age25_50 = (sum(Pg9_pct_age25_50 * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_age51_61 = (sum(Pg9_pct_age51_61 * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_age62plus = (sum(Pg9_pct_age62plus * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_age85plus = (sum(Pg9_pct_age85plus * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            # Race and Ethnicity #
            pg9_pct_minority = (sum(Pg9_pct_minority * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_black_nonhsp = (sum(Pg9_pct_black_nonhsp * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_native_american_nonhsp = (sum(Pg9_pct_native_american_nonhsp * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_asian_pacific_nonhsp = (sum(Pg9_pct_asian_pacific_nonhsp * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_white_nothsp = (sum(Pg9_pct_white_nothsp * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_black_hsp = (sum(Pg9_pct_black_hsp * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_wht_hsp = (sum(Pg9_pct_wht_hsp * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_oth_hsp = (sum(Pg9_pct_oth_hsp * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_hispanic = (sum(Pg9_pct_hispanic * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_multi = (sum(Pg9_pct_multi * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            # Social Conditions #
            pg9_months_waiting = (sum(Pg9_months_waiting * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_months_from_movein = (sum(Pg9_months_from_movein * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_utility_allow = (sum(Pg9_pct_utility_allow * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE),
            pg9_pct_lt5kave_util_allow = (sum(Pg9_ave_util_allow * Pg9_number_reported, na.rm=TRUE))/sum(Pg9_number_reported, na.rm=TRUE)) %>% distinct(ctycsub, .keep_all = TRUE)

place_PSH_trcts_Final <- place_PSH_trcts_Final %>% mutate_all(~ifelse(is.nan(.), NA, .)) # remove duplicate rows and NaN values
#place_PSH_trcts_Final$name[is.na(place_PSH_trcts_Final$ctycsub)] <- "Missing" # 


#renaming variable to match full place file
place_PSH_trcts_Final<-place_PSH_trcts_Final %>% 
  rename(
    CENSUS2010 = ctycsub)

place_PSH_trcts_Final$county<-place_PSH_trcts_Final$name

place_PSH_trcts_Final$county <- str_extract_all(place_PSH_trcts_Final$county, "\\([^()]+\\)")[[1]]
place_PSH_trcts_Final$county<-gsub(", NJ)","",as.character(place_PSH_trcts_Final$county))
place_PSH_trcts_Final$county <-sub('.', '', place_PSH_trcts_Final$county)


# replace all the infinite and NaN values with NA to avoid causing trouble with mapping?
write.csv(place_PSH_trcts_Final, 'C:\\Users\\laurenenolan\\Box\\NJSOARH\\Data\\Picture of Subsidized Households\\Aggregated Cousub Data\\place_PSH_trcts_Final.csv', na = "")

