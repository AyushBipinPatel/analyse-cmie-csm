## This file contains script to generate 
## statistics of interest for all states
## across all regions, gender category
## and caste category.


# libraries ---------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(DescTools)

# load data and generate weights ------------------------------------------

## People of India ----

people <- readr::read_csv(
  here("data","data-for-descriptive-analysis",
       "people_of_india_20230101_20230430_R.csv")
)|>
  
  ### only take accepted survey responses
  
  filter(RESPONSE_STATUS == "Accepted")|>
  
  ### create a unique id for each member
  
  mutate(
    uid = paste(HH_ID,MEM_ID,sep = "")
  )|>
  
  ### create state and country level weights
  
  mutate(
    w_state = MEM_WEIGHT_FOR_STATE_W *
      MEM_NON_RESPONSE_FOR_STATE_W,
    w_country = MEM_WEIGHT_FOR_COUNTRY_W *
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )


## Aspirational India ----

aspiration <- readr::read_csv(
  here("data","data-for-descriptive-analysis",
       "aspirational_india_20230101_20230430_R.csv")
)|>
  
  ### Keep only accepted survey responses
  
  filter(RESPONSE_STATUS == "Accepted")|>
  
  ### create weights for country and state
  
  mutate(
    w_state = HH_WEIGHT_FOR_STATE_W *
      HH_NON_RESPONSE_FOR_STATE_W,
    w_country = HH_WEIGHT_FOR_COUNTRY_W *
      HH_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  
  ## refactor income levels
  
  mutate(
    INCOME_GROUP = forcats::fct(INCOME_GROUP,
                                levels = c(
                                  "<=36000",
                                  "36000 - 48000",
                                  "48000 - 60000",
                                  "60000 - 72000",
                                  "72000 - 84000",
                                  "84000 - 100000",
                                  "100000 - 120000",
                                  "120000 - 150000",
                                  "150000 - 200000",
                                  "200000 - 250000",
                                  "250000 - 300000",
                                  "300000 - 400000",
                                  "400000 - 500000",
                                  "500000 - 600000",
                                  "600000 - 700000",
                                  "700000 - 800000",
                                  "800000 - 900000",
                                  "900000 - 1000000",
                                  "1000000 - 1200000",
                                  "1200000 - 1500000",
                                  "1500000 - 1800000",
                                  "1800000 - 2000000",
                                  "2000000 - 2400000",
                                  "2400000 - 3600000",
                                  ">3600000"
                                ))
  )



# Create target group using people data -----------------------------------

## get uid of members who are TG ----

people|>
  
  ### get people only above 15 years of age
  
  filter(AGE_YRS>15 | 
           (AGE_YRS ==15 & AGE_MTHS >0))|> 
  
  ### These occupation natures are selected 
  ### to based on various occupations each 
  ### includes
  
  filter(NATURE_OF_OCCUPATION %in% c(
    "Wage Labourer",
    "Industrial Workers",
    "Non-Industrial Technical Employee",
    "Small Trader/Hawker/ Businessman without Fixed Premises",
    "Home-based Worker",
    "Self Employed Entrepreneur",
    "Support Staff",
    "White Collar Clerical Employees"
  ))|>
  
  ### excluding those who are businessmen/owner
  
  filter(OCCUPATION != "Smaller businessmen (smaller shops or offices), Shopkeepers, small dhaba owners")|>
  
  ### TG does not have a PF account
  
  filter(HAS_PF_AC == "N")|>
  
  ### PLFS mentions that a worker is someone 
  ### who is employed ...
  
  filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
  
  ### All members
  
  pull(uid) -> all_india_TG_members


## Get unique HHID of all TG members ----

people|>
  
  ### get people only above 15 years of age
  
  filter(AGE_YRS>15 | 
           (AGE_YRS ==15 & AGE_MTHS >0))|> 
  
  ### These occupation natures are selected 
  ### to based on various occupations each 
  ### includes
  
  filter(NATURE_OF_OCCUPATION %in% c(
    "Wage Labourer",
    "Industrial Workers",
    "Non-Industrial Technical Employee",
    "Small Trader/Hawker/ Businessman without Fixed Premises",
    "Home-based Worker",
    "Self Employed Entrepreneur",
    "Support Staff",
    "White Collar Clerical Employees"
  ))|>
  
  ### excluding those who are businessmen/owner
  
  filter(OCCUPATION != "Smaller businessmen (smaller shops or offices), Shopkeepers, small dhaba owners")|>
  
  ### TG does not have a PF account
  
  filter(HAS_PF_AC == "N")|>
  
  ### PLFS mentions that a worker is someone 
  ### who is employed ...
  
  filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
  
  ### All members
  
  pull(HH_ID)|>
  
  ### Get unique HHID as more that on TG 
  ### member can be from same HH
  
  unique() -> all_india_TG_hhs


## Mark members in people who are TG ----

people|>
  mutate(
    is_tg = ifelse(
      uid %in% all_india_TG_members,
      1,0
    )
  )-> people

## Mark HHs in aspiration that have at least 1 TG member ----

aspiration|>
  mutate(
    is_tg = ifelse(
      HH_ID %in% all_india_TG_hhs,
      1,0
    )
  ) -> aspiration


# Keep variables of interest only -----------------------------------------

## People ----

people|>
  select(
    STATE,
    HH_ID,
    MEM_ID,
    AGE_YRS,
    AGE_MTHS,
    is_tg,
    w_state,
    w_country,
    REGION_TYPE,
    GENDER,
    RELIGION,
    CASTE_CATEGORY,
    EDUCATION,
    EMPLOYMENT_STATUS,
    EMPLOYMENT_ARRANGEMENT,
    HAS_BANK_AC,
    HAS_CREDITCARD,
    HAS_PF_AC,
    HAS_LIC,
    HAS_HEALTH_INSURANCE,
    HAS_MOBILE
  ) -> people

## Aspiration ----

aspiration|>
  select(
    STATE,
    HH_ID,
    REGION_TYPE,
    is_tg,
    w_state,
    w_country,
    INCOME_GROUP,
    HOUSES_OWNED,
    REFRIGERATORS_OWNED,
    AIR_CONDITIONERS_OWNED,
    COOLERS_OWNED,
    WASHING_MACHINES_OWNED,
    TELEVISIONS_OWNED,
    COMPUTERS_OWNED,
    CARS_OWNED,
    TWO_WHEELERS_OWNED,
    HAS_OUTSTANDING_SAVING_IN_FIXED_DEPOSITS,
    HAS_OUTSTANDING_SAVING_IN_POST_OFFICE_SAVINGS,
    HAS_OUTSTANDING_SAVING_IN_NSC_BONDS,
    HAS_OUTSTANDING_SAVING_IN_PROVIDENT_FUND,
    HAS_OUTSTANDING_SAVING_IN_LIFE_INSURANCE,
    HAS_OUTSTANDING_SAVING_IN_MUTUAL_FUNDS,
    HAS_OUTSTANDING_SAVING_IN_LISTED_SHARES,
    HAS_OUTSTANDING_SAVING_IN_BUSINESS,
    HAS_OUTSTANDING_SAVING_IN_GOLD,
    HAS_OUTSTANDING_BORROWING,
    BORROWED_FOR_HOUSING,
    BORROWED_FOR_EDUCATION,
    BORROWED_FOR_MEDICAL_EXPENDITURE,
    BORROWED_FOR_CONSUMPTION_EXPENDITURE,
    BORROWED_FOR_CONSUMER_DURABLES,
    BORROWED_FOR_WEDDING,
    HAS_ACCESS_TO_ELECTRICITY,
    POWER_AVAILABILITY_IN_HOURS_PER_DAY,
    HAS_ACCESS_TO_WATER_IN_HOUSE,
    WATER_AVAILABILITY_IN_DAYS_PER_WEEK,
    WATER_AVAILABILITY_IN_HOURS_PER_DAY,
    HAS_TOILET_IN_HOUSE
    
  ) -> aspiration


# nest people and aspiration  ---------------------------------------------

## The goal is to create statistics for every state
## We nest the people and aspiration data by state

## nest people ----

people|>
  nest(.by = STATE) -> nested_people

## nest aspiration ----

aspiration|>
  nest(.by = STATE) -> nested_aspiration



# calculate state level statistics for people data ------------------------

## Number,share of people in TG ----

nested_people|>
  
  ### create columns in nested data for state level stats
  
  mutate(
    
    ### Number of people in TG in the entire state
    
    num_in_tg_state = map_dbl(
      data,
      ~ filter(.x, is_tg == 1)|>
        pull(w_state)|>
        sum()
    ),
    
    ### Proportion of people in TG in the entire state
    
    prop_in_tg_state = map(
      data,
      ~ group_by(.x,is_tg)|>
        summarise(
          prop_in_tg_state = sum(w_state)
        )|>
        adorn_percentages(denominator = "col")
    ),
    
    ### Proportion of people in TG in entire state w.r.t. all above 15 and employed
    
    prop_in_tg_wrt_above15_and_employed_state = map(
      data,
      ~ filter(.x,AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg)|>
        summarise(
          prop_in_tg_wrt_above15_and_employed_state = sum(w_state)
        )|>
        adorn_percentages(denominator = "col")
    ),
    
    ### Proportion of TG in Region type
    
    prop_of_tg_in_region_type = map(
      data,
      ~ filter(.x, is_tg == 1)|>
        group_by(REGION_TYPE)|>
        summarise(
          prop_of_tg_in_region_type = sum(w_state)
        )|>
      adorn_percentages(denominator = "col")
    ),
    
    ### Proportion of TG in Gender type
    
    prop_of_tg_in_gender = map(
      data,
      ~ filter(.x, is_tg == 1)|>
        group_by(GENDER)|>
        summarise(
          prop_of_tg_in_gender = sum(w_state)
        )|>
        adorn_percentages(denominator = "col")
    ),
    
    ### Proportion of TG in Gender type in Urban area
    
    prop_of_tg_in_gender_in_urban = map(
      data,
      ~ filter(.x, is_tg == 1 &
               REGION_TYPE == "URBAN")|>
        group_by(GENDER)|>
        summarise(
          prop_of_tg_in_gender_in_urban = sum(w_state)
        )|>
        adorn_percentages(denominator = "col")
    ),
    
    ### Proportion of TG in caste category in Urban area
    
    prop_of_tg_in_castecategory_in_urban = map(
      data,
      ~ filter(.x, is_tg == 1 &
                 REGION_TYPE == "URBAN")|>
        group_by(CASTE_CATEGORY)|>
        summarise(
          prop_of_tg_in_castecategory_in_urban = sum(w_state)
        )|>
        adorn_percentages(denominator = "col")
    ),
  ) -> nested_people


## Employment arrangement comparison ----

nested_people|>
  mutate(
    
    ### Distribution of TG across employment arrangement in urban area
    
    dist_of_tg_acr_emp_arr_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(is_tg == 1)|>
        group_by(EMPLOYMENT_ARRANGEMENT)|>
        summarise(
          dist_of_tg_acr_emp_arr_urb = sum(w_state)
        )
        
    ),
    
    ### Distribution of TG across employment arrangement and gender in urban area
    
    dist_of_tg_acr_emp_arr_gender_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(is_tg == 1)|>
        group_by(EMPLOYMENT_ARRANGEMENT, GENDER)|>
        summarise(
          dist_of_tg_acr_emp_arr_gender_urb = sum(w_state)
        )
      
    ),
    
    ### Distribution of TG across employment arrangement and castecatgory in urban area
    
    dist_of_tg_acr_emp_arr_cc_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(is_tg == 1)|>
        group_by(EMPLOYMENT_ARRANGEMENT, CASTE_CATEGORY)|>
        summarise(
          dist_of_tg_acr_emp_arr_cc_urb = sum(w_state)
        )
      
    ),
    
    ### Distribution of NTG across employment arrangement in urban area
    
    dist_of_ntg_acr_emp_arr_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        filter(is_tg == 0)|>
        group_by(EMPLOYMENT_ARRANGEMENT)|>
        summarise(
          dist_of_ntg_acr_emp_arr_urb = sum(w_state)
        )
      
    ),
    
    ### Distribution of NTG across employment arrangement and gender in urban area
    
    dist_of_ntg_acr_emp_arr_gender_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        filter(is_tg == 0)|>
        group_by(EMPLOYMENT_ARRANGEMENT, GENDER)|>
        summarise(
          dist_of_ntg_acr_emp_arr_gender_urb = sum(w_state)
        )
      
    ),
    
    ### Distribution of NTG across employment arrangement and castecatgory in urban area
    
    dist_of_ntg_acr_emp_arr_cc_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        filter(is_tg == 0)|>
        group_by(EMPLOYMENT_ARRANGEMENT, CASTE_CATEGORY)|>
        summarise(
          dist_of_ntg_acr_emp_arr_cc_urb = sum(w_state)
        )
      
    )
    
  ) -> nested_people



# bank ac, mobile, pf,insurance -------------------------------------------


nested_people|>
  mutate(
    
    ### proportion of people in TG and NTG with bank in urban area 
    
    prop_TG_or_NTH_has_bnk_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg)|>
        summarise(
          prop_TG_or_NTH_has_bnk_urb = Mean(
            ifelse(
              HAS_BANK_AC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
        
    ),
    
    ### proportion of people in TG and NTG with bank across gender in urban area 
    
    prop_TG_or_NTH_has_bnk_gender_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,GENDER)|>
        summarise(
          prop_TG_or_NTH_has_bnk_gender_urb = Mean(
            ifelse(
              HAS_BANK_AC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with bank across caste category in urban area 
    
    prop_TG_or_NTH_has_bnk_cc_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,CASTE_CATEGORY)|>
        summarise(
          prop_TG_or_NTH_has_bnk_cc_urb = Mean(
            ifelse(
              HAS_BANK_AC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with credit card in urban area 
    
    prop_TG_or_NTH_has_crec_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg)|>
        summarise(
          prop_TG_or_NTH_has_crec_urb = Mean(
            ifelse(
              HAS_CREDITCARD == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with credit card across gender in urban area 
    
    prop_TG_or_NTH_has_crec_gender_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,GENDER)|>
        summarise(
          prop_TG_or_NTH_has_crec_gender_urb = Mean(
            ifelse(
              HAS_CREDITCARD == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with cred card across caste category in urban area 
    
    prop_TG_or_NTH_has_crec_cc_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,CASTE_CATEGORY)|>
        summarise(
          prop_TG_or_NTH_has_crec_cc_urb = Mean(
            ifelse(
              HAS_CREDITCARD == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with PF in urban area 
    
    prop_TG_or_NTH_has_pfac_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg)|>
        summarise(
          prop_TG_or_NTH_has_pfac_urb = Mean(
            ifelse(
              HAS_PF_AC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with PF across gender in urban area 
    
    prop_TG_or_NTH_has_pfac_gender_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,GENDER)|>
        summarise(
          prop_TG_or_NTH_has_pfac_gender_urb = Mean(
            ifelse(
              HAS_PF_AC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with PF across caste category in urban area 
    
    prop_TG_or_NTH_has_pfac_cc_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,CASTE_CATEGORY)|>
        summarise(
          prop_TG_or_NTH_has_pfac_cc_urb = Mean(
            ifelse(
              HAS_PF_AC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with LIC in urban area 
    
    prop_TG_or_NTH_has_lic_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg)|>
        summarise(
          prop_TG_or_NTH_has_lic_urb = Mean(
            ifelse(
              HAS_LIC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with LIC across gender in urban area 
    
    prop_TG_or_NTH_has_lic_gender_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,GENDER)|>
        summarise(
          prop_TG_or_NTH_has_lic_gender_urb = Mean(
            ifelse(
              HAS_LIC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with LIC across caste category in urban area 
    
    prop_TG_or_NTH_has_lic_cc_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,CASTE_CATEGORY)|>
        summarise(
          prop_TG_or_NTH_has_lic_cc_urb = Mean(
            ifelse(
              HAS_LIC == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with Health Insurance in urban area 
    
    prop_TG_or_NTH_has_hi_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg)|>
        summarise(
          prop_TG_or_NTH_has_hi_urb = Mean(
            ifelse(
              HAS_HEALTH_INSURANCE == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with Health Insurance across gender in urban area 
    
    prop_TG_or_NTH_has_hi_gender_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,GENDER)|>
        summarise(
          prop_TG_or_NTH_has_hi_gender_urb = Mean(
            ifelse(
              HAS_HEALTH_INSURANCE == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with Health Insurance across caste category in urban area 
    
    prop_TG_or_NTH_has_hi_cc_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,CASTE_CATEGORY)|>
        summarise(
          prop_TG_or_NTH_has_hi_cc_urb = Mean(
            ifelse(
              HAS_HEALTH_INSURANCE == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with mobile in urban area 
    
    prop_TG_or_NTH_has_mob_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg)|>
        summarise(
          prop_TG_or_NTH_has_mob_urb = Mean(
            ifelse(
              HAS_MOBILE == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with mobile across gender in urban area 
    
    prop_TG_or_NTH_has_mob_gender_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,GENDER)|>
        summarise(
          prop_TG_or_NTH_has_mob_gender_urb = Mean(
            ifelse(
              HAS_MOBILE == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    ),
    
    ### proportion of people in TG and NTG with mobile across caste category in urban area 
    
    prop_TG_or_NTH_has_mob_cc_urb = map(
      data,
      ~ filter(.x, REGION_TYPE == "URBAN")|>
        filter(AGE_YRS>15 | 
                 (AGE_YRS ==15 & AGE_MTHS >0))|>
        filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
        group_by(is_tg,CASTE_CATEGORY)|>
        summarise(
          prop_TG_or_NTH_has_mob_cc_urb = Mean(
            ifelse(
              HAS_MOBILE == "Y",
              1,0
            ),
            weights = w_state
          )
        )
      
    )
    
  ) -> nested_people


# save the people stats ---------------------------------------------------

## here I save the summary stats of people's
## data. To do that i remove the data column 
## from the nested data and then save the remaining
## as a rda file.


nested_people|>
  select(-data) -> people_summary_stats

save(people_summary_stats,
     file = "data/cmie-summary-results.RData")
