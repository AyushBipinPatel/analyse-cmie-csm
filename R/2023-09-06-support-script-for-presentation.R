## A presentation is scheduled with Shrayana, WB, to
## discuss the secondary analysis of CMIE on the 11th Sept.

## This script is to perform the the back-end calculations
## for the presentation.

## This Script will essentialy try and answer the following
## questions:

### Volume of Emigrants/Immigrant.
#### Reason for Em/Im.
#### Various characteristics of these people or 
#### people related.


# load libraries ----------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)



# load data ---------------------------------------------------------------

# People of India data
people <- readr::read_csv(
  here("data","data-for-descriptive-analysis",
       "people_of_india_20230101_20230430_R.csv")
)

## Keep only accepted responses from the 'people' data

people|>
  filter(RESPONSE_STATUS == "Accepted") -> people

# Aspirational India data

aspiration <- readr::read_csv(
  here("data","data-for-descriptive-analysis",
       "aspirational_india_20230101_20230430_R.csv")
)

## Keep only accepted responses from the 'people' data

aspiration|>
  filter(RESPONSE_STATUS == "Accepted") -> aspiration



# Descriptives People of India --------------------------------------------

## Variables of Interest ----

names(people)

    # Member Status
    # 
    # Reason for Emigration or Immigration
    # 
    # Emigrated/Immigrated from to state
    # 
    # Emigrated/Immigrated from to district

    # Will Emigrate
    # 
    # Gender 
    # 
    # Age
    # 
    # Marital Status
    # 
    # Religion
    # 
    # Caste Category
    # 
    # Education
    # 
    # Nature of occupation
    # 
    # Occupation
    # 
    # Employment Status
    # 
    # Time measures on various activities



## Basic Numbers  ----

  ### Total Number of Indians ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  pull(w)|>
  sum() -> people_tot_indians


  ### Total Number of Indians - Rural/Urban----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(REGION_TYPE)|>
  summarise(
    pop = sum(w)
  ) -> people_indians_u_r


  ### Total Number of Indians - Gender ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(GENDER)|>
  summarise(
    pop = sum(w)
  )-> people_indians_gender


  ### Total Number of Indians - Member Status ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(MEMBER_STATUS)|>
  summarise(
    pop = sum(w)
  )-> people_indians_mem_status



  ### Total Number of Indians - Religion ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(RELIGION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(pop)-> people_indians_religion



  ### Total Number of Indians - Caste Category ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(CASTE_CATEGORY)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(pop) -> people_indians_caste_category



  ### Total Number of Indians - Education ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(EDUCATION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(pop) -> people_indians_education



  ### Total Number of Indians - Occupation ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(OCCUPATION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> people_indians_occupations


people|>
  filter(STATE == "Maharashtra")|>
  mutate(
    w = MEM_WEIGHT_FOR_STATE_W * 
      MEM_NON_RESPONSE_FOR_STATE_W
  )|>
  group_by(OCCUPATION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> people_maharashtra_occupations




  ### Total Number of Indians - Nature of Occupation ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(NATURE_OF_OCCUPATION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> people_indians_nature_occupation


people|>
  filter(STATE == "Maharashtra")|>
  mutate(
    w = MEM_WEIGHT_FOR_STATE_W * 
      MEM_NON_RESPONSE_FOR_STATE_W
  )|>
  group_by(NATURE_OF_OCCUPATION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> people_maharashtra_nature_occupation

### figure out the over lap in three target grouops

people|>
  filter(STATE == "Maharashtra")|>
  filter(NATURE_OF_OCCUPATION %in% c("Wage Labourer",
                                     "Industrial Workers",
                                     "Non-Industrial Technical Employee",
                                     "Small Trader/Hawker/ Businessman without Fixed Premises",
                                     "Home-based Worker"))|>
  mutate(
    w = MEM_WEIGHT_FOR_STATE_W * 
      MEM_NON_RESPONSE_FOR_STATE_W
  )|>
  group_by(OCCUPATION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> mh_tg1

people|>
  filter(STATE == "Maharashtra")|>
  filter(NATURE_OF_OCCUPATION %in% c("Support Staff",
                                     "White Collar Clerical Employees" ))|>
  mutate(
    w = MEM_WEIGHT_FOR_STATE_W * 
      MEM_NON_RESPONSE_FOR_STATE_W
  )|>
  group_by(OCCUPATION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop))-> mh_tg2

people|>
  filter(STATE == "Maharashtra")|>
  filter(NATURE_OF_OCCUPATION %in% c("Self Employed Entrepreneur"))|>
  mutate(
    w = MEM_WEIGHT_FOR_STATE_W * 
      MEM_NON_RESPONSE_FOR_STATE_W
  )|>
  group_by(OCCUPATION)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop))-> mh_tg3

table(c(mh_tg1$OCCUPATION,
        mh_tg2$OCCUPATION,
        mh_tg3$OCCUPATION))[table(c(mh_tg1$OCCUPATION,
                                    mh_tg2$OCCUPATION,
                                    mh_tg3$OCCUPATION))>1]|> 
  names() -> common_in_all


  mh_tg1|>
    filter(OCCUPATION %in% common_in_all)|>
    rename("Population TG1" = pop)|>
    arrange(OCCUPATION)|>
    left_join(mh_tg2|>
                filter(OCCUPATION %in% common_in_all)|>
                rename("Population TG2" = pop)|>
                arrange(OCCUPATION),by = join_by(OCCUPATION))|>
    left_join(
      mh_tg3|>
        filter(OCCUPATION %in% common_in_all)|>
        rename("Population TG3" = pop)|>
        arrange(OCCUPATION),
      by = join_by(OCCUPATION)
    )|>
    mutate(
      across(
        .cols = -OCCUPATION,
        .fns = ~ifelse(is.na(.x),0, .x)
      ),
      `Total Population` = `Population TG1` + `Population TG2` + `Population TG3`
    )|>
    arrange(desc(`Total Population`))-> mh_blurred_lines_tg
  
  


  ### Total Number of Indians - Employment Status ----


people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(EMPLOYMENT_STATUS)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> people_indians_employment_status

people|>
  filter(STATE == "Maharashtra")|>
  mutate(
    w = MEM_WEIGHT_FOR_STATE_W * 
      MEM_NON_RESPONSE_FOR_STATE_W
  )|>
  group_by(EMPLOYMENT_STATUS)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> people_maharashtra_employment_status



  ### Total Number of Indians - Employment Arrangement ----

people|>
  mutate(
    w = MEM_WEIGHT_FOR_COUNTRY_W * 
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  )|>
  group_by(EMPLOYMENT_ARRANGEMENT)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> people_indians_employment_arrangement

people|>
  filter(STATE == "Maharashtra")|>
  mutate(
    w = MEM_WEIGHT_FOR_STATE_W * 
      MEM_NON_RESPONSE_FOR_STATE_W
  )|>
  group_by(EMPLOYMENT_ARRANGEMENT)|>
  summarise(
    pop = sum(w)
  )|>
  arrange(desc(pop)) -> people_maharashtra_employment_arrangement



## Volume estimates ----

## first create a dummy for reasonable target group in people dataset

people|>
  mutate(
    w_state = MEM_WEIGHT_FOR_STATE_W * MEM_NON_RESPONSE_FOR_STATE_W,
    is_rtg = ifelse(
      NATURE_OF_OCCUPATION %in% c("Wage Labourer",
                                  "Industrial Workers",
                                  "Non-Industrial Technical Employee",
                                  "Small Trader/Hawker/ Businessman without Fixed Premises",
                                  "Home-based Worker")| (
                                    OCCUPATION %in% common_in_all & 
                                      NATURE_OF_OCCUPATION %in% c(
                                        "Support Staff",
                                        "White Collar Clerical Employees",
                                        "Self Employed Entrepreneur"
                                      )
                                  ),
      1,
      0
    )
  )-> people



## Informally engaged Urban/Rural workers in Maharashtra

people|>
  filter(STATE == "Maharashtra")|>
  filter(is_rtg == 1)|>
  group_by(REGION_TYPE,GENDER)|>
  summarise(
    `Total IEW` = sum(w_state)
  )|>
  filter(`Total IEW` > 0) -> mh_iew

## employment status of IEW in Maharashtra

people|>
  filter(STATE == "Maharashtra")|>
  filter(REGION_TYPE == "URBAN")|>
  filter(is_rtg == 1)|>
  group_by(EMPLOYMENT_STATUS)|>
  summarise(
    `Total IEW` = sum(w_state)
  )|>
  ungroup() -> mh_iew_emp_status

## employment arrangement of iew in Maharashtra

people|>
  filter(STATE == "Maharashtra")|>
  filter(REGION_TYPE == "URBAN")|>
  filter(is_rtg == 1)|>
  group_by(EMPLOYMENT_ARRANGEMENT)|>
  summarise(
    `Total IEW` = sum(w_state)
  )|>
  ungroup() -> mh_iew_emp_arrangement


## List IDs of rtg ----

# We need to capture the HHiD and MEMiD of people in RTG

## All mh unique HHID length  is 16275
## All mh unique MEMID length is 78483

people|>
  filter(STATE == "Maharashtra")|>
  filter(is_rtg == 1)|>
  pull(HH_ID)|> 
  unique()-> mh_rtg_hhid

people|>
  filter(STATE == "Maharashtra")|>
  filter(is_rtg == 1)|>
  mutate(
    uid = stringr::str_c(HH_ID,MEM_ID,sep = "")
  )|>
  pull(uid)|>
  unique() -> mh_rtg_memid

