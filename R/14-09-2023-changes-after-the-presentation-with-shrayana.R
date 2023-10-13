
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
  filter(RESPONSE_STATUS == "Accepted")|>
  mutate(
    uid = paste(HH_ID,MEM_ID,sep = "")
  )-> people

# Aspirational India data

aspiration <- readr::read_csv(
  here("data","data-for-descriptive-analysis",
       "aspirational_india_20230101_20230430_R.csv")
)

## Keep only accepted responses from the 'people' data

aspiration|>
  filter(RESPONSE_STATUS == "Accepted") -> aspiration



# Create Country and state level Weights -----

people|>
  mutate(
    w_state = MEM_WEIGHT_FOR_STATE_W *
      MEM_NON_RESPONSE_FOR_STATE_W,
    w_country = MEM_WEIGHT_FOR_COUNTRY_W *
      MEM_NON_RESPONSE_FOR_COUNTRY_W
  ) -> people

aspiration|>
  mutate(
    w_state = HH_WEIGHT_FOR_STATE_W *
      HH_NON_RESPONSE_FOR_STATE_W,
    w_country = HH_WEIGHT_FOR_COUNTRY_W *
      HH_NON_RESPONSE_FOR_COUNTRY_W
  ) -> aspiration

# Discovering the target group ----

# Since there is no way to know if a person has a 
# written contract or gets paid leave, the only
# reasonable variable we are left with is people
# having a PF account. We can try using nature of
# occupation along with PF.

### let us see, from those who are employed and looking
### for a job have a pf account in india and maharashtra.
### This will give us uid foe target group.

people|>
  filter(AGE_YRS>15 | 
           (AGE_YRS ==15 & AGE_MTHS >0))|> 
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
  filter(OCCUPATION != "Smaller businessmen (smaller shops or offices), Shopkeepers, small dhaba owners")|>
  filter(HAS_PF_AC == "N")|>
  filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
  pull(uid) -> uid_target_group_without_homemaker_student

people|>
  filter(AGE_YRS>15 | 
           (AGE_YRS ==15 & AGE_MTHS >0))|> 
  filter(NATURE_OF_OCCUPATION %in% c(
    "Wage Labourer",
    "Industrial Workers",
    "Non-Industrial Technical Employee",
    "Small Trader/Hawker/ Businessman without Fixed Premises",
    "Home-based Worker",
    "Self Employed Entrepreneur",
    "Support Staff",
    "White Collar Clerical Employees",
    "Student",
    "Home Maker"
  ))|>
  filter(OCCUPATION != "Smaller businessmen (smaller shops or offices), Shopkeepers, small dhaba owners")|>
  filter(HAS_PF_AC == "N")|>
  filter(EMPLOYMENT_STATUS %in% c("Employed"))|>
  pull(uid) -> uid_target_group_with_homemaker_student

people|>
  filter(uid %in% uid_target_group_without_homemaker_student)|>
  pull(HH_ID)|>
  unique() -> hhid_target_group_without_homemaker_student

people|>
  filter(uid %in% uid_target_group_with_homemaker_student)|>
  pull(HH_ID)|>
  unique() -> hhid_target_group_with_homemaker_student


people|>
  mutate(
    is_tg = ifelse(
      uid %in% uid_target_group_without_homemaker_student,
      1,0
    ),
    is_tg_plus_stu_hm = ifelse(
      uid %in% uid_target_group_with_homemaker_student,
      1,
      0
    )
  )-> people

aspiration|>
  mutate(
    is_tg = ifelse(
      HH_ID %in% hhid_target_group_without_homemaker_student,
      1,0
    ),
    is_tg_plus_stu_hm = ifelse(
      HH_ID %in% hhid_target_group_with_homemaker_student,
      1,0
    )
  ) -> aspiration


# One function to calculate all abs numbers ----------------------------------------

calc_any_estimate_pop <- function(geo = NA,
                                  istg = c(1,0),
                                  istgstuhm = c(1,0),
                                  description,
                                  ind,
                                  unit,
                                  grp_cols = NULL){
 
  
  people|>
    filter(
      STATE %in% if(geo == "India"){
        people$STATE|>
          unique()
      }else{
        c(geo)
      }
    )|>
    filter(is_tg %in% istg)|>
    filter(is_tg_plus_stu_hm %in% istgstuhm)|>
    group_by(across(all_of(grp_cols)))|>
    summarise(
      val = sum(
        if(geo == "India"){
          w_country
        }else{
          w_state
        }
      )
    )|>
    ungroup()|>
    mutate(
      geography = geo,
      unit = unit,
      description = description,
      indicator = ind
    )
    
}

## Create all set of arguments to purrr ----

### Create arguments to Calculate population of all geographies ----

args_all_pop <- tibble(
  geo = c("India",people$STATE|>unique()),
  description = paste("Total population of ",geo),
  ind = paste("total_pop_of_",geo,sep = ""),
  unit = "number of people"
)


### Create arguments to Calculate population of all geographies -region ----

args_all_pop_region <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("Total population of ",geo,"in regions"),
  ind = paste("total_pop_of_",geo,"_region",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("REGION_TYPE")),29)
)

### Create arguments to Calculate population of all geographies - Gender ----

args_all_pop_gender <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("Total population of ",geo,"in gender type"),
  ind = paste("total_pop_of_",geo,"_gender",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("GENDER")),29)
)

### Create arguments to Calculate population of all geographies - Caste Category ----

args_all_pop_caste_category <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("Total population of ",geo,"in caste category"),
  ind = paste("total_pop_of_",geo,"_CasteCategory",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("CASTE_CATEGORY")),29)
)

### Create arguments to Calculate population of all geographies - Religion ----

args_all_pop_religion <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("Total population of ",geo,"in religion type"),
  ind = paste("total_pop_of_",geo,"_religion",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("RELIGION")),29)
)

### Create arguments to calculate population in tg in all geographies----

args_all_pop_in_tg <- tibble(
  geo = c("India",people$STATE|>unique()),
  istg = rep(list(c(1)),29),
  description = paste("Total population of ",geo,"in tg"),
  ind = paste("total_pop_of_",geo,"_tg",sep = ""),
  unit = "number of people"
)


### Create arguments to Calculate population in tg for all geographies -region ----

args_all_pop_region_in_tg <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("Total population of ",geo,"in target group and regions"),
  ind = paste("total_pop_of_",geo,"_tg_region",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("REGION_TYPE")),29)
)

### Create arguments to Calculate population in tg for all geographies - Gender ----

args_all_pop_gender_in_tg <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("Total population of ",geo,"in target group and Gender type"),
  ind = paste("total_pop_of_",geo,"_tg_gender",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("GENDER")),29)
)

### Create arguments to Calculate population in tg for all geographies - Caste Category ----

args_all_pop_caste_category_in_tg <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("Total population of ",geo,"in target group and caste category"),
  ind = paste("total_pop_of_",geo,"_tg_CasteCategory",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("CASTE_CATEGORY")),29)
)

### Create arguments to Calculate population in tg for all geographies - Religion ----

args_all_pop_religion_in_tg <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("Total population of ",geo,"in target group and religion"),
  ind = paste("total_pop_of_",geo,"_tg_religion",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("RELIGION")),29)
)

### Create arguments to calculate population in tgstuhm stu hm in all geographies----

args_all_pop_in_tgstuhm <- tibble(
  geo = c("India",people$STATE|>unique()),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("Total population of ",geo,"in target group with students and home makers"),
  ind = paste("total_pop_of_",geo,"_tgstuhm",sep = ""),
  unit = "number of people"
)


### Create arguments to Calculate population in tgstuhm for all geographies -region ----

args_all_pop_region_in_tgstuhm <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("Total population of ",geo,"in target group with students and home makers, and regions"),
  ind = paste("total_pop_of_",geo,"_tgstuhm_region",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("REGION_TYPE")),29)
)

### Create arguments to Calculate population in tgstuhm for all geographies - Gender ----

args_all_pop_gender_in_tgstuhm <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("Total population of ",geo,"in target group with students and home makers,and Gender type"),
  ind = paste("total_pop_of_",geo,"_tgstuhm_gender",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("GENDER")),29)
)

### Create arguments to Calculate population in tgstuhm for all geographies - Caste Category ----

args_all_pop_caste_category_in_tgstuhm <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("Total population of ",geo,"in target group with students and home makers,and caste category"),
  ind = paste("total_pop_of_",geo,"_tgstuhm_CasteCategory",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("CASTE_CATEGORY")),29)
)

### Create arguments to Calculate population in tgstuhm for all geographies - Religion ----

args_all_pop_religion_in_tgstuhm <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("Total population of ",geo,"in target group with students and home makers,and religion"),
  ind = paste("total_pop_of_",geo,"_tgstuhm_religion",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("RELIGION")),29)
)

### Create arguments to calculate employment arrangement of  population in tg in all geographies----

args_emp_arr_all_pop_in_tg <- tibble(
  geo = c("India",people$STATE|>unique()),
  istg = rep(list(c(1)),29),
  description = paste("employment arrangement of  population of ",geo,"in tg"),
  ind = paste("emp_arr_pop_of_",geo,"_tg",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT")),29)
)


### Create arguments to Calculate employment arrangement of population in tg for all geographies -region ----

args_emp_arr_pop_region_in_tg <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("employment arrangement of  population of ",geo,"in target group and regions"),
  ind = paste("emp_arr_pop_of_",geo,"_tg_region",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT","REGION_TYPE")),29)
)

### Create arguments to Calculate employment arrangement of  population in tg for all geographies - Gender ----

args_emp_arr_pop_gender_in_tg <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("employment arrangement of  population of ",geo,"in target group and Gender type"),
  ind = paste("emp_arr_pop_of_",geo,"_tg_gender",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT","GENDER")),29)
)

### Create arguments to Calculate employment arrangement of  population in tg for all geographies - Caste Category ----

args_emp_arr_pop_caste_category_in_tg <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("employment arrangement of  population of ",geo,"in target group and caste category"),
  ind = paste("emp_arr_pop_of_",geo,"_tg_CasteCategory",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT","CASTE_CATEGORY")),29)
)

### Create arguments to Calculate employment arrangement of population in tg for all geographies - Religion ----

args_emp_arr_pop_religion_in_tg <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1)),29),
  istgstuhm = rep(list(c(1,0)),29),
  description = paste("employment arrangement of  population of ",geo,"in target group and religion"),
  ind = paste("emp_arr_pop_of_",geo,"_tg_religion",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT","RELIGION")),29)
)

### Create arguments to calculate employment arrangement of population in tgstuhm stu hm in all geographies----

args_emp_arr_pop_in_tgstuhm <- tibble(
  geo = c("India",people$STATE|>unique()),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("employment arrangement of  population of ",geo,"in target group with students and home makers"),
  ind = paste("emp_arr_pop_of_",geo,"_tgstuhm",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT")),29)
)


### Create arguments to Calculate employment arrangement of  population in tgstuhm for all geographies -region ----

args_emp_arr_pop_region_in_tgstuhm <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("employment arrangement of  population of ",geo,"in target group with students and home makers, and regions"),
  ind = paste("emp_arr_pop_of_",geo,"_tgstuhm_region",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT","REGION_TYPE")),29)
)

### Create arguments to Calculate employment arrangement of population in tgstuhm for all geographies - Gender ----

args_emp_arr_pop_gender_in_tgstuhm <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("employment arrangement of  population of ",geo,"in target group with students and home makers,and Gender type"),
  ind = paste("emp_arr_pop_of_",geo,"_tgstuhm_gender",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT","GENDER")),29)
)

### Create arguments to Calculate employment arrangement of  population in tgstuhm for all geographies - Caste Category ----

args_emp_arr_pop_caste_category_in_tgstuhm <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("employment arrangement of  population of ",geo,"in target group with students and home makers,and caste category"),
  ind = paste("emp_arr_pop_of_",geo,"_tgstuhm_CasteCategory",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT","CASTE_CATEGORY")),29)
)

### Create arguments to Calculate employment arrangement of  population in tgstuhm for all geographies - Religion ----

args_emp_arr_pop_religion_in_tgstuhm <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  istg = rep(list(c(1,0)),29),
  istgstuhm = rep(list(c(1)),29),
  description = paste("employment arrangement of population of ",geo,"in target group with students and home makers,and religion"),
  ind = paste("emp_arr_pop_of_",geo,"_tgstuhm_religion",sep = ""),
  unit = "number of people",
  grp_cols = rep(list(c("EMPLOYMENT_ARRANGEMENT","RELIGION")),29)
)

## Run args tables to get pop estimates ----

mget(objects(pattern = "args")) -> all_args

purrr::map(all_args,
           ~purrr::pmap_dfr(.x,calc_any_estimate_pop)
           )-> all_pop_estimates

### Getting all tables in same format to join by row

get_same_dims <- function(x){
  if(names(x)|>length() == 5){
    x |>
      mutate(
      lens1 = NA,
      lens2 = NA
      )|>
      relocate(lens1,lens2,everything())->x
    return(x)
  }else{
    if(names(x)|> length() == 6){
      x |>
        mutate(
          lens2 =NA
        )->x
      
      colnames(x) <- c(
        "lens1"      ,
        "val"        ,
        "geography"  ,
        "unit"       ,
        "description",
        "indicator"  ,
        "lens2"
      )
      
      x|>
        relocate(lens1,lens2,everything()) -> x
      
      return(x)
    }else{
      colnames(x) <- c(
        "lens1"      ,
        "lens2",
        "val"        ,
        "geography"  ,
        "unit"       ,
        "description",
        "indicator"  
      )
      
      x |>
        relocate(lens1,lens2,everything())->x
      return(x)
    }
  }
}

purrr::map_dfr(all_pop_estimates,
           .f = ~ get_same_dims(.x) 
             )-> all_pop_estimates_same_dims

all_pop_estimates_same_dims|>
  arrange(geography)-> all_pop_estimates_same_dims



## Estimates of share of people in tg with banks acc, mobiles etc ----

est_shares_of_pop <- function(
  geo,
  description,
  ind,
  unit,
  grp_cols,
  coverage
){
  
  
  people|>
    filter(
      STATE %in% if(geo == "India"){
        people$STATE|>
          unique()
      }else{
        c(geo)
      }
    )|>
    filter(AGE_YRS >15 | (AGE_YRS==15 & AGE_MTHS >0))|>
    group_by(across(all_of(grp_cols)))|>
    summarise(
      val = DescTools::Mean(
        ifelse(
          .data[[coverage]] == "Y",
          1,0
        ),
        weights = if(geo == "India"){
          w_country
        }else{
          w_state
        }
      )
    )|>
    ungroup()|>
    arrange(desc(val))|>
    janitor::adorn_pct_formatting() -> intermediate
  
  if(length(grp_cols)>1){
    
    intermediate|>
      mutate(
        geography = geo,
        unit = unit,
        description = description,
        indicator = ind
      )-> intermediate
    
    colnames(intermediate) <- c(
      "lens1",      
      "lens2",      
      "val",        
      "geography",  
      "unit",       
      "description",
      "indicator" 
    )
    
    return(intermediate)
    
  }else{
    intermediate|>
      mutate(
        lens2 = NA,
        geography = geo,
        unit = unit,
        description = description,
        indicator = ind
      )-> intermediate
    
    colnames(intermediate) <- c(
      "lens1",      
      "val", 
      "lens2",
      "geography",  
      "unit",       
      "description",
      "indicator" 
    )
    
    intermediate|>
      relocate(lens1,lens2,val,everything()) -> intermediate
    return(intermediate)
  }
  
  
}
  
### Create arguments for estimates of share ----

args_estimates_tg_bank <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a bank account vs share of people not in target group and abouve 15 that have a bank account."),
  ind = paste("perc_has_bank_acc_TGvNTG_",geo,sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg")),29),
  coverage = rep(list(c("HAS_BANK_AC")),29)
)

args_estimates_tg_pf <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a PF account vs share of people not in target group and abouve 15 that have a PF account."),
  ind = paste("perc_has_PF_acc_TGvNTG_",geo,sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg")),29),
  coverage = rep(list(c("HAS_PF_AC")),29)
)

args_estimates_tg_lic <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a life insurance vs share of people not in target group and abouve 15 that have a life insurance."),
  ind = paste("perc_has_LIC_TGvNTG_",geo,sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg")),29),
  coverage = rep(list(c("HAS_LIC")),29)
)

args_estimates_tg_health_insurance <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a Health insurance vs share of people not in target group and abouve 15 that have a health insurance."),
  ind = paste("perc_has_health_ins_TGvNTG_",geo,sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg")),29),
  coverage = rep(list(c("HAS_HEALTH_INSURANCE")),29)
)

args_estimates_tg_mobile <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a mobile vs share of people not in target group and abouve 15 that have a mobile."),
  ind = paste("perc_has_mobile_TGvNTG_",geo,sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg")),29),
  coverage = rep(list(c("HAS_MOBILE")),29)
)

# -------------------REGION_TYPE---------


args_estimates_tg_bank_region <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a bank account across regions vs share of people not in target group and abouve 15 that have a bank account across regions."),
  ind = paste("perc_has_bank_acc_TGvNTG_",geo,"_region",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","REGION_TYPE")),29),
  coverage = rep(list(c("HAS_BANK_AC")),29)
)

args_estimates_tg_pf_region <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a PF account across regions vs share of people not in target group and abouve 15 that have a PF account across regions."),
  ind = paste("perc_has_PF_acc_TGvNTG_",geo,"_region",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","REGION_TYPE")),29),
  coverage = rep(list(c("HAS_PF_AC")),29)
)

args_estimates_tg_lic_region <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a life insurance across regions vs share of people not in target group and abouve 15 that have a life insurance across regions."),
  ind = paste("perc_has_LIC_TGvNTG_",geo,"_region",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","REGION_TYPE")),29),
  coverage = rep(list(c("HAS_LIC")),29)
)

args_estimates_tg_health_insurance_region <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a Health insurance across regions vs share of people not in target group and abouve 15 that have a health insurance across regions."),
  ind = paste("perc_has_health_ins_TGvNTG_",geo,"_region",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","REGION_TYPE")),29),
  coverage = rep(list(c("HAS_HEALTH_INSURANCE")),29)
)

args_estimates_tg_mobile_region <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a mobile across regions vs share of people not in target group and abouve 15 that have a mobile across regions."),
  ind = paste("perc_has_mobile_TGvNTG_",geo,"_region",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","REGION_TYPE")),29),
  coverage = rep(list(c("HAS_MOBILE")),29)
)

# --------------GENDER------------------------------


args_estimates_tg_bank_gender <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a bank account across genders vs share of people not in target group and abouve 15 that have a bank account across genders."),
  ind = paste("perc_has_bank_acc_TGvNTG_",geo,"_gender",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","GENDER")),29),
  coverage = rep(list(c("HAS_BANK_AC")),29)
)

args_estimates_tg_pf_gender <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a PF account across genders vs share of people not in target group and abouve 15 that have a PF account across genders."),
  ind = paste("perc_has_PF_acc_TGvNTG_",geo,"_gender",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","GENDER")),29),
  coverage = rep(list(c("HAS_PF_AC")),29)
)

args_estimates_tg_lic_gender <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a life insurance across genders vs share of people not in target group and abouve 15 that have a life insurance across genders."),
  ind = paste("perc_has_LIC_TGvNTG_",geo,"_gender",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","GENDER")),29),
  coverage = rep(list(c("HAS_LIC")),29)
)

args_estimates_tg_health_insurance_gender <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a Health insurance across genders vs share of people not in target group and abouve 15 that have a health insurance across genders."),
  ind = paste("perc_has_health_ins_TGvNTG_",geo,"_gender",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","GENDER")),29),
  coverage = rep(list(c("HAS_HEALTH_INSURANCE")),29)
)

args_estimates_tg_mobile_gender <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a mobile across genders vs share of people not in target group and abouve 15 that have a mobile across genders."),
  ind = paste("perc_has_mobile_TGvNTG_",geo,"_gender",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","GENDER")),29),
  coverage = rep(list(c("HAS_MOBILE")),29)
)

# -----------------------RELIGION------------------


args_estimates_tg_bank_religion <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a bank account across religions vs share of people not in target group and abouve 15 that have a bank account across religions."),
  ind = paste("perc_has_bank_acc_TGvNTG_",geo,"_religion",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","RELIGION")),29),
  coverage = rep(list(c("HAS_BANK_AC")),29)
)

args_estimates_tg_pf_religion <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a PF account across religions vs share of people not in target group and abouve 15 that have a PF account across religions."),
  ind = paste("perc_has_PF_acc_TGvNTG_",geo,"_religion",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","RELIGION")),29),
  coverage = rep(list(c("HAS_PF_AC")),29)
)

args_estimates_tg_lic_religion <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a life insurance across religions vs share of people not in target group and abouve 15 that have a life insurance across religions."),
  ind = paste("perc_has_LIC_TGvNTG_",geo,"_religion",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","RELIGION")),29),
  coverage = rep(list(c("HAS_LIC")),29)
)

args_estimates_tg_health_insurance_religion <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a Health insurance across religions vs share of people not in target group and abouve 15 that have a health insurance across religions."),
  ind = paste("perc_has_health_ins_TGvNTG_",geo,"_religion",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","RELIGION")),29),
  coverage = rep(list(c("HAS_HEALTH_INSURANCE")),29)
)

args_estimates_tg_mobile_religion <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a mobile across religions vs share of people not in target group and abouve 15 that have a mobile across religions."),
  ind = paste("perc_has_mobile_TGvNTG_",geo,"_religion",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","RELIGION")),29),
  coverage = rep(list(c("HAS_MOBILE")),29)
)

# --------------------------CASTE_CATEGORY ------------------------------


args_estimates_tg_bank_caste <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a bank account across castes vs share of people not in target group and abouve 15 that have a bank account across castes."),
  ind = paste("perc_has_bank_acc_TGvNTG_",geo,"_caste",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","CASTE_CATEGORY")),29),
  coverage = rep(list(c("HAS_BANK_AC")),29)
)

args_estimates_tg_pf_caste <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a PF account across castes vs share of people not in target group and abouve 15 that have a PF account across castes."),
  ind = paste("perc_has_PF_acc_TGvNTG_",geo,"_caste",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","CASTE_CATEGORY")),29),
  coverage = rep(list(c("HAS_PF_AC")),29)
)

args_estimates_tg_lic_caste <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a life insurance across castes vs share of people not in target group and abouve 15 that have a life insurance across castes."),
  ind = paste("perc_has_LIC_TGvNTG_",geo,"_caste",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","CASTE_CATEGORY")),29),
  coverage = rep(list(c("HAS_LIC")),29)
)

args_estimates_tg_health_insurance_caste <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a Health insurance across castes vs share of people not in target group and abouve 15 that have a health insurance across castes."),
  ind = paste("perc_has_health_ins_TGvNTG_",geo,"_caste",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","CASTE_CATEGORY")),29),
  coverage = rep(list(c("HAS_HEALTH_INSURANCE")),29)
)

args_estimates_tg_mobile_caste <- tibble(
  geo = as.list(c("India",people$STATE|>unique())),
  description = paste("For ",geo, 
                      ", Share of people in target group that have a mobile across castes vs share of people not in target group and abouve 15 that have a mobile across castes."),
  ind = paste("perc_has_mobile_TGvNTG_",geo,"_caste",sep = ""),
  unit = "Share of People",
  grp_cols = rep(list(c("is_tg","CASTE_CATEGORY")),29),
  coverage = rep(list(c("HAS_MOBILE")),29)
)

### Purrr through for share estimates ----
mget(objects(pattern = "args_estimates"))-> all_est_args

purrr::map_dfr(
  all_est_args,
  ~purrr::pmap_dfr(.x, est_shares_of_pop)
) -> all_share_estimate_same_dim

all_share_estimate_same_dim|>
  arrange(geography) -> all_share_estimate_same_dim


write.csv(rbind(all_pop_estimates_same_dims,all_share_estimate_same_dim),
          "data/estimates-people-cmie.csv")

# Estimates from the Aspirational India data set ------------




