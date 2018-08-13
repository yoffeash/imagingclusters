## 02 Initial Import and Cleaning of COPDGene Data ##
## edited 2018_08_08

#### general COPDGene database ####
copd <- read_delim("data/raw_data/COPDGene_P1P2_All_Visit_09OCT17.txt", 
                   "\t", escape_double = FALSE, trim_ws = TRUE)
# clean variable names and remove columns and rows with all NA
copd_clean <- clean_names(copd) %>% 
  rename(dlco_pp=dlc_opp, 
         tlc_pp_race_adjusted=tl_cpp_race_adjusted,
         fvcpp_utah=fv_cpp_utah,
         dlco_raw=d_lco_raw) %>% 
  remove_empty()
## calculate change variables ##
# remove subset of variables that we are interested in their change over time
copd_change = copd_clean %>% select(sid,visitnum,height_cm,weight_kg,
                                     distwalked,sgrq_score_total,dlco_pp, dlco_raw,
                                     tlc_pp_race_adjusted,fev1pp_utah,fvcpp_utah,
                                     fev1_fvc_utah,pct_emph_thirona,tlc_thirona,
                                     pct_gas_trap_thirona,awt_seg_thirona,
                                     fev1_utah,fvc_utah)
# spread to wide format
copd_change_wide = copd_change %>% 
  tidyr::gather_(key="variable", value="value", 
                 c("height_cm",
                   "weight_kg",
                   "distwalked",
                   "sgrq_score_total",
                   "dlco_pp", 
                   "dlco_raw",
                   "tlc_pp_race_adjusted",
                   "fev1pp_utah",
                   "fvcpp_utah",
                   "fev1_fvc_utah",
                   "pct_emph_thirona",
                   "tlc_thirona",
                   "pct_gas_trap_thirona",
                   "awt_seg_thirona",
                   "fev1_utah",
                   "fvc_utah"
                   )) %>% 
  tidyr::unite("time_by_variable", variable, visitnum, remove=T) %>%  
  tidyr::spread(key=time_by_variable, value=value)                
# calculate change between two variables
copd_delta = copd_change_wide %>% 
  mutate(awt_seg_thirona_delta = awt_seg_thirona_2 - awt_seg_thirona_1,
         distwalked_delta = distwalked_2 - distwalked_1,
         dlco_pp_delta = dlco_pp_2 - dlco_pp_1,
         dlco_raw_delta = dlco_pp_2 - dlco_raw_1,
         fev1_fvc_utah_delta = fev1_fvc_utah_2 - fev1_fvc_utah_1,
         fev1_utah_delta = fev1_utah_2 - fev1_utah_1,
         fev1pp_utah_delta = fev1pp_utah_2 - fev1pp_utah_1,
         fvc_utah_delta = fvc_utah_2 - fvc_utah_1,
         fvcpp_utah_delta = fvcpp_utah_2 - fvcpp_utah_1,
         height_cm_delta = height_cm_2 - height_cm_1,
         pct_emph_thirona_delta = pct_emph_thirona_2 - pct_emph_thirona_1,
         pct_gas_trap_thirona_delta = pct_gas_trap_thirona_2 - pct_gas_trap_thirona_1,
         sgrq_score_total_delta = sgrq_score_total_2 - sgrq_score_total_1,
         tlc_pp_race_adjusted_delta = tlc_pp_race_adjusted_2 - tlc_pp_race_adjusted_1,
         tlc_thirona_delta = tlc_thirona_2 - tlc_thirona_1,
         weight_kg_delta = weight_kg_2 - weight_kg_1) %>% 
  select(sid,awt_seg_thirona_delta,distwalked_delta,dlco_pp_delta,
         fev1_fvc_utah_delta,fev1_utah_delta,fev1pp_utah_delta,fvc_utah_delta,fvcpp_utah_delta,
         height_cm_delta,pct_emph_thirona_delta,pct_gas_trap_thirona_delta,sgrq_score_total_delta,
         tlc_pp_race_adjusted_delta,weight_kg_delta)
# extract time between visits and copdgene calculated change variables 
copd_time = copd_clean %>% select(sid,days_ct1_ct2,days_phase1_phase2,
                                   change_tlc_thirona, change_pct_emph_thirona, change_pct_gas_trap_thirona,
                                   change_p1_p2_fev1_ml,change_p1_p2_fev1pp) %>% 
  filter(days_phase1_phase2>0)
# remove visit 2 data and finalgold -2 from base dataset 
copd_base = copd_clean %>% filter(visitnum==1) %>% filter(final_gold > -2) %>% select(-visitnum) 
# merge delta variable data with baseline data
copd_pre = inner_join(copd_base,copd_delta)
# add time variables and remove variables that are all NA
copd_base_full = left_join(copd_pre,copd_time,by="sid") %>% remove_empty()
# add prior exacerbation binary variable
copd_base_full = copd_base_full %>% mutate(priorexacerbation = ifelse(exacerbation_frequency > 0, 1, 0))

### mortality and LFU dataset addition to baseline dataset ###
# import datasets
copd_mort <- read_delim("data/raw_data/COPDGene_Mortality_Surv_2016dec.txt", 
                                                          "\t", escape_double = FALSE, col_types = cols(DOD_merged = col_date(format = "%m/%d/%Y"), 
                                                                                                        Phase1_date = col_date(format = "%m/%d/%Y"), 
                                                                                                        date_SSDI_confirmed = col_date(format = "%m/%d/%Y"), 
                                                                                                        date_last_SSDI_search = col_date(format = "%m/%d/%Y"), 
                                                                                                        days_followed = col_number(), last_LFU_survey_date = col_date(format = "%m/%d/%Y")), 
                                                          trim_ws = TRUE)
copd_lfu <- read_delim("data/raw_data/LFU_SidLevel_w_Comorbid_31JUL17.txt", 
                       "\t", escape_double = FALSE, col_types = cols( 
                                                                     reportedSmokingStatusString = col_skip()), 
                       trim_ws = TRUE) 
# merge datasets
copd_clinical_full_1 = left_join(copd_base_full,copd_mort)
copd_clinical_full = left_join(copd_clinical_full_1,copd_lfu) %>% remove_empty()

### local histogram data using third training set ###
copd_lh = read_excel("data/raw_data/LHsubtypes_2016_09_21_v2.xlsx") %>% clean_names()
copd_pre1 = left_join(copd_clinical_full,copd_lh) %>% remove_empty()

### body composition from manual measurements ###
copd_body <- Body_Composition_COPDGene <- read_csv("data/raw_data/Body Composition COPDGene.csv")
copd_pre2 <- left_join(copd_pre1,copd_body) %>% remove_empty()

### biomarker data from Russ Bowler (RBM only) ###
copd_bio <- read_csv("data/raw_data/COPDGene_Bowler_RBM_raw.csv") %>% clean_names() %>% remove_empty()
copd_full <- left_join(copd_pre2,copd_bio)

#############additional data manipulation################
# rename gender
copd_full = copd_full %>% mutate(sex = ifelse(gender==2,"female","male"))
# normalize imaging variables
copd_full = copd_full %>% mutate(percent_ild_z = percent_ild, percent_emphysema_z = percent_emphysema, 
                                 percent_normal_z = percent_normal, awt_seg_thirona_z = awt_seg_thirona, PMA_z = PMA) %>% 
  mutate_at(scale,.vars = vars(percent_ild_z, percent_emphysema_z, percent_normal_z, awt_seg_thirona_z, PMA_z))

### dataset with no missing imaging data for clustering ###
copd_full_imaging = copd_full %>% filter(!is.na(percent_ild), !is.na(percent_emphysema), !is.na(awt_seg_thirona), !is.na(PMA))
