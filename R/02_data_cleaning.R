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
# remove visit 2 data from base dataset
copd_base = copd_clean %>% filter(visitnum==1) %>% select(-visitnum)
# merge delta variable data with baseline data
copd_pre = inner_join(copd_base,copd_delta)
# add time variables and remove variables that are all NA
copd_base_full = left_join(copd_pre,copd_time,by="sid") %>% remove_empty()

### mortality dataset ###
copd_mort <- read_delim("data/raw_data/COPDGene_Mortality_Surv_2016dec.txt", 
                                                          "\t", escape_double = FALSE, col_types = cols(DOD_merged = col_date(format = "%m/%d/%Y"), 
                                                                                                        Phase1_date = col_date(format = "%m/%d/%Y"), 
                                                                                                        date_SSDI_confirmed = col_date(format = "%m/%d/%Y"), 
                                                                                                        date_last_SSDI_search = col_date(format = "%m/%d/%Y"), 
                                                                                                        days_followed = col_number(), last_LFU_survey_date = col_date(format = "%m/%d/%Y")), 
                                                          trim_ws = TRUE)
