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

############# manipulate COPDGene round 2 local histogram data ##################################################
### clean round 2 local histogram data from COPDGene and create
## 1) wide format file with delta variables then will then merge with baseline file in wide format for rate calculations
## 2) file with visit number two that can then be merged (in addition to baseline LH data) with base file to create long format file for longitudinal anlysis using mixed methods

### import ECLIPSE baseline LH dataset ###
COPDGene_LH_2_raw <- read_csv("data/raw_data/COPDGene2_localHistogram_parenchymaPhenotypes_20180319_wideFormat.csv")
COPDGene_LH_2_pre1 <- clean_names(COPDGene_LH_2_raw)

###################################### file with summary whole lung LH values ######################################
COPDGene_LH_2_summary_whole_pre1 <- COPDGene_LH_2_pre1 %>% select(starts_with("whole")) %>% select(-contains("wild")) %>% select(contains("type_frac"))
COPDGene_LH_2_CID <- COPDGene_LH_2_pre1 %>% select(contains("cid")) %>% mutate(sid=str_sub(cid,start=1L,end=6L))
COPDGene_LH_2_summary_whole_pre2 <- bind_cols(COPDGene_LH_2_summary_whole_pre1,COPDGene_LH_2_CID)

COPDGene_LH_2_summary_whole <- COPDGene_LH_2_summary_whole_pre2 %>% 
  mutate(percent_normal = whole_lung_normal_parenchyma_type_frac*100) %>% 
  mutate(percent_emphysema = (whole_lung_centrilobular_emphysema_type_frac + whole_lung_emphysematous_type_frac)*100) %>% 
  mutate(percent_ild = (whole_lung_reticular_type_frac + whole_lung_subpleural_line_type_frac + whole_lung_ground_glass_type_frac +
           whole_lung_linear_scar_type_frac + whole_lung_centrilobular_nodule_type_frac + whole_lung_cyst_type_frac + whole_lung_nodular_type_frac +
           whole_lung_nodule_type_frac + whole_lung_bronchiectatic_airway_type_frac + whole_lung_honeycombing_type_frac)*100) %>% 
  select(cid, sid, percent_normal, percent_emphysema, percent_ild)

####################################################################################################################################################################################


############################### wide format file with rate of change in LH measures #########
# note run after clustering assignement in order to include cluster assignment
COPDGene_LH_2_whole_2 <- COPDGene_LH_2_summary_whole %>% rename(percent_normal_2 = percent_normal, percent_emphysema_2 = percent_emphysema, percent_ild_2 = percent_ild)
copd_full_imaging_2 <- inner_join(copd_full_imaging, COPDGene_LH_2_whole_2) %>% 
  filter(!is.na(percent_ild_2), !is.na(percent_emphysema_2)) %>% 
  mutate(percent_normal_delta = percent_normal_2 - percent_normal, percent_emphysema_delta = percent_emphysema_2 - percent_emphysema, percent_ild_delta = percent_ild_2 - percent_ild) %>% 
  mutate(percent_normal_rate = percent_normal_delta/(days_ct1_ct2.y/365), percent_emphysema_rate = percent_emphysema_delta/(days_ct1_ct2.y/365), percent_ild_rate = percent_ild_delta/(days_ct1_ct2.y/365))

############################ merge with long format base dataset ###########
COPDGene_LH_2_whole_3 <- COPDGene_LH_2_summary_whole %>% mutate(visitnum=2) %>% select(-cid)
copd_lh_base <- copd_lh %>% mutate(visitnum=1)
copd_lh_both <- rbind(copd_lh_base,COPDGene_LH_2_whole_3)

copd_cluster_assignments <- copd_full_imaging %>% select(sid,cluster_decamp,days_ct1_ct2.y)  

copd_long <- left_join(copd_lh_both,copd_cluster_assignments) %>% filter(!is.na(cluster_decamp)) %>% 
  mutate(days=ifelse(visitnum==2,days_ct1_ct2.y,0)) %>% 
  filter(!is.na(days)) %>% select(-days_ct1_ct2.y) %>% 
  mutate(years=days/365)

## merge long format imaging dataset with long format baseline dataset
copd_clean_pre <- copd_clean %>% select(sid,same_scanner_model,visitnum) %>% 
  spread(visitnum,same_scanner_model) 
names(copd_clean_pre)[names(copd_clean_pre)=='1'] <- "visit_1"
names(copd_clean_pre)[names(copd_clean_pre)=='2'] <- "visit_2"
copd_clean_pre2 <- copd_clean_pre %>% mutate(visit_1 = visit_2) %>% drop_na(visit_1) %>% 
  gather(visitnum_char,same_scanner_model_full,visit_1:visit_2) %>% mutate(visitnum=ifelse(visitnum_char=="visit_1",1,2))
  
copd_long_full <- left_join(copd_long, copd_clean)

copd_long_full_same_scan <- left_join(copd_long_full,copd_clean_pre2) %>% filter(same_scanner_model_full == 1)

copd_long_clinical <- left_join(copd_clean,copd_cluster_assignments) %>% filter(!is.na(cluster_decamp)) %>% 
  mutate(days=ifelse(visitnum==2,days_ct1_ct2.y,0)) %>% 
  filter(!is.na(days)) %>% select(-days_ct1_ct2.y) %>% 
  mutate(years=days/365)


################################################################################DECAMP#############################################################
decamp_pre1 <- read_excel("data/raw_data/DECAMP clustering imaging variables.xlsx")
decamp_pre2 <- decamp_pre1 %>% mutate(percent_ild_z = Percent_ILA, percent_emphysema_z = Percent_Emphysema, 
                                  percent_normal_z = Percent_Normal, awt_seg_thirona_z = WallThickness, PMA_z = Standard_PMA) %>% 
  mutate_at(scale,.vars = vars(percent_ild_z, percent_emphysema_z, percent_normal_z, awt_seg_thirona_z, PMA_z)) %>% 
  select(Case,percent_ild_z,percent_emphysema_z,
         percent_normal_z,PMA_z,awt_seg_thirona_z)
