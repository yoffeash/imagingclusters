### table 1 for COPDGene Data ###
## see https://cran.r-project.org/web/packages/Gmisc/vignettes/Descriptives.html
## also see https://sejdemyr.github.io/r-tutorials/basics/tables-in-r/ for alternative approach

### formatting variables ###

label(copd_full_imaging$age_enroll) <- "Age"
units(copd_full_imaging$age_enroll) <- "Years"

label(copd_full_imaging$bmi) <- "BMI"
units(copd_full_imaging$bmi) <- "kg/m<sup>2</sup>"

label(copd_full_imaging$fev1pp_utah) <- "Forced Expiratory Volume in 1 Second"
units(copd_full_imaging$fev1pp_utah) <- "Percent Predicted"

label(copd_full_imaging$percent_ild) <- "Interstitial Features"
units(copd_full_imaging$percent_ild) <- "Percent Lung Volume"

label(copd_full_imaging$percent_emphysema) <- "Emphysema"
units(copd_full_imaging$percent_emphysema) <- "Percent Lung Volume"

label(copd_full_imaging$PMA) <- "Pectoralis Muscle Area"
units(copd_full_imaging$PMA) <- "cm<sup>2</sup>"

label(copd_full_imaging$awt_seg_thirona) <- "Airway Wall Thickness"
units(copd_full_imaging$awt_seg_thirona) <- "mm"

copd_full_imaging$sex.f <- as.factor(copd_full_imaging$sex)
label(copd_full_imaging$sex.f) <- "Gender"

copd_full_imaging$race.f <- factor(copd_full_imaging$race, levels=1:2, labels=c("White","Black"))
label(copd_full_imaging$race.f) <- "Race"

copd_full_imaging$smok_cig_now.f <- factor(copd_full_imaging$smok_cig_now, levels=0:1, labels=c("Former Smoker","Current Smoker"))
label(copd_full_imaging$smok_cig_now.f) <- "Current Smoking Status"

copd_full_imaging$by_marker = 1
copd_full_imaging$by_marker.f = factor(copd_full_imaging$by_marker, levels=1, labels=c("COPDGene"))

### define function ###

getTable1Stats <- function(x, digits = 0, ...){
  getDescriptionStatsBy(x = x, 
                        by = copd_full_imaging$by_marker.f,
                        show_all_values = TRUE,
                        use_units = TRUE,
                        NEJMstyle = TRUE,
                        ...)
  
}

### calculate individual values ###
t1 <- list()
t1[["Age"]] <- getTable1Stats(copd_full_imaging$age_enroll, digits=1)
t1[["BMI"]] <- getTable1Stats(copd_full_imaging$bmi, digits=1)
t1[["Forced Expiratory Volume in 1 Seconds"]] <- getTable1Stats(copd_full_imaging$fev1pp_utah, digits=1)
t1[["Interstitial Features"]] <- getTable1Stats(copd_full_imaging$percent_ild, digits=1)
t1[["Emphysema"]] <- getTable1Stats(copd_full_imaging$percent_emphysema, digits=1)
t1[["Pectoralis Muscle Area"]] <- getTable1Stats(copd_full_imaging$PMA, digits=1)
t1[["Airway Wall Thickness"]] <- getTable1Stats(copd_full_imaging$awt_seg_thirona, digits=2)
t1[["Gender"]] <- getTable1Stats(copd_full_imaging$sex.f)
t1[["Race"]] <- getTable1Stats(copd_full_imaging$race.f)
t1[["Current Smoking Status"]] <- getTable1Stats(copd_full_imaging$smok_cig_now.f)

mergeDesc(t1,
          htmlTable_args = list(css.rgroup = "",
                                caption  = "Basic descriptive statistics from the COPDGene Dataset",
                                align="l",
                                align.header="l",
                                css.cell = "padding-left: .8em; padding-right: .8em;",
                                css.header = "padding-left: .8em; padding-right: .8em;"))
