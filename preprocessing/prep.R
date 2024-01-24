#### Libraries #### 

library(tidyverse)


#### Data ####

df <- read.csv("data-raw/1734TuberculosisPati_DATA_2023-08-24_1604.csv") %>%
  dplyr::select(
    record_id, # ID
    redcap_event_name, # time of visit
    redcap_data_access_group, # site/region
    age, # demographics
    dem_sex,
    hiv_test_result,
    ic1, ic2, ic3, ic4, ic5, ic6, ic7, # inclusion criteria
    incl_substudy_yn, # to remove patients with sputum collection (only at South African site)
    ic_enhanced,
    mb_xpert_t1_rifresist, mb_drug_rif, # drug resistance
    starts_with("sf_"), # SF12 quesstionnaire    
    intersect(starts_with("as_"), ends_with("_yn")), # ASSIST questionnaire
    smwt_distance_nr, # 6-min walking test
    mb_smear1_result_who, mb_smear1_result_who, # smear test result
    xr_cavitation_yn, # cavitation
    xr_opacity_percentage_nr,
    starts_with("phq_") # PHQ-9 questionnaire
  ) %>%
  # rename variables 
  rename(sex = dem_sex,
         smwt_dist = smwt_distance_nr,
         opacity = xr_opacity_percentage_nr) %>%
  # filter baseline and end of treatment
  filter(grepl("baseline", redcap_event_name) | grepl("end_of_tx", redcap_event_name)) %>%
  # filter first two entries
  group_by(record_id) %>%
  fill(matches("ic\\d"), .direction = "downup") %>%
  # filter >15years old
  filter(any(ic1 == 1)) %>%
  # additional filters not yet required (enhanced sampling and substudy)
  #!any(ifelse(is.na(ic_enhanced), 1, ic_enhanced) == 2), # assumption: missings belong to the recruitment group "consecutive sample"
  #!any(ifelse(is.na(incl_substudy_yn), 0, incl_substudy_yn) == 1 & redcap_data_access_group == "global_southafrica")) %>% 
  # filter patients with only baseline and end of treatment (should be the first 2 values for every record)
  slice(1:2) %>%
  filter(n() == 2) %>%
  mutate(time = c("Start of treatment", "End of treatment")) %>%
  ungroup() %>%
  # generate variables
  mutate(site = gsub("global_", "", redcap_data_access_group), # re-formatting site
         site = gsub("Global_", "", site), # re-formatting site
         site = gsub("regional_", "", site),
         site = gsub("Regional_", "", site),
         site = tools::toTitleCase(site),
         site = ifelse(site == "Mosambiqu", "Mosambique", site),
         site = ifelse(site == "Southafrica", "South Africa", site),
         drug_resistant = ifelse(mb_xpert_t1_rifresist == 1, 1, ifelse(mb_drug_rif == 2, 1, 0)), # define drug resistance
         drug_resistant = ifelse(is.na(drug_resistant), 0, drug_resistant),
         highbact = ifelse(mb_smear1_result_who %in% c(3, 4), 1, 0),
         highbact = ifelse(is.na(highbact), 0, highbact),
         cavity = ifelse(xr_cavitation_yn == 1, 1, 0),
         cavity = ifelse(is.na(cavity), 0, cavity),
         clindiag = ifelse(ic4 == 1 | ic5 == 5 | ic6 == 1 | ic7 == 1, 0, 1),
         clindiag = ifelse(is.na(clindiag), 1, clindiag),
         opacity = ifelse(is.na(opacity), 0, opacity),
         opacity = ifelse(opacity > 60, 1, 0),
         # recoding SF-12 quesstionnaire
         across(c(sf_moderate_act, sf_stairs), ~ ifelse(.x == 0, 3, .x)),
         across(c(sf_health_accomp, sf_health_limitation, sf_emotional_accomp, sf_emotional_limitation, sf_calm, sf_energy, sf_down, sf_social_activities), ~ ifelse(.x == 0, 5, .x)),
         sf_pain = sf_pain + 1) %>%
  # rename SF12 variables
  rename(I1 = sf_health,
         I2A = sf_moderate_act,
         I2B = sf_stairs,
         I3A = sf_health_accomp,
         I3B = sf_health_limitation,
         I4A = sf_emotional_accomp,
         I4B = sf_emotional_limitation,
         I5 = sf_pain,
         I6A = sf_calm,
         I6B = sf_energy,
         I6C = sf_down,
         I7 = sf_social_activities) %>%
  # recode items, 1, 5, 6a, and 6b
  mutate(
    I1r = recode(I1, `1` = 5, `2` = 4.4, `3` = 3.4, `4` = 2.0, `5` = 1.0),
    I5r = 6 - I5, I6Ar = 6 - I6A, I6Br = 6 - I6B
  ) %>%
  # compute SF12 scores
  # Transform raw scores:
  # PF = Physical Functioning
  # RP = Role Physical
  # BP = Bodily Pain
  # GH = General Health
  # VT = Vitality
  # SF = Social Functioning
  # RE = Role Emotional
  # MH = Mental Health
  mutate(
    PF = ((I2A + I2B - 2) / 4) * 100,
    RP = ((I3A + I3B - 2) / 8) * 100,
    BP = ((I5r - 1) / 4) * 100,
    GH = ((I1r - 1) / 4) * 100,
    VT = ((I6Br - 1) / 4) * 100,
    SF = ((I7 - 1) / 4) * 100,
    RE = ((I4A + I4B - 2) / 8) * 100,
    MH = ((I6Ar + I6C - 2) / 8) * 100,
    # standardisation of SF-12v2 Scores
    PF = (PF - 83.01098) / 28.03756,
    RP = (RP - 79.23666) / 27.12947,
    BP = (BP - 78.64166) / 27.46395,
    GH = (GH - 68.06165) / 24.67408,
    VT = (VT - 53.79299) / 25.97755,
    SF = (SF - 85.16767) / 23.98612,
    RE = (RE - 80.78423) / 25.22565,
    MH = (MH - 70.08673) / 20.66073,
    # physical and Mental Component Scores
    AGG_PHYS = (PF * 0.409) + (RP * 0.325) + (BP * 0.289) + (GH * 0.231) 
    + (VT * 0.106) + (SF * 0.014) + (RE * -.183) + (MH * -.205),
    AGG_MENT = (PF * -.224) + (RP * -.096) + (BP * -.105) + (GH * 0.001) 
    + (VT * 0.157) + (SF * 0.249) + (RE * 0.449) + (MH * 0.476),
    # convert to t-scores
    across(c(PF, RP, BP, GH, VT, SF, RE, MH, AGG_PHYS, AGG_MENT), ~ 50 + (10 * .x))
  ) %>%
  rename(sf12_phys = AGG_PHYS,
         sf12_ment = AGG_MENT) %>%
  # PHP-9 questionnaire
  mutate(phq9_score = phq_interest_yn + phq_down_yn + phq_sleep_yn + phq_energy_yn 
         + phq_appetite_yn + phq_fail_yn + phq_focus_yn + phq_restless_yn + phq_suicidal_yn,
         phq9_score = ifelse(is.na(phq_date), NA, phq9_score),
         phq9_label = ifelse(phq9_score == 1, "No depression",
                             ifelse(phq9_score %in% c(2:4), "Minimal depression",
                                    ifelse(phq9_score %in% c(5:9), "Mild depression",
                                           ifelse(phq9_score %in% c(15:19), "Moderately severe depression", "Severe depression"))))) %>%
  dplyr::select(record_id, time, site, age, sex, hiv_test_result, drug_resistant, highbact, cavity, clindiag, opacity,
                sf12_phys, sf12_ment, smwt_dist, phq9_score, phq9_label) %>%
  mutate(across(c(age, sex, hiv_test_result, drug_resistant, highbact, cavity, clindiag, opacity), 
                ~ ifelse(time == "End of treatment", NA, .x)))


saveRDS(df, "data-clean/phys-ment-data.rds")  
