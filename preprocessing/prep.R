#### Libraries #### 

library(tidyverse)


#### Data ####

# raw data
file_name <- "data-raw/reporting_file_all_29122023_mod.dta"
df <- haven::read_dta(file_name)

# preprocessing
df_prep <- df %>%
  dplyr::select(
    record_id, # ID
    redcap_event_name, # time of visit: baseline, end of treatment, post treatment, etc
    ce_completion_date, # date of visit
    redcap_data_access_group, # site/region
    age, # demographics
    gender,
    hiv_test_result,
    ic1, ic2, ic3, ic4, ic5, ic6, ic7, # inclusion criteria
    incl_substudy_yn, # to remove patients with sputum collection (only at South African site)
    ic_enhanced,
    mb_xpert_t1_rifresist, mb_drug_rif, # drug resistance
    starts_with("sf_"), # SF12 quesstionnaire    
    intersect(starts_with("as_"), ends_with("_yn")), # ASSIST questionnaire
    smwt_distance_nr, # 6-min walking test
    fa_sit_to_stand_nr, # sit to stand test
    mb_smear1_result_who, mb_smear1_result_who, # smear test result
    xr_cavitation_yn, # cavitation
    xr_opacity_percentage_nr,
    starts_with("phq_") # PHQ-9 questionnaire
  ) %>%
  # rename variables 
  rename(sex = gender,
         smwt_dist = smwt_distance_nr,
         stst_nr = fa_sit_to_stand_nr,
         opacity = xr_opacity_percentage_nr) %>%
  # filter baseline and end of treatment
  filter(grepl("baseline", redcap_event_name) | grepl("end_of_tx", redcap_event_name) | grepl("6m_post_tx", redcap_event_name)) %>%
  mutate(time = ifelse(grepl("baseline", redcap_event_name), "Start treatment", ifelse(grepl("post", redcap_event_name), "Post treatment", "End treatment")),
         time = factor(time, levels = c("Start treatment", "End treatment", "Post treatment"))) %>%
  # filter first two entries
  group_by(record_id) %>%
  fill(matches("ic\\d"), .direction = "downup") %>%
  # filter >15years old
  filter(any(ic1 == 1)) %>%
  # additional filters not yet required (enhanced sampling and substudy)
  #!any(ifelse(is.na(ic_enhanced), 1, ic_enhanced) == 2), # assumption: missings belong to the recruitment group "consecutive sample"
  #!any(ifelse(is.na(incl_substudy_yn), 0, incl_substudy_yn) == 1 & redcap_data_access_group == "global_southafrica")) %>% 
  ungroup() %>%
  # generate variables
  mutate(site = gsub("global_", "", redcap_data_access_group), # re-formatting site
         site = gsub("Global_", "", site), # re-formatting site
         site = gsub("regional_", "", site),
         site = gsub("Regional_", "", site),
         site = tools::toTitleCase(site),
         site = ifelse(site == "Mosambiqu", "Mosambique", site),
         site = ifelse(site == "Southafrica", "South Africa", site),
         sex = ifelse(sex == 1, 0, 1), # 1 is male
         hiv_test_result = ifelse(hiv_test_result == 99, NA, ifelse(hiv_test_result == 1, 1, 0)),
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
    across(c(I1, I2A, I2B, I3A, I3B, I4A, I4B, I5, I6A, I6B, I6C), as.numeric),
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
         phq9_score = ifelse(is.na(phq_date), NA, phq9_score)) %>%
  dplyr::select(record_id, time, ce_completion_date, site, age, sex, 
                hiv_test_result, drug_resistant, highbact, cavity, clindiag, opacity,
                sf12_phys, sf12_ment, smwt_dist, stst_nr, phq9_score) %>%
  mutate(across(c(age, hiv_test_result, drug_resistant, highbact, cavity, clindiag, opacity), 
                ~ ifelse(time == "End treatment" | time == "Post treatment", NA, .x))) %>%
  # cutoffs
  mutate(phq9_score_bin = ifelse(phq9_score > 10, 1, 0),
         sf12_phys_bin = ifelse(sf12_phys < 50, 1, 0),
         sf12_ment_bin = ifelse(sf12_ment < 42, 1, 0),
         smwt_dist_bin = ifelse(smwt_dist < 400, 1, 0),
         stst_nr_bin = ifelse(stst_nr < 20, 1, 0))

# full data
df_full <- expand.grid(record_id = unique(df_prep$record_id), time = unique(df_prep$time)) %>%
  left_join(df_prep, by = c("record_id", "time")) %>%
  # fill baseline variables
  group_by(record_id) %>%
  fill(age, hiv_test_result, sex, drug_resistant, highbact, clindiag, cavity, opacity, .direction = "downup") %>%
  ungroup() 


# determine loss to followup 
current_date = as.Date(stringi::stri_extract(file_name, regex = "\\d{8}"), format = "%d%m%Y")
waiting_time <- 8 * 30
#' assume loss to follow-up if waiting_time since last visit is exceeded both for end and post treatment
#' further assume loss to follow-up at end/post treatment if that was the case at start/end treatment already

df_full <- df_full %>%
  group_by(record_id) %>%
  arrange(time) %>%
  mutate(missing_obs = is.na(sf12_phys) + is.na(sf12_ment) + is.na(smwt_dist) + is.na(phq9_score) + is.na(stst_nr) == 5,
         days_since_last_visit = as.numeric(current_date - lag(ce_completion_date), "days"),
         ltfu = ifelse(time == "Start treatment" | !is.na(ce_completion_date) | !missing_obs, F, NA),
         ltfu = ifelse(!is.na(ltfu), ltfu, ifelse(days_since_last_visit > waiting_time, T, NA)),
         ltfu = ifelse(!is.na(ltfu), ltfu,  ifelse(time %in% c("End treatment", "Post treatment"), ifelse(lag(ltfu), T, NA), NA))) %>%
  ungroup()

table(df_full$ltfu, useNA = "always")

# inclusion: complete case data including loss to follow-up
df_cc <- df_full %>%
  group_by(record_id) %>%
  filter(all(!is.na(ltfu))) %>%
  ungroup() 
K_incl <- nrow(df_cc)
print(sprintf("Complete patients: %i with %i observations (%i percent)", 
              K_incl / 3, K_incl, round(K_incl / N * 100)))

# lost to follow-up
K_ltfu <- df_full %>%
  filter(ltfu) %>%
  nrow()
print(sprintf("Observations lost to follow-up: %i (%i percent of total, %i percent of complete cases)", 
              K_ltfu / 3, round(K_ltfu / N * 100), round(K_ltfu / K_incl * 100)))

# ce completion date but missing information
N <- nrow(df_full) 
strange_cases <- df_full %>% filter(!ltfu, missing_obs, !is.na(ce_completion_date))
K_missing <- nrow(strange_cases)
print(sprintf("Completely missing but ce completion date: %i (%i percent of total, %i percent of complete data)", 
              K_missing, round(100 * K_missing/ N), round(100 * K_missing/ K_incl)))

# exclusion
K_excl_end <- df_full %>%
  group_by(record_id) %>%
  filter(time == "End treatment", is.na(ltfu)) %>%
  ungroup() %>%
  nrow()
print(sprintf("Waiting for end of treatment data: %i (%i percent)", K_excl_end, round(K_excl_end / (N/3) * 100)))
K_excl_post <- df_full %>%
  group_by(record_id) %>%
  filter(time == "Post treatment", is.na(ltfu)) %>%
  ungroup() %>%
  nrow()
print(sprintf("Waiting for post treatment data: %i (%i percent)", K_excl_post, round(K_excl_post / (N/3) * 100)))

saveRDS(df_cc, "data-clean/phys-ment-data.rds")  
