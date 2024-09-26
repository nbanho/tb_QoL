#### Libraries ####

library(tidyverse)
library(lubridate)

#### Data ####

# raw data
file_name <- "data-raw/1734TuberculosisPati_DATA_2024-09-24_1548 1.csv"
df <- read.csv(file_name)

# preprocessing
df_prep <- df %>%
  filter(is.na(redcap_repeat_instance)) %>%
  dplyr::select(
    record_id, # ID
    redcap_event_name, # time of visit
    redcap_data_access_group, # site
    ce_completion_date, # date of visit
    age, # demographics
    dem_sex, # female is 2
    hiv_test_result, # 1 is positive
    ic1, ic2, ic3, ic4, ic5, ic6, ic7, # inclusion criteria
    incl_substudy_yn,
    mb_xpert_t1_rifresist, mb_drug_rif, # drug resistance
    tbd_pat_cat, # relapse or new tb case
    starts_with("sf_"), # SF12 quesstionnaire
    intersect(starts_with("as_"), ends_with("_yn")), # ASSIST questionnaire
    smwt_distance_nr, # 6-min walking test
    fa_sit_to_stand_nr, # sit to stand test
    mb_smear1_result_who, mb_smear1_result_who, # smear test result
    xr_cavitation_yn, # cavitation
    xr_opacity_percentage_nr, # opacity percentage
    starts_with("phq_"), # PHQ-9 questionnaire
    ment_trt_yn, # mental health treatment
    ment_healer_yn,
    ment_inpatient_yn,
    ment_outpatient_yn,
    ment_med_yn,
    ce_cough_blood_yn, # TB symptom score
    ce_cough_yn,
    ce_chestpain_yn,
    ce_dyspnea_yn,
    ce_temp_nr,
    ce_heart_rate_nr,
    ce_crackles,
    ce_wheezing,
    ce_lung_sounds,
    ce_weight_nr,
    ce_height_nr,
    as_alc_calc, # ASSIST questionnaire
    as_tabacco_calc,
  ) %>%
  # rename variables
  rename(
    date_visit = ce_completion_date,
    time = redcap_event_name,
    site = redcap_data_access_group,
    sex = dem_sex,
    hiv = hiv_test_result,
    smwt_dist = smwt_distance_nr,
    stst_nr = fa_sit_to_stand_nr,
    opacity = xr_opacity_percentage_nr,
    as_tobacco_calc = as_tabacco_calc
  ) %>%
  # filter baseline and end of treatment
  mutate(
    is_start = grepl("baseline", time),
    is_end = grepl("end_of_tx", time),
    is_post = grepl("6m_post_tx", time)
  ) %>%
  filter(is_start | is_end | is_post) %>%
  mutate(
    time = ifelse(is_start, "Start", ifelse(is_end, "End", "Post"))
  ) %>%
  # filter >15years old
  group_by(record_id) %>%
  fill(matches("ic\\d"), .direction = "downup") %>%
  filter(any(ic1 == 1)) %>%
  ungroup() %>%
  # generate variables
  mutate(
    incl_substudy_yn = ifelse(is.na(incl_substudy_yn), 0, incl_substudy_yn),
    site = gsub("global_", "", site), # re-format site
    site = gsub("Global_", "", site),
    site = gsub("regional_", "", site),
    site = gsub("Regional_", "", site),
    site = tools::toTitleCase(site),
    site = ifelse(site == "Mosambiqu", "Mosambique", site),
    site = ifelse(site == "Southafrica", "South Africa", site),
    age = ifelse(age > 120, NA, age), # remove outliers
    sex = ifelse(sex == 1, 0, 1), # now: female is 1
    hiv = ifelse(hiv == 99, NA, ifelse(hiv == 1, 1, 0)),
    mdr = ifelse(mb_xpert_t1_rifresist == 1, 1, ifelse(mb_drug_rif == 2, 1, 0)),
    mdr = ifelse(is.na(mdr), 0, mdr),
    highbact = ifelse(mb_smear1_result_who %in% c(3, 4), 1, 0),
    highbact = ifelse(is.na(highbact), 0, highbact),
    cavity = ifelse(xr_cavitation_yn == 1, 1, 0),
    cavity = ifelse(is.na(cavity), 0, cavity),
    clindiag = ifelse(ic4 == 1 | ic5 == 5 | ic6 == 1 | ic7 == 1, 0, 1),
    clindiag = ifelse(is.na(clindiag), 1, clindiag),
    tbd_pat_cat = ifelse(tbd_pat_cat == 1, 0, 1), # 0: new, 1: relapse
    tbd_pat_cat = ifelse(is.na(tbd_pat_cat), 0, tbd_pat_cat),
    opacity = ifelse(is.na(opacity), 0, opacity),
    opacity = ifelse(opacity > 60, 1, 0),
    # physical variables
    smwt_dist = ifelse(smwt_dist == 999, NA, smwt_dist),
    stst_nr = ifelse(stst_nr == 999, NA, stst_nr),
    stst_nr = ifelse(stst_nr > 100, NA, stst_nr),
    # recoding SF-12 quesstionnaire
    across(c(sf_moderate_act, sf_stairs), ~ ifelse(.x == 0, 3, .x)),
    across(
      c(
        sf_health_accomp, sf_health_limitation, sf_emotional_accomp,
        sf_emotional_limitation, sf_calm,
        sf_energy, sf_down, sf_social_activities
      ),
      ~ ifelse(.x == 0, 5, .x)
    ),
    sf_pain = sf_pain + 1
  ) %>%
  # filter test site
  filter(site != "Test_site") %>%
  # rename SF12 variables
  rename(
    I1 = sf_health,
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
    I7 = sf_social_activities
  ) %>%
  # recode items, 1, 5, 6a, and 6b
  mutate(
    across(c(I1, I2A, I2B, I3A, I3B, I4A, I4B, I5, I6A, I6B, I6C), as.numeric),
    I1r = recode(I1, `1` = 5, `2` = 4.4, `3` = 3.4, `4` = 2.0, `5` = 1.0),
    I5r = 6 - I5, I6Ar = 6 - I6A, I6Br = 6 - I6B
  ) %>%
  # compute SF12 scores
  # Transform raw scores:
  # PF: Physical Functioning
  # RP: Role Physical
  # BP: Bodily Pain
  # GH: General Health
  # VT: Vitality
  # SF: Social Functioning
  # RE: Role Emotional
  # MH: Mental Health
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
    across(
      c(PF, RP, BP, GH, VT, SF, RE, MH, AGG_PHYS, AGG_MENT),
      ~ 50 + (10 * .x)
    )
  ) %>%
  rename(
    sf12_phys = AGG_PHYS,
    sf12_ment = AGG_MENT
  ) %>%
  # PHP-9 questionnaire
  mutate(
    phq9_score = phq_interest_yn + phq_down_yn + phq_sleep_yn + phq_energy_yn
      + phq_appetite_yn + phq_fail_yn + phq_focus_yn
      + phq_restless_yn + phq_suicidal_yn,
    phq9_score = ifelse(is.na(phq_date), NA, phq9_score)
  ) %>%
  # mental health treatment
  mutate(
    ment_trt_yn = ifelse(
      ment_trt_yn == 1 |
        ment_healer_yn == 1 |
        ment_inpatient_yn == 1 |
        ment_outpatient_yn == 1 |
        ment_med_yn == 1,
      1, 0
    ),
    ment_trt_yn = ifelse(is.na(ment_trt_yn), 0, ment_trt_yn)
  ) %>%
  # TB symptom score
  mutate(
    across(
      c(
        ce_heart_rate_nr, ce_temp_nr, ce_weight_nr, ce_height_nr,
        ce_cough_blood_yn, ce_cough_yn, ce_chestpain_yn, ce_dyspnea_yn,
        ce_crackles, ce_wheezing, ce_lung_sounds
      ),
      ~ ifelse(.x == 99 | .x == 999 | .x == 9999, NA, .x)
    ),
    tachycardia = ifelse(ce_heart_rate_nr > 90, 1, 0),
    auscultation = ifelse(
      ce_crackles == 1 | ce_wheezing == 1 | ce_lung_sounds == 1, 1, 0
    ),
    high_temp = ifelse(ce_temp_nr > 37, 1, 0),
    ce_height_nr = ifelse(ce_height_nr > 1000, ce_height_nr / 1000,
      ifelse(ce_height_nr > 100, ce_height_nr / 100,
        ifelse(ce_height_nr > 10, ce_height_nr / 10, ce_height_nr)
      )
    ),
    bmi = ce_weight_nr / (ce_height_nr^2),
    bmi16 = ifelse(bmi < 16, 1, 0),
    bmi18 = ifelse(bmi < 18, 1, 0),
    tb_symp_score = ce_cough_blood_yn + ce_cough_yn +
      ce_chestpain_yn + ce_dyspnea_yn + high_temp +
      tachycardia + auscultation + bmi16 + bmi18
  ) %>%
  dplyr::select(
    record_id, time, date_visit, site, 
    incl_substudy_yn,
    age, sex, bmi,
    hiv, mdr, highbact, cavity, clindiag, opacity, tbd_pat_cat,
    as_alc_calc, as_tobacco_calc,
    sf12_phys, sf12_ment, smwt_dist, stst_nr, phq9_score,
    tb_symp_score,
    ce_cough_blood_yn, ce_cough_yn,
    ce_chestpain_yn, ce_dyspnea_yn,
    ment_trt_yn
  ) %>%
  mutate(across(
    c(
      age, sex, hiv,
      mdr, highbact, cavity, opacity,
      clindiag, tbd_pat_cat
    ),
    ~ ifelse(time %in% c("End", "Post"), NA, .x)
  )) %>%
  # if date_visit we impute as +/- 6 months of previous/next visit
  # there are a few patients without any date_visit, which are left NA
  # luckily these are all patients with only one visit,
  # so there are no adverse events we could miss for them
  mutate(
    time = factor(time, levels = c("Start", "End", "Post")),
    date_visit = as.Date(date_visit),
    date_visit_origin = date_visit
  ) %>%
  group_by(record_id) %>%
  arrange(time) %>%
  mutate(
    date_visit = ifelse(
      is.na(date_visit),
      ifelse(!is.na(lag(date_visit)),
        as.character(lag(date_visit) %m+% months(6)),
        ifelse(!is.na(lead(date_visit)),
          as.character(lead(date_visit) %m-% months(6)),
          NA
        )
      ),
      as.character(date_visit)
    ),
    date_visit = as.Date(date_visit)
  ) %>%
  ungroup()


# add st george score (format data and then compute score in excel file)
sgrq <- df %>%
  filter(is.na(redcap_repeat_instance)) %>%
  dplyr::select(
    record_id,
    redcap_event_name,
    starts_with("sgrq_")
  ) %>%
  mutate(
    `1a` = ifelse(sgrq_q1_cough == 0, 1, 0),
    `1b` = ifelse(sgrq_q1_cough == 1, 1, 0),
    `1c` = ifelse(sgrq_q1_cough == 2, 1, 0),
    `1d` = ifelse(sgrq_q1_cough == 3, 1, 0),
    `1e` = ifelse(sgrq_q1_cough == 4, 1, 0),
    `2a` = ifelse(sgrq_q1_sputum == 0, 1, 0),
    `2b` = ifelse(sgrq_q1_sputum == 1, 1, 0),
    `2c` = ifelse(sgrq_q1_sputum == 2, 1, 0),
    `2d` = ifelse(sgrq_q1_sputum == 3, 1, 0),
    `2e` = ifelse(sgrq_q1_sputum == 4, 1, 0),
    `3a` = ifelse(sgrq_q1_breath == 0, 1, 0),
    `3b` = ifelse(sgrq_q1_breath == 1, 1, 0),
    `3c` = ifelse(sgrq_q1_breath == 2, 1, 0),
    `3d` = ifelse(sgrq_q1_breath == 3, 1, 0),
    `3e` = ifelse(sgrq_q1_breath == 4, 1, 0),
    `4a` = ifelse(sgrq_q1_attacks == 0, 1, 0),
    `4b` = ifelse(sgrq_q1_attacks == 1, 1, 0),
    `4c` = ifelse(sgrq_q1_attacks == 2, 1, 0),
    `4d` = ifelse(sgrq_q1_attacks == 3, 1, 0),
    `4e` = ifelse(sgrq_q1_attacks == 4, 1, 0),
    `5a` = ifelse(sgrq_attacks == 0, 1, 0),
    `5b` = ifelse(sgrq_attacks == 1, 1, 0),
    `5c` = ifelse(sgrq_attacks == 2, 1, 0),
    `5d` = ifelse(sgrq_attacks == 3, 1, 0),
    `5e` = ifelse(sgrq_attacks == 4, 1, 0),
    `6a` = ifelse(sgrq_attack_duration == 1, 1, 0),
    `6b` = ifelse(sgrq_attack_duration == 2, 1, 0),
    `6c` = ifelse(sgrq_attack_duration == 3, 1, 0),
    `6d` = ifelse(sgrq_attack_duration == 4, 1, 0),
    `7a` = ifelse(sgrq_good_days == 0, 1, 0),
    `7b` = ifelse(sgrq_good_days == 1, 1, 0),
    `7c` = ifelse(sgrq_good_days == 2, 1, 0),
    `7d` = ifelse(sgrq_good_days == 3, 1, 0),
    `7e` = ifelse(sgrq_good_days == 4, 1, 0),
    `8` = sgrq_wheeze,
    `9a` = ifelse(sgrq_condition == 1, 1, 0),
    `9b` = ifelse(sgrq_condition == 2, 1, 0),
    `9c` = ifelse(sgrq_condition == 3, 1, 0),
    `9d` = ifelse(sgrq_condition == 4, 1, 0),
    `10a` = ifelse(sgrq_employment == 1, 1, 0),
    `10b` = ifelse(sgrq_employment == 2, 1, 0),
    `10c` = ifelse(sgrq_employment == 3, 1, 0),
    `11a` = `sgrq_breathless_act___1`,
    `11b` = `sgrq_breathless_act___2`,
    `11c` = `sgrq_breathless_act___3`,
    `11d` = `sgrq_breathless_act___4`,
    `11e` = `sgrq_breathless_act___5`,
    `11f` = `sgrq_breathless_act___4`,
    `11g` = `sgrq_breathless_act___5`,
    `12a` = `sgrq_breathless_cough___1`,
    `12b` = `sgrq_breathless_cough___2`,
    `12c` = `sgrq_breathless_cough___3`,
    `12d` = `sgrq_breathless_cough___4`,
    `12e` = `sgrq_breathless_cough___5`,
    `12f` = `sgrq_breathless_cough___6`,
    `13a` = `sgrq_effects_chest___1`,
    `13b` = `sgrq_effects_chest___2`,
    `13c` = `sgrq_effects_chest___3`,
    `13d` = `sgrq_effects_chest___4`,
    `13e` = `sgrq_effects_chest___5`,
    `13f` = `sgrq_effects_chest___6`,
    `13g` = `sgrq_effects_chest___7`,
    `13h` = `sgrq_effects_chest___8`,
    `14a` = `sgrq_meds___1`,
    `14b` = `sgrq_meds___2`,
    `14c` = `sgrq_meds___3`,
    `14d` = `sgrq_meds___3`,
    `15a` = `sgrq_activities___1`,
    `15b` = `sgrq_activities___2`,
    `15c` = `sgrq_activities___3`,
    `15d` = `sgrq_activities___4`,
    `15e` = `sgrq_activities___5`,
    `15f` = `sgrq_activities___6`,
    `15g` = `sgrq_activities___7`,
    `15h` = `sgrq_activities___8`,
    `15i` = `sgrq_activities___9`,
    `16a` = `sgrq_effect_dailylife___1`,
    `16b` = `sgrq_effect_dailylife___2`,
    `16c` = `sgrq_effect_dailylife___3`,
    `16d` = `sgrq_effect_dailylife___4`,
    `16e` = `sgrq_effect_dailylife___5`,
    `17a` = ifelse(sgrq_affecting_chest == 1, 1, 0),
    `17b` = ifelse(sgrq_affecting_chest == 2, 1, 0),
    `17c` = ifelse(sgrq_affecting_chest == 3, 1, 0),
    `17d` = ifelse(sgrq_affecting_chest == 4, 1, 0)
  ) %>%
  rename(time = redcap_event_name) %>%
  mutate(
    is_start = grepl("baseline", time),
    is_end = grepl("end_of_tx", time),
    is_post = grepl("6m_post_tx", time)
  ) %>%
  filter(is_start | is_end | is_post) %>%
  mutate(
    time = ifelse(is_start, "Start", ifelse(is_end, "End", "Post"))
  ) %>%
  dplyr::select(
    record_id,
    time,
    sgrq_date_2,
    matches("^\\d")
  )

writexl::write_xlsx(
  sgrq %>% dplyr::select(record_id, matches("^\\d")),
  "preprocessing/sgrq-data.xlsx"
)

sgrq_tot_score <- sgrq %>%
  mutate(
    sgrq_tot_score = readxl::read_xlsx(
      "preprocessing/sgrq-data-result.xlsx"
    )$sgrq_tot_score
  ) %>%
  mutate(
    sgrq_tot_score = ifelse(sgrq_tot_score == "MISSING", NA, sgrq_tot_score),
    sgrq_tot_score = as.numeric(sgrq_tot_score)
  ) %>%
  dplyr::select(record_id, time, sgrq_date_2, sgrq_tot_score)

df_prep <- left_join(
  df_prep,
  sgrq_tot_score %>%
    mutate(time = factor(time, levels = c("Start", "End", "Post"))),
  by = c("record_id", "time")
)

# filter patients with no clinical data
df_prep <- df_prep %>%
  group_by(record_id) %>%
  filter(!any(incl_substudy_yn == 1)) %>%
  ungroup() %>%
  dplyr::select(-incl_substudy_yn)

# full data
df_full <- expand.grid(
  record_id = unique(df_prep$record_id),
  time = unique(df_prep$time)
) %>%
  left_join(df_prep, by = c("record_id", "time")) %>%
  group_by(record_id) %>%
  fill(
    site,
    age, sex, hiv,
    mdr, highbact, cavity, opacity,
    clindiag, tbd_pat_cat,
    .direction = "downup"
  ) %>%
  ungroup() %>%
  mutate(time = factor(time, levels = c("Start", "End", "Post")))

# add TB treatment outcome
trt_out <- df %>%
  group_by(record_id) %>%
  summarise(
    eot_outcome = ifelse(
      all(is.na(eot_outcome)), NA,
      mean(eot_outcome, na.rm = TRUE)
    )
  ) %>%
  ungroup() %>%
  mutate(
    treat_success =
      ifelse(eot_outcome <= 2, 1,
        ifelse(eot_outcome <= 4, 0, NA)
      )
  ) %>%
  mutate(
    time = "End",
    time = factor(time, levels = c("Start", "End", "Post"))
  )

df_full <- df_full %>%
  left_join(trt_out, by = c("record_id", "time"))

# add non-fatal (nf) adverse events (ae)
adv_events <- df %>%
  filter(
    grepl("adverse_event", redcap_event_name)
  ) %>%
  dplyr::select(
    record_id,
    starts_with("imp_")
  ) %>%
  mutate(
    is_ae = imp_ae_serious___1 == 1 |
      imp_ae_serious___2 == 1 |
      imp_ae_serious___3 == 1 |
      imp_ae_serious___4 == 1 |
      imp_ae_serious___5 == 1 |
      imp_ae_serious___88 == 1,
    is_nf_ae = ifelse(is_ae, ifelse(imp_death_date == "", 1, 0), 0)
  ) %>%
  filter(is_nf_ae == 1)

sprintf(
  "Adverse events in entire data: %i in %i patients",
  nrow(adv_events), n_distinct(adv_events$record_id)
)

sprintf(
  "IDs with AE but no entry in database: %s",
  paste(setdiff(adv_events$record_id, df_full$record_id), collapse = ", ")
)

# if AE before baseline visit, code it as during treatment (Zimbabwe)
# if AE date is missing, code it as during treatment (two cases)
# if AE is missing because date_visit is missing, code it as no AE
# as these are all patients who only had a baseline visit

df_full <- df_full %>%
  left_join(
    adv_events %>%
      dplyr::select(record_id, imp_onset_date, is_nf_ae) %>%
      rename(nf_ae_date = imp_onset_date),
    by = "record_id"
  ) %>%
  group_by(record_id) %>%
  arrange(time) %>%
  mutate(
    date_visit_exp = ifelse(
      is.na(date_visit),
      ifelse(!is.na(lag(date_visit)),
        as.character(lag(date_visit) %m+% months(6)),
        ifelse(!is.na(lead(date_visit)),
          as.character(lead(date_visit) %m-% months(6)),
          ifelse(!is.na(lag(date_visit, 2)),
            as.character(lag(date_visit, 2) %m+% months(12)),
            NA
          )
        )
      ),
      as.character(date_visit)
    ),
    date_visit_exp = as.Date(date_visit_exp)
  ) %>%
  mutate(
    nf_ae_date = as.Date(nf_ae_date),
    nf_ae_date = ifelse(
      is_nf_ae == 1 & is.na(nf_ae_date),
      as.character(date_visit_exp[time == "End"]),
      as.character(nf_ae_date)
    ),
    nf_ae_date = as.Date(nf_ae_date)
  ) %>%
  mutate(
    nf_ae = ifelse(is.na(nf_ae_date), 0,
      ifelse(nf_ae_date <= date_visit_exp, 1, 0)
    ),
    nf_ae = ifelse(is.na(nf_ae), 0, nf_ae),
    nf_ae = ifelse(time == "Start", 0,
      ifelse(time == "Post", ifelse(nf_ae[time == "End"] == 1, 0, nf_ae),
        nf_ae
      )
    )
  ) %>%
  ungroup() %>%
  mutate(
    nf_ae_date = ifelse(nf_ae == 1, as.character(nf_ae_date), NA),
    nf_ae_date = as.Date(nf_ae_date)
  ) %>%
  dplyr::select(-is_nf_ae)

# add deaths
df_death <- df %>%
  rename(death = death_yn) %>%
  dplyr::select(record_id, death, death_date) %>%
  filter(death == 1) %>%
  group_by(record_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(death = TRUE)

df_full <- df_full %>%
  left_join(df_death, by = "record_id") %>%
  mutate(
    death = ifelse(is.na(death), FALSE, death),
    death = ifelse(time == "Start", FALSE, death)
  )

k_deaths <- df_full %>%
  filter(death) %>%
  group_by(record_id) %>%
  slice(1) %>%
  ungroup() %>%
  nrow()

print(
  sprintf(
    "Deaths: %i (%i percent of patients)",
    k_deaths, round(k_deaths / n_distinct(df_full$record_id) * 100)
  )
)

# determine loss to followup
#' Assume loss to follow-up if time since last visit > 8 months.
#' Further assume loss to follow-up at post treatment
#' if that was already the case at end of treatment.
#' If death, then impute with worst outcomes.

file_date_chr <- stringi::stri_extract(
  file_name,
  regex = "\\d{4}-\\d{2}-\\d{2}"
)
current_date <- as.Date(file_date_chr, format = "%Y-%m-%d")
waiting_time <- 8 * 30


df_full <- df_full %>%
  mutate(time = factor(time, levels = c("Start", "End", "Post"))) %>%
  group_by(record_id) %>%
  arrange(time) %>%
  mutate(
    ana = is.na(phq9_score) + is.na(sf12_ment) + is.na(sf12_phys) +
      is.na(smwt_dist) + is.na(stst_nr) +
      is.na(sgrq_tot_score) == 6,
    dslv = as.numeric(current_date - lag(date_visit), "days"),
    mfu =
      ifelse(time == "Start" | !is.na(date_visit) | !ana, FALSE, NA),
    mfu =
      ifelse(!is.na(mfu), mfu,
        ifelse(dslv > waiting_time, TRUE, NA)
      ),
    mfu =
      ifelse(!is.na(mfu), mfu,
        ifelse(time %in% c("End", "Post"),
          ifelse(lag(mfu), TRUE, NA), NA
        )
      ),
    death = ifelse(!ana, FALSE, death),
    mfu = ifelse(death, FALSE, mfu),
    treat_success = ifelse(death, 0, treat_success),
  ) %>%
  ungroup() %>%
  # exclusion: post treatment data if death at end of treatment
  group_by(record_id) %>%
  mutate(cum_death = cumsum(death)) %>%
  ungroup() %>%
  filter(cum_death < 2) %>%
  dplyr::select(-cum_death)

table(df_full$mfu, df_full$death, useNA = "always")

# inclusion: complete case data including loss to follow-up
df_cc <- df_full %>%
  filter(!is.na(mfu))

n <- nrow(df_full)
k_incl <- nrow(df_cc)
k_pat_incl <- n_distinct(df_cc$record_id)

print(sprintf(
  "Complete: %i patients with %i observations (%i percent)",
  k_pat_incl,
  k_incl,
  round(k_incl / n * 100)
))

# lost to follow-up
k_mfu <- df_full %>%
  filter(mfu) %>%
  nrow()
k_pat_mfu <- df_full %>%
  filter(mfu) %>%
  dplyr::select(record_id) %>%
  unlist() %>%
  n_distinct()

print(sprintf(
  "MFU w/o deaths: %i (%i percent of patients, %i percent of complete data)",
  k_pat_mfu,
  round(k_pat_mfu / k_pat_incl * 100),
  round(k_mfu / k_incl * 100)
))

# visit date date but missing information
strange <- filter(df_full, !mfu, ana, !is.na(date_visit), death == 0)
k_strange <- nrow(strange)

print(sprintf(
  "All outcomes missing: %i (%i percent of complete data)",
  k_strange,
  round(100 * k_strange / k_incl)
))

# exclusion
k_excl_end <- df_full %>%
  group_by(record_id) %>%
  filter(time == "End", is.na(mfu), death == 0) %>%
  ungroup() %>%
  nrow()

print(
  sprintf(
    "Waiting for end of treatment data: %i (%i percent)",
    k_excl_end, round(k_excl_end / (n / 3) * 100)
  )
)

k_excl_post <- df_full %>%
  group_by(record_id) %>%
  filter(time == "Post", is.na(mfu), death == 0) %>%
  ungroup() %>%
  nrow()

print(
  sprintf(
    "Waiting for post treatment data: %i (%i percent)",
    k_excl_post, round(k_excl_post / (n / 3) * 100)
  )
)

# save data
saveRDS(df_cc, "data-clean/phys-ment-data.rds")
