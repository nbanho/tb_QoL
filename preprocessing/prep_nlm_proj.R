library(tidyverse)

# Load existing data
qol <- readRDS("data-clean/phys-ment-data.rds") %>%
  arrange(time, record_id) %>%
  # cutoffs
  mutate(
    phq9_score_bin = ifelse(phq9_score > 10, 1, 0),
    sf12_phys_bin = ifelse(sf12_phys < 50, 1, 0),
    sf12_ment_bin = ifelse(sf12_ment < 42, 1, 0),
    smwt_dist_bin = ifelse(smwt_dist < 400, 1, 0),
    stst_nr_bin = ifelse(stst_nr < 20, 1, 0),
    sgrq_tot_score_bin = ifelse(sgrq_tot_score > 25, 1, 0),
    tb_symp_score_bin = ifelse(tb_symp_score > 4, 1, 0)
  ) %>%
  rename(
    qol_phq9_score = phq9_score,
    qol_sf12_phys = sf12_phys,
    qol_sf12_ment = sf12_ment,
    qol_smwt_dist = smwt_dist,
    qol_stst_nr = stst_nr,
    qol_sgrq_tot_score = sgrq_tot_score,
    qol_tb_symp_score = tb_symp_score,
    qol_phq9_score_bin = phq9_score_bin,
    qol_sf12_phys_bin = sf12_phys_bin,
    qol_sf12_ment_bin = sf12_ment_bin,
    qol_smwt_dist_bin = smwt_dist_bin,
    qol_stst_nr_bin = stst_nr_bin,
    qol_sgrq_tot_score_bin = sgrq_tot_score_bin,
    qol_tb_symp_score_bin = tb_symp_score_bin
  ) %>%
  rename(
    cavitation = cavity,
    hiv_status = hiv
  ) %>%
  dplyr::select(
    record_id,
    time,
    eot_outcome,
    matches("qol_"),
    death,
    mfu,
    age,
    sex,
    hiv_status,
    mdr,
    highbact,
    cavitation,
    tbd_pat_cat,
    as_alc_calc,
    as_tobacco_calc
  ) %>%
  mutate(
    across(
      c(age, sex, hiv_status, highbact, mdr, cavitation, tbd_pat_cat),
      ~ ifelse(time == "Start", .x, NA)
    ),
  ) %>%
  mutate(
    sex = case_when(
      sex == 0 ~ "Male",
      sex == 1 ~ "Female"
    ),
    hiv_status = case_when(
      hiv_status == 0 ~ "Negative",
      hiv_status == 1 ~ "Positive"
    ),
    cavitation = case_when(
      cavitation == 0 ~ "No",
      cavitation == 1 ~ "Yes"
    ),
    tbd_pat_cat = case_when(
      tbd_pat_cat == 0 ~ "New case",
      tbd_pat_cat == 1 ~ "Relapse"
    ),
    mdr = case_when(
      mdr == 0 ~ "rifampin-susceptible",
      mdr == 1 ~ "rifampin-resistant"
    ),
    highbact = case_when(
      highbact == 0 ~ "Negative/Scant/Positive 1+",
      highbact == 1 ~ "Positive 2+/3+"
    ),
    death = ifelse(death, "Yes", "No"),
    mfu = ifelse(mfu, "Yes", "No"),
    eot_outcome = ifelse(death == "Yes", 4, eot_outcome),
    eot_outcome = case_when(
      eot_outcome == 1 ~ "Cured",
      eot_outcome == 2 ~ "Treatment completed",
      eot_outcome == 3 ~ "Treatment failed",
      eot_outcome == 4 ~ "Died (any cause)",
      eot_outcome == 5 ~ "Lost to follow-up",
      eot_outcome == 6 ~ "Transferred out from study site",
      eot_outcome == 99 ~ "Not known"
    ),
    across(matches("_bin"), ~ ifelse(.x == 1, "impaired", "not impaired"))
  ) %>%
  rename(
    dem_age = age,
    dem_sex = sex,
    mb_mdr = mdr,
    mb_highbact = highbact,
    xr_cavitation_yn = cavitation,
    death_yn = death,
    mfu_yn = mfu,
  )

# Add additional variables
file_name <- "data-raw/1734TuberculosisPati_DATA_2024-09-24_1548 1.csv"
add_vars <- read.csv(file_name) %>%
  filter(is.na(redcap_repeat_instance)) %>%
  dplyr::select(
    record_id,
    redcap_data_access_group,
    redcap_event_name,
    ce_completion_date,
    hiv_cd4_mm3_nr,
    hiv_viral_load_nr,
    hiv_care_status,
    hiv_arv_regimen,
    tbtcurr_change_why___2,
    tbtcurr_isoniazid_end,
    tbtcurr_rifampicin_end,
    tbtcurr_rh_end,
    tbreg_regimen,
    tbreg_pan_regimen,
    tbreg_drug_regimen,
    dem_educ,
    tbh_tb_type,
    xr_penetration,
    xr_result,
    xr_caviation_type,
    xr_miliary_lesions_yn,
    xr_opacity_yn,
    xr_opacity_type,
    xr_opacity_percentage_nr,
    xr_effusion_yn,
    xr_effusion_type,
    xr_pneumothorax_yn,
    xr_pneumothorax_type,
    xr_calcification_yn,
    xr_calcification_type,
    xr_adenopathy_yn,
    xr_adenopathy_type,
    xr_car_silhouette_yn,
    xr_nodules_yn,
    xr_nodules_type,
    xr_size_lesion,
    xr_nodules_proportion,
    xr_timika_nr,
    xr_nodules_evolution,
    matches("xr_oth_findings_"),
    spi_pattern_yn,
    spi_broncho_repo,
    spi_restr_pattern_yn,
    spi_fev1_severity,
    spi_fev1_pre_nr,
    spi_fev1_pre_vcmax_nr,
    spi_fev1_post_nr,
    spi_fev1_post_vcmax_nr,
    ce_sp02_nr,
    ce_resp_rate_nr,
    cpf_mean_attempt_calc
  ) %>%
  dplyr::select(
    -xr_oth_findings___0,
    -xr_oth_findings___88,
  ) %>%
  rename(
    time = redcap_event_name,
    xr_bronchiectasis_yn = xr_oth_findings___1,
    xr_emphysema_yn = xr_oth_findings___2,
    xr_lung_fibrosis_yn = xr_oth_findings___3,
    xr_pulmonary_hypertension_yn = xr_oth_findings___4,
    xr_heart_failure_yn = xr_oth_findings___5,
  ) %>%
  mutate(
    is_start = grepl("baseline", time),
    is_end = grepl("end_of_tx", time),
    is_post = grepl("6m_post_tx", time)
  ) %>%
  filter(is_start | is_end | is_post) %>%
  mutate(
    time = ifelse(is_start, "Start", ifelse(is_end, "End", "Post"))
  ) %>%
  dplyr::select(-is_start, -is_end, -is_post) %>%
  mutate(
    across(c(
      hiv_arv_regimen, tbreg_regimen,
      tbreg_pan_regimen, tbreg_drug_regimen,
      dem_educ, tbh_tb_type
    ), ~ ifelse(time == "Start", .x, NA))
  ) %>%
  mutate(
    hiv_care_status = case_when(
      hiv_care_status == 1 ~ "Newly diagnosed (and not on treatment yet)",
      hiv_care_status == 2 ~ "Patient in care at the same clinic as before",
      hiv_care_status == 3 ~ "Patient died",
      hiv_care_status == 4 ~ "Patient is LTFU",
      hiv_care_status == 5 ~ "Patient in care at another ART program (transferred)",
      hiv_care_status == 6 ~ "Patient is off ART",
      hiv_care_status == 88 ~ "Other"
    ),
    hiv_arv_regimen = case_when(
      hiv_arv_regimen == 1 ~ "TDF + 3TC/FTC + EFV/NVP (NNRTI-based regimen, first-line)",
      hiv_arv_regimen == 2 ~ "AZT + 3TC/FTC+ DTG (NNRTI-based regimen, first-line)",
      hiv_arv_regimen == 3 ~ "TDF1 + AZT + 3TC/FTC + DTG (NNRTI-based regimen, first-line)",
      hiv_arv_regimen == 4 ~ "TDF + 3TC/FTC + DTG (InSTI-based regimen, first-line)",
      hiv_arv_regimen == 5 ~ "AZT + 3TC/FTC + LPV/r (InSTI-based regimen, first-line)",
      hiv_arv_regimen == 6 ~ "TDF + 3TC/FTC + LPV/r (InSTI-based regimen, first-line)",
      hiv_arv_regimen == 7 ~ "AZT/TDF + 3TC/FTC + LPV/r or ATV/r or DTG (second-line regimen)",
      hiv_arv_regimen == 8 ~ "Other"
    ),
    tbreg_regimen = case_when(
      tbreg_regimen == 1 ~ "Pan-susceptible TB treatment regimen (1st line regimen)",
      tbreg_regimen == 2 ~ "Drug resistant TB treatment regimen (2nd line regimen)"
    ),
    tbreg_pan_regimen = case_when(
      tbreg_pan_regimen == 1 ~ "2 H-R-Z-E / 4 H-R",
      tbreg_pan_regimen == 88 ~ "Other",
      tbreg_pan_regimen == 99 ~ "Unknown"
    ),
    tbreg_drug_regimen = case_when(
      tbreg_drug_regimen == 1 ~ "4-6 Am-Mfx-Cfz-Eto-Z-E-Hh/ 5 Mfx-Cfz-Z-E",
      tbreg_drug_regimen == 2 ~ "4-6 Lfx/Mfx-Cfz-Z-E-Hh-Eto/ 5 Lfx/Mfx-Cfz-Z-E",
      tbreg_drug_regimen == 3 ~ "4-6 Cm-Mfx-Cfz-Eto-Z-E- Hh / 5 Mfx-Cfz-E-Z",
      tbreg_drug_regimen == 4 ~ "6 Bdq + 4-6 Lfx/Mfx-Cfz-Z-E-Hh-Eto/ 5 Lfx/Mfx-Cfz- Z-E",
      tbreg_drug_regimen == 5 ~ "6 Bdq + 4-6 Lfx/Mfx-Cfz-Z-E-Hh + 2Lzd / 5 Lfx/Mfx- Cfz-Z-E",
      tbreg_drug_regimen == 6 ~ "6 Bdq + 4-6 Lfx/Mfx-Cfz-Z-E-Hh + 2Lzd / 5 Lfx/Mfx- Cfz-Z-E",
      tbreg_drug_regimen == 88 ~ "Other",
      tbreg_drug_regimen == 99 ~ "Unknown"
    ),
    dem_educ = case_when(
      dem_educ == 0 ~ "None",
      dem_educ == 1 ~ "Primary education",
      dem_educ == 2 ~ "Lower secondary or end of basic education",
      dem_educ == 3 ~ "Upper secondary",
      dem_educ == 4 ~ "Post-secondary non-tertiary (e.g. post-secondary certificate or diploma)",
      dem_educ == 5 ~ "University",
      dem_educ == 6 ~ "Post-graduate",
      dem_educ == 7 ~ "Koranic school",
      dem_educ == 88 ~ "Other",
      dem_educ == 99 ~ "Unknown"
    ),
    tbh_tb_type = case_when(
      tbh_tb_type == 1 ~ "Pulmonary",
      tbh_tb_type == 2 ~ "Extrapulmonary only",
      tbh_tb_type == 3 ~ "Pulmonary and extrapulmonary",
      tbh_tb_type == 99 ~ "Unknown"
    ),
    across(
      matches("xr_.*._yn"),
      ~ case_when(
        .x == 0 ~ "No",
        .x == 1 ~ "Yes",
        .x == 77 ~ "Not possible to determine based on test"
      )
    ),
    across(
      matches("xr_.*._type"),
      ~ case_when(
        .x == 1 ~ "Unilateral",
        .x == 2 ~ "Bilateral"
      )
    ),
    xr_penetration = case_when(
      xr_penetration == 1 ~ "Good (vertebra visible behind heart)",
      xr_penetration == 0 ~ "Not acceptable"
    ),
    xr_result = case_when(
      xr_result == 1 ~ "Normal in both lungs",
      xr_result == 2 ~ "Unilateral abnormality",
      xr_result == 3 ~ "Bilateral abnormality"
    ),
    xr_nodules_proportion = case_when(
      xr_nodules_proportion == 1 ~ "1/6",
      xr_nodules_proportion == 2 ~ "2/6",
      xr_nodules_proportion == 3 ~ "3/6",
      xr_nodules_proportion == 4 ~ "4/6",
      xr_nodules_proportion == 5 ~ "5/6",
      xr_nodules_proportion == 6 ~ "6/6"
    ),
    xr_size_lesion = case_when(
      xr_size_lesion == 1 ~ "<1cm",
      xr_size_lesion == 2 ~ "1-5cm",
      xr_size_lesion == 3 ~ ">5cm"
    ),
    xr_nodules_evolution = case_when(
      xr_nodules_evolution == 1 ~ "Worsened",
      xr_nodules_evolution == 2 ~ "Unchanged",
      xr_nodules_evolution == 3 ~ "Improved",
      xr_nodules_evolution == 4 ~ "Complete resolution of lesions"
    ),
    spi_pattern_yn = case_when(
      spi_pattern_yn == 0 ~ "No",
      spi_pattern_yn == 1 ~ "Yes (FEV1/FVC < LLN)"
    ),
    spi_broncho_repo = case_when(
      spi_broncho_repo == 0 ~ "No change (FVC < 12% & 200ml or FEV1 < 12% & 200ml over baseline)",
      spi_broncho_repo == 1 ~ "Improved (FVC 12% AND 200ml or FEV1 12% AND 200ml over baseline)",
      spi_broncho_repo == 2 ~ "Normalized (FEV1/FVC ratio after bronchodilator normalized)"
    ),
    spi_fev1_severity = case_when(
      spi_fev1_severity == 1 ~ "80%-100%",
      spi_fev1_severity == 2 ~ "50-80%",
      spi_fev1_severity == 3 ~ "30-50%",
      spi_fev1_severity == 4 ~ "< 30%"
    ),
    spi_restr_pattern_yn = case_when(
      spi_restr_pattern_yn == 0 ~ "No",
      spi_restr_pattern_yn == 1 ~ "Yes (FVC< LLN)"
    ),
    across(c(ce_sp02_nr, ce_resp_rate_nr), ~ ifelse(.x == 999, NA, .x))
  ) %>%
  mutate(
    tbtcurr_rh_end = ifelse(
      record_id == "3521-184" & time == "End", "12.07.2014", tbtcurr_rh_end
    ),
    across(matches("tbtcurr.*_end"), ~ ifelse(.x == "", NA, .x)),
    across(matches("tbtcurr.*_end"), ~ as.Date(.x)),
    ce_completion_date = as.Date(ce_completion_date)
  ) %>%
  group_by(record_id) %>%
  mutate(tbt_length = case_when(
    time %in% c("Start", "Post") ~ NA,
    !is.na(tbtcurr_rifampicin_end) ~
      as.numeric(tbtcurr_rifampicin_end - ce_completion_date[time == "Start"]),
    !is.na(tbtcurr_isoniazid_end) ~
      as.numeric(tbtcurr_isoniazid_end - ce_completion_date[time == "Start"]),
    !is.na(tbtcurr_rh_end) ~
      as.numeric(tbtcurr_rh_end - ce_completion_date[time == "Start"])
  )) %>%
  ungroup()

bad_tpt <- add_vars %>%
  filter(
    time == "End",
    !is.na(ce_completion_date),
    is.na(tbt_length) | tbt_length > 210 | tbt_length < 0
  ) %>%
  dplyr::select(
    record_id,
    redcap_data_access_group,
    time,
    ce_completion_date,
    matches("tbt"),
  ) %>%
  rename(ce_completion_date_end = ce_completion_date) %>%
  left_join(
    add_vars %>%
      filter(time == "Start") %>%
      dplyr::select(
        record_id,
        ce_completion_date
      ) %>%
      rename(ce_completion_date_baseline = ce_completion_date),
    by = c("record_id")
  ) %>%
  dplyr::select(
    record_id,
    redcap_data_access_group,
    time,
    ce_completion_date_end,
    ce_completion_date_baseline,
    matches("tbt"),
  )

bad_tpt %>%
  filter(is.na(tbt_length)) %>%
  write.csv("data-check/tbt_length_missing.csv", row.names = FALSE)
bad_tpt %>%
  filter(tbt_length > 210) %>%
  write.csv("data-check/tbt_length_210+.csv", row.names = FALSE)
bad_tpt %>%
  filter(tbt_length < 0) %>%
  write.csv("data-check/tbt_length_negative.csv")

# Merge data
nlm_proj <- qol %>%
  left_join(add_vars, by = c("record_id", "time")) %>%
  rename(time_of_visit = time) %>%
  dplyr::select(
    record_id,
    time_of_visit,
    matches("dem_"),
    matches("hiv_"),
    tbh_tb_type,
    tbd_pat_cat,
    mb_highbact,
    mb_mdr,
    matches("tbreg_"),
    death_yn,
    mfu_yn,
    eot_outcome,
    tbt_length,
    xr_penetration,
    xr_result,
    xr_cavitation_yn,
    xr_caviation_type,
    matches("xr_"),
    matches("spi_"),
    qol_phq9_score,
    qol_sf12_ment,
    qol_sf12_phys,
    qol_smwt_dist,
    qol_stst_nr,
    qol_sgrq_tot_score,
    qol_tb_symp_score,
    qol_phq9_score_bin,
    qol_sf12_ment_bin,
    qol_sf12_phys_bin,
    qol_smwt_dist_bin,
    qol_stst_nr_bin,
    qol_sgrq_tot_score_bin,
    qol_tb_symp_score_bin,
    as_alc_calc,
    as_tobacco_calc,
    ce_sp02_nr,
    ce_resp_rate_nr,
    cpf_mean_attempt_calc
  ) %>%
  mutate_all(
    ~ ifelse(.x == "Unknown", NA, .x)
  ) %>%
  mutate(
    time_of_visit = case_when(
      time_of_visit == "Start" ~ "Baseline",
      time_of_visit == "End" ~ "End of treatment",
      time_of_visit == "Post" ~ "6 months post treatment"
    )
  )

# descriptions
vars <- c(
  "record_id" = "Unique patient identifier",
  "time_of_visit" = "Timepoint of data collection",
  "dem_age" = "Age in years",
  "dem_sex" = "Sex",
  "dem_educ" = "Highest level of education",
  "hiv_status" = "HIV status",
  "hiv_cd4_mm3_nr" = "CD4 count in per mm3 (capped at 2000)",
  "hiv_viral_load_nr" = "Viral load in copies per ml (-1=undetectable)",
  "hiv_care_status" = "HIV care status",
  "hiv_arv_regimen" = "HIV antiretroviral regimen",
  "tbh_tb_type" = "Type of TB",
  "tbd_pat_cat" = "TB patient category",
  "mb_highbact" = "High bacterial load on smear test",
  "mb_mdr" = "MDR-TB status",
  "tbreg_regimen" = "TB regimen",
  "tbreg_pan_regimen" = "Pan-susceptible TB regimen",
  "tbreg_drug_regimen" = "Drug-resistant TB regimen",
  "death_yn" = "Death",
  "mfu_yn" = "Missing follow-up visit",
  "eot_outcome" = "End of treatment outcome",
  "tbt_length" = "Time to treatment completion (days)",
  "xr_penetration" = "X-ray: Penetration of chest x-ray image",
  "xr_result" = "X-ray: Result of chest x-ray",
  "xr_cavitation_yn" = "X-ray: Cavitation on chest x-ray",
  "xr_caviation_type" = "X-ray: Type of cavitation on chest x-ray",
  "xr_miliary_lesions_yn" = "X-ray: Miliary lesions on chest x-ray",
  "xr_opacity_yn" = "X-ray: Alveolar and interstitial opacity (infiltrate) on chest x-ray",
  "xr_opacity_type" = "X-ray: Type of opacity on chest x-ray",
  "xr_opacity_percentage_nr" = "X-ray: Percentage of lung fields affected by any kind of lesion (alveolar or interstitial opacities)",
  "xr_effusion_yn" = "X-ray: Pleural effusion on chest x-ray",
  "xr_effusion_type" = "X-ray: Type of pleural effusion on chest x-ray",
  "xr_pneumothorax_yn" = "X-ray: Pneumothorax on chest x-ray",
  "xr_pneumothorax_type" = "X-ray: Type of pneumothorax on chest x-ray",
  "xr_calcification_yn" = "X-ray: Calcification on chest x-ray",
  "xr_calcification_type" = "X-ray: Type of calcification on chest x-ray",
  "xr_adenopathy_yn" = "X-ray: Mediastinal lymphadenopathy/adenopathy on chest x-ray",
  "xr_adenopathy_type" = "X-ray: Type of mediastinal lymphadenopathy/adenopathy on chest x-ray",
  "xr_car_silhouette_yn" = "X-ray: Enlarged Cardiac Silhouette (>50% of thoracic diameter)",
  "xr_nodules_yn" = "X-ray: Nodules or Masses on chest x-ray",
  "xr_nodules_type" = "X-ray: Type of nodules or masses on chest x-ray",
  "xr_size_lesion" = "X-ray: Size of largest lesion on chest x-ray",
  "xr_nodules_proportion" = "X-ray: Proportion of lung fields affected by nodules or masses",
  "xr_timika_nr" = "X-ray: Timika score (proportion of lungs affected + 40 if cavity)",
  "xr_nodules_evolution" = "X-ray: Evolution of nodules or masses on chest x-ray",
  "xr_bronchiectasis_yn" = "X-ray: Bronchiectasis on chest x-ray",
  "xr_emphysema_yn" = "X-ray: Emphysema on chest x-ray",
  "xr_lung_fibrosis_yn" = "X-ray: Lung fibrosis on chest x-ray",
  "xr_pulmonary_hypertension_yn" = "X-ray: Signs of pulmonary hypertension on chest x-ray",
  "xr_heart_failure_yn" = "X-ray: Signs of heart failure on chest x-ray",
  "xr_oth_findings_oth_txt" = "X-ray: Any other findings on chest x-ray",
  "spi_pattern_yn" = "Spirometry: Obstructive pattern detected",
  "spi_broncho_repo" = "Spirometry: Bronchodilator response",
  "spi_restr_pattern_yn" = "Spirometry: Restrictive pattern detected",
  "spi_fev1_severity" = "Spirometry: FEV1 (severity) % age of predicted value",
  "spi_fev1_pre_nr" = "Spirometry: Pre Bronchodilator FEV1 [Liters]",
  "spi_fev1_pre_vcmax_nr" = "Spirometry: Pre Bronchodilator FEV1 [% VC MAX]",
  "spi_fev1_post_nr" = "Spirometry: Post Bronchodilator FEV1 [Liters]",
  "spi_fev1_post_vcmax_nr" = "Spirometry: Post Bronchodilator FEV1 [% VC MAX]",
  "qol_phq9_score" = "Quality of Life: PHQ-9 depression score",
  "qol_sf12_ment" = "Quality of Life: SF-12 mental component health score",
  "qol_sf12_phys" = "Quality of Life: SF-12 physical health component score",
  "qol_smwt_dist" = "Quality of Life: Six-minute walk test distance",
  "qol_stst_nr" = "Quality of Life: Sit-and-stand test number of repetitions",
  "qol_sgrq_tot_score" = "Quality of Life: St. George's Respiratory Questionnaire total score",
  "qol_tb_symp_score" = "Quality of Life: TB symptom score",
  "qol_phq9_score_bin" = "Quality of Life: PHQ-9 depression score impairment (score >10)",
  "qol_sf12_ment_bin" = "Quality of Life: SF-12 mental component health score impairment (score <42)",
  "qol_sf12_phys_bin" = "Quality of Life: SF-12 physical health component score impairment (score <50)",
  "qol_smwt_dist_bin" = "Quality of Life: Six-minute walk test distance impairment (distance <400m)",
  "qol_stst_nr_bin" = "Quality of Life: Sit-and-stand test number of repetitions impairment (repititons <20)",
  "qol_sgrq_tot_score_bin" = "Quality of Life: St. George's Respiratory Questionnaire total score impairment (score >25)",
  "qol_tb_symp_score_bin" = "Quality of Life: TB symptom score impairment (score >4)",
  "as_alc_calc" = "ASSIST questionnaire: Alcohol consumption score",
  "as_tobacco_calc" = "ASSIST questionnaire: Tobacco consumption score",
  "ce_sp02_nr" = "Resting peripheral capillary oxygen saturation (SpO2 in %)",
  "ce_resp_rate_nr" = "Respiratory rate [breaths/min]",
  "cpf_mean_attempt_calc" = "Cough peak flow: mean of three attemps"
)

# Save data
library(openxlsx)

# Create a data frame for the Description sheet
description <- data.frame(
  `Variable` = names(vars),
  `Description` = vars
)

# Create a new workbook
wb <- createWorkbook()

# Add the Description sheet
addWorksheet(wb, "Description")
writeData(wb, "Description", description)

# Add the nlm_proj sheet
addWorksheet(wb, "Data")
writeData(wb, "Data", nlm_proj)

# Save the workbook to an Excel file
saveWorkbook(wb, "data-clean/IeDEA-Cohort-Data.xlsx", overwrite = TRUE)
