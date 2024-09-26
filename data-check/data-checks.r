#### Libraries ####

library(tidyverse)


#### Data ####

# raw data
file_name <- "data-raw/1734TuberculosisPati_DATA_2024-09-24_1548 1.csv"
df <- read.csv(file_name)

# prep data
df_clean <- readRDS("data-clean/phys-ment-data.rds")

#### QoL outcomes ####
df_clean %>%
  filter(
    is.na(phq9_score),
    is.na(sf12_ment),
    is.na(sf12_phys),
    is.na(smwt_dist),
    is.na(stst_nr),
    is.na(sgrq_tot_score),
    !mfu, !death
  ) %>%
  dplyr::select(
    record_id,
    time,
    site,
    date_visit,
    phq9_score, sf12_ment,
    sf12_phys, smwt_dist, stst_nr,
    sgrq_tot_score
  ) %>%
  write.csv("data-check/all-outcomes-missing.csv", row.names = FALSE)

#### Mental health data ####
df %>%
  dplyr::select(
    record_id,
    redcap_event_name,
    redcap_data_access_group,
    ment_trt_yn,
    ment_healer_yn,
    ment_inpatient_yn,
    ment_outpatient_yn,
    ment_med_yn
  ) %>%
  mutate(across(c(matches("ment")), as.numeric)) %>%
  mutate(ment_trt_yn2 = ifelse(
    ment_healer_yn == 1 | ment_inpatient_yn == 1 | ment_outpatient_yn == 1 |
      ment_med_yn == 1,
    1, ifelse(
      ment_healer_yn == 99 &
        ment_inpatient_yn == 99 &
        ment_outpatient_yn == 99 &
        ment_med_yn == 99, 99, 0
    )
  )) %>%
  filter(ment_trt_yn != ment_trt_yn2) %>%
  write.csv(
    "data-check/mental-health-treatment-inconsistencies.csv",
    row.names = FALSE
  )

df_clean %>%
  group_by(record_id) %>%
  filter(any(ment_trt_yn == 1)) %>%
  ungroup() %>%
  dplyr::select(
    record_id,
    site,
    time,
    date_visit,
    ment_trt_yn
  ) %>%
  arrange(record_id, time) %>%
  write.csv("data-check/any-mental-health-treatment.csv", row.names = FALSE)

#### Adverse event data check ####
adv_events <- df %>%
  filter(
    grepl("adverse_event", redcap_event_name)
  ) %>%
  dplyr::select(
    record_id,
    starts_with("imp_")
  )

write.csv(adv_events, "data-check/adverse-events.csv", row.names = FALSE)

df_clean %>%
  mutate(across(c(date_visit, nf_ae_date), as.Date)) %>%
  group_by(record_id) %>%
  filter(any(nf_ae == 1)) %>%
  arrange(date_visit) %>%
  mutate(nf_ae_before = ifelse(
    nf_ae_date <= date_visit, TRUE, FALSE
  )) %>%
  filter(nf_ae_before) %>%
  ungroup() %>%
  filter(time == "Start") %>%
  dplyr::select(
    record_id,
    site,
    time,
    date_visit,
    nf_ae,
    nf_ae_date
  ) %>%
  write.csv("data-check/adverse-events-before-visit.csv", row.names = FALSE)

#### Height #####
df %>%
  dplyr::select(
    record_id,
    redcap_event_name,
    redcap_repeat_instance,
    redcap_data_access_group,
    ce_height_nr
  ) %>%
  filter(ce_height_nr < 99) %>%
  write.csv("data-check/height-in-m.csv", row.names = FALSE)


#### Deaths ####

df %>%
  dplyr::select(
    record_id,
    redcap_event_name,
    redcap_data_access_group,
    death_yn,
    eot_outcome
  ) %>%
  group_by(record_id) %>%
  filter(any(death_yn == 1)) %>%
  ungroup() %>%
  arrange(record_id) %>%
  write.csv(
    "data-check/death-records.csv",
    row.names = FALSE
  )


#### Treatment outcome ####
trt_out <- df_clean %>%
  filter(time == "End") %>%
  filter(!mfu) %>%
  mutate(eot_outcome = case_when(
    eot_outcome == 1 ~ "Cured",
    eot_outcome == 2 ~ "Treatment completed",
    eot_outcome == 3 ~ "Treatment failed",
    eot_outcome == 4 ~ "Died",
    eot_outcome == 5 ~ "Lost to follow-up",
    eot_outcome == 6 ~ "Transferred out from study site",
    eot_outcome == 99 ~ "Not known",
    is.na(eot_outcome) ~ "Not available"
  )) %>%
  mutate(eot_outcome = factor(eot_outcome, levels = c(
    "Cured",
    "Treatment completed",
    "Treatment failed",
    "Died",
    "Lost to follow-up",
    "Transferred out from study site",
    "Not known",
    "Not available"
  )))

trt_out %>%
  group_by(eot_outcome) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  mutate(perc = paste0(round(freq / sum(freq) * 100), "%"))

trt_out %>%
  group_by(site) %>%
  summarise(sum(eot_outcome == "Not available"))

trt_out %>%
  filter(
    eot_outcome == "Not available",
    !death, !mfu
  ) %>%
  dplyr::select(record_id, site, time, eot_outcome) %>%
  write.csv("data-check/missing-treatment-outcome.csv", row.names = FALSE)

trt_out %>%
  filter(
    death,
    eot_outcome != "Died"
  ) %>%
  dplyr::select(record_id, time, date_visit, site, death, eot_outcome) %>%
  write.csv("data-check/deaths-not-in-eot_outcome.csv", row.names = FALSE)

trt_out %>%
  filter(
    !death,
    eot_outcome == "Died"
  ) %>%
  dplyr::select(record_id, time, date_visit, site, death, eot_outcome) %>%
  write.csv("data-check/died-in-eot_outcome-but-not-deaths.csv", row.names = FALSE)

df_clean %>%
  filter(time %in% c("Start", "End")) %>%
  group_by(time, site) %>%
  summarise(across(
    c(sf12_phys, sf12_ment),
    .fns = list(
      mean = function(x) mean(x, na.rm = TRUE),
      sd = function(x) sd(x, na.rm = TRUE)
    )
  )) %>%
  write.csv("data-check/sf12-by-country.csv", row.names = TRUE)
