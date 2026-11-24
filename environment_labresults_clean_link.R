##############################################################
# 3GCR-E source attribution - CLEAN LAB RESULTS AND LINK    #
# to stool collection, household WASH and 2022-24 carriage: #
# - environmental samples: presence & quantif esble & e.coli#
# - stool collection records (ALARUM community REDCap)      #
# - household WASH questionnaire (ALARUM community REDCap)  #
# - ESBL E. coli carriage 2022-24 (CABU-EICO)               #
#############################################################

# install/load packages
pacman::p_load(tidyverse, lubridate, readxl)

# IMPORT DATA----- ------------------------------------------
# first make sure you have a folder named 'data' in your project folder (called 3GCREsourceattribution) that contains all the below data files
quantificationlabresults <- read_csv("data/ALARUMSourceAttribut_DATA_2026-11-18_1605.csv", col_types = cols(.default = "c"))
alarumredcap <- read_excel("data/ALARUM-Community-DATA_FSH.xlsx", sheet = "Sheet1", col_types = "text")
stoolcollection <- alarumredcap %>% filter(redcap_repeat_instrument=="collecte_de_selles")
hhdata <- alarumredcap %>% filter(is.na(redcap_repeat_instrument)) # the general HH data is those without redcap_repeat_instrument value
carriage2022 <- read_csv("data/Household_stool_WASH_BF_long.csv", col_types = cols(.default = "c"), name_repair = "unique_quiet") #CABU carriage + WASH data

names(quantificationlabresults) <- c(
  "record_id", "id", "date_prelev", "heure_prelev", "date_recep_lab",
  "heure_recep_lab", "date_deb_trait", "heur_deb_trait", "filtration",
  "filt_volume", "med_mcc", "med_mcc_ctx", "med_tbx", "med_tbx_ctx",
  "dilution", "dilut_titre", "volum_ensem",
  "nbre_colo_tbx", "nbre_colo_tbx_ctx", "nbre_colo_mcc", "nbre_colo_mcc_ctx",
  "nbre_e_coli_volum", "nbre_blse_volum",
  "date_saisie", "initiales", "signature", "date_verif", "initial_verif",
  "signat_verif", "complete") # based on redcap var names

# remove row with missing ID and date
quantificationlabresults <- quantificationlabresults %>% filter(!(id %in% "B08-081"))

# CLEAN ID, SAMPLE TYPE AND SEASON-----------------------------
# use record_id (holds the corrected codes), make sure there's always a "-" between the first 5-digit number and the first letter
quantificationlabresults <- quantificationlabresults |> mutate(
    id_clean = str_to_upper(str_squish(record_id)),
    id_clean = str_replace(id_clean, "^(\\d\\d)0(0\\d\\d)(?=\\D)", "\\1\\2"),   # 250006 -> 25006
    id_clean = str_replace(id_clean, "(\\d{5})[\\s-]*(?=[A-Z])", "\\1-"),
    household = str_extract(id_clean, "^\\d{5}"))
# check: IDs where id and record_id point to a different household
quantificationlabresults |> filter(str_extract(str_remove_all(id, "\\s"), "\\d{5,6}") != household) |>
  select(record_id, id, date_prelev) |> write_csv("check_id_vs_record_id.csv")

# deduct sample type from ID (letters/numbers after the first "-" in the ID)
quantificationlabresults <- quantificationlabresults |>
  mutate(type_code = id_clean |> str_remove("^[^-]*-") |> str_remove("[DR][^A-Z]*$") |> str_remove_all("[^A-Z0-9]"),
    sample_type = case_when(type_code %in% c("HWA", "HDA", "KWA") ~ "handrinse_adult",
      type_code %in% c("HWP", "GHWP")            ~ "handrinse_underfive",
      type_code == "W"                           ~ "drinkingwater",
      type_code %in% c("2L", "CL")               ~ "latrine",
      type_code %in% c("1L", "IL", "1LR23004AF") ~ "opendefecation",
      type_code == "C"                           ~ "chickenfaeces",
      type_code %in% c("AF", "AF2", "AFP")       ~ "animalfaeces",
      type_code == "F"                           ~ "food",
      type_code == "S"                           ~ "soil"))          # anything else stays NA
table(quantificationlabresults$sample_type, useNA = "always")

# dates; replace implausible collection dates (e.g. 0426-03-01, anything before 2025-08-01) by the date of reception at the laboratory
quantificationlabresults <- quantificationlabresults |> mutate(
    across(c(date_prelev, date_recep_lab, date_deb_trait, date_saisie, date_verif), ymd),
    date_prelev = if_else(date_prelev < ymd("2025-08-01") | is.na(date_prelev), date_recep_lab, date_prelev))

# deduct season from the last letter of the ID: D = dry, R = rainy; if missing, from the date
quantificationlabresults <- quantificationlabresults |> mutate(
    last_letter = str_extract(id_clean, "[A-Z](?=[^A-Z]*$)"),
    season = case_when(last_letter == "D" ~ "dry",
                       last_letter == "R" ~ "rainy",
                       date_prelev > ymd("2026-03-10") & date_prelev < ymd("2026-06-01") ~ "dry",
                       month(date_prelev) %in% 6:10 ~ "rainy"),
    period = case_when(season == "dry" ~ "dry 2026",
                       year(date_prelev) == 2025 ~ "rainy 2025",
                       season == "rainy" ~ "rainy 2026"))
quantificationlabresults %>% group_by(season, period) |> summarise(n = n(),
    first_date = min(date_prelev, na.rm = TRUE), last_date = max(date_prelev, na.rm = TRUE))

# VOLUMES--------------------------------------------------------
# drinking water: filter plated, volume in 'Si oui preciser le volume' (100 mL if missing; all water reported per 100 mL, so all treated as filtered)
# others: diluted sample plated, volume in volum_ensem (uL; 100 uL if missing or 0)
quantificationlabresults <- quantificationlabresults |> mutate(
    filt_vol_mL = parse_number(filt_volume),
    plate_uL    = parse_number(volum_ensem),
    plate_uL    = if_else(is.na(plate_uL) | plate_uL == 0, 100, plate_uL),
    vol_mL      = if_else(sample_type %in% "drinkingwater", coalesce(filt_vol_mL, 100), plate_uL / 1000))

# extract lists with potential problems
# (a) Filtration = 'Oui' although the sample is not drinking water
quantificationlabresults |> filter(filtration %in% "1", !(sample_type %in% "drinkingwater")) |>
  select(id_clean, sample_type, date_prelev, filtration, filt_volume, volum_ensem) |>
  write_csv("list_filtration_not_drinkingwater.csv")
# (b) TBX used although the sample is neither drinking water nor a hand rinse
quantificationlabresults |> filter(med_tbx %in% "1", !(sample_type %in% c("drinkingwater", "handrinse_adult", "handrinse_underfive"))) |>
  select(id_clean, sample_type, date_prelev, date_recep_lab, date_deb_trait, dilut_titre) |>
  write_csv("list_tbx_used_non_water_non_handrinse.csv")

# DILUTIONS -> TITERS AS 10^x---------------------------------
# 'Si oui preciser les titres' is free text, e.g. "1/100 MC+CTX - 1/10000 MC"   "1/10 TBX ET TBX+CTX"   "1/1000"
# a dilution applies to the media named after it, a lone dilution applies to all media
# a medium still without dilution takes that of the equivalent medium (MC <-> TBX, MC+CTX <-> TBX+CTX), or else the only
# loose dilution; if still missing, ESBL plates take the lowest and other plates the highest dilution (marked in titer_inferred)
get_dilutions <- function(text) {
  out <- c(MC = NA_real_, MCC = NA_real_, TB = NA_real_, TBC = NA_real_)
  if (is.na(text)) return(c(out, inferred = NA))
  z <- text |> str_to_upper() |> str_remove_all("\\s+") |> str_replace_all("TBX", "TB") |> str_replace_all("ET", "|") |>
    str_replace_all("\\+CTX", "C") |> str_replace_all("\\+CT", "C") |> str_replace_all("1/0", "1/10")  # typo 1/00000
  pieces <- str_extract_all(z, "1/\\d+|\\d+|MCC|MC|TBC|TB")[[1]]
  current <- NA_real_; all_dil <- c(); used_dil <- c(); inferred <- ""
  for (p in pieces) {
    if (str_detect(p, "^[0-9/]+$")) { current <- as.numeric(str_remove(p, "^1/")); all_dil <- c(all_dil, current)
    } else { out[p] <- current; used_dil <- c(used_dil, current) }
  }
  if (length(used_dil) == 0) { out[] <- current } else {
    twin  <- c(MC = "TB", TB = "MC", MCC = "TBC", TBC = "MCC")
    loose <- setdiff(all_dil, used_dil)
    for (m in names(out)) if (is.na(out[[m]])) {
      if (!is.na(out[[twin[[m]]]]))            out[[m]] <- out[[twin[[m]]]]
      else if (length(loose) == 1)             out[[m]] <- loose
      else if (length(unique(all_dil)) == 1)   out[[m]] <- all_dil[1]
      if (!is.na(out[[m]])) inferred <- paste(inferred, m)
    }
  }
  if (length(all_dil) > 0) {
    for (m in c("MCC", "TBC")) if (is.na(out[[m]])) { out[[m]] <- min(all_dil); inferred <- paste(inferred, m) }
    for (m in c("MC", "TB"))   if (is.na(out[[m]])) { out[[m]] <- max(all_dil); inferred <- paste(inferred, m) }
  }
  c(out, inferred = str_squish(inferred))
}
dil_table <- do.call(rbind, lapply(if_else(quantificationlabresults$dilution %in% "1", quantificationlabresults$dilut_titre, NA), get_dilutions)) |> as_tibble()

quantificationlabresults <- quantificationlabresults |> mutate(
    titer_inferred = na_if(dil_table$inferred, ""),
    # titer as exponent x of 10^x (1/100 -> -2); no dilution = 10^0
    titer_mc      = if_else(dilution %in% "1", -log10(as.numeric(dil_table$MC)), 0),
    titer_tbx     = if_else(dilution %in% "1", -log10(as.numeric(dil_table$TB)), 0),
    titer_mc_ctx  = if_else(dilution %in% "1", -log10(as.numeric(dil_table$MCC)), 0),
    titer_tbx_ctx = if_else(dilution %in% "1", -log10(as.numeric(dil_table$TBC)), 0))
quantificationlabresults %>% filter(sample_type == "drinkingwater" & !is.na(dilut_titre)) # none, as it should be

# COLONY COUNTS AND CONCENTRATIONS----------------------------
# counts like ">200 UFC" -> 200 + censored flag (">20000" typed for ">200"); "OO" typed for "00"
num  <- function(x) parse_number(str_replace(x, "^\\s*OO", "00"))
cens <- function(x) coalesce(str_detect(x, "^\\s*>"), FALSE)
quantificationlabresults <- quantificationlabresults |> mutate(
    # medium read = the one with a count (TBX or MacConkey)
    ec_medium   = if_else(!is.na(nbre_colo_tbx), "TBX", "MC"),
    esbl_medium = if_else(!is.na(nbre_colo_tbx_ctx), "TBX+CTX", "MC+CTX"),
    ec_count    = if_else(ec_medium == "TBX", num(nbre_colo_tbx), num(nbre_colo_mcc)),
    ec_cens     = if_else(ec_medium == "TBX", cens(nbre_colo_tbx), cens(nbre_colo_mcc)),
    ec_titer    = if_else(ec_medium == "TBX", titer_tbx, titer_mc),
    esbl_count  = if_else(esbl_medium == "TBX+CTX", num(nbre_colo_tbx_ctx), num(nbre_colo_mcc_ctx)),
    esbl_cens   = if_else(esbl_medium == "TBX+CTX", cens(nbre_colo_tbx_ctx), cens(nbre_colo_mcc_ctx)),
    esbl_titer  = if_else(esbl_medium == "TBX+CTX", titer_tbx_ctx, titer_mc_ctx),
    ec_count    = if_else(ec_cens & ec_count > 200, 200, ec_count),
    esbl_count  = if_else(esbl_cens & esbl_count > 200, 200, esbl_count))
# check: medium with a count differs from medium ticked in the form
quantificationlabresults |> filter((ec_medium == "TBX") != (med_tbx %in% "1") | (esbl_medium == "TBX+CTX") != (med_tbx_ctx %in% "1")) |>
  select(id_clean, sample_type, med_tbx, med_mcc, med_tbx_ctx, med_mcc_ctx, nbre_colo_tbx, nbre_colo_mcc, nbre_colo_tbx_ctx, nbre_colo_mcc_ctx)

# lab-calculated concentrations, e.g. ">20000000 UFC / ML" -> 20000000 + censored flag + unit the lab used
quantificationlabresults <- quantificationlabresults |> mutate(
    lab_ec = num(nbre_e_coli_volum), lab_ec_cens = cens(nbre_e_coli_volum),
    lab_es = num(nbre_blse_volum),   lab_es_cens = cens(nbre_blse_volum),
    lab_text = str_to_upper(str_remove_all(paste(nbre_e_coli_volum, nbre_blse_volum), "\\s")),
    lab_unit = case_when(str_detect(lab_text, "100ML") ~ "per_100mL",
                         str_detect(lab_text, "ECOU|ECL") ~ "per_swab",
                         str_detect(lab_text, "/G")   ~ "per_g",
                         str_detect(lab_text, "/ML")  ~ "per_mL"))
# one by one checks
quantificationlabresults$lab_unit[quantificationlabresults$id_clean=="28003-C-R"] <- "per_swab"
quantificationlabresults$lab_unit[quantificationlabresults$id_clean=="24017-AF-R"] <- "per_mL"
quantificationlabresults$lab_unit[quantificationlabresults$id_clean=="26018-W-R"] <- "per_100mL"
quantificationlabresults$lab_unit[is.na(quantificationlabresults$lab_unit)&quantificationlabresults$sample_type %in% c("handrinse_adult", "handrinse_underfive")] <- "per_mL" # is 0, hence no unit
quantificationlabresults$lab_unit[is.na(quantificationlabresults$lab_unit)&quantificationlabresults$sample_type=="soil"] <- "per_g" # is 0, hence no unit
quantificationlabresults |> count(sample_type, lab_unit)

# recalculate concentrations = colony count / (volume in mL x 10^titer)
# expected_: in the lab's unit, to compare with the lab value
# ec_conc/esbl_conc: in one unit per sample type (water per 100 mL, chicken faeces per swab, soil per g, others per mL), for analyses
quantificationlabresults <- quantificationlabresults |> mutate(
    expected_ecoli = ec_count   / (vol_mL * 10^ec_titer)   * if_else(lab_unit %in% "per_100mL", 100, 1),
    expected_esbl  = esbl_count / (vol_mL * 10^esbl_titer) * if_else(lab_unit %in% "per_100mL", 100, 1),
    unit      = case_when(sample_type == "drinkingwater" ~ "CFU/100 mL", sample_type == "chickenfaeces" ~ "CFU/swab",
                          sample_type == "soil" ~ "CFU/g", TRUE ~ "CFU/mL"),
    unit_mult = if_else(sample_type %in% "drinkingwater", 100, 1),
    ec_conc   = ec_count   / (vol_mL * 10^ec_titer)   * unit_mult,
    esbl_conc = esbl_count / (vol_mL * 10^esbl_titer) * unit_mult,
    ec_lod    = 1 / (vol_mL * 10^ec_titer)   * unit_mult,      # limit of detection = 1 colony
    esbl_lod  = 1 / (vol_mL * 10^esbl_titer) * unit_mult)

# compare lab value with recalculated value: match if within 1%
compare <- function(lab, lab_cens, exp, exp_cens) case_when(
  is.na(lab) & is.na(exp) ~ "both missing", is.na(lab) ~ "lab value missing", is.na(exp) ~ "cannot recompute",
  lab == 0 & exp == 0 ~ "match", lab_cens != exp_cens | lab == 0 | exp == 0 ~ "DISCREPANCY",
  abs(lab / exp - 1) <= 0.01 ~ "match", TRUE ~ "DISCREPANCY")
quantificationlabresults <- quantificationlabresults |> mutate(
    ec_status = compare(lab_ec, lab_ec_cens, expected_ecoli, ec_cens),
    es_status = compare(lab_es, lab_es_cens, expected_esbl, esbl_cens),
    esbl_above_ecoli = !lab_ec_cens & !lab_es_cens & lab_es > lab_ec * 1.01)   # impossible: more ESBL E. coli than E. coli
table(quantificationlabresults$ec_status); table(quantificationlabresults$es_status)
quantificationlabresults |> filter(ec_status == "DISCREPANCY" | es_status == "DISCREPANCY" | esbl_above_ecoli) |>
  select(record_id, id_clean, sample_type, season, date_prelev, filt_volume, volum_ensem, dilut_titre,
         nbre_colo_tbx, nbre_colo_mcc, nbre_colo_tbx_ctx, nbre_colo_mcc_ctx,
         nbre_e_coli_volum, expected_ecoli, ec_status, nbre_blse_volum, expected_esbl, es_status) |>
  write_csv("flagged_discrepancies.csv")

# PRESENCE AND QUANTITY----------------------------------------
# E. coli present = growth on non-selective medium or ESBL E. coli; E. coli concentration at least the ESBL concentration
quantificationlabresults <- quantificationlabresults |> mutate(
    esbl_pos   = esbl_count > 0,
    ec_pos     = if_else(is.na(ec_count) & is.na(esbl_count), NA, ec_count > 0 | coalesce(esbl_pos, FALSE)),
    ec_conc    = if_else(ec_pos %in% TRUE, pmax(ec_conc, coalesce(esbl_conc, 0), na.rm = TRUE), ec_conc),
    ec_log10   = if_else(ec_pos %in% TRUE, log10(ec_conc), NA),
    esbl_log10 = if_else(esbl_pos %in% TRUE, log10(esbl_conc), NA))

# keep one sample per household x sample type x season (earliest with a result; repeat visits, double entries, AF2)
quantificationlabresults <- quantificationlabresults |>
  arrange(household, sample_type, season, is.na(ec_pos), date_prelev, str_detect(id_clean, "AF2"), id_clean) |>
  mutate(excluded = case_when(is.na(household) | is.na(sample_type) | is.na(season) ~ "household, sample type or season unknown",
                              is.na(ec_pos) ~ "no colony count",
                              duplicated(paste(household, sample_type, season)) ~ "repeat sample (earlier sample kept)"))
quantificationlabresults |> filter(!is.na(excluded)) |> select(record_id, id_clean, sample_type, season, date_prelev, excluded) |>
  write_csv("samples_excluded.csv")

# export cleaned database
quantificationlabresults_clean <- quantificationlabresults |> filter(is.na(excluded)) |>
  select(record_id, id = id_clean, household, sample_type, season, period, date_prelev, date_recep_lab, date_deb_trait,
         filtration, vol_mL, ec_medium, esbl_medium, ec_titer, esbl_titer, titer_inferred,
         ec_count, ec_cens, esbl_count, esbl_cens,
         lab_unit, lab_ec, lab_es, expected_ecoli, expected_esbl, ec_status, es_status,
         unit, ec_pos, ec_conc, ec_log10, ec_lod, esbl_pos, esbl_conc, esbl_log10, esbl_lod)
write_csv(quantificationlabresults_clean, "lab_environment_clean.csv", na = "")

# HOUSEHOLD WASH DATA-------------------------------------------
# one questionnaire per household per season; coded answers without labels yet are kept as _code
cb <- function(x) as.integer(x %in% "1")    # REDCap checkbox -> 0/1
hhwash <- hhdata |> filter(!is.na(redcap_event_name), is.na(redcap_repeat_instrument)) |>
  transmute(household = str_extract(record_id, "^\\d{5}"),
    season = if_else(str_detect(redcap_event_name, "^dry"), "dry", "rainy"),
    village_code = Code_Village, village_name = str_to_title(Village),
    hh_date_survey = ymd(date_survey_hh),
    hh_members = as.numeric(num_hh_members_hh), hh_members_present = as.numeric(num_memb_pres_hh),
    hh_children_u5 = as.numeric(num_children_0_5yrs_hh), hh_households_compound = as.numeric(num_households_hh),
    # drinking water
    water_source = factor(main_drink_water_srce_hh, levels = 1:8,
      labels = c("tap in household", "public tap/fountain/kiosk", "borehole", "protected well",
                 "unprotected well", "rainwater", "surface water", "water vendor (sachet/bottle)")),
    water_source_improved = if_else(water_source %in% c("unprotected well", "surface water"), 0L, 1L),
    water_source_improved = if_else(is.na(water_source), NA, water_source_improved),
    water_distance_code = distance_srce_hh,
    water_container_uncovered = cb(storage_container_hh___0), water_container_covered = cb(storage_container_hh___1),
    water_container_dirty = cb(storage_container_hh___3),
    water_treatment = case_when(water_treat_hh___1 == "1" ~ "boil", water_treat_hh___2 == "1" ~ "chlorinate/disinfect",
                                water_treat_hh___3 == "1" ~ "filter with cloth", water_treat_hh___4 == "1" ~ "filter (sand/ceramic)",
                                water_treat_hh___5 == "1" ~ "solar disinfection", water_treat_hh___6 == "1" ~ "let settle",
                                water_treat_hh___88 == "1" ~ "other", water_treat_hh_2___0 == "1" ~ "none"),
    water_treated = as.integer(water_treatment != "none"),
    # sanitation
    toilet_type_0_code = cb(toilet_type_hh___0), toilet_type_6_code = cb(toilet_type_hh___6), toilet_type_7_code = cb(toilet_type_hh___7),
    toilet_share_code = toilet_share_hh, toilet_clean = as.integer(toilet_clean_hh),
    child_defecate_1_code = cb(child_defecate_hh___1), child_defecate_2_code = cb(child_defecate_hh___2),
    child_defecate_5_code = cb(child_defecate_hh___5), child_defecate_6_code = cb(child_defecate_hh___6),
    child_faeces_dispose_1_code = cb(child_feaces_dispose_hh___1), child_faeces_dispose_3_code = cb(child_feaces_dispose_hh___3),
    child_faeces_dispose_4_code = cb(child_feaces_dispose_hh___4),
    # animals
    animal_chicken = cb(animal1_hh___1), animal_duck = cb(animal2_hh___1), animal_pig = cb(animal3_hh___1),
    animal_cattle = cb(animal4_hh___1), animal_goat = cb(animal5_hh___1), animal_sheep = cb(animal6_hh___1),
    animal_horse = cb(animal7_hh___1), animal_dog = cb(animal8_hh___1), animal_cat = cb(animal9_hh___1),
    animal_other = cb(animal_kept_other_hh___1), animal_excrement_code = animal_excrement_hh) |>
  mutate(n_animal_species = rowSums(across(animal_chicken:animal_other)))
# check village per first 2 digits of the household number: 21 EC Boulpon, 22 SB Gouloure, 23 SG Kalwaka, 24 SJ Mogdin,
# 25 SM Rakalo, 26 EF Seguedin, 27 EG Soala, 28 SN Soaw (same in the 2022-24 data)
hhwash %>% count(village = str_sub(household, 1, 2), village_code, village_name)

# link to 2022-24 household (hm_number_cse = menage_id; still empty in this export)
link2022 <- hhdata |> filter(!is.na(hm_number_cse)) |>
  transmute(household = str_extract(record_id, "^\\d{5}"), menage_id = str_remove(str_to_upper(str_squish(hm_number_cse)), "-\\d+$")) |>
  distinct() |> group_by(household) |> summarise(menage_id = paste(sort(unique(menage_id)), collapse = ";"))

# STOOL COLLECTION----------------------------------------------
stool <- stoolcollection |> filter(redcap_repeat_instrument %in% "collecte_de_selles") |>
  mutate(household = str_extract(record_id, "^\\d{5}"),
         season = if_else(str_detect(redcap_event_name, "^dry"), "dry", "rainy"),
         stool_id = str_to_upper(str_remove_all(dss_number_stool, "\\s")),
         date_collect = coalesce(ymd(date_collect_stool), ymd(date_stool_passing), ymd(date_stool))) |>
  filter(!(duplicated(paste(stool_id, season)) & !is.na(stool_id)))   # duplicate person-season record (27011-1-H-R)
stool_hh <- stool |> group_by(household, season) |>
  summarise(n_stool_collected = n(), n_stool_no_consent = sum(!(hh_member_consent_stool %in% "1")),
            stool_date_first = min(date_collect, na.rm = TRUE), stool_date_last = max(date_collect, na.rm = TRUE), .groups = "drop")

# PREVIOUS STUDY 2022-24 CARRIAGE-------------------------------
# season from the stool collection date (else storage, then survey date): rainy = June-November
carriage2022 <- carriage2022 |>
  mutate(menage_id = str_to_upper(str_squish(menage_id)),
         date_stool = coalesce(as.Date(date.stool.collection), as.Date(date), as.Date(date.use)),
         season = if_else(month(date_stool) %in% 6:11, "rainy", "dry"),
         esble = as.integer(esble))
table(carriage2022$time, carriage2022$season)   # rounds 0 and 3 (Oct-Feb) span both seasons

carriage2022_hh <- carriage2022 |> group_by(menage_id) |>
  summarise(prev_hh_n_persons = n_distinct(menage_id_member), prev_hh_n_samples = n(), prev_hh_n_esble = sum(esble),
            prev_hh_n_samples_dry = sum(season == "dry"), prev_hh_n_esble_dry = sum(esble[season == "dry"]),
            prev_hh_n_samples_rainy = sum(season == "rainy"), prev_hh_n_esble_rainy = sum(esble[season == "rainy"]))
carriage2022_village <- carriage2022 |> group_by(village_code = village, season) |>
  summarise(prev_vil_n_households = n_distinct(menage_id), prev_vil_n_samples = n(), prev_vil_n_esble = sum(esble),
            prev_vil_prev_esble = mean(esble), .groups = "drop")

# LINK: ONE ROW PER HOUSEHOLD PER SEASON------------------------
# environment per household-season, and per sample type in wide format:
# _pos (NA = not sampled), _conc (0 if not detected), _log10 (detected only), _cens (>200 colonies, conc is a lower bound)
env_hh <- quantificationlabresults_clean |> group_by(household, season) |>
  summarise(env_date_first = min(date_prelev), env_date_last = max(date_prelev), env_period = first(period),
            env_n_samples = n(), env_n_ecoli_pos = sum(ec_pos), env_n_esbl_pos = sum(esbl_pos, na.rm = TRUE),
            env_types_sampled = paste(sort(unique(sample_type)), collapse = ";"), .groups = "drop")
env_wide <- quantificationlabresults_clean |>
  transmute(household, season, sample_type,
            ecoli_pos = ec_pos, ecoli_conc = if_else(ec_pos, ec_conc, 0), ecoli_log10 = ec_log10, ecoli_cens = ec_cens & ec_pos,
            esbl_pos = esbl_pos, esbl_conc = if_else(esbl_pos, esbl_conc, 0), esbl_log10 = esbl_log10, esbl_cens = esbl_cens & esbl_pos) |>
  pivot_wider(names_from = sample_type, values_from = ecoli_pos:esbl_cens, names_glue = "{.value}_{sample_type}")

household_season <- bind_rows(select(hhwash, household, season), select(env_hh, household, season), select(stool_hh, household, season)) |>
  distinct() |> complete(household, season) |>
  left_join(hhwash, by = c("household", "season")) |>
  left_join(stool_hh, by = c("household", "season")) |>
  left_join(env_hh, by = c("household", "season")) |>
  left_join(env_wide, by = c("household", "season")) |>
  left_join(link2022, by = "household") |>
  left_join(carriage2022_hh, by = "menage_id", na_matches = "never") |>
  mutate(village = str_sub(household, 1, 2),
         prev_hh_n_samples_season = if_else(season == "dry", prev_hh_n_samples_dry, prev_hh_n_samples_rainy),
         prev_hh_n_esble_season   = if_else(season == "dry", prev_hh_n_esble_dry, prev_hh_n_esble_rainy),
         days_stool_to_env = as.numeric(env_date_first - stool_date_first)) |>
  left_join(carriage2022_village, by = c("village_code", "season"))
nrow(household_season)  # 120 households x 2 seasons

# FLAGS---------------------------------------------------------
# expected env samples: adult hand rinse, water, food, latrine or open defecation, chicken faeces, animal faeces, soil,
# and an under-5 hand rinse if the questionnaire reports a child under 5
household_season <- household_season |> mutate(
  env_types_missing = paste0(
      if_else(!str_detect(coalesce(env_types_sampled, ""), "handrinse_adult"), "handrinse_adult;", ""),
      if_else(!str_detect(coalesce(env_types_sampled, ""), "drinkingwater"), "drinkingwater;", ""),
      if_else(!str_detect(coalesce(env_types_sampled, ""), "food"), "food;", ""),
      if_else(!str_detect(coalesce(env_types_sampled, ""), "latrine|opendefecation"), "latrine/opendefecation;", ""),
      if_else(!str_detect(coalesce(env_types_sampled, ""), "chickenfaeces"), "chickenfaeces;", ""),
      if_else(!str_detect(coalesce(env_types_sampled, ""), "animalfaeces"), "animalfaeces;", ""),
      if_else(!str_detect(coalesce(env_types_sampled, ""), "soil"), "soil;", ""),
      if_else(!(hh_children_u5 %in% 0) & !str_detect(coalesce(env_types_sampled, ""), "handrinse_underfive"), "handrinse_underfive;", "")) |>
    str_remove(";$"),
  flag_no_questionnaire  = is.na(hh_date_survey),
  flag_questionnaire_incomplete = !flag_no_questionnaire & (is.na(hh_members) | is.na(water_source) | is.na(water_treatment)),
  flag_no_env_samples    = is.na(env_n_samples),
  flag_env_incomplete    = !flag_no_env_samples & env_types_missing != "",
  flag_u5_rinse_no_child = !is.na(ecoli_pos_handrinse_underfive) & hh_children_u5 %in% 0,
  flag_no_stool          = is.na(n_stool_collected),
  flag_no_2022_id        = is.na(menage_id),
  flag_2022_id_not_found = !is.na(menage_id) & is.na(prev_hh_n_samples),
  flag_village_mismatch  = !is.na(village_code) & village_code != hhwash$village_code[match(village, str_sub(hhwash$household, 1, 2))],
  flags = paste0(
      if_else(flag_no_questionnaire, "no household questionnaire; ", ""),
      if_else(flag_questionnaire_incomplete, "questionnaire: household size, water source or treatment missing; ", ""),
      if_else(flag_no_env_samples, "no environmental samples; ", ""),
      if_else(flag_env_incomplete, paste0("environmental samples missing: ", env_types_missing, "; "), ""),
      if_else(flag_u5_rinse_no_child, "under-5 hand rinse but no child under 5 reported; ", ""),
      if_else(flag_no_stool, "no stool collected; ", ""),
      if_else(flag_no_2022_id, "no 2022-24 household ID (hm_number_cse); ", ""),
      if_else(flag_2022_id_not_found, "2022-24 household ID not found; ", ""),
      if_else(flag_village_mismatch, "village code differs from household number; ", "")) |>
    str_remove("; $") |> na_if(""))
household_season |> summarise(across(starts_with("flag_"), sum))
household_season |> filter(flag_env_incomplete) |> count(env_types_missing, sort = TRUE)

# EXPORT--------------------------------------------------------
household_season <- household_season |>
  relocate(household, village, village_code, village_name, season, flags, hh_date_survey:n_animal_species,
           n_stool_collected:stool_date_last, env_date_first:env_period, days_stool_to_env, env_n_samples:env_types_sampled,
           env_types_missing, starts_with("ecoli_"), starts_with("esbl_"), menage_id, starts_with("prev_hh_"), starts_with("prev_vil_")) |>
  arrange(household, season)
saveRDS(household_season, "household_season_linked.rds")
write_csv(household_season, "household_season_linked.csv", na = "")
