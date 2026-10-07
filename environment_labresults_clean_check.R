#############################################################
# 3GCR-E source attribution - SCRIPT TO CHECK LAB RESULTS   #
#############################################################

# install/load packages
pacman::p_load(tidyverse, lubridate, dplyr)

# IMPORT DATA----- ------------------------------------------
quantificationlabresults <- read_csv("~/3GCREsourceattribution/data/ALARUMSourceAttribut_DATA_2026-10-05_0916.csv", col_types = cols(.default = col_character()),
                                     na = c("", "NA"), locale = locale(encoding = "UTF-8"))
names(quantificationlabresults) <- c(
  "record_id", "id", "date_prelev", "heure_prelev", "date_recep_lab",
  "heure_recep_lab", "date_deb_trait", "heur_deb_trait", "filtration",
  "filt_volume", "med_mcc", "med_mcc_ctx", "med_tbx", "med_tbx_ctx",
  "dilution", "dilut_titre", "volum_ensem",
  "nbre_colo_tbx", "nbre_colo_tbx_ctx", "nbre_colo_mcc", "nbre_colo_mcc_ctx",
  "nbre_e_coli_volum", "nbre_blse_volum",
  "date_saisie", "initiales", "signature", "date_verif", "initial_verif",
  "signat_verif", "complete") # based on redcap var names

# clean variable ID, make sure there's always a "-" between the first 5-digit number and the first letter
quantificationlabresults <- quantificationlabresults |>  mutate(id_orig  = id,
    id_clean = str_to_upper(str_squish(id)),
    id_clean = str_replace(id_clean, "(\\d{5})[\\s-]*(?=[A-Z])", "\\1-"),
    household = str_extract(id_clean, "\\d{5}"))

# deduct sample type from ID (letters/numbers after the first "-" in the ID)
quantificationlabresults <- quantificationlabresults |>
  mutate(type_code = id_clean |>
      str_remove("^[^-]*-") |>
      str_remove("[DR][^A-Z]*$") |>
      str_remove_all("[^A-Z0-9]"),
    sample_type = case_when(type_code == "HWA" ~ "handrinse_adult", type_code == "HDA" ~ "handrinse_adult", type_code == "KWA" ~ "handrinse_adult",
      type_code == "HWP" ~ "handrinse_underfive",
      type_code == "W"   ~ "drinkingwater",
      type_code == "2L"  ~ "latrine",
      type_code == "1L"  ~ "opendefecation", type_code == "1LR23004AF"  ~ "opendefecation", type_code == "IL"  ~ "opendefecation", 
      type_code == "C"   ~ "chickenfaeces",
      type_code == "AF"  ~ "animalfaeces", type_code == "AF2"  ~ "animalfaeces", type_code == "AFP"  ~ "animalfaeces",
      type_code == "F"   ~ "food",
      type_code == "S"   ~ "soil"))          # anything else stays NA

quantificationlabresults |> count(sample_type) |> print(n = 20)
cat("\nIDs with an unrecognised sample type (sample_type = NA):\n")
quantificationlabresults |> filter(is.na(sample_type)) |> select(record_id, id_clean, type_code) |> print(n = 30)

# deduct season from the last letter of the ID: D = dry, R = rainy
quantificationlabresults <- quantificationlabresults |>  mutate(
    last_letter = str_extract(id_clean, "[A-Z](?=[^A-Z]*$)"),
    season = case_when(last_letter == "D" ~ "dry",
                       last_letter == "R" ~ "rainy"))     # otherwise NA
  

quantificationlabresults %>%  filter(is.na(season)) %>% select(id, id_clean)

# check date outliers
quantificationlabresults %>%  group_by(season) |>  summarise(n = n(),
    first_date   = min(date_prelev, na.rm = TRUE),
    last_date    = max(date_prelev, na.rm = TRUE))
# check manually & replace implausible collection dates (e.g. 0426-03-01, anything before 2025-08-01) by the date of reception at the laboratory
quantificationlabresults <- quantificationlabresults |>
  mutate(date_prelev = if_else(date_prelev < ymd("2025-08-01"), date_recep_lab, date_prelev))
# add season for those with missing season
quantificationlabresults <- quantificationlabresults |>
  mutate(season = if_else(is.na(season) & date_prelev > ymd("2026-03-10") & date_prelev < ymd("2026-06-01"), "dry", season))
# all dates
quantificationlabresults <- quantificationlabresults |>  mutate(
    across(c(date_prelev, date_recep_lab, date_deb_trait, date_saisie, date_verif), ymd))

# for drinking water a filter was plated (volume in 'Si oui preciser le volume'), for all others diluted sample was plated with volume entered in volum_ensem
default_plate_uL <- 100   # volume plated (uL) if 'Volume ensemence' is missing or 0
# parse_number() keeps the number in "100", "100 UL", "100ML", "100 ML"
quantificationlabresults <- quantificationlabresults |>  mutate(filt_vol_mL   = parse_number(filt_volume),
    plate_uL      = parse_number(volum_ensem),
    plate_uL      = if_else(is.na(plate_uL) | plate_uL == 0, default_plate_uL, plate_uL),
    vol_mL        = if_else(sample_type %in% "drinkingwater", filt_vol_mL, plate_uL / 1000))
quantificationlabresults |> filter(sample_type %in% "drinkingwater", is.na(filt_vol_mL)) |>
  select(record_id, id_clean, filtration, filt_volume, volum_ensem) |> print(n = 30)
# if water volume is missing, then use 100mL, which was supposed to be filtered
quantificationlabresults$vol_mL[is.na(quantificationlabresults$filt_volume)] <- "100"

# extract lists with potential problems
# (a) Filtration = 'Oui' although the sample is not drinking water
list_filtration <- quantificationlabresults |>
  filter(filtration==1, !(sample_type %in% "drinkingwater")) |>
  select(id_clean, sample_type, date_prelev, filtration, filt_volume, volum_ensem)
cat("\nFiltration 'Oui' but not drinking water:", nrow(list_filtration), "samples\n")
print(list_filtration)
write_csv(list_filtration, "list_filtration_not_drinkingwater.csv")

# (b) TBX used although the sample is neither drinking water nor a hand rinse
list_tbx <- quantificationlabresults |>  filter(med_tbx=="1" & !(sample_type %in% c("drinkingwater", "handrinse_adult", "handrinse_underfive"))) |>
  select(id_clean, sample_type, date_prelev, date_recep_lab, date_deb_trait, dilut_titre)
cat("\nTBX used, not drinking water / hand rinse:", nrow(list_tbx), "samples\n")
print(list_tbx)
write_csv(list_tbx, file.path(out_dir, "list_tbx_used_non_water_non_handrinse.csv"))
list_tbx_ctx <- quantificationlabresults |>  filter(med_tbx_ctx=="1" & !(sample_type %in% c("drinkingwater", "handrinse_adult", "handrinse_underfive"))) |>
  select(id_clean, sample_type, date_prelev, date_recep_lab, date_deb_trait, dilut_titre)
print(list_tbx_ctx)

# check whether dilution reported as per sample type
table(quantificationlabresults$sample_type, quantificationlabresults$dilution)

# clean dilution data -> titers written as 10^x 
# 'Si oui preciser les titres' is free text, e.g."1/100 MC+CTX - 1/10000 MC"   "1/10 TBX ET TBX+CTX"   "1/1000"
# a dilution applies to the media named after it, a lone dilution applies to all media
# if a medium is still without dilution (e.g. text names MC but the plate was TBX) we take the dilution of the
# equivalent medium (MC <-> TBX, MC+CTX <-> TBX+CTX), or else the only
# dilution mentioned; such rows are marked in `titer_inferred`.
get_dilutions <- function(text) {
  out <- c(MC = NA_real_, MCC = NA_real_, TB = NA_real_, TBC = NA_real_)
  if (is.na(text)) return(c(out, inferred = NA))
  
  # standardise the text: TBX -> TB, "+CTX" -> C, "ET" -> "|"
  z <- text |> str_to_upper() |> str_remove_all("\\s+") |>
    str_replace_all("TBX", "TB") |> str_replace_all("ET", "|") |>
    str_replace_all("\\+CTX", "C") |> str_replace_all("\\+CT", "C")
  # split into dilutions (1/100 or 100) and medium names (MCC, MC, TBC, TB)
  pieces <- str_extract_all(z, "1/\\d+|\\d+|MCC|MC|TBC|TB")[[1]]
  
  current <- NA_real_; all_dil <- c(); used_dil <- c()
  for (p in pieces) {
    if (str_detect(p, "^[0-9/]+$")) {          # a dilution
      current <- as.numeric(str_remove(p, "^1/"))
      all_dil <- c(all_dil, current)
    } else {                                   # a medium: gets the latest dilution
      out[p]  <- current
      used_dil <- c(used_dil, current)
    }
  }
  inferred <- ""
  if (length(used_dil) == 0) {                 # only a dilution, no medium named
    out[] <- current
  } else {
    twin  <- c(MC = "TB", TB = "MC", MCC = "TBC", TBC = "MCC")
    loose <- setdiff(all_dil, used_dil)        # a dilution not attached to a medium
    for (m in names(out)) {
      if (is.na(out[[m]])) {
        if (!is.na(out[[twin[[m]]]]))         { out[[m]] <- out[[twin[[m]]]]
        } else if (length(loose) == 1)         { out[[m]] <- loose
        } else if (length(unique(all_dil)) == 1) { out[[m]] <- all_dil[1] }
        if (!is.na(out[[m]])) inferred <- paste(inferred, m)
      }
    }
  }
  c(out, inferred = str_squish(inferred))
}

# apply to every row where the dilution was done ('Oui')
dil_text <- if_else(quantificationlabresults$dilution=="1", quantificationlabresults$dilut_titre, NA_character_)
dil_table <- do.call(rbind, lapply(dil_text, get_dilutions)) |> as_tibble()

quantificationlabresults <- quantificationlabresults |>  mutate(
    dil_mc      = as.numeric(dil_table$MC),    # 1/dil_xx, e.g. 100 for 1/100
    dil_mc_ctx  = as.numeric(dil_table$MCC),
    dil_tbx     = as.numeric(dil_table$TB),
    dil_tbx_ctx = as.numeric(dil_table$TBC),
    titer_inferred = na_if(dil_table$inferred, ""),
    # no dilution ('Non') = undiluted = 10^0
    across(c(dil_mc, dil_mc_ctx, dil_tbx, dil_tbx_ctx),
           ~ if_else(dilution=="1", .x, 1)),
    # titer as exponent x of 10^x  (1/100 -> -2)
    titer_mc_log10      = -log10(dil_mc),       # MC       any E. coli
    titer_tbx_log10     = -log10(dil_tbx),      # TBX      any E. coli
    titer_mc_ctx_log10  = -log10(dil_mc_ctx),   # MC+CTX   ESBL-selective
    titer_tbx_ctx_log10 = -log10(dil_tbx_ctx),  # TBX+CTX  ESBL-selective
    # titer of the medium that was actually used for that sample
    titer_ecoli_log10 = if_else(med_tbx=="1",     titer_tbx_log10,     titer_mc_log10),
    titer_esbl_log10  = if_else(med_tbx_ctx=="1", titer_tbx_ctx_log10, titer_mc_ctx_log10),
    titer_ecoli = if_else(is.na(titer_ecoli_log10), NA_character_, paste0("10^", titer_ecoli_log10)),
    titer_esbl  = if_else(is.na(titer_esbl_log10),  NA_character_, paste0("10^", titer_esbl_log10)) )


table(quantificationlabresults$titer_ecoli, quantificationlabresults$sample_type, useNA = "always")
table(quantificationlabresults$titer_esbl, quantificationlabresults$sample_type, useNA = "always")
dilutionissues <- quantificationlabresults %>% filter(titer_ecoli=="10^Inf"|titer_esbl=="10^Inf"|is.na(titer_ecoli)|is.na(titer_esbl))%>%
  filter(sample_type!="drinkingwater") %>%
  select(id, date_prelev, sample_type, dilution, dilut_titre)
dilutionissues

drinkingwater_diluted <- quantificationlabresults %>% filter(sample_type=="drinkingwater"&!is.na(dilut_titre))%>%
  select(id, date_prelev, dilution, dilut_titre)
drinkingwater_diluted # none, which is as it should be

# Colony counts and lab-calculated concentrations ---------------------
# Counts like ">200 UFC" -> number 200 + flag "censored" (true value is higher).
# Lab concentrations like ">20000000 UFC / ML" -> 20000000 + censored flag +
# the unit the lab used (per mL, per g, per 100 mL, per swab).
quantificationlabresults <- quantificationlabresults |> mutate(
    # colony counts
    c_tbx     = parse_number(nbre_colo_tbx),     cens_tbx     = str_detect(nbre_colo_tbx, "^\\s*>"),
    c_tbx_ctx = parse_number(nbre_colo_tbx_ctx), cens_tbx_ctx = str_detect(nbre_colo_tbx_ctx, "^\\s*>"),
    c_mc      = parse_number(nbre_colo_mcc),     cens_mc      = str_detect(nbre_colo_mcc, "^\\s*>"),
    c_mc_ctx  = parse_number(nbre_colo_mcc_ctx), cens_mc_ctx  = str_detect(nbre_colo_mcc_ctx, "^\\s*>"),
    # lab concentrations
    lab_ec      = parse_number(nbre_e_coli_volum), lab_ec_cens = str_detect(nbre_e_coli_volum, "^\\s*>"),
    lab_es      = parse_number(nbre_blse_volum),   lab_es_cens = str_detect(nbre_blse_volum, "^\\s*>"),
    # unit used by the lab
    lab_text = str_to_upper(str_remove_all(paste(nbre_e_coli_volum, nbre_blse_volum), "\\s")),
    lab_unit = case_when(str_detect(lab_text, "100ML") ~ "per_100mL",
                         str_detect(lab_text, "ECOU") ~ "per_swab",
                         str_detect(lab_text, "ECL") ~ "per_swab",
                         str_detect(lab_text, "/G")    ~ "per_g",
                         str_detect(lab_text, "/ML")   ~ "per_mL"),
    unit_mult = if_else(lab_unit %in% "per_100mL", 100, 1))   # per 100 mL = 100 x per mL
# one by one checks
quantificationlabresults$lab_unit[quantificationlabresults$id_clean=="28003-C-R"] <- "per_swab"
quantificationlabresults$lab_unit[quantificationlabresults$id_clean=="24017-AF-R"] <- "per_mL"
quantificationlabresults$lab_unit[quantificationlabresults$id_clean=="26018-W-R"] <- "per_100mL"
quantificationlabresults$lab_unit[is.na(quantificationlabresults$lab_unit)&quantificationlabresults$sample_type=="handrinse_adult"] <- "per_mL" # is 0, hence no unit
quantificationlabresults$lab_unit[is.na(quantificationlabresults$lab_unit)&quantificationlabresults$sample_type=="handrinse_underfive"] <- "per_mL"
quantificationlabresults$lab_unit[is.na(quantificationlabresults$lab_unit)&quantificationlabresults$sample_type=="soil"] <- "per_g" # is 0, hence no unit

     
quantificationlabresults |> count(sample_type, lab_unit) |> print(n = 30)

# ifentidy implausible units
# Unit / sample type combinations that don't make sense
implausible_unit <- tribble(
  ~sample_type,          ~lab_unit,
  "chickenfaeces",       "per_100mL",
  "chickenfaeces",       "per_mL",
  "drinkingwater",       "per_mL",
  "food",                "per_100mL",
  "food",                "per_g",
  "handrinse_adult",     "per_g",
  "handrinse_underfive", "per_g",
  "latrine",             "per_g",
  "opendefecation",      "per_100mL",
  "soil",                "per_mL")

unit_issues <- bind_rows(
  # unit missing, any sample type
  quantificationlabresults |> filter(is.na(lab_unit)),
  # unit present but not plausible for that sample type
  quantificationlabresults |> semi_join(implausible_unit, by = c("sample_type", "lab_unit"))) |>
  select(id_clean, date_prelev, sample_type, lab_unit, nbre_e_coli_volum, nbre_blse_volum) |>
  arrange(sample_type, lab_unit, date_prelev)

print(unit_issues, n = 100)
count(unit_issues, sample_type, lab_unit)    # how many per combination
write_csv(unit_issues, "output_lab_cleaning/list_unit_issues.csv")

mising_or_illogical_units_for_sample_type <- quantificationlabresults %>%
  
  
# are the counts in the range where plates are reliable (10-200 colonies)?
summary_counts <- quantificationlabresults %>%
  select(c_tbx, c_tbx_ctx, c_mc, c_mc_ctx) |>
  pivot_longer(everything(), names_to = "medium", values_to = "count") |>
  filter(!is.na(count)) |>
  mutate(range = case_when(count == 0 ~ "0", count < 10 ~ "1-9", count <= 200 ~ "10-200", TRUE ~ ">200")) |>
  count(medium, range) |> print(n = 30)
summary_counts

# recalculate concentrations -------------------------------------------
# concentration = colony count / (volume in mL x dilution factor 10^x)
# (x 100 when the lab reports per 100 mL, so both are in the lab's unit)
quantificationlabresults <- quantificationlabresults |>  mutate(
    conc_ecoli_tbx = c_tbx     / (vol_mL * 10^titer_tbx_log10)     * unit_mult,
    conc_ecoli_mc  = c_mc      / (vol_mL * 10^titer_mc_log10)      * unit_mult,
    conc_esbl_tbx  = c_tbx_ctx / (vol_mL * 10^titer_tbx_ctx_log10) * unit_mult,
    conc_esbl_mc   = c_mc_ctx  / (vol_mL * 10^titer_mc_ctx_log10)  * unit_mult,
    # the recomputed value to compare with the lab: from the medium used
    expected_ecoli = if_else(med_tbx,     conc_ecoli_tbx, conc_ecoli_mc),
    expected_esbl  = if_else(med_tbx_ctx, conc_esbl_tbx,  conc_esbl_mc),
    expected_ecoli_cens = if_else(med_tbx,     cens_tbx,     cens_mc),
    expected_esbl_cens  = if_else(med_tbx_ctx, cens_tbx_ctx, cens_mc_ctx))


# ---- 11. Compare lab value with recomputed value ----------------------------
# "match": lab value within tol_rel (1%) of the recomputed value
quantificationlabresults <- quantificationlabresults |>
  mutate(
    ec_status = case_when(
      is.na(lab_ec) & is.na(expected_ecoli)             ~ "both missing",
      is.na(lab_ec)                                     ~ "lab value missing",
      is.na(expected_ecoli)                             ~ "cannot recompute",
      lab_ec == 0 & expected_ecoli == 0                 ~ "match",
      lab_ec_cens != expected_ecoli_cens                ~ "DISCREPANCY",
      lab_ec == 0 | expected_ecoli == 0                 ~ "DISCREPANCY",
      abs(lab_ec / expected_ecoli - 1) <= tol_rel       ~ "match",
      TRUE                                              ~ "DISCREPANCY"),
    es_status = case_when(
      is.na(lab_es) & is.na(expected_esbl)              ~ "both missing",
      is.na(lab_es)                                     ~ "lab value missing",
      is.na(expected_esbl)                              ~ "cannot recompute",
      lab_es == 0 & expected_esbl == 0                  ~ "match",
      lab_es_cens != expected_esbl_cens                 ~ "DISCREPANCY",
      lab_es == 0 | expected_esbl == 0                  ~ "DISCREPANCY",
      abs(lab_es / expected_esbl - 1) <= tol_rel        ~ "match",
      TRUE                                              ~ "DISCREPANCY"),
    # size of the discrepancy: log10(lab / recomputed); +1 = lab 10x higher
    ec_log10_ratio = if_else(lab_ec > 0 & expected_ecoli > 0, log10(lab_ec / expected_ecoli), NA_real_),
    es_log10_ratio = if_else(lab_es > 0 & expected_esbl > 0,  log10(lab_es / expected_esbl),  NA_real_),
    # impossible: more ESBL E. coli than E. coli
    esbl_above_ecoli = !lab_ec_cens & !lab_es_cens & lab_es > lab_ec * (1 + tol_rel)
  )

cat("\nE. coli: lab value vs recomputed\n");        print(count(d, ec_status))
cat("\nESBL E. coli: lab value vs recomputed\n");   print(count(d, es_status))
cat("\nE. coli status by sample type\n");           print(count(d, sample_type, ec_status), n = 40)
cat("\nSize of the discrepancies, log10(lab / recomputed):\n")
print(table(round(d$ec_log10_ratio[d$ec_status == "DISCREPANCY"], 1)))
cat("\nLab ESBL concentration above lab E. coli concentration:", sum(d$esbl_above_ecoli, na.rm = TRUE), "\n")

# Samples to look at
quantificationlabresults |> filter(ec_status == "DISCREPANCY" | es_status == "DISCREPANCY" | esbl_above_ecoli) |>
  select(record_id, id_clean, sample_type, season, date_prelev, filtration, filt_volume,
         volum_ensem, dilut_titre, titer_ecoli, titer_esbl,
         nbre_colo_tbx, nbre_colo_mcc, nbre_colo_tbx_ctx, nbre_colo_mcc_ctx,
         nbre_e_coli_volum, expected_ecoli, ec_status,
         nbre_blse_volum, expected_esbl, es_status) |>
  write_csv(file.path(out_dir, "flagged_discrepancies.csv"))

# Plot: lab vs recomputed (points on the diagonal agree)
quantificationlabresults |> filter(expected_ecoli > 0, lab_ec > 0) |>
  ggplot(aes(expected_ecoli, lab_ec, colour = ec_status)) +
  geom_abline(linetype = 2) + geom_point(alpha = .5) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "Recomputed E. coli concentration", y = "Lab-calculated E. coli concentration") +
  theme_bw()
ggsave(file.path(out_dir, "lab_vs_recomputed_ecoli.png"), width = 6, height = 5, dpi = 200)


# ---- 12. TBX versus MacConkey (samples where both were read) ----------------
both_ecoli <- quantificationlabresults |> filter(!is.na(conc_ecoli_tbx), !is.na(conc_ecoli_mc))
both_esbl  <- quantificationlabresults |> filter(!is.na(conc_esbl_tbx),  !is.na(conc_esbl_mc))
cat("\nSamples with both TBX and MC read: E. coli", nrow(both_ecoli),
    "| ESBL", nrow(both_esbl), "\n")

if (nrow(both_ecoli) > 0) {
  both_ecoli |>
    filter(conc_ecoli_tbx > 0, conc_ecoli_mc > 0) |>
    mutate(log10_ratio_tbx_mc = log10(conc_ecoli_tbx / conc_ecoli_mc)) |>
    summarise(n = n(), median = median(log10_ratio_tbx_mc),
              within_2fold = mean(abs(log10_ratio_tbx_mc) <= log10(2))) |> print()
}
if (nrow(both_esbl) > 0) {
  both_esbl |>
    filter(conc_esbl_tbx > 0, conc_esbl_mc > 0) |>
    mutate(log10_ratio_tbx_mc = log10(conc_esbl_tbx / conc_esbl_mc)) |>
    summarise(n = n(), median = median(log10_ratio_tbx_mc),
              within_2fold = mean(abs(log10_ratio_tbx_mc) <= log10(2))) |> print()
}


# ---- 13. Other ID checks and save -------------------------------------------
# record_id different from id, duplicated IDs, season in ID vs collection date
d <- quantificationlabresults |>
  mutate(season_date = case_when(is.na(date_prelev) ~ NA_character_,
                                 month(date_prelev) %in% 3:5  ~ "dry",
                                 month(date_prelev) %in% 6:10 ~ "rainy",
                                 TRUE ~ "other"))

quantificationlabresults |> filter(record_id != id_orig | duplicated(id_clean) | is.na(household) |
              is.na(season) | is.na(sample_type) | str_count(id_clean, "\\d{5}") > 1 |
              (!is.na(season) & !is.na(season_date) & season != season_date)) |>
  select(record_id, id_orig, id_clean, household, type_code, sample_type, season,
         season_date, date_prelev) |>
  write_csv(file.path(out_dir, "flagged_id_issues.csv"))

saveRDS(d, file.path(out_dir, "lab_environment_clean.rds"))
write_csv(d, file.path(out_dir, "lab_environment_clean.csv"))
cat("\nDone. Files written to", out_dir, "\n")
