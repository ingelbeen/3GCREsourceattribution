#############################################################
# 3GCR-E source attribution - SCRIPT TO MERGE RAW DATASETS  #
#############################################################

# install/load packages
pacman::p_load(readxl, dplyr, tidyr, stringr, purrr, ggplot2, patchwork,
               geepack, brms, posterior, ggnewscale)

# IMPORT DATA----- ------------------------------------------
# import Nanoro Redcap alarum database, containing all individual CRFs
redcap_alarum_raw
# household data from the HH visits in 8 villages (ALARUM 2025-26)
hhsurvey_alarum_raw <- redcap_alarum_raw %>% filter(redcap_repeat_instrument=="menage_crf")
hhsurvey_alarum_raw <- read_excel("data/ALARUM-Community-DATA_Nanoro_FV.xlsx") |> rename_with(str_squish)
# household hcu survey
hh_hcu_alarum_raw <- redcap_alarum_raw %>% filter(redcap_repeat_instrument=="recherche_des_pisodes_de_soins")
# household data from HH member stool collections in 22 villages (CABU-EICO 2022-24)
hhsurvey_cabu_raw <- redcap_alarum_raw %>% filter(redcap_repeat_instrument=="microbiology_crf")
# environmental sample characteristics and culture results (source attrib 2026)
envsamples <-
# environmental sample sequences (source attrib 2026)
envseq
# human stool sample (CABU-EICO 2022-24) characteristics and culture results
stoolsamples
# human stool sequences (CABU-EICO 2022-24)
stoolseq
# household observation exposure count data (2026)

# PREPARE DATASETS TO ALLOW LINKAGE BY HOUSEHOLD AND TIME-----

# GENERATE LINKED DATASETS PER ANALYSIS ----------------------
