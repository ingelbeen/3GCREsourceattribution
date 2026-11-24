#############################
# BSI data from CMA Nanoro  #
#############################

# install and load packages
pacman::p_load(dplyr, stringr, readxl, digest, writexl, lubridate, ggplot2, purrr)

# import data
df <- read_excel("~/ABR - RDU/onehealthsurveillance JPIAMR/BSI CMA Nanoro/260402 Base_complete_Hémo.xlsx", 
                 sheet = "Base_complète_Emo")
#### clean data ####
# remove phone nr
df <- df %>% select(-TELEPHONE_E1_C1)

# 1. Fix recurring mojibake / encoding artefacts (e.g. "HÃ‰MOCULTURE", "BACTEC FÃ—40") and normalise whitespace/case before pattern matching.
fix_encoding <- function(x) {
  x %>%
    str_replace_all("Ã‰|Ã©|Ã¨|Ãˆ|Ã¡", "E") %>%   # garbled accented E -> E
    str_replace_all("Ã—", "X") %>%                 # corrupted "x" (FX40) -> X
    str_replace_all("Ã‚|â€™|â€˜", "") %>%
    str_squish()}

df <- df %>%
  mutate(
    comment_clean = LAB_COMMENTAIRE_FINALE_E2_C2 %>%
      fix_encoding() %>%
      str_to_upper())
# 2. add a var with year
df$year <- as.integer(format(df$DATE_PRELEVEMENT_E1_C1, "%Y"))
df$year[is.na(df$DATE_PRELEVEMENT_E1_C1)] <- as.integer(format(df$DATE_ADMISSION_E1_C1[is.na(df$DATE_PRELEVEMENT_E1_C1)], "%Y")) # if date of collect is missing
table(df$year, useNA = "ifany")
df$year[df$year==2001] <- 2021 # chacked and corrected using date de prelev

# 3. classify culture result (all positive-bacteremia in the data start with "BACT..." (BACTERIEMIE / BACTERIENNE / BACTEREMIE / BACTRIEMIE /
#    BACTENEMIE / BACTEXIEMIE / BACTERIMIE / BACTERINE / BACTIEREMIE ...) except for "BACTEC" 
df <- df %>%
  mutate(
    first_word = word(comment_clean, 1),
    culture_result = case_when(
      is.na(comment_clean) | comment_clean == ""                    ~ NA_character_,
      str_starts(first_word, "BACT") & !str_starts(first_word, "BACTEC") ~ "Positive",
      str_detect(comment_clean, "CONTAMIN|CONTOURNE")               ~ "Contaminated",
      str_detect(comment_clean, "N[AE]GATI")                        ~ "Negative",
      TRUE                                                        ~ "Other/Unclear" ))
# 4. extract the raw organism substring for "Positive" rows
df <- df %>%
  mutate(
    pathogen_raw = if_else(
      culture_result == "Positive",
      comment_clean %>%
        str_remove("^\\S+\\s+") %>%                # drop BACTERIEMIE/BACTERIENNE/...
        str_remove("^(A|AU|AUX|G|DE)\\s+") %>%      # drop connector, incl. "G" typo for "A"
        str_trim(),
      NA_character_),    # flag mixed / polymicrobial or multi-bottle results for manual review
    pathogen_multiple = culture_result == "Positive" &
      str_detect(pathogen_raw, "\\bET\\b|/|FLACON\\s*[12]"))
# 4. standardize pathogen name using genus/species keyword stems
pathogen_dictionary <- tibble::tribble(
  ~canonical,                                ~pattern,
  "Salmonella Typhimurium",                  "TYPHIMURIUM",
  "Salmonella Typhi",                        "TYPHI",
  "Salmonella enteritidis",                  "ENTERITI",
  "Salmonella spp (other/unspecified)",      "SALMONELLA",
  "Klebsiella pneumoniae",                   "KLEBSIELLA",
  "Streptococcus pneumoniae",                "PNEUMONI",
  "Staphylococcus aureus",                   "AUREU|ANREUS",
  "Escherichia coli",                        "\\bCOLI\\b|ESCHE",
  "Shigella spp",                            "SHIGELLA",
  "Enterococcus durans",                     "DURANS",
  "Enterococcus spp",                        "ENTEROCOCCUS",
  "Streptococcus group B",                   "GROUPE B|GROUP B",
  "Streptococcus group F",                   "GROUPE F|GROUP F",
  "Streptococcus spp (unspecified)",         "STREPTOCOCCUS|STREPTOCOQUE",
  "Acinetobacter baumannii",                 "ACINETOBACTER|ACINOBACTER",
  "Pseudomonas stutzeri",                    "STUZERI|STUTZERI",
  "Pseudomonas aeruginosa",                  "AERUGINOSA",
  "Listeria spp",                            "LISTERIA|LISTERIO",
  "Campylobacter sp",                        "CAMPYLOBACTER",
  "Proteus mirabilis",                       "PROTEUS|MIXABILIS|MIRABILIS",
  "Burkholderia cepacia",                    "BURKHOLDERIA",
  "Haemophilus influenzae b",                "HAEMOPHILUS",
  "Aerococcus viridans",                     "AEROCOCCUS",
  "Lactococcus lactis",                      "LACTOCOCCUS",
  "Bacillus spp",                            "^BACILLUS",
  "Candida spp",                             "CANDID",
  "Coagulase-negative staphylococci (CNS)",  "COAGULASE|\\bCNS\\b"
)

match_pathogen <- function(x, dict = pathogen_dictionary) {
  if (is.na(x)) return(NA_character_)
  hit <- which(vapply(dict$pattern, function(p) str_detect(x, regex(p)), logical(1)))
  if (length(hit) == 0) NA_character_ else dict$canonical[hit[1]]
}

df <- df %>%
  mutate(pathogen_std = vapply(pathogen_raw, match_pathogen, character(1)))

# 5. recode a few that didn't match because of a typo
df$pathogen_std[df$pathogen_raw=="ECOLI 1"] <- "Escherichia coli"
df$pathogen_std[df$pathogen_raw=="S.PENEUMONIAE PROBABLEMENT DECAPITEE"] <- "Streptococcus pneumoniae"
df$pathogen_std[df$pathogen_raw=="S.PNEUMOMIAE"] <- "Streptococcus pneumoniae"
df$pathogen_std[df$pathogen_raw=="SALMONELLE SPP"] <- "Salmonella spp (other/unspecified)"

# 6. age
df$ageyears <- df$AGE_ANNEE_E1_C1
df$ageyears[is.na(df$AGE_ANNEE_E1_C1)] <- round(df$AGE_MOIS_E1_C1[is.na(df$AGE_ANNEE_E1_C1)]/12,1)
df$ageyears[is.na(df$AGE_ANNEE_E1_C1)&is.na(df$AGE_MOIS_E1_C1)&is.na(df$DOB_E1_C1)] <- round(df$AGE_JOURS_E1_C1 [is.na(df$AGE_ANNEE_E1_C1)&is.na(df$AGE_MOIS_E1_C1)&is.na(df$DOB_E1_C1)]/365,1)
df$ageyears[is.na(df$AGE_ANNEE_E1_C1) & is.na(df$AGE_MOIS_E1_C1)] <- year(df$DATE_PRELEVEMENT_E1_C1[is.na(df$AGE_ANNEE_E1_C1) & is.na(df$AGE_MOIS_E1_C1)]) - year(df$DOB_E1_C1[is.na(df$AGE_ANNEE_E1_C1) & is.na(df$AGE_MOIS_E1_C1)])
df$ageyears[is.na(df$AGE_ANNEE_E1_C1) & is.na(df$AGE_MOIS_E1_C1)&is.na(df$DATE_PRELEVEMENT_E1_C1)] <- year(df$DATE_ADMISSION_E1_C1[is.na(df$AGE_ANNEE_E1_C1)&is.na(df$AGE_MOIS_E1_C1)&is.na(df$DATE_PRELEVEMENT_E1_C1)]) - year(df$DOB_E1_C1[is.na(df$AGE_ANNEE_E1_C1)&is.na(df$AGE_MOIS_E1_C1)&is.na(df$DATE_PRELEVEMENT_E1_C1)])
df$ageyears[df$ageyears<0] <- NA
table(df$ageyears, useNA = "always")
# if age is missing, check different variables
df %>% filter(is.na(ageyears)) %>% select(AGE_JOURS_E1_C1, DOB_OU_AGE_E1_C1, DOB_E1_C1, DATE_PRELEVEMENT_E1_C1, DATE_ADMISSION_E1_C1)

# if age >100, check dob
df %>% filter(ageyears>100) %>% select(AGE_ANNEE_E1_C1, DOB_OU_AGE_E1_C1, DOB_E1_C1) # says 120 years. also, DOB_OU_AGE_E1_C1 is 2, so should be years and dept is internal med, not pediatrics

# remove dob
df <- df %>%  select(-DOB_E1_C1)

# 7. group the NTS
df <- df %>%  mutate(pathogen_group = case_when(
      pathogen_std %in% c("Salmonella Typhimurium","Salmonella enteritidis","Salmonella spp (other/unspecified)") ~ "Salmonella",
      TRUE ~ pathogen_std))

# 8. sex
str(df$SEXE_E1_C1)
df$sex[df$SEXE_E1_C1==2] <- "female"
df$sex[df$SEXE_E1_C1==1] <- "male"

# 9. service
table(df$SERVICE_HOPITAL_E1_C1)
df <- df %>%  mutate(
    service_clean = str_squish(str_to_upper(SERVICE_HOPITAL_E1_C1)),
    dept = case_when(
      # Emergency (includes triage and paediatric/medical emergency)
      str_detect(service_clean, "^URG|^URENGE")                   ~ "Emergency",
      # Maternity (also CM/MATERNITE, CMA/MATERNITE)
      str_detect(service_clean, "MATERNITE|MARTERNITE")           ~ "Maternity",
      # Paediatrics (also catches CMA/PEDIATRIE, PEDIATRIE/CM URBAIN)
      str_detect(service_clean, "PEDIA|PEDIATRO")                 ~ "Paediatrics",
      # Surgery (incl. post-operative)
      str_detect(service_clean, "CHIRURGIE|^P/O")                 ~ "Surgery",
      # Internal medicine (also catches CMA (MEDECINE))
      str_detect(service_clean, "MEDECIN|MEDICINE")               ~ "Internal medicine",
      # Dispensary / outpatient
      str_detect(service_clean, "DISPANSAIRE|DISPENSAIRE")        ~ "Outpatient",
      str_detect(service_clean, "INFIRMERIE")                     ~ "Outpatient",
      str_detect(service_clean, "URCN")                           ~ "Outpatient",
      # Everything else (CM, CMA, CMI, CMU, URBAIN, SOAW, SDV, UM, NANORO, NANDO, NA, ...)
      TRUE                                                        ~ NA_character_
    ),
    dept = factor(dept)
  ) %>%
  select(-service_clean)

table(df$SERVICE_HOPITAL_E1_C1, df$dept, useNA = "ifany")  # some could be outpatient but not specified (CM, CM urbain, urbain,...)
table(df$dept, useNA = "ifany")

# 10. date to month
df <- df %>%  mutate(
    date_adm   = as.Date(DATE_ADMISSION_E1_C1),   # or dmy(DATE_ADMISSION_E1_C1)
    year       = year(date_adm),
    month      = month(date_adm, label = TRUE, abbr = TRUE),   # calendar month (seasonality)
    year_month = format(date_adm, "%Y-%m"))                     # chronological month

# 11. recode malaria -> the categories are not correct yet, I think
recode_malaria <- function(x) {
  case_match(
    as.numeric(as.character(x)),   # as.character first, in case the column is a factor
    1 ~ "Not tested",
    2 ~ "Negative",
    3 ~ "Positive",            
    .default = NA_character_)       # anything else, including NA
}

df <- df %>%
  mutate(tdr_clin = recode_malaria(TDR_PALU_E1_C1),
    tdr_lab  = recode_malaria(LAB_TDR_PALU_E2_C2),
    thick    = recode_malaria(LAB_GOUTTE_EPAISSE_E2_C2),
    malaria  = case_when(
      if_any(c(tdr_clin, tdr_lab, thick), ~ .x %in% "Positive") ~ "Positive",
      if_any(c(tdr_clin, tdr_lab, thick), ~ .x %in% "Negative") ~ "Negative",
      TRUE ~ NA_character_),
    malaria = factor(malaria, levels = c("Negative", "Positive")))

# check the result and how well the tests agree
table(df$tdr_clin, df$tdr_lab, df$thick, useNA = "ifany")
table(df$malaria, useNA = "ifany")

# 12. extra grouping vars
df <- df %>% mutate(
  age_group   = cut(ageyears, c(0, 1, 5, 15, 50, Inf),
                    labels = c("<1", "1-4", "5-14", "15-49", "50+"),
                    right = FALSE, include.lowest = TRUE),
  culture_pos = case_when(culture_result == "Positive" ~ 1,
                          culture_result == "Negative" ~ 0),
  month_num   = month(date_adm),
  season      = if_else(month_num %in% 7:11, "High malaria (Jul-Nov)", "Low malaria (Dec-Jun)")
)

# 13. remove identifying data
# --- Drop identifying columns ---
vars_to_drop <- c(
  "NOM_CLINICIEN_E1_C1",
  "TELEPHONE_CLINICIEN_E1_C1",
  "SIGNATURE_E1_C1",
  "INITIALE_PRELEVEUR_E1_C1",
  "LAB_INITIALE_COLORATEUR_E2_C2",
  "LAB_INITIALE_FINALE_E2_C2",
  "LAB_INITIALE_FINALE_E2_C3",
  "LAB_SIGNATURE_E2_C2",
  "LAB_SIGNATURE_RESP_LABO_E2_C2",
  "LAB_SIGNATURE_RESP_LABO_E2_C3",
  "DOB_OU_AGE_E1_C1")
df <- df[, !names(df) %in% vars_to_drop]

# --- Replace ID columns with a salted hash ---
salt <- "nanoro" # Keep this salt in a separate, secure file — NOT in the script that goes on github
hash_col <- function(x, salt) {
  vapply(as.character(x), function(v) {
    if (is.na(v) || v == "") return(NA_character_)
    digest(paste0(salt, v), algo = "sha256")
  }, character(1), USE.NAMES = FALSE)
}
vars_to_hash <- c(
  "NUM_HOPITAL_PAT_E1_C1",
  "LAB_NUM_HOPITAL_E2_C2",
  "LAB_NUM_HOPITAL_E2_C3",
  "NUM_HEMOCULTURE_E1_C1",
  "LAB_NUM_HEMOCULTURES_E2_C2_1",
  "LAB_NUM_HEMOCULTURES_E2_C2_2",
  "LAB_NUM_HEMOCULTURE_E2_C3",
  "LAB_NUM_FLACON_HEMOCULTURE_E2_C2_1",
  "LAB_NUM_FLACON_HEMOCULTURE_E2_C2_2",
  "LAB_NUM_FLACON_HEMOCULTURE_E2_C3_1")
df[vars_to_hash] <- lapply(df[vars_to_hash], hash_col, salt = salt)

# save cleaned data
write_xlsx(df, "df.xlsx")
write.csv(df, "df.csv")

#### describe data ####
# import cleaned data
df <- read_xlsx("df.xlsx")
# describe variable names
colnames(df)
# positivity by year
table(df$year, df$culture_result, useNA = "ifany")
round(prop.table(table(df$year, df$culture_result, useNA = "ifany"),1)*100,1) # pct

# distribution of standardized pathogens
df %>%
  filter(culture_result == "Positive") %>%
  count(pathogen_std, sort = TRUE) %>%
  print(n = 50)
# by year
table(df$pathogen_std, df$year)

# on a plot
pathogen_year_counts <- df %>%
  count(year, pathogen_std)

ggplot(pathogen_year_counts, aes(x = factor(year), y = n)) +
  geom_col() +
  facet_wrap(~ pathogen_std, ncol = 2, scales = "free_y") +
  labs(
    x = "Year of collection",
    y = "Number of observations",
    title = "Distribution of pathogens over years") +
  theme_minimal()
# only looking at frequent pathogens
pathogen_year_counts <- df %>%
  mutate(
    pathogen_group = case_when(
      pathogen_std %in% c(
        "Salmonella Typhimurium",
        "Salmonella enteritidis",
        "Salmonella spp (other/unspecified)"
      ) ~ "Non-Typhi Salmonella",
      TRUE ~ pathogen_std
    )
  ) %>%
  count(pathogen_group) %>%
  filter(n > 5) %>%
  select(pathogen_group) %>%
  inner_join(
    df %>%
      mutate(
        pathogen_group = case_when(
          pathogen_std %in% c(
            "Salmonella Typhimurium",
            "Salmonella enteritidis",
            "Salmonella spp (other/unspecified)"
          ) ~ "Non-Typhi Salmonella",
          TRUE ~ pathogen_std
        )
      ) %>%
      count(year, pathogen_group),
    by = "pathogen_group"
  )

ggplot(pathogen_year_counts, aes(x = factor(year), y = n)) +
  geom_col() +
  facet_wrap(~ pathogen_group, scales = "free_y", ncol = 1) +
  labs(
    x = "Year of collection",
    y = "Number of observations",
    title = "Distribution of pathogens over years (>10 observations)"
  ) +
  theme_minimal()

# age distribution per pathogen
ggplot(df, aes(x = ageyears)) +
  geom_histogram(binwidth = 5, boundary = 0, color = "black") +
  facet_wrap(~ pathogen_std, scales = "free_y") +
  labs(
    x = "Age (years)",
    y = "Number of observations",
    title = "Age distribution by pathogen"
  ) +
  theme_minimal()

ggplot(df, aes(x = ageyears, fill = factor(year))) +
  geom_histogram(binwidth = 5, boundary = 0, position = "stack") +
  facet_wrap(~ pathogen_std, scales = "free_y") +
  labs(
    x = "Age (years)",
    y = "Number of observations",
    fill = "Year of collection",
    title = "Age distribution by pathogen and year"
  ) +
  theme_minimal()

# age of E. coli cases by year
table(df$ageyears[df$pathogen_std=="Escherichia coli"])
table(df$ageyears[df$pathogen_std=="Escherichia coli" & df$year==2025])
table(df$year[df$pathogen_std=="Escherichia coli"&df$ageyears<18])


# histogram of years with many E. coli cases, to check if no outbreaks
# all bloodcultures collected
weekly_counts <- df %>%
  filter(year(DATE_ADMISSION_E1_C1) == 2025 & pathogen_std=="Escherichia coli") %>%
  count(week = isoweek(DATE_ADMISSION_E1_C1))

ggplot(weekly_counts, aes(x = week, y = n)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1, 53, by = 2)) +
  labs(
    x = "ISO Week of 2025",
    y = "Number of observations",
    title = "Observations by Week in 2025"
  ) +
  theme_minimal()
# by pathogen
weekly_counts <- df %>%
  filter(year(DATE_ADMISSION_E1_C1) == 2021) %>%
  count(pathogen_std, week = isoweek(DATE_ADMISSION_E1_C1))

ggplot(weekly_counts, aes(x = week, y = n)) +
  geom_col() +
  facet_wrap(~ pathogen_std, scales = "free_y") +
  scale_x_continuous(breaks = seq(1, 53, by = 4)) +
  labs(
    x = "ISO Week of 2025",
    y = "Number of observations",
    title = "Weekly observations in 2021 by pathogen"
  ) +
  theme_minimal()
# daily by pathogen
keep_pathogens <- df %>%
  count(pathogen_group) %>%
  filter(n > 10) %>%
  pull(pathogen_group)

daily_counts <- df %>%
  filter(pathogen_group %in% keep_pathogens) %>%
  mutate(date = as.Date(DATE_ADMISSION_E1_C1),
    day_of_year = yday(date),
    # map every year onto a single reference year (2000 = leap year, so day 366 exists)
    plot_date  = as.Date(day_of_year - 1, origin = "2000-01-01")) %>%
  count(pathogen_group, plot_date)

ggplot(daily_counts, aes(x = plot_date, y = n)) +
  geom_col(width = 1) +
  facet_wrap(~ pathogen_group, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Day of year (all years combined)", y = "n")

# all years facetted, but filtered on E. coli
  daily_ecoli <- df %>%
    filter(pathogen_group == "Escherichia coli",
           !is.na(DATE_ADMISSION_E1_C1)) %>%
    mutate(date = as.Date(DATE_ADMISSION_E1_C1),
      year = year(date)) %>%
    count(year, date) %>%
    mutate(date = as.Date(date),
      doy = yday(date),
      year = factor(year, levels = 2018:2025))
  write.table(daily_ecoli, file = "daily_ecoli.txt")
  ggplot(daily_ecoli, aes(x = doy, y = n)) +
    geom_col(width = 3, fill = "firebrick") +
    facet_wrap(~ year,
               ncol = 1,
               strip.position = "left",
               drop = FALSE) +
    scale_x_continuous(limits = c(1, 366),
      breaks = seq(0, 360, 30)) +
    labs( x = "Day of year",
      y = "Cases (n)",
      title = "Daily E. coli case counts, faceted by year") +
    theme_minimal(base_size = 11) +
    theme(strip.text.y.left = element_text(angle = 0),
      panel.grid.minor = element_blank()  )

# clustering analysis by Claude
  library(tidyverse)
  library(patchwork)
  
  CLUSTER_GAP <- 14
  
  raw <- read.table("daily_ecoli.txt", header = TRUE) %>%
    mutate(date = as.Date(date))
  
  # expand to one row per case, so the n=2 day contributes two ticks
  cases <- raw %>% uncount(n) %>% mutate(year = year(date), doy = yday(date))
  
  # cluster assignment on unique dates
  u <- cases %>%
    distinct(date) %>% arrange(date) %>%
    mutate(gap     = as.numeric(date - lag(date)),
           cluster = cumsum(replace_na(gap > CLUSTER_GAP, TRUE))) %>%
    group_by(cluster) %>% mutate(in_cluster = n() > 1) %>% ungroup()
  
  cases <- left_join(cases, u, by = "date")
  
  # force empty years to appear
  all_years <- tibble(year = 2018:2025)
  
  pA <- ggplot(cases, aes(x = doy, y = factor(year, levels = rev(2018:2025)))) +
    geom_point(aes(colour = in_cluster), shape = "|", size = 5, stroke = 2) +
    geom_text(data = anti_join(all_years, distinct(cases, year), by = "year"),
              aes(x = 183, y = factor(year, levels = rev(2018:2025))),
              label = "no cases recorded", inherit.aes = FALSE,
              fontface = "italic", size = 3, colour = "grey60") +
    scale_x_continuous(limits = c(1, 366),
                       breaks = yday(as.Date(paste0("2021-", 1:12, "-15"))),
                       labels = month.abb) +
    scale_y_discrete(drop = FALSE) +
    scale_colour_manual(values = c(`TRUE` = "#b02418", `FALSE` = "#9aa3ab"),
                        labels = c(`TRUE` = paste0("within ", CLUSTER_GAP, " d of another case"),
                                   `FALSE` = "isolated case"), name = NULL) +
    labs(x = "Month of year", y = NULL,
         title = "A   Each case as a tick, aligned on the calendar") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_line(colour = "grey93", linewidth = 5),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 11))
  
  pB <- u %>% filter(!is.na(gap)) %>%
    ggplot(aes(x = date, y = gap)) +
    annotate("rect", xmin = min(u$date), xmax = max(u$date),
             ymin = 0.7, ymax = CLUSTER_GAP, fill = "#b02418", alpha = 0.07) +
    geom_hline(yintercept = CLUSTER_GAP, colour = "#b02418", linetype = "dashed") +
    geom_hline(yintercept = median(u$gap, na.rm = TRUE), colour = "grey60", linetype = "dotted") +
    geom_segment(aes(xend = date, y = 0.7, yend = gap,
                     colour = gap <= CLUSTER_GAP), alpha = 0.55) +
    geom_point(aes(colour = gap <= CLUSTER_GAP), size = 2) +
    scale_y_log10(breaks = c(1, 7, 14, 30, 90, 365, 1000),
                  labels = c("1 d", "1 wk", "2 wk", "1 mo", "3 mo", "1 yr", "~3 yr")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(values = c(`TRUE` = "#b02418", `FALSE` = "#9aa3ab"), guide = "none") +
    labs(x = NULL, y = "Days since previous case\n(log scale)",
         title = "B   How long since the last case? Low points = clustering") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 11))
  
  pA / pB + plot_layout(heights = c(1.25, 1)) +
    plot_annotation(title = "E. coli cases, 2018–2025: timing and clustering",
                    theme = theme(plot.title = element_text(face = "bold", size = 14)))
  
# check the origin of the E. coli cases from 2025 that seemed to cluster
table(df$NOM_CENTRE_SANTE_PROV_E1_C1[df$year==2025&df$pathogen_std=="Escherichia coli"]) # all from CMA. seems like cases an outbreak in the CMA
# check the ward the E. coli cases were from
table(df$SERVICE_HOPITAL_E1_C1[df$year==2025&df$pathogen_std=="Escherichia coli"]) 
# check villages they were from
table(df$COMMUNAUTE_VILLAGE_E1_C1[df$year==2025&df$pathogen_std=="Escherichia coli"]) 
table(df$COMMUNAUTE_VILLAGE_E1_C1[df$year==2018&df$pathogen_std=="Escherichia coli"]) 
table(df$COMMUNAUTE_VILLAGE_E1_C1[df$year==2021&df$pathogen_std=="Escherichia coli"]) 

# check if likely HAI (collection >48h after admission)
df$days_admission_to_collection[!is.na(df$DATE_PRELEVEMENT_E1_C1) & !is.na(df$DATE_ADMISSION_E1_C1)] <- as.numeric(as.Date(df$DATE_PRELEVEMENT_E1_C1[!is.na(df$DATE_PRELEVEMENT_E1_C1) & !is.na(df$DATE_ADMISSION_E1_C1)]) - as.Date(df$DATE_ADMISSION_E1_C1[!is.na(df$DATE_PRELEVEMENT_E1_C1) & !is.na(df$DATE_ADMISSION_E1_C1)]))
df$days_admission_to_collection[is.na(df$DATE_PRELEVEMENT_E1_C1)|is.na(df$DATE_ADMISSION_E1_C1)] <- NA
df$days_admission_to_collection[df$days_admission_to_collection<0] <- NA
df$days_admission_to_collection[df$days_admission_to_collection>100] <- NA

table(df$days_admission_to_collection)
df %>% filter(days_admission_to_collection<0|days_admission_to_collection>61) %>% select(DATE_ADMISSION_E1_C1, DATE_PRELEVEMENT_E1_C1, DATE_DEBUT_PREM_SYMPT_E1_C1, DATE_SIGNATURE_E1_C1)
# distribution of delay until BC collection
df_plot <- df %>%
  filter(!is.na(days_admission_to_collection),
         !is.na(pathogen_group)) %>%
  group_by(pathogen_group) %>%
  filter(n() > 5) %>%
  ungroup()

ggplot(df_plot, aes(x = days_admission_to_collection)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black") +
  facet_wrap(~ pathogen_group, scales = "free_y") +
  labs(
    x = "Days from admission to collection",
    y = "Number of observations",
    title = "Distribution of days from admission to collection by pathogen (>10 observations)"
  ) +
  theme_minimal()

# are the two shigella cases also misclassified E. coli cases?
df %>% filter(pathogen_std=="Shigella spp") %>% select(DATE_ADMISSION_E1_C1, DATE_PRELEVEMENT_E1_C1, ageyears, `Study Subject ID`, COMMUNAUTE_VILLAGE_E1_C1, COMMENTAIRE_E1_C1, SERVICE_HOPITAL_E1_C1)

# check AMR profiles of cases in potential E. coli outbreaks

# only work with E. coli and Shigella cases
ecoli <- df %>%  filter(pathogen_std=="Escherichia coli"|pathogen_std=="Shigella spp") 
# define outbreak windows
df$DATE_PRELEVEMENT_E1_C1 <- as.Date(df$DATE_PRELEVEMENT_E1_C1)   # add format = "%d/%m/%Y" etc. if needed
ecoli$outbreak_period <- "non-outbreak"
ecoli$outbreak_period[ecoli$DATE_PRELEVEMENT_E1_C1>as.Date("2018-05-30") & ecoli$DATE_PRELEVEMENT_E1_C1 < as.Date("2018-10-01")] <- "2018 outbreak"
ecoli$outbreak_period[ecoli$DATE_PRELEVEMENT_E1_C1>as.Date("2021-01-13") & ecoli$DATE_PRELEVEMENT_E1_C1 < as.Date("2021-03-31")] <- "2021 outbreak"
ecoli$outbreak_period[ecoli$DATE_PRELEVEMENT_E1_C1>as.Date("2025-06-17") & ecoli$DATE_PRELEVEMENT_E1_C1 < as.Date("2025-08-12")] <- "2025 outbreak"
print(table(ecoli$outbreak_period))

# check if age different outbreaks vs. background 
age_summary_A <- ecoli %>% 
  group_by(outbreak_period) %>%
  summarise(
    n      = sum(!is.na(ageyears)),
    median = median(ageyears, na.rm = TRUE),
    IQR_lo = quantile(ageyears, .25, na.rm = TRUE),
    IQR_hi = quantile(ageyears, .75, na.rm = TRUE),
    .groups = "drop")
print(age_summary_A)

# Kruskal-Wallis across all 4 groups (non-outbreak + 3 outbreaks)
kw_A <- kruskal.test(ageyears ~ outbreak_period, data = ecoli)
print(kw_A)

# If significant, pairwise Wilcoxon (each outbreak vs. background, Holm-corrected)
if (kw_A$p.value < 0.05) {
  pw_A <- pairwise.wilcox.test(ecoli$ageyears, ecoli$outbreak_period,
                               p.adjust.method = "holm")
  print(pw_A)
}

p_age_A <- ggplot(ecoli, aes(x = outbreak_period, y = ageyears, fill = outbreak_period)) +
  geom_boxplot(outlier.shape = NA, alpha = .5) +
  geom_jitter(width = .15, alpha = .6, size = 1.8) +
  labs(title = "Age by period: outbreaks vs. background",
       x = NULL, y = "Age (years)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
print(p_age_A)
ggsave("age_outbreaks_vs_background.png", p_age_A, width = 7, height = 5, dpi = 150)

# comparing AMR profile
table(ecoli$LAB_RESULTAT_CEFTRIAXONE_BACILLE_E2_C2)
table(ecoli$LAB_RESULTAT_CIPROFLOXACIN_BACILLE_E2_C2)
amr_vars <- c(
  ceftriaxone   = "LAB_RESULTAT_CEFTRIAXONE_BACILLE_E2_C2",
  ciprofloxacin = "LAB_RESULTAT_CIPROFLOXACIN_BACILLE_E2_C2")

# isolate numbers E. coli cases
ecoli_shigella_nrs <- df %>% filter(pathogen_std=="Shigella spp" | pathogen_std=="Escherichia coli") %>%
  select(pathogen_std, outbreak_period, DATE_ADMISSION_E1_C1, DATE_PRELEVEMENT_E1_C1, ageyears, `Study Subject ID`, COMMUNAUTE_VILLAGE_E1_C1, comment_clean, SERVICE_HOPITAL_E1_C1, LAB_RESULTAT_CEFTRIAXONE_BACILLE_E2_C2, LAB_RESULTAT_CIPROFLOXACIN_BACILLE_E2_C2)
ecoli_shigella_nrs
write_xlsx(ecoli_shigella_nrs, "ecoli_shigella_nrsr.xlsx")

# age-sex distr
# function to summarise sex and age summary for any grouping variable
describe_age_sex <- function(data, var) {
  data %>%
    mutate(level = as.character(.data[[var]])) %>%
    group_by(level) %>%
    summarise(
      n            = n(),
      n_female     = sum(sex == "female", na.rm = TRUE),
      pct_female   = round(100 * n_female / sum(!is.na(sex)), 1),
      n_male       = sum(sex == "male", na.rm = TRUE),
      pct_male     = round(100 * n_male / sum(!is.na(sex)), 1),
      sex_missing  = sum(is.na(sex)),
      age_median   = median(ageyears, na.rm = TRUE),
      age_q1       = quantile(ageyears, 0.25, na.rm = TRUE),
      age_q3       = quantile(ageyears, 0.75, na.rm = TRUE),
      age_mean     = round(mean(ageyears, na.rm = TRUE), 1),
      age_sd       = round(sd(ageyears, na.rm = TRUE), 1),
      age_min      = min(ageyears, na.rm = TRUE),
      age_max      = max(ageyears, na.rm = TRUE),
      age_missing  = sum(is.na(ageyears)),
      .groups = "drop"
    ) %>%
    mutate(variable = var, .before = 1)
}
# overall row
overall <- df %>% mutate(overall = "overall") %>% describe_age_sex("overall")

# strat variables
vars <- c("dept", "year", "culture_result", "pathogen_group", "month")

results <- map(vars, ~ describe_age_sex(df, .x)) %>% set_names(vars)

results$department_en   # view one

all_tables <- bind_rows(overall, !!!results)

# Export everything to one csv (or use writexl::write_xlsx(c(list(overall = overall), results), "age_sex_summary.xlsx") for one sheet per grouping)
write.csv(all_tables, "age_sex_summary.csv", row.names = FALSE)
write_xlsx(all_tables, "age_sex_summary.xlsx")

# histogram of age distr
agedistr <- df %>%
  filter(!is.na(ageyears), !is.na(culture_result), culture_result!="Contaminated", culture_result!="Other/Unclear") %>%
  ggplot(aes(x = ageyears)) +
  geom_histogram(binwidth = 2, boundary = 0, closed = "left",
                 fill = "steelblue", colour = "white") +
  facet_wrap(~ culture_result, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  labs(x = "Age (years)", y = "Number of patients",
       title = "Age distribution by culture result") +
  theme_minimal()
ggsave("age_hist_by_culture_per_2_y.png", plot = agedistr, width = 8, height = 6, dpi = 300)

# check if difference in age between pos vs negative cases could be due to more severe malaria among young patients
bact <- df %>%
  filter(culture_result %in% c("Positive", "Negative"),
         !is.na(ageyears)) %>%
  mutate(culture_result = factor(culture_result, levels = c("Negative", "Positive")))

# a) How many patients were tested, and how many were malaria-positive?
bact %>% count(culture_result, malaria) %>%
  group_by(culture_result) %>% mutate(pct = round(100 * n / sum(n), 1))

# b) Age by culture result and malaria status
bact %>% group_by(culture_result, malaria) %>%
  summarise(n = n(), median = median(ageyears),
            q1 = quantile(ageyears, .25), q3 = quantile(ageyears, .75), .groups = "drop")

# c) Median age, culture-negative vs culture-positive, under three conditions
summ <- function(d, label) {
  d %>% group_by(culture_result) %>%
    summarise(n = n(), median = median(ageyears),
              q1 = quantile(ageyears, .25), q3 = quantile(ageyears, .75), .groups = "drop") %>%
    mutate(analysis = label, .before = 1)
}
bind_rows(
  summ(bact,                                              "All"),
  summ(filter(bact, !is.na(malaria)),                     "Tested for malaria"),
  summ(filter(bact, malaria %in% "Negative"),             "Malaria-negative only")) # malaria explains the age difference between positive and negative BC if the median age of culture-negative patients rises towards that of culture-positive patients once malaria-positive patients are removed, i.e. Malaria-negative only

# d) malaria season
df %>%
  filter(!is.na(culture_pos), !is.na(ageyears)) %>%
  group_by(season, culture_result) %>%
  summarise(n = n(), median_age = median(ageyears),
            q1 = quantile(ageyears, .25), q3 = quantile(ageyears, .75), .groups = "drop")
# check if more children in specific months
df %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(n = n(),
            pct_under5     = round(100 * mean(ageyears < 5, na.rm = TRUE), 1),
            pct_malaria_pos = round(100 * mean(malaria == "Positive", na.rm = TRUE), 1)) # more children in Oct to April, against expectations
# malaria positive children less likely to have a positive culture?
df %>%
  filter(!is.na(malaria), !is.na(culture_pos), !is.na(age_group)) %>%
  group_by(age_group, malaria) %>%
  summarise(n = n(), n_pos = sum(culture_pos),
            pct_culture_pos = round(100 * mean(culture_pos), 1), .groups = "drop")

glm(culture_pos ~ age_group + malaria, family = binomial, data = df) %>%
  summary()
# different enrollment criteria for children vs adults?
df %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(n = n(),
            pct_fever  = round(100 * mean(FIEVRE_E1_C1 %in% c(1, "Oui", "oui"), na.rm = TRUE), 1),
            pct_severe = round(100 * mean(SIGNE_GRAVITE_E1_C1 %in% c(1, "Oui", "oui"), na.rm = TRUE), 3))
