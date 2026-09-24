# ============================================================================
# ALARUM Nanoro community data
# ============================================================================

install.packages("dplyr")
install.packages("rlang")
install.packages("scales")
install.packages("xfun", type = "binary")
install.packages("coin")
install.packages("RVAideMemoire")
install.packages("vcd")
install.packages("DescTools")


library(dplyr)
library(tidyr)
library(readxl)
library(rlang)
library(gtsummary)
library(ggplot2)
library(xfun)
library(DescTools)
library(tidyverse)
library(coin)


ALARUM_Community_DATA <- read_excel("C:/Users/briems/OneDrive - ITG/Bureaublad/ALARUM/7. Implementation/Data extracts/ALARUM-Community-DATA_Nanoro_FV.xlsx")


ALARUM_menage <- ALARUM_Community_DATA %>%
  filter(is.na(redcap_repeat_instrument)) %>%
  filter(redcap_event_name %in% c("rainy_arm_2", "dry_arm_2")) %>%
  select(
    record_id,
    redcap_event_name,
    num_hh_members_hh, num_children_0_5yrs_hh,
    num_households_hh, num_memb_pres_hh,
    main_drink_water_srce_hh, distance_srce_hh,
    starts_with("water_storage_hh"),
    starts_with("storage_container_hh"),
    starts_with("water_treat_hh"),
    starts_with("toilet_type_hh"),
    toilet_share_hh, toilet_clean_hh,
    starts_with("septic_disposal_hh"),
    starts_with("child_defecate_hh"),
    starts_with("child_feaces_dispose_hh"),
    hand_wash_hh,
    starts_with("hand_wash_times_hh"),
    starts_with("hand_wash_mtrl_hh"),
    animals_kept_hh,
    starts_with("animal"),
    starts_with("place_"),
    animal_excrement_hh, 
  )

# ============================================================================
# 1. DRINKING WATER DESCRIPTIVE AND SEASONALITY
# ============================================================================
# Sources :
#   1 = Household tap
#   2 = Public tap
#   3 = Borehole
#   4 = Protected well
#   5 = Unprotected well            
#   6 = Rainwater                  
#   7 = Surface water                 
#   8 = Bottled/sachet water
#   9 = Water trucking                 -> 0 obs
#
# Distance :
#   1 = on premises
#   2 = < 100 m   |  3 = 100-500 m   
#   4 = 500m-1km  |  5 = > 1 km     



# ============================================================================
# 1.1 Drinking water variables (proportions)
# ============================================================================
# Calculate proportions for source, distance, storage and treatment

water_proportions <- ALARUM_menage %>%
  filter(!is.na(redcap_event_name), 
         !is.na(main_drink_water_srce_hh),
         !is.na(distance_srce_hh), 
         !is.na(storage_container_hh___0), 
         !is.na(storage_container_hh___1),
         !is.na(storage_container_hh___2),
         !is.na(storage_container_hh___3)) %>%
  
  group_by(redcap_event_name, main_drink_water_srce_hh) %>%
  tally() %>%
  mutate(
    Total_In_Strata = sum(n),
    Percentage = round((n / Total_In_Strata) * 100, 1)
  )
print(water_proportions)

distance_proportions <- ALARUM_menage %>%
  filter(!is.na(redcap_event_name),
         !is.na(distance_srce_hh)) %>%
  count(redcap_event_name, distance_srce_hh) %>%
  group_by(redcap_event_name) %>%
  mutate(
    Total_In_Strata = sum(n),
    Percentage = round(100 * n / Total_In_Strata, 1)
  ) %>%
  ungroup()

print(distance_proportions)

storage_proportions <- ALARUM_menage %>%
  select(
    record_id,
    redcap_event_name,
    starts_with("water_storage_hh")
  ) %>%
  pivot_longer(
    cols = starts_with("water_storage_hh"),
    names_to = "storage_type",
    values_to = "selected"
  ) %>%
  group_by(redcap_event_name, storage_type) %>%
  summarise(
    N = sum(selected == 1, na.rm = TRUE),
    Total = n_distinct(record_id),
    Percentage = round(100 * N / Total, 1),
    .groups = "drop"
  )

print(storage_proportions)

storagequality_proportions <- ALARUM_menage %>%
  select(
    record_id,
    redcap_event_name,
    starts_with("storage_container_hh")
  ) %>%
  pivot_longer(
    cols = starts_with("storage_container_hh"),
    names_to = "storage_quality",
    values_to = "selected"
  ) %>%
  group_by(redcap_event_name, storage_quality) %>%
  summarise(
    N = sum(selected == 1, na.rm = TRUE),
    Total = n_distinct(record_id),
    Percentage = round(100 * N / Total, 1),
    .groups = "drop"
  )

print(storagequality_proportions)

treatment_proportions <- ALARUM_menage %>%
  select(
    record_id,
    redcap_event_name,
    starts_with("water_treat_hh")
  ) %>%
  pivot_longer(
    cols = starts_with("water_treat_hh"),
    names_to = "treatment",
    values_to = "selected"
  ) %>%
  group_by(redcap_event_name, treatment) %>%
  summarise(
    N = sum(selected == 1, na.rm = TRUE),
    Total = n_distinct(record_id),
    Percentage = round(100 * N / Total, 1),
    .groups = "drop"
  )

print(treatment_proportions)

# ============================================================================
# 1.2 Test differences in drinking water source/distance due to seasonality
# ============================================================================
# Test difference in main water source #

water_wide <- ALARUM_menage %>%
  select(record_id, redcap_event_name, main_drink_water_srce_hh) %>%
  pivot_wider(
    names_from = redcap_event_name,
    values_from = main_drink_water_srce_hh
  )

all_levels <- sort(unique(c(
  as.character(water_wide$dry_arm_2),
  as.character(water_wide$rainy_arm_2)
)))

water_wide$dry <- factor(water_wide$dry_arm_2, levels = all_levels)
water_wide$rainy <- factor(water_wide$rainy_arm_2, levels = all_levels)

tab <- table(
  water_wide$dry,
  water_wide$rainy,
  dnn = c("dry_arm_2", "rainy_arm_2")
)

tab
dim(tab)
StuartMaxwellTest(tab)

# Test difference in distance to water source #

distance_wide <- ALARUM_menage %>%
  select(
    record_id,
    redcap_event_name,
    distance_srce_hh
  ) %>%
  filter(!is.na(distance_srce_hh)) %>%
  pivot_wider(
    id_cols = record_id,
    names_from = redcap_event_name,
    values_from = distance_srce_hh
  )

head(distance_wide)
distance_wide %>%
  filter(is.na(dry_arm_2) | is.na(rainy_arm_2))

tab2 <- table(
  distance_wide$dry_arm_2,
  distance_wide$rainy_arm_2
)

StuartMaxwellTest(tab2)

# Test difference in container coverage between seasons # 

storage_wide <- ALARUM_menage %>%
  select(
    record_id,
    redcap_event_name,
    storage_container_hh___0
  ) %>%
  pivot_wider(
    names_from = redcap_event_name,
    values_from = storage_container_hh___0
  )

tab3 <- table(
  storage_wide$dry_arm_2,
  storage_wide$rainy_arm_2
)
tab3
mcnemar.test(tab3)

#Test difference in treatment (binary y/n) between seasons 

treat_wide <- ALARUM_menage %>%
  select(
    record_id,
    redcap_event_name,
    water_treat_hh_2___0
  ) %>%
  pivot_wider(
    names_from = redcap_event_name,
    values_from = water_treat_hh_2___0
  )

treat_wide <- treat_wide %>%
  mutate(
    any_treatment_dry = dry_arm_2 == 0,
    any_treatment_rainy = rainy_arm_2 == 0
  )
treat_wide <- treat_wide %>%
  mutate(
    any_treatment_dry = factor(
      any_treatment_dry,
      levels = c(FALSE, TRUE)
    ),
    any_treatment_rainy = factor(
      any_treatment_rainy,
      levels = c(FALSE, TRUE)
    )
  )
tab4 <- table(
  treat_wide$any_treatment_dry,
  treat_wide$any_treatment_rainy
)
tab4
mcnemar.test(tab3)

# ============================================================================
# 1.3 JMP service ladder 
# ============================================================================

ALARUM_menage <- ALARUM_menage %>%
  mutate(
    jmp_water = case_when(
      main_drink_water_srce_hh == 5                                  ~ "Unimproved",
      main_drink_water_srce_hh == 7                                  ~ "Surface water",
      main_drink_water_srce_hh == 8                                  ~ "Safely managed",
      main_drink_water_srce_hh == 6                                  ~ "Basic",
      main_drink_water_srce_hh %in% c(2, 3, 4) &
        distance_srce_hh %in% c(2, 3)                               ~ "Basic",
      main_drink_water_srce_hh %in% c(2, 3, 4) &
        distance_srce_hh %in% c(4, 5)                               ~ "Limited",
      main_drink_water_srce_hh %in% c(1, 2, 3, 4) &
        distance_srce_hh %in% c(1)                                  ~ "Safely managed",
      TRUE ~ NA_character_
    ),
    jmp = factor(
      jmp_water,
      levels = c("Safely managed", "Basic", "Limited", "Unimproved", "Surface water")
    )
  )

table_JMP <- table(ALARUM_menage$jmp_water, ALARUM_menage$redcap_event_name)
print("--- Classification JMP - Eau potable ---")
print(table_JMP)
print(round(prop.table(table_JMP, margin = 2) * 100, 1))


# Test differences in JMP classification between seasons #

water_wide2 <- ALARUM_menage %>%
  select(record_id, redcap_event_name, jmp_water) %>%
  pivot_wider(
    names_from = redcap_event_name,
    values_from = jmp_water,
    id_cols = record_id
  ) %>%
  rename(
    jmp_dry = dry_arm_2,
    jmp_rainy = rainy_arm_2
  )

cat("Number of households with both seasons:", 
    sum(!is.na(water_wide2$jmp_dry) & !is.na(water_wide2$jmp_rainy)), "\n")

# Create numeric scores (ordered from best to worst water access) #
water_wide2 <- water_wide2 %>%
  mutate(
    jmp_dry_score = case_when(
      jmp_dry == "Safely managed" ~ 5,
      jmp_dry == "Basic" ~ 4,
      jmp_dry == "Limited" ~ 3,
      jmp_dry == "Unimproved" ~ 2,
      jmp_dry == "Surface water" ~ 1,
      TRUE ~ NA_real_
    ),
    jmp_rainy_score = case_when(
      jmp_rainy == "Safely managed" ~ 5,
      jmp_rainy == "Basic" ~ 4,
      jmp_rainy == "Limited" ~ 3,
      jmp_rainy == "Unimproved" ~ 2,
      jmp_rainy == "Surface water" ~ 1,
      TRUE ~ NA_real_
    )
  )

# Remove households missing either season
water_paired <- water_wide2 %>%
  filter(!is.na(jmp_dry_score) & !is.na(jmp_rainy_score))

n_pairs <- nrow(water_paired)
cat("Number of paired observations:", n_pairs, "\n\n")

# Wilcoxon Signed-Rank Test
wilcox_result <- wilcox.test(water_paired$jmp_dry_score, 
                             water_paired$jmp_rainy_score,
                             paired = TRUE,
                             alternative = "less")

print(wilcox_result)

# Effect size (r = Z / sqrt(N))
z_score <- qnorm(wilcox_result$p.value / 2)  # Convert p-value to z-score
effect_size_r <- abs(z_score) / sqrt(n_pairs)
cat("\nEffect size (r):", effect_size_r, "\n")
cat("Interpretation: ", 
    if(effect_size_r < 0.1) "negligible"
    else if(effect_size_r < 0.3) "small"
    else if(effect_size_r < 0.5) "medium"
    else "large", "\n")

table(water_paired$jmp_dry, water_paired$jmp_rainy)
diff <- water_paired$jmp_rainy_score -
  water_paired$jmp_dry_score

table(sign(diff))

tab5 <- table(
  water_paired$jmp_dry_score,
  water_paired$jmp_rainy_score
)
tab5
dim(tab5)
levels_jmp <- 1:5

tab5 <- table(
  factor(water_paired$jmp_dry_score, levels = levels_jmp),
  factor(water_paired$jmp_rainy_score, levels = levels_jmp)
)

tab5

StuartMaxwellTest(tab5)

# Changes in JMP ladder (number of households) #

cat("\n=== CROSS-TABULATION: DRY × RAINY ===\n")
cross_tab <- table(Dry = water_paired$jmp_dry, 
                   Rainy = water_paired$jmp_rainy)
print(cross_tab)

changes <- sum(water_paired$jmp_dry != water_paired$jmp_rainy)
cat("\nHouseholds that changed JMP category:", changes, "/", n_pairs, 
    "(" , round(100*changes/n_pairs, 1), "%)\n")

improved <- sum(water_paired$jmp_dry_score < water_paired$jmp_rainy_score)
worsened <- sum(water_paired$jmp_dry_score > water_paired$jmp_rainy_score)
unchanged <- sum(water_paired$jmp_dry_score == water_paired$jmp_rainy_score)

cat("Improved (dry→rainy):", improved, "\n")
cat("Worsened (dry→rainy):", worsened, "\n")
cat("Unchanged:", unchanged, "\n")

# JMP Visual 

# Distribution by season
dist_data <- water_paired %>%
  pivot_longer(
    cols = c(jmp_dry, jmp_rainy),
    names_to = "season",
    names_pattern = "jmp_(.*)",
    values_to = "category"
  ) %>%
  filter(!is.na(category))

jmp_order <- c(
  "Safely managed",
  "Basic",
  "Limited",
  "Unimproved",
  "Surface water"
)

dist_data <- dist_data %>%
  mutate(
    category = factor(category,
                      levels = jmp_order,
                      ordered = TRUE)
  )

p_dist <- ggplot(dist_data, aes(x = category, fill = season)) +
  geom_bar(position = "dodge") +
  labs(title = "JMP Water Ladder Distribution by Season",
       x = "Drinking water service level",
       y = "Number of Households",
       fill = "Season") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_dist)


# ============================================================================
# 2. Bivariate analysis: factors associated with water access & health
# ============================================================================

# ============================================================================
# 2.1. Demographic variables x JMP, source and distance
# ============================================================================

# Descriptive statistics per JMP category: mean, median and SD #

demog_by_jmp <- ALARUM_menage %>%
  mutate(
    jmp_water = factor(jmp_water,
                       levels = jmp_order,
                       ordered = TRUE)
  ) %>%
  group_by(jmp_water)%>%
  summarise(
    n=n(),
    
    mean_hh_compound = mean(num_households_hh, na.rm = TRUE),
    median_hh_compound = median(num_households_hh, na.rm = TRUE),
    sd_hh_compound = sd(num_households_hh, na.rm = TRUE),
    
    mean_hh_members = mean(num_hh_members_hh, na.rm = TRUE),
    median_hh_members = median(num_hh_members_hh, na.rm = TRUE),
    sd_hh_members = sd(num_hh_members_hh, na.rm = TRUE),
    
    mean_children_u5 = mean(num_children_0_5yrs_hh, na.rm = TRUE),
    median_children_u5 = median(num_children_0_5yrs_hh, na.rm = TRUE),
    sd_children_u5 = sd(num_children_0_5yrs_hh, na.rm = TRUE)
  )

print(demog_by_jmp)

# Test difference in distribution of demographic variables between JMP categories (Kruskal-Wallis tests) #

# Test 1: Households in compound × JMP category #
kw_households_dry <- kruskal.test(num_households_hh ~ jmp_water, 
                              data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Households in compound by JMP water category in dry season:")
print(kw_households_dry)

kw_households_rainy <- kruskal.test(num_households_hh ~ jmp_water, 
                                  data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Households in compound by JMP water category in rainy season:")
print(kw_households_rainy)

# Test 2: Household members × JMP category #
kw_members_dry <- kruskal.test(num_hh_members_hh ~ jmp_water, 
                           data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Household members by JMP water category in dry season:")
print(kw_members_dry)

kw_members_rainy <- kruskal.test(num_hh_members_hh ~ jmp_water, 
                               data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Household members by JMP water category in rainy season:")
print(kw_members_rainy)

# visual
p <- ggplot(ALARUM_menage %>%
              filter(redcap_event_name == "rainy_arm_2") %>%
              mutate(
                 jmp_water = factor(jmp_water,
                                    levels = jmp_order,
                                    ordered = TRUE)
               ),
             aes(x = jmp_water, y = num_hh_members_hh, fill = jmp_water)) +
  geom_boxplot() +
  labs(title = "Household size by JMP Water Category in rainy season",
       x = "JMP Water Category",
       y = "Number of Household Members") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p)

# Test 3: Children under 5 × JMP category #
kw_children_dry <- kruskal.test(num_children_0_5yrs_hh ~ jmp_water, 
                            data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Children under 5 by JMP water category in dry season:")
print(kw_children_dry)

kw_children_rainy <- kruskal.test(num_children_0_5yrs_hh ~ jmp_water, 
                                data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Children under 5 by JMP water category in rainy season:")
print(kw_children_rainy)

# Test difference between demographic variables and water source (Kruskal-Wallis tests) #

# Test 1: Households in compound × source type #
kw_hhxsource_dry <- kruskal.test(num_households_hh ~ main_drink_water_srce_hh, 
                                  data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Households in compound by water source in dry season:")
print(kw_hhxsource_dry)

kw_hhxsource_rainy <- kruskal.test(num_households_hh ~ main_drink_water_srce_hh, 
                                    data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Households in compound by water source in rainy season:")
print(kw_hhxsource_rainy)

# Test 2: Household members × source type #
kw_membersxsource_dry <- kruskal.test(num_hh_members_hh ~ main_drink_water_srce_hh, 
                               data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Household members by source in dry season:")
print(kw_membersxsource_dry)

kw_membersxsource_rainy <- kruskal.test(num_hh_members_hh ~ main_drink_water_srce_hh, 
                                 data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Household members by source in rainy season:")
print(kw_membersxsource_rainy)

# visual
p2 <- ggplot(ALARUM_menage %>%
            filter(redcap_event_name == "rainy_arm_2")%>%
            mutate(
               main_drink_water_srce_hh = factor(main_drink_water_srce_hh)
             ),
            aes(x = main_drink_water_srce_hh, y = num_hh_members_hh, fill = main_drink_water_srce_hh)) +
  geom_boxplot() +
  labs(title = "Household size by type of water source in rainy season",
       x = "Type of source",
       y = "Number of Household Members") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p2)

# Test 3: Children under 5 × source type #
kw_childrenxsource_dry <- kruskal.test(num_children_0_5yrs_hh ~ main_drink_water_srce_hh, 
                                data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Children under 5 by source type in dry season:")
print(kw_childrenxsource_dry)

kw_childrenxsource_rainy <- kruskal.test(num_children_0_5yrs_hh ~ main_drink_water_srce_hh, 
                                  data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Children under 5 by source type in rainy season:")
print(kw_childrenxsource_rainy)

# Test difference in demographic variables according to distance (Kruskal-Wallis tests) #

# Test 1: Households in compound × distance #
kw_hhxdist_dry <- kruskal.test(num_households_hh ~ distance_srce_hh, 
                                 data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Households in compound by distance in dry season:")
print(kw_hhxdist_dry)

kw_hhxdist_rainy <- kruskal.test(num_households_hh ~ distance_srce_hh, 
                                   data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Households in compound by distance in rainy season:")
print(kw_hhxdist_rainy)

# Test 2: Household members × distance #
kw_membersxdist_dry <- kruskal.test(num_hh_members_hh ~ distance_srce_hh, 
                                      data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Household members by distance in dry season:")
print(kw_membersxdist_dry)

kw_membersxdist_rainy <- kruskal.test(num_hh_members_hh ~ distance_srce_hh, 
                                        data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Household members by distance in rainy season:")
print(kw_membersxdist_rainy)

# Test 3: Children under 5 × distance #
kw_childrenxdist_dry <- kruskal.test(num_children_0_5yrs_hh ~ distance_srce_hh, 
                                       data = subset(ALARUM_menage, redcap_event_name == "dry_arm_2"))
print("Children under 5 by distance in dry season:")
print(kw_childrenxdist_dry)

# visual
p3 <- ggplot(ALARUM_menage %>%
               filter(redcap_event_name == "dry_arm_2")%>%
               mutate(
                 distance_srce_hh = factor(
                   distance_srce_hh, 
                   levels = c(1,2,3,4,5), 
                   labels = c("On premises", "<100m", "100-500m", "500m-1km", "> 1 km"), 
                   ordered = TRUE)
               ),
             aes(x = distance_srce_hh, y = num_children_0_5yrs_hh, fill = distance_srce_hh)) +
  geom_boxplot() +
  labs(title = "Number of children <5y by distance to water source (dry season)",
       x = "Distance",
       y = "Number of children <5y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p3)

ggplot(
  ALARUM_menage %>%
    mutate(
      distance_srce_hh = factor(
        distance_srce_hh,
        levels = c(1,2,3,4,5),
        labels = c(
          "On premises",
          "<100 m",
          "100–500 m",
          "500 m–1 km",
          ">1 km"
        )
      )
    ),
  aes(
    x = distance_srce_hh,
    y = num_children_0_5yrs_hh,
    fill = distance_srce_hh
  )
) +
  geom_boxplot() +
  facet_wrap(~ redcap_event_name) +
  theme_minimal()

kw_childrenxdist_rainy <- kruskal.test(num_children_0_5yrs_hh ~ distance_srce_hh, 
                                         data = subset(ALARUM_menage, redcap_event_name == "rainy_arm_2"))
print("Children under 5 by distance in rainy season:")
print(kw_childrenxdist_rainy)

# =================================================================================
# 2.2. Seasonality in health care seeking and gastro-int symptoms 
# =================================================================================

# Health care seeking episodes # 

care <- ALARUM_Community_DATA %>%
  select(
    record_id,
    redcap_event_name,
    care_seeking_times_cse
  ) %>%
  mutate(
    care_seeking_times_cse =
      tidyr::replace_na(care_seeking_times_cse, 0)
  ) %>%
  group_by(record_id, redcap_event_name) %>%
  summarise(
    care_seeking_times_cse =
      max(care_seeking_times_cse, na.rm = TRUE),
    .groups = "drop"
  )

care_wide <- care %>%
  tidyr::pivot_wider(
    names_from = redcap_event_name,
    values_from = care_seeking_times_cse
  )

wilcox.test(
  care_wide$dry_arm_2,
  care_wide$rainy_arm_2,
  paired = TRUE
)

diff <- care_wide$rainy_arm_2 - care_wide$dry_arm_2
table(sign(diff))

# gastro-intestinal symptoms: stomach ache, diarrhea, vomiting # 

symptoms <- ALARUM_Community_DATA %>%
  select(
    record_id,
    redcap_event_name,
    symptoms_cse___6,
    symptoms_cse___7,
    symptoms_cse___8
  ) %>%
  mutate(
    across(
      starts_with("symptoms_cse___"),
      ~ tidyr::replace_na(., 0)
    )
  ) %>%
  group_by(record_id, redcap_event_name) %>%
  summarise(
    symptoms_cse___6 = max(symptoms_cse___6, na.rm = TRUE),
    symptoms_cse___7 = max(symptoms_cse___7, na.rm = TRUE),
    symptoms_cse___8 = max(symptoms_cse___8, na.rm = TRUE),
    symptoms_total =
      max(symptoms_cse___6, na.rm = TRUE) +
      max(symptoms_cse___7, na.rm = TRUE) +
      max(symptoms_cse___8, na.rm = TRUE),
    .groups = "drop"
  )
symptoms_wide <- symptoms %>%
  select(
    record_id,
    redcap_event_name,
    symptoms_total
  ) %>%
  pivot_wider(
    names_from = redcap_event_name,
    values_from = symptoms_total
  )
wilcox.test(
  symptoms_wide$dry_arm_2,
  symptoms_wide$rainy_arm_2,
  paired = TRUE
)
diff <- symptoms_wide$rainy_arm_2 - symptoms_wide$dry_arm_2

symptom6_wide <- symptoms %>%
  select(
    record_id,
    redcap_event_name,
    symptoms_cse___6
  ) %>%
  pivot_wider(
    names_from = redcap_event_name,
    values_from = symptoms_cse___6
  )

mcnemar.test(
  table(
    symptom6_wide$dry_arm_2,
    symptom6_wide$rainy_arm_2
  )
)
symptom7_wide <- symptoms %>%
  select(
    record_id,
    redcap_event_name,
    symptoms_cse___7
  ) %>%
  pivot_wider(
    names_from = redcap_event_name,
    values_from = symptoms_cse___7
  )

mcnemar.test(
  table(
    symptom7_wide$dry_arm_2,
    symptom7_wide$rainy_arm_2
  )
)
symptom8_wide <- symptoms %>%
  select(
    record_id,
    redcap_event_name,
    symptoms_cse___8
  ) %>%
  pivot_wider(
    names_from = redcap_event_name,
    values_from = symptoms_cse___8
  )

mcnemar.test(
  table(
    symptom8_wide$dry_arm_2,
    symptom8_wide$rainy_arm_2
  )
)

# ==================================================================================================
# 2.3. Differences in health care seeking and gastro-int symptoms across socio-demo & WASH variables
# ==================================================================================================

# Care seeking episodes and Socio-demographic #

ALARUM_menage <- ALARUM_menage %>%
  select(-matches("^care_seeking_times_cse"))

ALARUM_menage <- left_join(
  ALARUM_menage,
  care,
  by = c("record_id", "redcap_event_name")
)
# dry season #
dry_data <- ALARUM_menage %>%
  filter(redcap_event_name == "dry_arm_2")

cor.test(
  dry_data$care_seeking_times_cse,
  dry_data$num_households_hh,
  method = "spearman"
)

cor.test(
  dry_data$care_seeking_times_cse,
  dry_data$num_hh_members_hh,
  method = "spearman"
)

cor.test(
  dry_data$care_seeking_times_cse,
  dry_data$num_children_0_5yrs_hh,
  method = "spearman"
)

# Rainy season #
rainy_data <- ALARUM_menage %>%
  filter(redcap_event_name == "rainy_arm_2")

cor.test(
  rainy_data$care_seeking_times_cse,
  rainy_data$num_households_hh,
  method = "spearman"
)

cor.test(
  rainy_data$care_seeking_times_cse,
  rainy_data$num_hh_members_hh,
  method = "spearman"
)

cor.test(
  rainy_data$care_seeking_times_cse,
  rainy_data$num_children_0_5yrs_hh,
  method = "spearman"
)

# gatro-intestinal symptoms and socio-demographic #

symptoms <- ALARUM_Community_DATA %>%
  select(
    record_id,
    redcap_event_name,
    symptoms_cse___6,
    symptoms_cse___7,
    symptoms_cse___8
  ) %>%
  mutate(
    across(
      starts_with("symptoms_cse___"),
      ~ tidyr::replace_na(., 0)
    )
  ) %>%
  group_by(record_id, redcap_event_name) %>%
  summarise(
    symptoms_total =
      max(symptoms_cse___6, na.rm = TRUE) +
      max(symptoms_cse___7, na.rm = TRUE) +
      max(symptoms_cse___8, na.rm = TRUE),
    .groups = "drop"
  )

ALARUM_menage <- ALARUM_menage %>%
  select(-any_of("symptoms_total")) %>%
  left_join(symptoms,
            by = c("record_id", "redcap_event_name")
  )
dry_data <- ALARUM_menage %>%
  filter(redcap_event_name == "dry_arm_2")

cor.test(
  dry_data$symptoms_total,
  dry_data$num_households_hh,
  method = "spearman"
)

cor.test(
  dry_data$symptoms_total,
  dry_data$num_hh_members_hh,
  method = "spearman"
)

cor.test(
  dry_data$symptoms_total,
  dry_data$num_children_0_5yrs_hh,
  method = "spearman"
)

# Rainy season
rainy_data <- ALARUM_menage %>%
  filter(redcap_event_name == "rainy_arm_2")

cor.test(
  rainy_data$symptoms_total,
  rainy_data$num_households_hh,
  method = "spearman"
)

cor.test(
  rainy_data$symptoms_total,
  rainy_data$num_hh_members_hh,
  method = "spearman"
)

cor.test(
  rainy_data$symptoms_total,
  rainy_data$num_children_0_5yrs_hh,
  method = "spearman"
)

# Care seeking episodes and JMP #


rainy_data <- ALARUM_menage %>%
  filter(redcap_event_name == "rainy_arm_2") %>%
  mutate(
    jmp_water = factor(
      jmp_water,
      levels = jmp_order,
      ordered = TRUE
    )
  )
kruskal.test(care_seeking_times_cse ~ jmp_water, data = rainy_data)

dry_data <- ALARUM_menage %>%
  filter(redcap_event_name == "dry_arm_2") %>%
  mutate(
    jmp_water = factor(
      jmp_water,
      levels = jmp_order,
      ordered = TRUE
    )
  )
kruskal.test(care_seeking_times_cse ~ jmp_water, data = dry_data)

# gastro-intestinal symptoms and JMP #

rainy_data <- ALARUM_menage %>%
  filter(redcap_event_name == "rainy_arm_2") %>%
  mutate(
    jmp_water = factor(
      jmp_water,
      levels = jmp_order,
      ordered = TRUE
    )
  )
kruskal.test(
  symptoms_total ~ jmp_water,
  data = rainy_data)

dry_data <- ALARUM_menage %>%
  filter(redcap_event_name == "dry_arm_2") %>%
  mutate(
    jmp_water = factor(
      jmp_water,
      levels = jmp_order,
      ordered = TRUE
    )
  )
kruskal.test(
  symptoms_total ~ jmp_water,
  data = dry_data)

cor.test(
  as.numeric(dry_data$jmp_water),
  dry_data$symptoms_total,
  method = "spearman"
)
dry_data %>%
  group_by(jmp_water) %>%
  summarise(
    n = n(),
    median_symptoms = median(symptoms_total, na.rm = TRUE),
    IQR = IQR(symptoms_total, na.rm = TRUE)
  )
ggplot(
  dry_data,
  aes(x = jmp_water, y = symptoms_total, fill = jmp_water)
) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none")

# Care seeking episodes and water source/distance? #

dry_data <- ALARUM_menage %>%
  filter(redcap_event_name == "dry_arm_2")

kruskal.test(
  care_seeking_times_cse ~ main_drink_water_srce_hh,
  data = dry_data
)
rainy_data <- ALARUM_menage %>%
  filter(redcap_event_name == "rainy_arm_2")

kruskal.test(
  care_seeking_times_cse ~ main_drink_water_srce_hh,
  data = rainy_data
)

kruskal.test(
  care_seeking_times_cse ~ distance_srce_hh,
  data = dry_data
)
kruskal.test(
  care_seeking_times_cse ~ distance_srce_hh,
  data = rainy_data
)

# care seeking episodes and storage practice (covered, uncovered) #

dry_data <- ALARUM_menage %>%
  filter(redcap_event_name == "dry_arm_2")

kruskal.test(
  care_seeking_times_cse ~ storage_container_hh___0,
  data = dry_data
)
rainy_data <- ALARUM_menage %>%
  filter(redcap_event_name == "rainy_arm_2")

kruskal.test(
  care_seeking_times_cse ~ storage_container_hh___0,
  data = rainy_data
)

# gastro-intestinal symptoms and water source/distance # 

dry_data <- ALARUM_menage %>%
  filter(redcap_event_name == "dry_arm_2")

kruskal.test(
  symptoms_total ~ main_drink_water_srce_hh,
  data = dry_data
)

rainy_data <- ALARUM_menage %>%
  filter(redcap_event_name == "rainy_arm_2")

kruskal.test(
  symptoms_total ~ main_drink_water_srce_hh,
  data = rainy_data
)

kruskal.test(
  symptoms_total ~ distance_srce_hh,
  data = dry_data
)
kruskal.test(
  symptoms_total ~ distance_srce_hh,
  data = rainy_data
)

cor.test(
  dry_data$distance_srce_hh,
  dry_data$symptoms_total,
  method = "spearman"
)

# boxplot visual of symptoms and distance #

ggplot(
  dry_data %>%
    mutate(
      distance_srce_hh = factor(
        distance_srce_hh,
        levels = c(1,2,3,4,5),
        labels = c(
          "On premises",
          "<100 m",
          "100-500 m",
          "500 m-1 km",
          ">1 km"
        )
      )
    ),
  aes(x = distance_srce_hh, y = symptoms_total)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  theme_minimal() +
  labs(
    x = "Distance to water source (dry season)",
    y = "Total symptoms"
  )

# symptoms and container coverage # 

dry_data <- dry_data %>%
  mutate(
    storage_container_hh___0 = factor(
      storage_container_hh___0,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )
wilcox.test(
  symptoms_total ~ storage_container_hh___0,
  data = dry_data
)

rainy_data <- rainy_data %>%
  mutate(
    storage_container_hh___0 = factor(
      storage_container_hh___0,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )
wilcox.test(
  symptoms_total ~ storage_container_hh___0,
  data = rainy_data
)

# ===============================================================================
# 3. Multivariate analysis: factors associated with water contamination & health
# ===============================================================================

# 3.1. Regression with health outcomes

# Binary symptoms: any gastro-intestinal symptoms vs 0 symptoms #

analysis_data <- ALARUM_menage %>%
  mutate(
    symptoms_any = symptoms_total > 0
  )

analysis_data <- analysis_data %>%
  mutate(
    main_drink_water_srce_hh = factor(
      main_drink_water_srce_hh,
      levels = c(1,2,3,4,5,6,7,8),
      labels = c(
        "Piped on premise",
        "Public tap",
        "Borehole",
        "Protected well",
        "Unprotected well",
        "rainwater",
        "Surface water",
        "bottled/sachet"
      )
    )
  )

analysis_data <- analysis_data %>%
  mutate(
    jmp_group = case_when(
      jmp_water %in% c("Safely managed", "Basic") ~ "Improved",
      TRUE ~ "Less improved"
    )
  )

library(lme4)

# symptoms model with significant (<0.1) bivariate variables - not JMP #
modelsym1 <- glmer(
  symptoms_any ~
    num_hh_members_hh +
    num_children_0_5yrs_hh +
    redcap_event_name +
    main_drink_water_srce_hh +
    distance_srce_hh +
    (1 | record_id),
  family = binomial,
  data = analysis_data
)
summary(modelsym1)

# model with JMP # 

modelsym2 <- glmer(
  symptoms_any ~
    redcap_event_name +
    jmp_group +
    (1 | record_id),
  family = binomial,
  data = analysis_data
)
summary(modelsym2)

exp(-1.0436)
exp(0.9924)


# binary care seeking episodes: any care seeking vs 0 care seeking 

analysis_data <- ALARUM_menage %>%
  mutate(
    careseeking_any = care_seeking_times_cse > 0
  )

analysis_data <- analysis_data %>%
  mutate(
    main_drink_water_srce_hh = factor(
      main_drink_water_srce_hh,
      levels = c(1,2,3,4,5,6,7,8),
      labels = c(
        "Piped on premise",
        "Public tap",
        "Borehole",
        "Protected well",
        "Unprotected well",
        "rainwater",
        "Surface water",
        "bottled/sachet"
      )
    )
  )

modelcs1 <- glmer(
  careseeking_any ~
    num_households_hh +
    num_hh_members_hh +
    redcap_event_name +
    main_drink_water_srce_hh +
    distance_srce_hh +
    storage_container_hh___0 +
    (1 | record_id),
  family = binomial,
  data = analysis_data
)
summary(modelcs1)

modelcs2 <-glmer(
  careseeking_any ~
    num_households_hh +
    redcap_event_name +
    (1 | record_id),
  family = binomial,
  data = analysis_data
)
summary(modelcs2)
exp(-1.6426)

#################################################################################

