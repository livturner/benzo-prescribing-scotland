library(tidyverse)
library(janitor)
library(phsopendata)
library(here)

resources <- list_resources("prescriptions-in-the-community")

View(resources)

# Resource ID for "Data by Board - July - December 2025"
res_id <- "83763a67-9c4d-43d3-89b2-18041a368a1c"

full_data <- get_resource(res_id = res_id)

# Filter to benzos + z-drugs, VMP only to avoid double-counting
benzos <- full_data %>%
  clean_names()%>%
  filter(prescribed_type == "VMP") %>%
  filter(str_starts(bnf_item_code, "0401"))

benzos %>%
  count(bnf_item_description, sort = TRUE) %>%
  View()

benzodiazepines <- c("DIAZEPAM", "LORAZEPAM", "TEMAZEPAM", "NITRAZEPAM",
                     "CHLORDIAZEPOXIDE", "OXAZEPAM", "LORMETAZEPAM",
                     "LOPRAZOLAM", "CLONAZEPAM", "ALPRAZOLAM",
                     "MOGADON")  # MOGADON = nitrazepam brand

z_drugs <- c("ZOPICLONE", "ZOLPIDEM", "ZALEPLON",
             "ZIMOVANE", "LUNIVIA", "ZALZO")  # brands

target_drugs <- c(benzodiazepines, z_drugs)

# Build a regex that matches any of them at the start of the description
drug_pattern <- paste0("^(", paste(target_drugs, collapse = "|"), ")")

# Filter
benzos_clean <- full_data %>%
  clean_names() %>%
  filter(prescribed_type == "VMP") %>%
  filter(str_detect(bnf_item_description, drug_pattern))

benzos_clean %>% 
  count(bnf_item_description, sort = TRUE) %>% 
  print(n = Inf)

benzos_tagged <- benzos_clean %>%
  mutate(
    drug_group = case_when(
      str_detect(bnf_item_description, "CLONAZEPAM") ~ "clonazepam (epilepsy)",
      str_detect(bnf_item_description, "CHLORDIAZEPOXIDE") ~ "chlordiazepoxide (alcohol withdrawal)",
      str_detect(bnf_item_description, "RECTAL|AMPOULES|RECTUBES") ~ "emergency/rescue",
      str_detect(bnf_item_description, paste(z_drugs, collapse = "|")) ~ "z-drug",
      TRUE ~ "anxiolytic/hypnotic benzo"
    )
  )

benzos_tagged %>% 
  count(drug_group, sort = TRUE)

benzos_with_strength <- benzos_tagged %>%
  filter(drug_group %in% c("anxiolytic/hypnotic benzo", "z-drug")) %>%
  mutate(
    # Pull the strength number (mg or microgram)
    strength_raw = str_extract(bnf_item_description,
                               "\\d+\\.?\\d*\\s?(MG|MICROGRAM)"),
    strength_num = as.numeric(str_extract(strength_raw, "\\d+\\.?\\d*")),
    strength_mg = if_else(
      str_detect(strength_raw, "MICROGRAM"),
      strength_num / 1000,
      strength_num
    ),
    # Pull the volume denominator if it's a liquid (e.g. "/5ML" or "/ML")
    volume_raw = str_extract(bnf_item_description, "/\\d*\\.?\\d*\\s?ML"),
    volume_ml = as.numeric(str_extract(volume_raw, "\\d+\\.?\\d*")),
    # If no number before ML (e.g. "1MG/ML"), volume is 1
    volume_ml = if_else(
      str_detect(bnf_item_description, "/ML") & is.na(volume_ml),
      1,
      volume_ml
    ),
    # Final mg per unit dispensed
    # — tablets: 1 unit = 1 tablet, so mg_per_unit = strength_mg
    # — liquids: 1 unit = 1 ml, so mg_per_unit = strength_mg / volume_ml
    mg_per_unit = if_else(
      is.na(volume_ml),
      strength_mg,
      strength_mg / volume_ml
    )
)

benzos_with_strength %>%
  distinct(bnf_item_description, strength_mg, volume_ml, mg_per_unit) %>%
  arrange(bnf_item_description) %>%
  print(n = Inf)

ddd_lookup <- tribble(
  ~drug,              ~ddd_mg,
  "DIAZEPAM",         10,
  "LORAZEPAM",        2.5,
  "TEMAZEPAM",        20,
  "NITRAZEPAM",       5,
  "OXAZEPAM",         50,
  "LORMETAZEPAM",     1,
  "LOPRAZOLAM",       1,
  "ZOPICLONE",        7.5,
  "ZOLPIDEM",         10,
  # Brand-name mappings to their generic equivalents
  "MOGADON",          5,    # nitrazepam
  "ZIMOVANE",         7.5,  # zopiclone
  "ZALZO",            10,   # zolpidem
  "LUNIVIA",          3     # eszopiclone
)

benzos_ddd <- benzos_with_strength %>%
  mutate(drug = str_extract(bnf_item_description, "^[A-Z]+")) %>%
  left_join(ddd_lookup, by = "drug") %>%
  mutate(
    total_mg = paid_quantity * mg_per_unit,
    ddds = total_mg / ddd_mg
  )

benzos_ddd %>% 
  filter(is.na(ddd_mg)) %>% 
  count(bnf_item_description)

benzos_ddd %>% 
  group_by(drug) %>% 
  summarise(total_ddds = sum(ddds, na.rm = TRUE)) %>% 
  arrange(desc(total_ddds))

hb_lookup <- tribble(
  ~hbt,         ~hb_name,
  "S08000015",  "Ayrshire and Arran",
  "S08000016",  "Borders",
  "S08000017",  "Dumfries and Galloway",
  "S08000019",  "Forth Valley",
  "S08000020",  "Grampian",
  "S08000022",  "Highland",
  "S08000024",  "Lothian",
  "S08000025",  "Orkney",
  "S08000026",  "Shetland",
  "S08000028",  "Western Isles",
  "S08000029",  "Fife",
  "S08000030",  "Tayside",
  "S08000031",  "Greater Glasgow and Clyde",
  "S08000032",  "Lanarkshire",
  "SB0806", "Scottish Ambulance Service"
)

hb_totals <- benzos_ddd %>% 
  filter(str_starts(hbt, "S08")) %>%
  group_by(hbt) %>% 
  summarise(total_ddds = sum(ddds, na.rm = TRUE)) %>% 
  left_join(hb_lookup, by = "hbt") %>% 
  arrange(desc(total_ddds))

hb_totals

pop_resources <- list_resources("population-estimates")
View(pop_resources)

pop_res_id <- "27a72cc8-d6d8-430c-8b4f-3109a9ceadb1"

pop_data <- get_resource(res_id = pop_res_id)

pop_data %>% count(Year) %>% tail()

hb_pop <- pop_data %>% 
  clean_names() %>% 
  filter(year == "2024",
         sex == "All",
         str_starts(hb, "S08")) %>%   # exclude Scotland total
  select(hbt = hb, population = all_ages)

hb_rates <- hb_totals %>% 
  left_join(hb_pop, by = "hbt") %>% 
  mutate(
    days_in_period = 184,   # Jul–Dec 2025
    ddds_per_1000_per_day = total_ddds / population / days_in_period * 1000
  ) %>% 
  arrange(desc(ddds_per_1000_per_day))

hb_rates %>% 
  ggplot(aes(x = ddds_per_1000_per_day, 
             y = reorder(hb_name, ddds_per_1000_per_day))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(ddds_per_1000_per_day, 1)), 
            hjust = -0.2, size = 3.5) +
  labs(
    title = "Benzodiazepine and z-drug prescribing by NHS Scotland health board",
    subtitle = "DDDs per 1,000 population per day, July–December 2025",
    x = "DDDs per 1,000 population per day",
    y = NULL,
    caption = "Source: Public Health Scotland, NRS mid-year population estimates"
  ) +
  xlim(0, 22) +
  theme_minimal()
