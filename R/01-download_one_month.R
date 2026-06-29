library(tidyverse)
library(janitor)
library(phsopendata)
library(here)
library(readxl)

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

hb_rates %>% 
  mutate(
    hb_name = fct_reorder(hb_name, ddds_per_1000_per_day),
    # Tag top/bottom for highlighting
    highlight = case_when(
      ddds_per_1000_per_day >= 17 ~ "high",
      ddds_per_1000_per_day <= 10 ~ "low",
      TRUE ~ "mid"
    )
  ) %>% 
  ggplot(aes(x = ddds_per_1000_per_day, y = hb_name, fill = highlight)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f", ddds_per_1000_per_day)),
            hjust = -0.2, size = 3.3, colour = "grey30") +
  scale_fill_manual(values = c("high" = "#B33A3A", 
                               "mid" = "#7B9CC4", 
                               "low" = "#4A6FA5"),
                    guide = "none") +
  scale_x_continuous(limits = c(0, 22), expand = c(0, 0)) +
  labs(
    title = "Benzodiazepine and z-drug prescribing varies more than two-fold\nacross NHS Scotland health boards",
    subtitle = "Defined daily doses per 1,000 population per day, July–December 2025",
    x = NULL, y = NULL,
    caption = "Source: Public Health Scotland prescribing data; National Records of Scotland mid-year population estimates"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, lineheight = 1.2),
    plot.subtitle = element_text(colour = "grey40", margin = margin(b = 12)),
    plot.caption = element_text(colour = "grey50", size = 8, hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(colour = "grey20"),
    axis.text.x = element_text(colour = "grey40")
  )

# Look at all "Data by Board" resources, sorted by date
pres_resources <- list_resources("prescriptions-in-the-community")

pres_resources %>% 
  filter(str_detect(resource_name, "Data by Board")) %>% 
  arrange(resource_name) %>% 
  select(resource_name, resource_id, last_modified) %>% 
  print(n = Inf)

process_period <- function(res_id, period_label, days_in_period) {
  
  message("Downloading: ", period_label)
  
  raw <- get_resource(res_id = res_id)
  
  raw %>% 
    clean_names() %>% 
    filter(prescribed_type == "VMP") %>% 
    filter(str_starts(bnf_item_code, "0401")) %>% 
    # Apply the same drug filter
    filter(str_detect(bnf_item_description, drug_pattern)) %>% 
    # Tag clinical group
    mutate(
      drug_group = case_when(
        str_detect(bnf_item_description, "CLONAZEPAM") ~ "clonazepam (epilepsy)",
        str_detect(bnf_item_description, "CHLORDIAZEPOXIDE") ~ "chlordiazepoxide (alcohol withdrawal)",
        str_detect(bnf_item_description, "RECTAL|AMPOULES|RECTUBES") ~ "emergency/rescue",
        str_detect(bnf_item_description, paste(z_drugs, collapse = "|")) ~ "z-drug",
        TRUE ~ "anxiolytic/hypnotic benzo"
      )
    ) %>% 
    # Filter to headline analysis
    filter(drug_group %in% c("anxiolytic/hypnotic benzo", "z-drug")) %>% 
    # Parse strength
    mutate(
      strength_raw = str_extract(bnf_item_description, "\\d+\\.?\\d*\\s?(MG|MICROGRAM)"),
      strength_num = as.numeric(str_extract(strength_raw, "\\d+\\.?\\d*")),
      strength_mg = if_else(str_detect(strength_raw, "MICROGRAM"), strength_num / 1000, strength_num),
      volume_raw = str_extract(bnf_item_description, "/\\d*\\.?\\d*\\s?ML"),
      volume_ml = as.numeric(str_extract(volume_raw, "\\d+\\.?\\d*")),
      volume_ml = if_else(str_detect(bnf_item_description, "/ML") & is.na(volume_ml), 1, volume_ml),
      mg_per_unit = if_else(is.na(volume_ml), strength_mg, strength_mg / volume_ml),
      drug = str_extract(bnf_item_description, "^[A-Z]+")
    ) %>% 
    left_join(ddd_lookup, by = "drug") %>% 
    mutate(
      total_mg = paid_quantity * mg_per_unit,
      ddds = total_mg / ddd_mg
    ) %>% 
    # Aggregate to HB
    filter(str_starts(hbt, "S08")) %>% 
    group_by(hbt) %>% 
    summarise(total_ddds = sum(ddds, na.rm = TRUE), .groups = "drop") %>% 
    left_join(hb_lookup, by = "hbt") %>% 
    # Tag with period info
    mutate(
      period = period_label,
      days_in_period = days_in_period
    )
}

periods <- tribble(
  ~res_id,                                ~period_label,    ~days_in_period,
  "f0df380b-3f9b-4536-bb87-569e189b727a", "Jan-Jun 2024",   182,
  "f3b9f2e2-66c0-4310-9b8e-734781d2ed0a", "Jul-Dec 2024",   184,
  "9de908b3-9c28-4cc3-aa32-72350a0579d1", "Jan-Jun 2025",   181,
  "83763a67-9c4d-43d3-89b2-18041a368a1c", "Jul-Dec 2025",   184
)

all_periods <- pmap_dfr(periods, process_period)

all_periods %>% count(period)

pop_by_year <- pop_data %>% 
  clean_names() %>% 
  filter(sex == "All", str_starts(hb, "S08")) %>% 
  select(year, hbt = hb, population = all_ages)

# What's the latest population year available?
pop_by_year %>% count(year) %>% tail(5)

# Tag each period with the year of population to use
period_to_pop_year <- tribble(
  ~period,          ~pop_year,
  "Jan-Jun 2024",   2024,
  "Jul-Dec 2024",   2024,
  "Jan-Jun 2025",   2024,   # using 2024 as proxy
  "Jul-Dec 2025",   2024
)

# Join everything together and compute rates
trends <- all_periods %>% 
  left_join(period_to_pop_year, by = "period") %>% 
  left_join(pop_by_year, by = c("hbt", "pop_year" = "year")) %>% 
  mutate(
    ddds_per_1000_per_day = total_ddds / population / days_in_period * 1000
  )

trends_plot <- trends %>% 
  mutate(
    period = factor(period, levels = c("Jan-Jun 2024", "Jul-Dec 2024",
                                       "Jan-Jun 2025", "Jul-Dec 2025"))
  )

trends_plot %>% 
  mutate(hb_name = fct_reorder(hb_name, -ddds_per_1000_per_day)) %>%
  ggplot(aes(x = period, y = ddds_per_1000_per_day)) +
  # Faint reference: all boards in grey, no facet
  geom_line(data = trends_plot %>% rename(hb_facet = hb_name), 
            aes(group = hb_facet), colour = "grey85", linewidth = 0.4) +
  # The board's own line on top
  geom_line(aes(group = hb_name), colour = "#4A6FA5", linewidth = 0.9) +
  geom_point(colour = "#4A6FA5", size = 1.5) +
  facet_wrap(~ hb_name, ncol = 4) +
  scale_y_continuous(limits = c(5, 21), breaks = seq(5, 20, 5)) +
  scale_x_discrete(labels = c("J-J\n2024", "J-D\n2024", "J-J\n2025", "J-D\n2025")) +
  labs(
    title = "Prescribing is falling across NHS Scotland,\nbut the geographic gap persists",
    subtitle = "Defined daily doses per 1,000 population per day, by health board, 2024–2025",
    x = NULL, y = "DDDs per 1,000 population per day",
    caption = "Source: Public Health Scotland prescribing data; National Records of Scotland mid-year population estimates. Each panel shows one board (blue) against all others (grey)."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, lineheight = 1.2),
    plot.subtitle = element_text(colour = "grey40", margin = margin(b = 12)),
    plot.caption = element_text(colour = "grey50", size = 8, hjust = 0),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "grey40", size = 8)
)



hb4_rates <- read_excel("data/drug-related-deaths-data.xlsx",
                        sheet = "Table_HB4",
                        skip = 4) %>% clean_names()

hb3_drugs <- read_excel("data/drug-related-deaths-data.xlsx",
                        sheet = "Table_HB3",
                        skip = 4) %>% clean_names()

# HB3 — just the 2024 benzo columns
deaths_2024 <- hb3_drugs %>%
  filter(nhs_board_area != "Scotland") %>%
  select(
    hb_name = nhs_board_area,
    deaths_all = all_drug_misuse_deaths,
    deaths_any_benzo = any_benzodiazepine,
    deaths_prescribed_benzo = any_prescribable_benzodiazepine_note_7,
    deaths_diazepam = diazepam_note_8,
    deaths_street_benzo = any_street_benzodiazepine_note_7
  )

# HB4 — most recent 5-year rates
deaths_rate_2024 <- hb4_rates %>%
  filter(nhs_board_area != "Scotland",
         five_year_period == "2020 - 2024") %>%
  mutate(death_rate = as.numeric(age_standardised_rate_per_100_000_population)) %>%
  select(hb_name = nhs_board_area, death_rate, total_deaths_5yr = total_number_of_deaths)

deaths_2024
deaths_rate_2024

# Combine prescribing + deaths
combined <- hb_rates %>%   # your prescribing data with ddds_per_1000_per_day
  left_join(deaths_rate_2024, by = "hb_name") %>%
  left_join(deaths_2024, by = "hb_name")

# Compute rates
combined_rates <- combined %>%
  mutate(
    prescribed_benzo_rate = deaths_prescribed_benzo / population * 100000,
    street_benzo_rate = deaths_street_benzo / population * 100000
  )

p1 <- combined_rates %>%
  filter(!is.na(prescribed_benzo_rate)) %>%
  ggplot(aes(x = ddds_per_1000_per_day, y = prescribed_benzo_rate)) +
  geom_point(colour = "#4A6FA5", size = 3) +
  ggrepel::geom_text_repel(aes(label = hb_name), size = 3, colour = "grey25") +
  labs(
    title = "Prescribed benzodiazepine deaths",
    x = "Prescribing rate (DDDs per 1,000 per day)",
    y = "Deaths per 100,000 population, 2024",
    caption = "Boards with fewer than ~5 deaths (Orkney, Shetland,\nBorders, Western Isles) have unreliable rates."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(size = 7, colour = "grey50", hjust = 0))

p2 <- combined_rates %>%
  filter(!is.na(street_benzo_rate)) %>%
  ggplot(aes(x = ddds_per_1000_per_day, y = street_benzo_rate)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey60", 
              linetype = "dashed", linewidth = 0.6) +
  geom_point(colour = "#B33A3A", size = 3) +
  ggrepel::geom_text_repel(aes(label = hb_name), size = 3, colour = "grey25") +
  labs(
    title = "Street benzodiazepine deaths",
    x = "Prescribing rate (DDDs per 1,000 per day)",
    y = "Deaths per 100,000 population, 2024"
  ) +
  theme_minimal()

p1 + p2 + plot_annotation(
  title = "Two different drug death pathways across NHS Scotland",
  subtitle = "Benzodiazepine prescribing rate (Jul–Dec 2025) vs benzodiazepine-implicated death rates (2024)",
  caption = "Sources: PHS prescribing data; NRS drug-related deaths 2024; NRS population estimates."
)