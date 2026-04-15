#################################### Setup #####################################
### Clean Memory ===============================================================
rm(list = ls()) # clears working memory (good housekeeping)
gc()
### Libraries ==================================================================
library(tidyverse)
library(fuzzyjoin)
library(plotly)
library(htmlwidgets)
11

### Work Directory =============================================================
setwd("/Users/Jack/Desktop/CNE /Data Analysis/Engagement Hours")

### Loading Data ===============================================================
CH23 <- read.csv("Data/2023 stats and organization tracking JD 102925(12-13-23).csv", check.names = FALSE)
CH24 <- read.csv("Data/2024 stats and organization tracking JD 102925(2024 data).csv", check.names = FALSE)
CH25 <- read.csv("Data/2025 stats and organization tracking(2025 data YTD).csv", check.names = FALSE)

OrgIDs <- read.csv("Data/OrgIDs.csv", check.names = FALSE)

############################## Data Cleaning ###################################
### CH23 =======================================================================
## Rename ----------------------------------------------------------------------
CH23 <- CH23 %>%
  rename(
    "Office Hours" = `"office hours"`,
    "Workshops" = `workshops hours`,
    "LEAD" = `LEAD hours`,
    "Peer Support" = `Peer support hours`,
    "LEAD Coaching" = `LEAD Coaching hours`,
    "In-House Calls" = `in-house brief call - referrals`,
    "Mental Wellbeing Wrap Around" = `Mental Wellbeing wrap around`,
    "SDI for LEADers" = `SDI for EDs`
  )
  

## Adding April office hours to regular office hours ---------------------------
CH23.1 <- CH23 %>%
  mutate(
    `Office Hours` = `Office Hours` +
      if_else(
        `April Office hours invite notice` == 1,
        1,
        0,
        missing = 0
      )
  ) %>%
  select(-`April Office hours invite notice`)

## Redoing the 'New/Returning' section -----------------------------------------
CH23.2 <- CH23.1 %>%
  mutate(
    PartnerType2023 = if_else(
      `Existing customer in 2022` == 1,
      "Returning",
      "New",
      missing = "New"
    )
  ) %>%
  select(-`Existing customer in 2022`, -`Return/New customer 2023`)

# Manual re-assignment (returning customer from before 2022)
CH23.2 <- CH23.2 %>%
  mutate(
    PartnerType2023 = case_when(
      Organization == "Community Foundation for Monterey County" ~ "Returning",
      TRUE ~ PartnerType2023
    )
  )

## Standardizing recording metrics ---------------------------------------------
CH23.3 <- CH23.2 %>%
  mutate(
    SIEDI = ifelse(SIEDI == "x", "yes", "no")
  ) %>%
  mutate(
    across(
      .cols = where(is.character) & !c(Organization, SIEDI, PartnerType2023),
      ~ parse_number(.)
    )
  ) %>%
  mutate(
    across(
      .cols = where(is.numeric),
      ~ replace_na(., 0)
    )
  )

## Adding IDs ------------------------------------------------------------------
CH23.4 <- CH23.3 %>%
  left_join(OrgIDs, by = "Organization")

## Combining Duplicated Orgs ---------------------------------------------------
dup_ids23 <- CH23.4 %>%
  filter(!is.na(ID), ID != "UNAF") %>%
  count(ID) %>%
  filter(n > 1) %>%
  pull(ID)

# rows to collapse
dups_collapsed23 <- CH23.4 %>%
  filter(ID %in% dup_ids23) %>%
  group_by(ID) %>%
  summarise(
    Organization = first(na.omit(Organization[grepl("Santa Cruz", Organization)])) %>%
      coalesce(first(na.omit(Organization))),  # fallback if no Santa Cruz match
    
    across(
      where(is.numeric) & !any_of("SIEDI"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    SIEDI = first(SIEDI),
    PartnerType2023 = first(na.omit(PartnerType2023)),
    .groups = "drop"
  )

# rows to leave alone
not_dups23 <- CH23.4 %>%
  filter(!(ID %in% dup_ids23))

# recombine
CH23.5 <- bind_rows(not_dups23, dups_collapsed23)

CH23.5 <- CH23.5 %>%
  mutate(Organization = if_else(
    Organization == "San Benito Leadership Institute",
    "The Community Foundation for San Benito County",
    Organization
  ))


## Calculating Totals ----------------------------------------------------------
CH23.6 <- CH23.5 %>%
  mutate(
    "Total hours 2023" = rowSums(
      across(where(is.numeric) & !matches("Total hours 2023")), 
      na.rm = TRUE
    )
  )

## Removing Orgs with no engagement hours -----------------------------------
CH23.7 <- CH23.6 %>%
  filter(
    !Organization == "Total"
  ) %>%
  filter(
    !(`Total hours 2023` == 0)
  ) %>%
  select(-SIEDI)



## Creating 'Returning' List ---------------------------------------------------
ExistingOrgs23 <- CH23.7 %>%
  select(Organization, ID) %>%
  filter(!ID == "UNAF")


### CH24 =======================================================================
## Rename ----------------------------------------------------------------------
CH24 <- CH24 %>%
  rename(
    "Office Hours" = `"office hours"`,
    "Workshops" = `workshops hours`,
    "LEAD" = `LEAD hours`,
    "Peer Support" = `Peer support hours`,
    "LEAD Coaching" = `LEAD Coaching hours`,
    "Board Connect" = `Board Connect event`,
    "In-House Calls" = `in-house brief call - referrals`,
    "Consulting After Grant Training" = `Consulting hour after grant training`
  ) %>%
  select(-`Existing customer in 2023`, -`Return/New 2024`, -`Existing customer in 2022`, -`Return/New customer 2023`)


## Standardizing recording metrics ---------------------------------------------
CH24.1 <- CH24 %>%
  mutate(
    SIEDI = if_else(SIEDI == "x", "yes", "no")
  ) %>%
  mutate(
    across(
      .cols = where(is.character) & !any_of(c("Organization", "SIEDI")),
      .fns  = ~ parse_number(.)
    )
  ) %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns  = ~ replace_na(., 0)
    )
  )
## Adding IDs ------------------------------------------------------------------
CH24.2 <- CH24.1 %>%
  left_join(OrgIDs, by = "Organization")


## Combining Duplicated Orgs ---------------------------------------------------
dup_ids24 <- CH24.2 %>%
  filter(!is.na(ID), ID != "UNAF") %>%
  count(ID) %>%
  filter(n > 1) %>%
  pull(ID)

# rows to collapse
dups_collapsed24 <- CH24.2 %>%
  filter(ID %in% dup_ids24) %>%
  group_by(ID) %>%
  summarise(
    Organization = first(na.omit(`Account Name (SF)`)),
    
    across(
      where(is.numeric) & !any_of("SIEDI"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    SIEDI = first(SIEDI),
    .groups = "drop"
  )

# rows to leave alone
not_dups24 <- CH24.2 %>%
  filter(!(ID %in% dup_ids24))

# recombine
CH24.3 <- bind_rows(not_dups24, dups_collapsed24)

## Calculating Totals ----------------------------------------------------------
CH24.4 <- CH24.3 %>%
  mutate(
    "Total hours 2024" = rowSums(
      across(where(is.numeric) & !matches("Total hours 2024")), 
      na.rm = TRUE
    )
  )

## Removing Orgs with no engagement hours -----------------------------------
CH24.5 <- CH24.4 %>%
  filter(
    !Organization == "Total"
  ) %>%
  filter(
    !("Total hours 2024" == 0)
  ) %>%
  select(-SIEDI)


## Identifying New/Returning Partners -------------------------------------------
CH24.6 <- CH24.5 %>%
  mutate(
    PartnerType2024 = case_when(
      ID %in% na.omit(ExistingOrgs23$ID) ~ "Returning",
      TRUE ~ "New"
    )
  )

## Adding New orgs to the Existing Orgs List -----------------------------------
# Identifying New Orgs
NewOrgs <- CH24.6 %>%
  filter(
    PartnerType2024 == "New",
    !is.na(ID),
    ID != "UNAF"
  ) %>%
  select(Organization, ID)

# Adding to existing orgs
ExistingOrgs24 <- bind_rows(
  ExistingOrgs23,
  NewOrgs
) %>%
  distinct(ID, .keep_all = TRUE)


### CH25 =======================================================================
## Rename ----------------------------------------------------------------------
CH25 <- CH25 %>%
  rename(
    "Office Hours" = `"office hours"`,
    "Workshops" = `workshops hours`,
    "LEAD" = `LEAD hours`,
    "Peer Support" = `Peer support hours`,
    "LEAD Coaching" = `LEAD Coaching hours`,
    "Board Connect" = `Board Connect event`,
    "In-House Calls" = `in-house brief call - referrals`,
  )


## Standardizing recording metrics ---------------------------------------------
CH25.1 <- CH25 %>%
  mutate(
    SIEDI = ifelse(SIEDI == "x", "yes", "no")
  ) %>%
  mutate(
    across(
      .cols = where(is.character) & !c(Organization, SIEDI),
      ~ parse_number(.)
    )
  ) %>%
  mutate(
    across(
      .cols = where(is.numeric),
      ~ replace_na(., 0)
    )
  )

## Adding IDs ------------------------------------------------------------------
CH25.2 <- CH25.1 %>%
  left_join(OrgIDs, by = "Organization")


## Combining Duplicated Orgs ---------------------------------------------------
dup_ids25 <- CH25.2 %>%
  filter(!is.na(ID), ID != "UNAF") %>%
  count(ID) %>%
  filter(n > 1) %>%
  pull(ID)

# rows to collapse
dups_collapsed25 <- CH25.2 %>%
  filter(ID %in% dup_ids25) %>%
  group_by(ID) %>%
  summarise(
    Organization = first(na.omit(`Account Name (SF)`)),
    
    across(
      where(is.numeric) & !any_of("SIEDI"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    SIEDI = first(SIEDI),
    .groups = "drop"
  )

# rows to leave alone
not_dups25 <- CH25.2 %>%
  filter(!(ID %in% dup_ids25))

# recombine
CH25.3 <- bind_rows(not_dups25, dups_collapsed25)



## Calculating Totals ----------------------------------------------------------
CH25.4 <- CH25.3 %>%
  mutate(
    "Total hours 2025" = rowSums(
      across(where(is.numeric) & !matches("Total hours 2025")), 
      na.rm = TRUE
    )
  )

## Removing Orgs with no engagement hours -----------------------------------------
CH25.5 <- CH25.4 %>%
  filter(
    !Organization == "Total"
  ) %>%
  filter(
    !("Total hours 2025" == 0)
  ) %>%
  select(-SIEDI)


## Identifying New/Returning Partners -------------------------------------------
CH25.6 <- CH25.5 %>%
  mutate(
    PartnerType2025 = case_when(
      ID == "UNAF" ~ "New",
      ID %in% na.omit(ExistingOrgs24$ID) ~ "Returning",
      TRUE ~ "New"
    )
  )





### Exporting Clean Data  ======================================================
write.csv(CH23.7, "Data/2023 Clean.csv", row.names = FALSE)
write.csv(CH24.6, "Data/2024 Clean.csv", row.names = FALSE)
write.csv(CH25.6, "Data/2025 Clean.csv", row.names = FALSE)

CH23C <- read.csv("Data/2023 Clean.csv", check.names = FALSE)
CH24C <- read.csv("Data/2024 Clean.csv", check.names = FALSE)
CH25C <- read.csv("Data/2025 Clean.csv", check.names = FALSE)



############################# Data Visualization ###############################
### Pallettes
### Color Palettes =============================================================
modality_colors_full <- c(
  "LEAD"                            = "#F27B35",
  "LEAD Coaching"                   = "#C45A1A",
  "SDI for LEADers"                 = "#F7AA78",
  "LEAD Alumni"                     = "#FDD89A",
  "Peer Support"                    = "#FFC000",
  "Workshops"                       = "#76A646",
  "Board Connect"                   = "#457ABF",
  "Board Empowerment"               = "#76A8D9",
  "Office Hours"                    = "#4D7A2A",
  "ED Wrap Around"                  = "#9DC46A",
  "In-House Calls"                  = "#2D5A96",
  "OD"                              = "#ADC4D9",
  "Mental Wellbeing Wrap Around"    = "#A5A5A5",
  "Consulting After Grant Training" = "#C9C9C9"
)

modality_colors_no_lead <- c(
  "Peer Support"                    = "#FFC000",
  "Workshops"                       = "#76A646",
  "Board Connect"                   = "#457ABF",
  "Board Empowerment"               = "#76A8D9",
  "Office Hours"                    = "#4D7A2A",
  "ED Wrap Around"                  = "#9DC46A",
  "In-House Calls"                  = "#2D5A96",
  "OD"                              = "#ADC4D9",
  "Mental Wellbeing Wrap Around"    = "#A5A5A5",
  "Consulting After Grant Training" = "#C9C9C9"
)


# Color Pallete
hombre <- c("#A5A5A5", "#457ABF", "#76A646", "#FFC000", "#F27B35")
hombre2 <- c("#F27B35", "#FFC000", "#76A646", "#457ABF", "#A5A5A5")

### Individual Year Viz ========================================================
## New/Returning by Year - Pie -------------------------------------------------
# 2023
CH23C2 <- CH23C %>%
  filter(!`Total hours 2023` == 0)

ggplot(CH23C2, aes(x = "", fill = PartnerType2023)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("#76A646", "#457ABF")
  ) +
  labs(
    title = "Partner Type as Share of Total (2023)",
    fill = "Partner Type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Partner Type by Year (Pie)/2023 Partner Type Pie.png",
       width = 6,
       height = 4,
       dpi = 300
       )

na_orgs <- CH23C2 %>%
  filter(is.na(PartnerType2023)) %>%
  select(Organization) 

print(na_orgs)


# 2024
CH24C2 <- CH24C %>%
  filter(!`Total hours 2024` == 0)

ggplot(CH24C2, aes(x = "", fill = PartnerType2024)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("#76A646", "#457ABF")
  ) +
  labs(
    title = "Partner Type as Share of Total (2024)",
    fill = "Partner Type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Partner Type by Year (Pie)/2024 Partner Type Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)



# 2025
CH25C2 <- CH25C %>%
  filter(!`Total hours 2025` == 0)

ggplot(CH25C2, aes(x = "", fill = PartnerType2025)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("#76A646", "#457ABF")
  ) +
  labs(
    title = "Partner Type as Share of Total (2025)",
    fill = "Partner Type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


ggsave("Charts/Partner Type by Year (Pie)/2025 Partner Type Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)


## Engagement Hours by Partner Type - Bar  -----------------------------------------
# Shared y-axis limit
y_max_CHMT <- 2000

#2023
CH23_CHMT <- CH23C %>%
  group_by(PartnerType2023) %>%
  summarise(TotalHours = sum(`Total hours 2023`, na.rm = TRUE))

ggplot(CH23_CHMT, aes(x = PartnerType2023, y = TotalHours, fill = PartnerType2023)) +
  geom_col() +
  geom_text(
    aes(label = TotalHours),   # Show only total hours per partner type
    vjust = 1.5,
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  scale_y_continuous(limits = c(0, y_max_CHMT)) +
  labs(
    title = "Total Engagement Hours by Partner Type (2023)",
    x = "Partner Type",
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Engagement Hours by Partner Type (Bar)/2023 Engagement Hour by Partner Type Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)


# 2024
CH24_CHMT <- CH24C %>%
  group_by(PartnerType2024) %>%
  summarise(TotalHours = sum(`Total hours 2024`, na.rm = TRUE))

ggplot(CH24_CHMT, aes(x = PartnerType2024, y = TotalHours, fill = PartnerType2024)) +
  geom_col() +
  geom_text(
    aes(label = TotalHours),   # Show only total hours per partner type
    vjust = 1.5,
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  scale_y_continuous(limits = c(0, y_max_CHMT)) +
  labs(
    title = "Total Engagement Hours by Partner Type (2024)",
    x = "Partner Type",
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Engagement Hours by Partner Type (Bar)/2024 Engagement Hour by Partner Type Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)

# 2025
CH25_CHMT <- CH25C %>%
  group_by(PartnerType2025) %>%
  summarise(TotalHours = sum(`Total hours 2025`, na.rm = TRUE))

ggplot(CH25_CHMT, aes(x = PartnerType2025, y = TotalHours, fill = PartnerType2025)) +
  geom_col() +
  geom_text(
    aes(label = TotalHours),   # Show only total hours per partner type
    vjust = 1.5,
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  scale_y_continuous(limits = c(0, y_max_CHMT)) +
  labs(
    title = "Total Engagement Hours by Partner Type (2025)",
    x = "Partner Type",
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Engagement Hours by Partner Type (Bar)/2025 Engagement Hour by Partner Type Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)

## Orgs with Hours - Pie ---------------------------------------------------------
# 2023
CH23_NH <- CH23C %>%
  filter(PartnerType2023 == "Returning") %>%
  mutate(Engaged = ifelse(`Total hours 2023` == 0, 0, 1)) %>%
  select(Organization, Engaged)

ggplot(CH23_NH, aes(x = "", fill = factor(Engaged))) +  # <-- convert to factor
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("0" = "#457ABF", "1" = "#76A646"),   # map factor levels to colors
    labels = c("0" = "Not Engaged", "1" = "Engaged")
  ) +
  labs(
    title = "Returning Partners Engaged (2023)",
    fill = "Engagement Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Returning Partners Engaged/2023 Returning Partner Engagement Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)

# 2024
CH24_NH <- CH24C %>%
  filter(PartnerType2024 == "Returning") %>%
  mutate(Engaged = ifelse(`Total hours 2024` == 0, 0, 1)) %>%
  select(Organization, Engaged)

ggplot(CH24_NH, aes(x = "", fill = factor(Engaged))) +  # <-- convert to factor
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("0" = "#457ABF", "1" = "#76A646"),   # map factor levels to colors
    labels = c("0" = "Not Engaged", "1" = "Engaged")
  ) +
  labs(
    title = "Returning Partners Engaged (2024)",
    fill = "Engagement Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Returning Partners Engaged/2024 Returning Partner Engagement Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)

# 2025
CH25_NH <- CH25C %>%
  filter(PartnerType2025 == "Returning") %>%
  mutate(Engaged = ifelse(`Total hours 2025` == 0, 0, 1)) %>%
  select(Organization, Engaged)

ggplot(CH25_NH, aes(x = "", fill = factor(Engaged))) +  # <-- convert to factor
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("0" = "#457ABF", "1" = "#76A646"),   # map factor levels to colors
    labels = c("0" = "Not Engaged", "1" = "Engaged")
  ) +
  labs(
    title = "Returning Partners Engaged (2025)",
    fill = "Engagement Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Returning Partners Engaged/2025 Returning Partner Engagement Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)


## Orgs by Hours - Bar ---------------------------------------------------------
# Shared Max
y_max_OH <- 100 

# 2023
CH23_OH <- CH23C %>%
  filter(`Total hours 2023` > 0) %>%
  mutate(Organization = factor(
    Organization, 
    levels = Organization[order(`Total hours 2023`, decreasing = TRUE)]
  ))

CH23_OH <- CH23_OH %>%
  mutate(hover_text = paste0(Organization, "\nTotal Hours: ", `Total hours 2023`))

OH23 <- ggplot(CH23_OH, aes(
  x = Organization,
  y = `Total hours 2023`,
  fill = `Total hours 2023`,
  text = hover_text   # hover shows org + hours
)) +
  geom_col() +
  scale_fill_gradientn(colors = hombre) +
  scale_y_continuous(limits = c(0, y_max_OH)) +
  labs(
    title = "Organizations by Hours in Engagement (2023)",
    subtitle = "Organizations with 0 engagement hours have been excluded",
    x = NULL,
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

# Convert to interactive Plotly
ggplotly(OH23, tooltip = "text")

saveOH23<- ggplotly(OH23, tooltip = "text")


saveWidget(saveOH23, "Charts/Orgs by Engagement Hours (Bar)/2023 Organizations by Engagement Hours Bar.html", selfcontained = TRUE)


# 2024
CH24_OH <- CH24C %>%
  filter(`Total hours 2024` > 0) %>%
  mutate(Organization = factor(
    Organization, 
    levels = Organization[order(`Total hours 2024`, decreasing = TRUE)]
  ))

CH24_OH <- CH24_OH %>%
  mutate(hover_text = paste0(Organization, "\nTotal Hours: ", `Total hours 2024`))

OH24 <- ggplot(CH24_OH, aes(
  x = Organization,
  y = `Total hours 2024`,
  fill = `Total hours 2024`,
  text = hover_text   # hover shows org + hours
)) +
  geom_col() +
  scale_fill_gradientn(colors = hombre) +
  scale_y_continuous(limits = c(0, y_max_OH)) +
  labs(
    title = "Organizations by Hours in Engagement (2024)",
    subtitle = "Organizations with 0 engagement hours have been excluded",
    x = NULL,
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

# Convert to interactive Plotly
ggplotly(OH24, tooltip = "text")

saveOH24<- ggplotly(OH24, tooltip = "text")


saveWidget(saveOH24, "Charts/Orgs by Engagement Hours (Bar)/2024 Organizations by Engagement Hours Bar.html", selfcontained = TRUE)


# 2025
CH25_OH <- CH25C %>%
  filter(`Total hours 2025` > 0) %>%
  mutate(Organization = factor(
    Organization, 
    levels = Organization[order(`Total hours 2025`, decreasing = TRUE)]
  ))

CH25_OH <- CH25_OH %>%
  mutate(hover_text = paste0(Organization, "\nTotal Hours: ", `Total hours 2025`))

OH25 <- ggplot(CH25_OH, aes(
  x = Organization,
  y = `Total hours 2025`,
  fill = `Total hours 2025`,
  text = hover_text   # hover shows org + hours
)) +
  geom_col() +
  scale_fill_gradientn(colors = hombre) +
  scale_y_continuous(limits = c(0, y_max_OH)) +
  labs(
    title = "Organizations by Hours in Engagement (2025)",
    subtitle = "Organizations with 0 engagement hours have been excluded",
    x = NULL,
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

# Convert to interactive Plotly
ggplotly(OH25, tooltip = "text")

saveOH25<- ggplotly(OH25, tooltip = "text")


saveWidget(saveOH25, "Charts/Orgs by Engagement Hours (Bar)/2025 Organizations by Engagement Hours Bar.html", selfcontained = TRUE)


## Hours by Engagement Modality - Bar ---------------------------------------------
# Shared y-axis limit
y_max_HCB <- 2000

# 2023
CH23_HCB <- CH23C %>%
  select(-`Total hours 2023`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  filter(Hours > 0) %>%   
  arrange(desc(Hours))

CH23_HCB$Modality <- factor(CH23_HCB$Modality, levels = CH23_HCB$Modality)

ggplot(CH23_HCB, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(0, y_max_HCB)) +
  scale_fill_manual(values = modality_colors_full) +
  theme_minimal() +
  labs(
    title = "Engagement Hours by Modality (2023)",
    subtitle = "Sorted within year",
    x = "Engagement Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Charts/Engagement Hours by Modality/2023 Engagement Hours by Modality Bar.png",
       width = 6, height = 4, dpi = 300)


# 2024
CH24_HCB <- CH24C %>%
  select(-`Total hours 2024`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  filter(Hours > 0) %>%   
  arrange(desc(Hours))

CH24_HCB$Modality <- factor(CH24_HCB$Modality, levels = CH24_HCB$Modality)

ggplot(CH24_HCB, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(0, y_max_HCB)) +
  scale_fill_manual(values = modality_colors_full) +
  theme_minimal() +
  labs(
    title = "Total Engagement Hours by Modality (2024)",
    subtitle = "Sorted within year",
    x = "Engagement Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Charts/Engagement Hours by Modality/2024 Engagement Hours by Modality Bar.png",
       width = 6, height = 4, dpi = 300)


# 2025
CH25_HCB <- CH25C %>%
  select(-`Total hours 2025`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  filter(Hours > 0) %>%   
  arrange(desc(Hours))

CH25_HCB$Modality <- factor(CH25_HCB$Modality, levels = CH25_HCB$Modality)

ggplot(CH25_HCB, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(0, y_max_HCB)) +
  scale_fill_manual(values = modality_colors_full) +
  theme_minimal() +
  labs(
    title = "Total Engagement Hours by Modality (2025)",
    subtitle = "Sorted within year",
    x = "Engagement Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Charts/Engagement Hours by Modality/2025 Engagement Hours by Modality Bar.png",
       width = 6, height = 4, dpi = 300)

### Longitudinal Viz ===========================================================
## Engagement hours by Engagement Modality - YOY -------------------------------------
# Making the helper function: collapsing one year to (Year, Modality, Hours)
prep_modality_year <- function(df, year, total_col, partner_col) {
  df %>%
    select(-any_of(c(total_col, partner_col, "Organization"))) %>%  # keep only modality columns
    summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Modality",
      values_to = "Hours"
    ) %>%
    mutate(
      Year = year,
      Modality = stringr::str_squish(Modality)
    )
}

# Making the longitudinal dataset
CH_yoy <- bind_rows(
  prep_modality_year(CH23C, 2023, "Total hours 2023", "PartnerType2023"),
  prep_modality_year(CH24C, 2024, "Total hours 2024", "PartnerType2024"),
  prep_modality_year(CH25C, 2025, "Total hours 2025", "PartnerType2025")
) %>%
  group_by(Year, Modality) %>%                      # safety: if duplicates ever occur
  summarise(Hours = sum(Hours, na.rm = TRUE), .groups = "drop") %>%
  arrange(Modality, Year)

# Make sure years plot in order
CH_yoy <- CH_yoy %>%
  mutate(Year = as.integer(Year))


# Plot 
CHYOY_html <- plot_ly(
  data = CH_yoy,
  x = ~Year,
  y = ~Hours,
  color = ~Modality,
  colors = hombre, 
  type = "scatter",
  mode = "lines+markers",
  text = ~paste0(
    "Modality: ", Modality,
    "<br>Year: ", Year,
    "<br>Total hours: ", scales::comma(Hours)
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = list(text = "Total Engagement Hours by Modality (Year over Year)", x = 0.5),
    xaxis = list(title = "", tickmode = "linear", dtick = 1),
    yaxis = list(
      title = "Total Engagement Hours",
      tickformat = ",",     # commas
      autorange = TRUE
    ),
    legend = list(itemclick = "toggle", itemdoubleclick = "toggleothers")
  ) %>%
  config(displayModeBar = TRUE) %>%
  onRender("
    function(el, x) {
      function autoscaleY() {
        // wait for legend toggling to apply visibility changes
        setTimeout(function() {
          Plotly.relayout(el, {
            'yaxis.autorange': true,
            'yaxis.tickmode': 'auto'
          });
        }, 60);
      }

      // Legend interactions
      el.on('plotly_legendclick', function() { autoscaleY(); });
      el.on('plotly_legenddoubleclick', function() { autoscaleY(); });

      // Covers other visibility changes
      el.on('plotly_restyle', function() { autoscaleY(); });
    }
  ")

# Save as
saveWidget(
  CHYOY_html,
  "Charts/Engagement Hours by Modality YoY/YoY Engagement Hours by Modality.html",
  selfcontained = TRUE
)

## Engagement Hours by Partner Type - YOY ------------------------------------------
# Helper: collapse one year to (Year, PartnerType, TotalHours)
prep_partnertype_year <- function(df, year, total_col, partner_col) {
  df %>%
    filter(.data[[total_col]] != 0) %>%   # optional: match your pie-chart logic
    group_by(PartnerType = .data[[partner_col]]) %>%
    summarise(
      TotalHours = sum(.data[[total_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Year = year)
}

# Build longitudinal dataset
CHMT_yoy <- bind_rows(
  prep_partnertype_year(CH23C, 2023, "Total hours 2023", "PartnerType2023"),
  prep_partnertype_year(CH24C, 2024, "Total hours 2024", "PartnerType2024"),
  prep_partnertype_year(CH25C, 2025, "Total hours 2025", "PartnerType2025")
) %>%
  mutate(
    Year = as.integer(Year),
    PartnerType = factor(PartnerType, levels = c("New", "Returning"))
  ) %>%
  arrange(PartnerType, Year)

# GGplot it
ggplot(CHMT_yoy, aes(x = Year, y = TotalHours, color = PartnerType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    aes(label = scales::comma(TotalHours)),
    vjust = -0.8,
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("New" = "#76A646", "Returning" = "#457ABF")
  ) +
  scale_x_continuous(
    breaks = c(2023, 2024, 2025),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  labs(
    title = "Total Engagement Hours by Partner Type (Year over Year)",
    x = "",
    y = "Total Engagement Hours",
    color = "Partner Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Save it
ggsave(
  "Charts/Engagement Hours by Partner Type YoY/YoY Engagement Hours by Partner Type.png",
  width = 7,
  height = 4,
  dpi = 300
)

######################## Data Visualization (Non-LEAD) #########################
### Filtering Datasets =========================================================
# Create filtered datasets excluding LEAD-related modalities
LEAD_cols <- c("SDI for LEADers", "LEAD", "LEAD Alumni", "LEAD Coaching")

CH23C_NL <- CH23C %>%
  select(-any_of(c(LEAD_cols, "Total hours 2023"))) %>%
  mutate(`Total hours 2023` = rowSums(across(where(is.numeric)), na.rm = TRUE))

CH24C_NL <- CH24C %>%
  select(-any_of(c(LEAD_cols, "Total hours 2024"))) %>%
  mutate(`Total hours 2024` = rowSums(across(where(is.numeric)), na.rm = TRUE))

CH25C_NL <- CH25C %>%
  select(-any_of(c(LEAD_cols, "Total hours 2025"))) %>%
  mutate(`Total hours 2025` = rowSums(across(where(is.numeric)), na.rm = TRUE))
### Individual Year Viz ========================================================
## New/Returning by Year - Pie -------------------------------------------------
# 2023
CH23C2_NL <- CH23C_NL %>%
  filter(!`Total hours 2023` == 0)

ggplot(CH23C2_NL, aes(x = "", fill = PartnerType2023)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#76A646", "#457ABF")) +
  labs(title = "Partner Type as Share of Total (2023, Non-LEAD)", fill = "Partner Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Partner Type by Year (Pie)/2023 Partner Type Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2024
CH24C2_NL <- CH24C_NL %>%
  filter(!`Total hours 2024` == 0)

ggplot(CH24C2_NL, aes(x = "", fill = PartnerType2024)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#76A646", "#457ABF")) +
  labs(title = "Partner Type as Share of Total (2024, Non-LEAD)", fill = "Partner Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Partner Type by Year (Pie)/2024 Partner Type Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2025
CH25C2_NL <- CH25C_NL %>%
  filter(!`Total hours 2025` == 0)

ggplot(CH25C2_NL, aes(x = "", fill = PartnerType2025)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#76A646", "#457ABF")) +
  labs(title = "Partner Type as Share of Total (2025, Non-LEAD)", fill = "Partner Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Partner Type by Year (Pie)/2025 Partner Type Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

## Engagement Hours by Partner Type - Bar ------------------------------------------
# Shared y-axis limit across all years
y_max_CHMT_NL <- 600

# 2023
CH23_CHMT_NL <- CH23C_NL %>%
  group_by(PartnerType2023) %>%
  summarise(TotalHours = sum(`Total hours 2023`, na.rm = TRUE))

ggplot(CH23_CHMT_NL, aes(x = PartnerType2023, y = TotalHours, fill = PartnerType2023)) +
  geom_col() +
  geom_text(aes(label = TotalHours), vjust = 1.5, color = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  scale_y_continuous(limits = c(0, y_max_CHMT_NL)) +
  labs(title = "Total Engagement Hours by Partner Type (2023, Non-LEAD)",
       x = "Partner Type", y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Engagement Hours by Partner Type (Bar)/2023 Engagement Hour by Partner Type Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2024
CH24_CHMT_NL <- CH24C_NL %>%
  group_by(PartnerType2024) %>%
  summarise(TotalHours = sum(`Total hours 2024`, na.rm = TRUE))

ggplot(CH24_CHMT_NL, aes(x = PartnerType2024, y = TotalHours, fill = PartnerType2024)) +
  geom_col() +
  geom_text(aes(label = TotalHours), vjust = 1.5, color = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  scale_y_continuous(limits = c(0, y_max_CHMT_NL)) +
  labs(title = "Total Engagement Hours by Partner Type (2024, Non-LEAD)",
       x = "Partner Type", y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Engagement Hours by Partner Type (Bar)/2024 Engagement Hour by Partner Type Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2025
CH25_CHMT_NL <- CH25C_NL %>%
  group_by(PartnerType2025) %>%
  summarise(TotalHours = sum(`Total hours 2025`, na.rm = TRUE))

ggplot(CH25_CHMT_NL, aes(x = PartnerType2025, y = TotalHours, fill = PartnerType2025)) +
  geom_col() +
  geom_text(aes(label = TotalHours), vjust = 1.5, color = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  scale_y_continuous(limits = c(0, y_max_CHMT_NL)) +
  labs(title = "Total Engagement Hours by Partner Type (2025, Non-LEAD)",
       x = "Partner Type", y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Engagement Hours by Partner Type (Bar)/2025 Engagement Hour by Partner Type Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

## Orgs with Hours - Pie -------------------------------------------------------
# 2023
CH23_NH_NL <- CH23C_NL %>%
  filter(PartnerType2023 == "Returning") %>%
  mutate(Engaged = ifelse(`Total hours 2023` == 0, 0, 1)) %>%
  select(Organization, Engaged)

ggplot(CH23_NH_NL, aes(x = "", fill = factor(Engaged))) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = c("0" = "#457ABF", "1" = "#76A646"),
                    labels = c("0" = "Not Engaged", "1" = "Engaged")) +
  labs(title = "Returning Partners Engaged (2023, Non-LEAD)", fill = "Engagement Status") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Returning Partners Engaged/2023 Returning Partner Engagement Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2024
CH24_NH_NL <- CH24C_NL %>%
  filter(PartnerType2024 == "Returning") %>%
  mutate(Engaged = ifelse(`Total hours 2024` == 0, 0, 1)) %>%
  select(Organization, Engaged)

ggplot(CH24_NH_NL, aes(x = "", fill = factor(Engaged))) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = c("0" = "#457ABF", "1" = "#76A646"),
                    labels = c("0" = "Not Engaged", "1" = "Engaged")) +
  labs(title = "Returning Partners Engaged (2024, Non-LEAD)", fill = "Engagement Status") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Returning Partners Engaged/2024 Returning Partner Engagement Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2025
CH25_NH_NL <- CH25C_NL %>%
  filter(PartnerType2025 == "Returning") %>%
  mutate(Engaged = ifelse(`Total hours 2025` == 0, 0, 1)) %>%
  select(Organization, Engaged)

ggplot(CH25_NH_NL, aes(x = "", fill = factor(Engaged))) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        after_stat(count), "\n",
        "(", scales::percent(after_stat(count / sum(count)), accuracy = 0.1), ")"
      )
    ),
    stat = "count",
    position = position_stack(vjust = 0.5),
    size = 4, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = c("0" = "#457ABF", "1" = "#76A646"),
                    labels = c("0" = "Not Engaged", "1" = "Engaged")) +
  labs(title = "Returning Partners Engaged (2025, Non-LEAD)", fill = "Engagement Status") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Returning Partners Engaged/2025 Returning Partner Engagement Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

## Orgs by Hours - Bar ---------------------------------------------------------
hombre <- c("#A5A5A5", "#457ABF", "#76A646", "#FFC000", "#F27B35")

# Consistent Scale
y_max_OH_NL <- 23.5

# 2023
CH23_OH_NL <- CH23C_NL %>%
  filter(`Total hours 2023` > 0) %>%
  mutate(Organization = factor(Organization,
                               levels = Organization[order(`Total hours 2023`, decreasing = TRUE)])) %>%
  mutate(hover_text = paste0(Organization, "\nTotal Hours: ", `Total hours 2023`))

OH23_NL <- ggplot(CH23_OH_NL, aes(x = Organization, y = `Total hours 2023`,
                                  fill = `Total hours 2023`, text = hover_text)) +
  geom_col() +
  scale_fill_gradientn(colors = hombre) +
  scale_y_continuous(limits = c(0, y_max_OH_NL)) +
  labs(title = "Organizations by Hours in Engagement (2023, Non-LEAD)",
       subtitle = "Organizations with 0 engagement hours have been excluded",
       x = NULL, y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

saveWidget(ggplotly(OH23_NL, tooltip = "text"),
           "Charts/Non-LEAD/Orgs by Engagement Hours (Bar)/2023 Organizations by Engagement Hours Bar Non-LEAD.html",
           selfcontained = TRUE)

# 2024
CH24_OH_NL <- CH24C_NL %>%
  filter(`Total hours 2024` > 0) %>%
  mutate(Organization = factor(Organization,
                               levels = Organization[order(`Total hours 2024`, decreasing = TRUE)])) %>%
  mutate(hover_text = paste0(Organization, "\nTotal Hours: ", `Total hours 2024`))

OH24_NL <- ggplot(CH24_OH_NL, aes(x = Organization, y = `Total hours 2024`,
                                  fill = `Total hours 2024`, text = hover_text)) +
  geom_col() +
  scale_fill_gradientn(colors = hombre) +
  scale_y_continuous(limits = c(0, y_max_OH_NL)) +
  labs(title = "Organizations by Hours in Engagement (2024, Non-LEAD)",
       subtitle = "Organizations with 0 engagement hours have been excluded",
       x = NULL, y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

saveWidget(ggplotly(OH24_NL, tooltip = "text"),
           "Charts/Non-LEAD/Orgs by Engagement Hours (Bar)/2024 Organizations by Engagement Hours Bar Non-LEAD.html",
           selfcontained = TRUE)

# 2025
CH25_OH_NL <- CH25C_NL %>%
  filter(`Total hours 2025` > 0) %>%
  mutate(Organization = factor(Organization,
                               levels = Organization[order(`Total hours 2025`, decreasing = TRUE)])) %>%
  mutate(hover_text = paste0(Organization, "\nTotal Hours: ", `Total hours 2025`))

OH25_NL <- ggplot(CH25_OH_NL, aes(x = Organization, y = `Total hours 2025`,
                                  fill = `Total hours 2025`, text = hover_text)) +
  geom_col() +
  scale_fill_gradientn(colors = hombre) +
  scale_y_continuous(limits = c(0, y_max_OH_NL)) +
  labs(title = "Organizations by Hours in Engagement (2025, Non-LEAD)",
       subtitle = "Organizations with 0 engagement hours have been excluded",
       x = NULL, y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

saveWidget(ggplotly(OH25_NL, tooltip = "text"),
           "Charts/Non-LEAD/Orgs by Engagement Hours (Bar)/2025 Organizations by Engagement Hours Bar Non-LEAD.html",
           selfcontained = TRUE)

## Hours by Engagement Modality - Bar ---------------------------------------------
# Shared Y-Max
y_max_HCB_NL <- 350


# 2023
CH23_HCB_NL <- CH23C_NL %>%
  select(-`Total hours 2023`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  filter(Hours > 0) %>%   
  arrange(desc(Hours))

CH23_HCB_NL$Modality <- factor(CH23_HCB_NL$Modality, levels = CH23_HCB_NL$Modality)

ggplot(CH23_HCB_NL, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(0, y_max_HCB_NL)) +
  scale_fill_manual(values = modality_colors_no_lead) +
  theme_minimal() +
  labs(
    title = "Engagement Hours by Modality (2023, Non-LEAD)",
    subtitle = "Sorted within year",
    x = "Engagement Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Charts/Non-LEAD/Engagement Hours by Modality/2023 Engagement Hours by Modality Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2024
CH24_HCB_NL <- CH24C_NL %>%
  select(-`Total hours 2024`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  filter(Hours > 0) %>%   
  arrange(desc(Hours))

CH24_HCB_NL$Modality <- factor(CH24_HCB_NL$Modality, levels = CH24_HCB_NL$Modality)

ggplot(CH24_HCB_NL, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(0, y_max_HCB_NL)) +
  scale_fill_manual(values = modality_colors_no_lead) +
  theme_minimal() +
  labs(
    title = "Total Engagement Hours by Modality (2024, Non-LEAD)",
    subtitle = "Sorted within year",
    x = "Engagement Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Charts/Non-LEAD/Engagement Hours by Modality/2024 Engagement Hours by Modality Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2025
CH25_HCB_NL <- CH25C_NL %>%
  select(-`Total hours 2025`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  filter(Hours > 0) %>%   
  arrange(desc(Hours))

CH25_HCB_NL$Modality <- factor(CH25_HCB_NL$Modality, levels = CH25_HCB_NL$Modality)

ggplot(CH25_HCB_NL, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(0, y_max_HCB_NL)) +
  scale_fill_manual(values = modality_colors_no_lead) +
  theme_minimal() +
  labs(
    title = "Total Engagement Hours by Modality (2025, Non-LEAD)",
    subtitle = "Sorted within year",
    x = "Engagement Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Charts/Non-LEAD/Engagement Hours by Modality/2025 Engagement Hours by Modality Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

### Longitudinal Viz ===========================================================
## Engagement Hours by Modality - YOY ---------------------------------------------
CH_yoy_NL <- bind_rows(
  prep_modality_year(CH23C_NL, 2023, "Total hours 2023", "PartnerType2023"),
  prep_modality_year(CH24C_NL, 2024, "Total hours 2024", "PartnerType2024"),
  prep_modality_year(CH25C_NL, 2025, "Total hours 2025", "PartnerType2025")
) %>%
  group_by(Year, Modality) %>%
  summarise(Hours = sum(Hours, na.rm = TRUE), .groups = "drop") %>%
  arrange(Modality, Year) %>%
  mutate(Year = as.integer(Year))

CHYOY_NL_html <- plot_ly(
  data = CH_yoy_NL,
  x = ~Year, y = ~Hours, color = ~Modality,
  colors = hombre,
  type = "scatter", mode = "lines+markers",
  text = ~paste0("Modality: ", Modality, "<br>Year: ", Year,
                 "<br>Total hours: ", scales::comma(Hours)),
  hoverinfo = "text"
) %>%
  layout(
    title = list(text = "Total Engagement Hours by Modality (Year over Year, Non-LEAD)", x = 0.5),
    xaxis = list(title = "", tickmode = "linear", dtick = 1),
    yaxis = list(title = "Total Engagement Hours", tickformat = ",", autorange = TRUE),
    legend = list(itemclick = "toggle", itemdoubleclick = "toggleothers")
  ) %>%
  config(displayModeBar = TRUE) %>%
  onRender("
    function(el, x) {
      function autoscaleY() {
        setTimeout(function() {
          Plotly.relayout(el, {'yaxis.autorange': true, 'yaxis.tickmode': 'auto'});
        }, 60);
      }
      el.on('plotly_legendclick', function() { autoscaleY(); });
      el.on('plotly_legenddoubleclick', function() { autoscaleY(); });
      el.on('plotly_restyle', function() { autoscaleY(); });
    }
  ")

saveWidget(CHYOY_NL_html,
           "Charts/Non-LEAD/Engagement Hours by Modality YoY/YoY Engagement Hours by Modality Non-LEAD.html",
           selfcontained = TRUE)

## Engagement Hours by Partner Type - YOY ------------------------------------------
CHMT_yoy_NL <- bind_rows(
  prep_partnertype_year(CH23C_NL, 2023, "Total hours 2023", "PartnerType2023"),
  prep_partnertype_year(CH24C_NL, 2024, "Total hours 2024", "PartnerType2024"),
  prep_partnertype_year(CH25C_NL, 2025, "Total hours 2025", "PartnerType2025")
) %>%
  mutate(Year = as.integer(Year),
         PartnerType = factor(PartnerType, levels = c("New", "Returning"))) %>%
  arrange(PartnerType, Year)

ggplot(CHMT_yoy_NL, aes(x = Year, y = TotalHours, color = PartnerType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = scales::comma(TotalHours)), vjust = -0.8, size = 4, show.legend = FALSE) +
  scale_color_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  scale_x_continuous(breaks = c(2023, 2024, 2025), expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.02, 0.12))) +
  labs(title = "Total Engagement Hours by Partner Type (Year over Year, Non-LEAD)",
       x = "", y = "Total Engagement Hours", color = "Partner Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Engagement Hours by Partner Type YoY/YoY Engagement Hours by Partner Type Non-LEAD.png",
       width = 7, height = 4, dpi = 300)


############################# Unique Org Summary ###############################
### Unique Org Summary =========================================================
# Unique orgs per year (excluding UNAF and 0 hour orgs)
unique_orgs_23 <- CH23C %>% filter(!is.na(ID), ID != "UNAF", `Total hours 2023` > 0) %>% distinct(ID) %>% nrow()
unique_orgs_24 <- CH24C %>% filter(!is.na(ID), ID != "UNAF", `Total hours 2024` > 0) %>% distinct(ID) %>% nrow()
unique_orgs_25 <- CH25C %>% filter(!is.na(ID), ID != "UNAF", `Total hours 2025` > 0) %>% distinct(ID) %>% nrow()

# Unique orgs across all 3 years combined
unique_orgs_total <- bind_rows(
  CH23C %>% filter(`Total hours 2023` > 0) %>% select(ID),
  CH24C %>% filter(`Total hours 2024` > 0) %>% select(ID),
  CH25C %>% filter(`Total hours 2025` > 0) %>% select(ID)
) %>%
  filter(!is.na(ID), ID != "UNAF") %>%
  distinct(ID) %>%
  nrow()

# New orgs in the past 3 years (orgs that appear in 24 or 25 but NOT in 23)
orgs_23 <- CH23C %>% filter(!is.na(ID), ID != "UNAF", `Total hours 2023` > 0) %>% distinct(ID) %>% pull(ID)

new_orgs_since_23 <- bind_rows(
  CH24C %>% filter(`Total hours 2024` > 0) %>% select(ID),
  CH25C %>% filter(`Total hours 2025` > 0) %>% select(ID)
) %>%
  filter(!is.na(ID), ID != "UNAF") %>%
  distinct(ID) %>%
  filter(!ID %in% orgs_23) %>%
  nrow()

# Print summary
cat("Unique orgs in 2023:", unique_orgs_23, "\n")
cat("Unique orgs in 2024:", unique_orgs_24, "\n")
cat("Unique orgs in 2025:", unique_orgs_25, "\n")
cat("Unique orgs across 2023-2025:", unique_orgs_total, "\n")
cat("New orgs added since 2023:", new_orgs_since_23, "\n")

### Confirming the same # of orgs in 2023 and 2025 is real =====================
## Obtaining Full Lists --------------------------------------------------------
# Get the org lists for each year
orgs_23_list <- CH23C %>% filter(!is.na(ID), ID != "UNAF", `Total hours 2023` > 0) %>% distinct(ID, Organization)
orgs_24_list <- CH24C %>% filter(!is.na(ID), ID != "UNAF", `Total hours 2024` > 0) %>% distinct(ID, Organization)
orgs_25_list <- CH25C %>% filter(!is.na(ID), ID != "UNAF", `Total hours 2025` > 0) %>% distinct(ID, Organization)

## Identifying Differences -----------------------------------------------------
# Orgs in 2025 but NOT in 2023 (genuinely new)
in_25_not_23 <- orgs_25_list %>% filter(!ID %in% orgs_23_list$ID)

# Orgs in 2023 but NOT in 2025 (dropped off)
in_23_not_25 <- orgs_23_list %>% filter(!ID %in% orgs_25_list$ID)

cat("Orgs in 2025 but not 2023:", nrow(in_25_not_23), "\n")
cat("Orgs in 2023 but not 2025:", nrow(in_23_not_25), "\n")

View(in_25_not_23)
View(in_23_not_25)

### Notes ======================================================================
### I want to do something figuring out how many returning partners stay PAST one year of returning.
## Might be getting unique identifiers for stakeholders from SALESFORCE
  # Asked for a CSV of their internal identifiers & org name from the salesforce system


### Engagement Hours by nonprofit YOY
  ## Profile of top #25 orgs
    # if the visual is too crowded, cut the number down (pick top __, or most interesting orgs.)


# of hours as a pie chart 2025
  # low-ish priority (end of month)


"#F27B35"
"#FFC000"
"#76A646"
"#457ABF"
"#A5A5A5"




