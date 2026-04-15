#################################### Setup #####################################
### Clean Memory ===============================================================
rm(list = ls()) # clears working memory (good housekeeping)
gc()
### Libraries ==================================================================
library(tidyverse)
library(fuzzyjoin)
library(plotly)
library(htmlwidgets)


### Work Directory =============================================================
setwd("/Users/Jack/Desktop/CNE /Data Analysis/Contact Hours")

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
    MemberType2023 = if_else(
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
    MemberType2023 = case_when(
      Organization == "Community Foundation for Monterey County" ~ "Returning",
      TRUE ~ MemberType2023
    )
  )

## Standardizing recording metrics ---------------------------------------------
CH23.3 <- CH23.2 %>%
  mutate(
    SIEDI = ifelse(SIEDI == "x", "yes", "no")
  ) %>%
  mutate(
    across(
      .cols = where(is.character) & !c(Organization, SIEDI, MemberType2023),
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
    MemberType2023 = first(na.omit(MemberType2023)),
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

## Removing Orgs with no contact hours -----------------------------------
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

## Removing Orgs with no contact hours -----------------------------------
CH24.5 <- CH24.4 %>%
  filter(
    !Organization == "Total"
  ) %>%
  filter(
    !("Total hours 2024" == 0)
  ) %>%
  select(-SIEDI)


## Identifying New/Returning Members -------------------------------------------
CH24.6 <- CH24.5 %>%
  mutate(
    MemberType2024 = case_when(
      ID %in% na.omit(ExistingOrgs23$ID) ~ "Returning",
      TRUE ~ "New"
    )
  )

## Adding New orgs to the Existing Orgs List -----------------------------------
# Identifying New Orgs
NewOrgs <- CH24.6 %>%
  filter(
    MemberType2024 == "New",
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

## Removing Orgs with no contact hours -----------------------------------------
CH25.5 <- CH25.4 %>%
  filter(
    !Organization == "Total"
  ) %>%
  filter(
    !("Total hours 2025" == 0)
  ) %>%
  select(-SIEDI)


## Identifying New/Returning Members -------------------------------------------
CH25.6 <- CH25.5 %>%
  mutate(
    MemberType2025 = case_when(
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
### Individual Year Viz ========================================================
## New/Returning by Year - Pie -------------------------------------------------
# 2023
CH23C2 <- CH23C %>%
  filter(!`Total hours 2023` == 0)

ggplot(CH23C2, aes(x = "", fill = MemberType2023)) +
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
    title = "Member Type as Share of Total (2023)",
    fill = "Member Type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Member Type by Year (Pie)/2023 Member Type Pie.png",
       width = 6,
       height = 4,
       dpi = 300
       )

na_orgs <- CH23C2 %>%
  filter(is.na(MemberType2023)) %>%
  select(Organization) 

print(na_orgs)


# 2024
CH24C2 <- CH24C %>%
  filter(!`Total hours 2024` == 0)

ggplot(CH24C2, aes(x = "", fill = MemberType2024)) +
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
    title = "Member Type as Share of Total (2024)",
    fill = "Member Type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Member Type by Year (Pie)/2024 Member Type Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)



# 2025
CH25C2 <- CH25C %>%
  filter(!`Total hours 2025` == 0)

ggplot(CH25C2, aes(x = "", fill = MemberType2025)) +
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
    title = "Member Type as Share of Total (2025)",
    fill = "Member Type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


ggsave("Charts/Member Type by Year (Pie)/2025 Member Type Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)


## Contact Hours by Member Type - Bar  -----------------------------------------
# 2023
CH23_CHMT <- CH23C %>%
  group_by(MemberType2023) %>%
  summarise(TotalHours = sum(`Total hours 2023`, na.rm = TRUE))

ggplot(CH23_CHMT, aes(x = MemberType2023, y = TotalHours, fill = MemberType2023)) +
  geom_col() +
  geom_text(
    aes(label = TotalHours),   # Show only total hours per member type
    vjust = 1.5,
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  labs(
    title = "Total Contact Hours by Member Type (2023)",
    x = "Member Type",
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Contact Hours by Member Type (Bar)/2023 Contact Hour by Member Type Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)


# 2024
CH24_CHMT <- CH24C %>%
  group_by(MemberType2024) %>%
  summarise(TotalHours = sum(`Total hours 2024`, na.rm = TRUE))

ggplot(CH24_CHMT, aes(x = MemberType2024, y = TotalHours, fill = MemberType2024)) +
  geom_col() +
  geom_text(
    aes(label = TotalHours),   # Show only total hours per member type
    vjust = 1.5,
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  labs(
    title = "Total Contact Hours by Member Type (2024)",
    x = "Member Type",
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Contact Hours by Member Type (Bar)/2024 Contact Hour by Member Type Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)

# 2025
CH25_CHMT <- CH25C %>%
  group_by(MemberType2025) %>%
  summarise(TotalHours = sum(`Total hours 2025`, na.rm = TRUE))

ggplot(CH25_CHMT, aes(x = MemberType2025, y = TotalHours, fill = MemberType2025)) +
  geom_col() +
  geom_text(
    aes(label = TotalHours),   # Show only total hours per member type
    vjust = 1.5,
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  labs(
    title = "Total Contact Hours by Member Type (2025)",
    x = "Member Type",
    y = "Total Hours"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Contact Hours by Member Type (Bar)/2025 Contact Hour by Member Type Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)

## Orgs with Hours - Pie ---------------------------------------------------------
# 2023
CH23_NH <- CH23C %>%
  filter(MemberType2023 == "Returning") %>%
  mutate(InContact = ifelse(`Total hours 2023` == 0, 0, 1)) %>%
  select(Organization, InContact)

ggplot(CH23_NH, aes(x = "", fill = factor(InContact))) +  # <-- convert to factor
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
    labels = c("0" = "Not in Contact", "1" = "In Contact")
  ) +
  labs(
    title = "Returning Members in Contact (2023)",
    fill = "Contact Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Returning Members in Contact/2023 Returning Member Contact Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)

# 2024
CH24_NH <- CH24C %>%
  filter(MemberType2024 == "Returning") %>%
  mutate(InContact = ifelse(`Total hours 2024` == 0, 0, 1)) %>%
  select(Organization, InContact)

ggplot(CH24_NH, aes(x = "", fill = factor(InContact))) +  # <-- convert to factor
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
    labels = c("0" = "Not in Contact", "1" = "In Contact")
  ) +
  labs(
    title = "Returning Members in Contact (2024)",
    fill = "Contact Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Returning Members in Contact/2024 Returning Member Contact Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)

# 2025
CH25_NH <- CH25C %>%
  filter(MemberType2025 == "Returning") %>%
  mutate(InContact = ifelse(`Total hours 2025` == 0, 0, 1)) %>%
  select(Organization, InContact)

ggplot(CH25_NH, aes(x = "", fill = factor(InContact))) +  # <-- convert to factor
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
    labels = c("0" = "Not in Contact", "1" = "In Contact")
  ) +
  labs(
    title = "Returning Members in Contact (2025)",
    fill = "Contact Status"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Charts/Returning Members in Contact/2025 Returning Member Contact Pie.png",
       width = 6,
       height = 4,
       dpi = 300
)


## Orgs by Hours - Bar ---------------------------------------------------------
# 2023
hombre <- c("#A5A5A5", "#457ABF", "#76A646", "#FFC000", "#F27B35")

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
  labs(
    title = "Organizations by Hours in Contact (2023)",
    subtitle = "Organizations with 0 contact hours have been excluded",
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


saveWidget(saveOH23, "Charts/Orgs by Contact Hours (Bar)/2023 Organizations by Contact Hours Bar.html", selfcontained = TRUE)


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
  labs(
    title = "Organizations by Hours in Contact (2024)",
    subtitle = "Organizations with 0 contact hours have been excluded",
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


saveWidget(saveOH24, "Charts/Orgs by Contact Hours (Bar)/2024 Organizations by Contact Hours Bar.html", selfcontained = TRUE)


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
  labs(
    title = "Organizations by Hours in Contact (2025)",
    subtitle = "Organizations with 0 contact hours have been excluded",
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


saveWidget(saveOH25, "Charts/Orgs by Contact Hours (Bar)/2025 Organizations by Contact Hours Bar.html", selfcontained = TRUE)


## Hours by Contact Modality - Bar ---------------------------------------------
# 2023
CH23_HCB <- CH23C %>%
  select(-`Total hours 2023`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Modality",
    values_to = "Hours"
  ) %>%
  arrange(desc(Hours))

hombre2 <- c("#F27B35", "#FFC000", "#76A646","#457ABF","#A5A5A5")
palette <- rep(hombre2, length.out = nrow(CH23_HCB))


CH23_HCB$Modality <- factor(CH23_HCB$Modality, levels = CH23_HCB$Modality)

ggplot(CH23_HCB, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(
    aes(label = Hours),
    vjust = -0.2,
    color = "black",
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  labs(
    title = "Contact Hours by Modality (2023)",
    subtitle = "Sorted within year;\nColors not consistent across years due to changing modalities.",
    x = "Contact Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("Charts/Contact Hours by Modality/2023 Contact Hours by Modality Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)


# 2024
CH24_HCB <- CH24C %>%
  select(-`Total hours 2024`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Modality",
    values_to = "Hours"
  ) %>%
  arrange(desc(Hours))

hombre2 <- c("#F27B35", "#FFC000", "#76A646","#457ABF","#A5A5A5")
palette <- rep(hombre2, length.out = nrow(CH24_HCB))


CH24_HCB$Modality <- factor(CH24_HCB$Modality, levels = CH24_HCB$Modality)

ggplot(CH24_HCB, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(
    aes(label = Hours),
    vjust = -0.2,
    color = "black",
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  labs(
    title = "Total Contact Hours by Modality (2024)",
    subtitle = "Sorted within year;\ncolors not consistent across years due to changing modalities",
    x = "Contact Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



ggsave("Charts/Contact Hours by Modality/2024 Contact Hours by Modality Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)


# 2025
CH25_HCB <- CH25C %>%
  select(-`Total hours 2025`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Modality",
    values_to = "Hours"
  ) %>%
  arrange(desc(Hours))

hombre2 <- c("#F27B35", "#FFC000", "#76A646","#457ABF","#A5A5A5")
palette <- rep(hombre2, length.out = nrow(CH25_HCB))


CH25_HCB$Modality <- factor(CH25_HCB$Modality, levels = CH25_HCB$Modality)

ggplot(CH25_HCB, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(
    aes(label = Hours),
    vjust = -0.2,
    color = "black",
    fontface = "bold",
    size = 4
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  labs(
    title = "Total Contact Hours by Modality (2025)",
    subtitle = "Sorted within year;\ncolors not consistent across years due to changing modalities",
    x = "Contact Modality",
    y = "Total Hours"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



ggsave("Charts/Contact Hours by Modality/2025 Contact Hours by Modality Bar.png",
       width = 6,
       height = 4,
       dpi = 300
)

### Longitudinal Viz ===========================================================
## Contact hours by Contact Modality - YOY -------------------------------------
# Making the helper function: collapsing one year to (Year, Modality, Hours)
prep_modality_year <- function(df, year, total_col, member_col) {
  df %>%
    select(-any_of(c(total_col, member_col, "Organization"))) %>%  # keep only modality columns
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
  prep_modality_year(CH23C, 2023, "Total hours 2023", "MemberType2023"),
  prep_modality_year(CH24C, 2024, "Total hours 2024", "MemberType2024"),
  prep_modality_year(CH25C, 2025, "Total hours 2025", "MemberType2025")
) %>%
  group_by(Year, Modality) %>%                      # safety: if duplicates ever occur
  summarise(Hours = sum(Hours, na.rm = TRUE), .groups = "drop") %>%
  arrange(Modality, Year)

# Make sure years plot in order
CH_yoy <- CH_yoy %>%
  mutate(Year = as.integer(Year))

# Color Pallete
hombre <- c("#A5A5A5", "#457ABF", "#76A646", "#FFC000", "#F27B35")

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
    title = list(text = "Total Contact Hours by Modality (Year over Year)", x = 0.5),
    xaxis = list(title = "", tickmode = "linear", dtick = 1),
    yaxis = list(
      title = "Total Contact Hours",
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
  "Charts/Contact Hours by Modality YoY/YoY Contact Hours by Modality.html",
  selfcontained = TRUE
)

## Contact Hours by Member Type - YOY ------------------------------------------
# Helper: collapse one year to (Year, MemberType, TotalHours)
prep_membertype_year <- function(df, year, total_col, member_col) {
  df %>%
    filter(.data[[total_col]] != 0) %>%   # optional: match your pie-chart logic
    group_by(MemberType = .data[[member_col]]) %>%
    summarise(
      TotalHours = sum(.data[[total_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Year = year)
}

# Build longitudinal dataset
CHMT_yoy <- bind_rows(
  prep_membertype_year(CH23C, 2023, "Total hours 2023", "MemberType2023"),
  prep_membertype_year(CH24C, 2024, "Total hours 2024", "MemberType2024"),
  prep_membertype_year(CH25C, 2025, "Total hours 2025", "MemberType2025")
) %>%
  mutate(
    Year = as.integer(Year),
    MemberType = factor(MemberType, levels = c("New", "Returning"))
  ) %>%
  arrange(MemberType, Year)

# GGplot it
ggplot(CHMT_yoy, aes(x = Year, y = TotalHours, color = MemberType)) +
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
    title = "Total Contact Hours by Member Type (Year over Year)",
    x = "",
    y = "Total Contact Hours",
    color = "Member Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Save it
ggsave(
  "Charts/Contact Hours by Member Type YoY/YoY Contact Hours by Member Type.png",
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

ggplot(CH23C2_NL, aes(x = "", fill = MemberType2023)) +
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
  labs(title = "Member Type as Share of Total (2023, Non-LEAD)", fill = "Member Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Member Type by Year (Pie)/2023 Member Type Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2024
CH24C2_NL <- CH24C_NL %>%
  filter(!`Total hours 2024` == 0)

ggplot(CH24C2_NL, aes(x = "", fill = MemberType2024)) +
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
  labs(title = "Member Type as Share of Total (2024, Non-LEAD)", fill = "Member Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Member Type by Year (Pie)/2024 Member Type Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2025
CH25C2_NL <- CH25C_NL %>%
  filter(!`Total hours 2025` == 0)

ggplot(CH25C2_NL, aes(x = "", fill = MemberType2025)) +
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
  labs(title = "Member Type as Share of Total (2025, Non-LEAD)", fill = "Member Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Member Type by Year (Pie)/2025 Member Type Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

## Contact Hours by Member Type - Bar ------------------------------------------
# 2023
CH23_CHMT_NL <- CH23C_NL %>%
  group_by(MemberType2023) %>%
  summarise(TotalHours = sum(`Total hours 2023`, na.rm = TRUE))

ggplot(CH23_CHMT_NL, aes(x = MemberType2023, y = TotalHours, fill = MemberType2023)) +
  geom_col() +
  geom_text(aes(label = TotalHours), vjust = 1.5, color = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  labs(title = "Total Contact Hours by Member Type (2023, Non-LEAD)",
       x = "Member Type", y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Contact Hours by Member Type (Bar)/2023 Contact Hour by Member Type Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2024
CH24_CHMT_NL <- CH24C_NL %>%
  group_by(MemberType2024) %>%
  summarise(TotalHours = sum(`Total hours 2024`, na.rm = TRUE))

ggplot(CH24_CHMT_NL, aes(x = MemberType2024, y = TotalHours, fill = MemberType2024)) +
  geom_col() +
  geom_text(aes(label = TotalHours), vjust = 1.5, color = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  labs(title = "Total Contact Hours by Member Type (2024, Non-LEAD)",
       x = "Member Type", y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Contact Hours by Member Type (Bar)/2024 Contact Hour by Member Type Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2025
CH25_CHMT_NL <- CH25C_NL %>%
  group_by(MemberType2025) %>%
  summarise(TotalHours = sum(`Total hours 2025`, na.rm = TRUE))

ggplot(CH25_CHMT_NL, aes(x = MemberType2025, y = TotalHours, fill = MemberType2025)) +
  geom_col() +
  geom_text(aes(label = TotalHours), vjust = 1.5, color = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  labs(title = "Total Contact Hours by Member Type (2025, Non-LEAD)",
       x = "Member Type", y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Contact Hours by Member Type (Bar)/2025 Contact Hour by Member Type Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

## Orgs with Hours - Pie -------------------------------------------------------
# 2023
CH23_NH_NL <- CH23C_NL %>%
  filter(MemberType2023 == "Returning") %>%
  mutate(InContact = ifelse(`Total hours 2023` == 0, 0, 1)) %>%
  select(Organization, InContact)

ggplot(CH23_NH_NL, aes(x = "", fill = factor(InContact))) +
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
                    labels = c("0" = "Not in Contact", "1" = "In Contact")) +
  labs(title = "Returning Members in Contact (2023, Non-LEAD)", fill = "Contact Status") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Returning Members in Contact/2023 Returning Member Contact Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2024
CH24_NH_NL <- CH24C_NL %>%
  filter(MemberType2024 == "Returning") %>%
  mutate(InContact = ifelse(`Total hours 2024` == 0, 0, 1)) %>%
  select(Organization, InContact)

ggplot(CH24_NH_NL, aes(x = "", fill = factor(InContact))) +
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
                    labels = c("0" = "Not in Contact", "1" = "In Contact")) +
  labs(title = "Returning Members in Contact (2024, Non-LEAD)", fill = "Contact Status") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Returning Members in Contact/2024 Returning Member Contact Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2025
CH25_NH_NL <- CH25C_NL %>%
  filter(MemberType2025 == "Returning") %>%
  mutate(InContact = ifelse(`Total hours 2025` == 0, 0, 1)) %>%
  select(Organization, InContact)

ggplot(CH25_NH_NL, aes(x = "", fill = factor(InContact))) +
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
                    labels = c("0" = "Not in Contact", "1" = "In Contact")) +
  labs(title = "Returning Members in Contact (2025, Non-LEAD)", fill = "Contact Status") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Returning Members in Contact/2025 Returning Member Contact Pie Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

## Orgs by Hours - Bar ---------------------------------------------------------
hombre <- c("#A5A5A5", "#457ABF", "#76A646", "#FFC000", "#F27B35")

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
  labs(title = "Organizations by Hours in Contact (2023, Non-LEAD)",
       subtitle = "Organizations with 0 contact hours have been excluded",
       x = NULL, y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

saveWidget(ggplotly(OH23_NL, tooltip = "text"),
           "Charts/Non-LEAD/Orgs by Contact Hours (Bar)/2023 Organizations by Contact Hours Bar Non-LEAD.html",
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
  labs(title = "Organizations by Hours in Contact (2024, Non-LEAD)",
       subtitle = "Organizations with 0 contact hours have been excluded",
       x = NULL, y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

saveWidget(ggplotly(OH24_NL, tooltip = "text"),
           "Charts/Non-LEAD/Orgs by Contact Hours (Bar)/2024 Organizations by Contact Hours Bar Non-LEAD.html",
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
  labs(title = "Organizations by Hours in Contact (2025, Non-LEAD)",
       subtitle = "Organizations with 0 contact hours have been excluded",
       x = NULL, y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

saveWidget(ggplotly(OH25_NL, tooltip = "text"),
           "Charts/Non-LEAD/Orgs by Contact Hours (Bar)/2025 Organizations by Contact Hours Bar Non-LEAD.html",
           selfcontained = TRUE)

## Hours by Contact Modality - Bar ---------------------------------------------
hombre2 <- c("#F27B35", "#FFC000", "#76A646", "#457ABF", "#A5A5A5")

# 2023
CH23_HCB_NL <- CH23C_NL %>%
  select(-`Total hours 2023`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  arrange(desc(Hours))

CH23_HCB_NL$Modality <- factor(CH23_HCB_NL$Modality, levels = CH23_HCB_NL$Modality)

ggplot(CH23_HCB_NL, aes(x = Modality, y = Hours,
                        fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = rep(hombre2, length.out = nrow(CH23_HCB_NL))) +
  theme_minimal() +
  labs(title = "Contact Hours by Modality (2023, Non-LEAD)",
       subtitle = "Sorted within year;\nColors not consistent across years due to changing modalities.",
       x = "Contact Modality", y = "Total Hours") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Charts/Non-LEAD/Contact Hours by Modality/2023 Contact Hours by Modality Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2024
CH24_HCB_NL <- CH24C_NL %>%
  select(-`Total hours 2024`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  arrange(desc(Hours))

CH24_HCB_NL$Modality <- factor(CH24_HCB_NL$Modality, levels = CH24_HCB_NL$Modality)

ggplot(CH24_HCB_NL, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = rep(hombre2, length.out = nrow(CH24_HCB_NL))) +
  theme_minimal() +
  labs(title = "Total Contact Hours by Modality (2024, Non-LEAD)",
       subtitle = "Sorted within year;\ncolors not consistent across years due to changing modalities",
       x = "Contact Modality", y = "Total Hours") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Charts/Non-LEAD/Contact Hours by Modality/2024 Contact Hours by Modality Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

# 2025
CH25_HCB_NL <- CH25C_NL %>%
  select(-`Total hours 2025`) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Modality", values_to = "Hours") %>%
  arrange(desc(Hours))

CH25_HCB_NL$Modality <- factor(CH25_HCB_NL$Modality, levels = CH25_HCB_NL$Modality)

ggplot(CH25_HCB_NL, aes(x = Modality, y = Hours, fill = Modality)) +
  geom_col() +
  geom_text(aes(label = Hours), vjust = -0.2, color = "black", fontface = "bold", size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = rep(hombre2, length.out = nrow(CH25_HCB_NL))) +
  theme_minimal() +
  labs(title = "Total Contact Hours by Modality (2025, Non-LEAD)",
       subtitle = "Sorted within year;\ncolors not consistent across years due to changing modalities",
       x = "Contact Modality", y = "Total Hours") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 6)),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40", margin = margin(b = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Charts/Non-LEAD/Contact Hours by Modality/2025 Contact Hours by Modality Bar Non-LEAD.png",
       width = 6, height = 4, dpi = 300)

### Longitudinal Viz ===========================================================
## Contact Hours by Modality - YOY ---------------------------------------------
CH_yoy_NL <- bind_rows(
  prep_modality_year(CH23C_NL, 2023, "Total hours 2023", "MemberType2023"),
  prep_modality_year(CH24C_NL, 2024, "Total hours 2024", "MemberType2024"),
  prep_modality_year(CH25C_NL, 2025, "Total hours 2025", "MemberType2025")
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
    title = list(text = "Total Contact Hours by Modality (Year over Year, Non-LEAD)", x = 0.5),
    xaxis = list(title = "", tickmode = "linear", dtick = 1),
    yaxis = list(title = "Total Contact Hours", tickformat = ",", autorange = TRUE),
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
           "Charts/Non-LEAD/Contact Hours by Modality YoY/YoY Contact Hours by Modality Non-LEAD.html",
           selfcontained = TRUE)

## Contact Hours by Member Type - YOY ------------------------------------------
CHMT_yoy_NL <- bind_rows(
  prep_membertype_year(CH23C_NL, 2023, "Total hours 2023", "MemberType2023"),
  prep_membertype_year(CH24C_NL, 2024, "Total hours 2024", "MemberType2024"),
  prep_membertype_year(CH25C_NL, 2025, "Total hours 2025", "MemberType2025")
) %>%
  mutate(Year = as.integer(Year),
         MemberType = factor(MemberType, levels = c("New", "Returning"))) %>%
  arrange(MemberType, Year)

ggplot(CHMT_yoy_NL, aes(x = Year, y = TotalHours, color = MemberType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = scales::comma(TotalHours)), vjust = -0.8, size = 4, show.legend = FALSE) +
  scale_color_manual(values = c("New" = "#76A646", "Returning" = "#457ABF")) +
  scale_x_continuous(breaks = c(2023, 2024, 2025), expand = expansion(mult = c(0.08, 0.08))) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.02, 0.12))) +
  labs(title = "Total Contact Hours by Member Type (Year over Year, Non-LEAD)",
       x = "", y = "Total Contact Hours", color = "Member Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Charts/Non-LEAD/Contact Hours by Member Type YoY/YoY Contact Hours by Member Type Non-LEAD.png",
       width = 7, height = 4, dpi = 300)


### Notes ======================================================================
### I want to do something figuring out how many returning members stay PAST one year of returning.
## Might be getting unique identifiers for stakeholders from SALESFORCE
  # Asked for a CSV of their internal identifiers & org name from the salesforce system


### Contact Hours by nonprofit YOY
  ## Profile of top #25 orgs
    # if the visual is too crowded, cut the number down (pick top __, or most interesting orgs.)


# of hours as a pie chart 2025
  # low-ish priority (end of month)


"#F27B35"
"#FFC000"
"#76A646"
"#457ABF"
"#A5A5A5"

