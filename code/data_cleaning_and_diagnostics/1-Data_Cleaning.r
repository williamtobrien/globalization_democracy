# SETWD ===========================
rm(list = ls())

# set working directory to the "globalization_democracy" folder

# LIBRARIES ===========================
library(tidyverse)

# IMPORT DATA ===========================
wdi <- read.csv("data/wdi.csv", ,stringsAsFactors = F)

# panel information for WDI
length(unique(wdi$country))
min(wdi$year)
max(wdi$year)

vdem <- read.csv("data/V-Dem-CY-Core-v15.csv", ,stringsAsFactors = F)
# panel information for V-Dem
length(unique(vdem$country_name))
min(vdem$year)
max(vdem$year)

# CLEAN DATA ===========================
vdem <- vdem %>%
  select(country_name, country_text_id, year,
         v2xel_frefair, v2xcl_rol, v2xeg_eqaccess,
         v2x_polyarchy, v2x_libdem)

# Filter out unmatched V-Dem years in order to ease re-coding mismatched country codes
min_wdi_year <- min(wdi$year, na.rm = TRUE)

vdem <- vdem %>%
  filter(year >= (min_wdi_year - 1)) %>% # Need to keep values from a year before the sample start because we will later use lags
  select(country_name, country_text_id, year,
         v2xel_frefair, v2xcl_rol, v2xeg_eqaccess,
         v2x_polyarchy, v2x_libdem)

rm(min_wdi_year)

# Check for mismatched coding
unmatched_codes <- vdem %>%
  distinct(country_text_id, country_name) %>%
  anti_join(
    wdi %>% distinct(iso3c) %>% rename(country_text_id = iso3c),
    by = "country_text_id"
  )

print(unmatched_codes)

# All unmatched countries need to be dropped from V-Dem, as they do not have corresponding WDI counterparts
vdem <- vdem %>%
  filter(!country_text_id %in% unmatched_codes$country_text_id)

rm(unmatched_codes)

vdem <- vdem %>%
  rename(iso3c = country_text_id,
         country = country_name,
         freefair = v2xel_frefair,
         equallaw = v2xcl_rol,
         equalaccess = v2xeg_eqaccess,
         polyarchy = v2x_polyarchy,
         libdem = v2x_libdem
         )

# Merge datasets and clean up excess columns and ordering
full_panel <- vdem %>%
  left_join(wdi %>% select(-country), by = c("iso3c","year")) %>%
  filter(!is.na(freefair), !is.na(equallaw), !is.na(equalaccess)) %>%
  select(
    country, year, iso3c,
    freefair, equallaw, equalaccess,
    polyarchy, libdem,
    fdi_inflows, fdi_outflows, trade, tariff_rate,
    gdppc, gdp_growth, inflation, pop_size, pop_growth
  )


# Add in columns for the relevant subsamples
ldcs <- c(
  "AGO","BEN","BFA","BDI","CAF","TCD","COM","COD","DJI","ERI","ETH",
  "GMB","GIN","GNB","LSO","LBR","MDG","MWI","MLI","MRT","MOZ","NER",
  "RWA","SEN","SLE","SOM","SSD","SDN","TGO","UGA","TZA","ZMB","AFG",
  "BGD","KHM","LAO","MMR","NPL","TLS","YEM","HTI", "KIR","SLB","TUV"
)

full_panel <- full_panel %>%
  mutate(
    development = "Developing"
  )

full_panel$development[full_panel$iso3c %in% ldcs] <- "LDC"

graduated_countries <- c(
  BWA = 1994,
  CPV = 2007,
  MDV = 2011,
  WSM = 2014,
  GNQ = 2017,
  VUT = 2020,
  BTN = 2023,
  STP = 2024
)

# Need to modify countries who have graduated from LDCs by changing their status at and past their graduation date
for (country in names(graduated_countries)) {
  grad_year <- graduated_countries[[country]]
  full_panel$development[full_panel$iso3c == country & full_panel$year >= grad_year] <- "Developing"
}

oecds <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX",
  "MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR",
  "GBR","USA"
)

accession_countries <- c(
  AUS = 1971, AUT = 1961, BEL = 1961, CAN = 1961, CHL = 2010, COL = 2020,
  CRI = 2021, CZE = 1995, DNK = 1961, EST = 2010, FIN = 1969, FRA = 1961,
  DEU = 1961, GRC = 1961, HUN = 1996, ISL = 1961, IRL = 1961, ISR = 2010,
  ITA = 1962, JPN = 1964, KOR = 1996, LVA = 2016, LTU = 2018, LUX = 1961,
  MEX = 1994, NLD = 1961, NZL = 1973, NOR = 1961, POL = 1996, PRT = 1961,
  SVK = 2000, SVN = 2010, ESP = 1961, SWE = 1961, CHE = 1961, TUR = 1961,
  GBR = 1961, USA = 1961
)

# Need to modify countries who joined the OECD by changing their status at and past their accession date
for (country in names(accession_countries)) {
  acc_year <- accession_countries[[country]]
  full_panel$development[full_panel$iso3c == country & full_panel$year >= acc_year] <- "OECD"
}

rm(ldcs, country, graduated_countries, grad_year, oecds, accession_countries, acc_year)


# SAMPLE CREATION ===========================
# Create lags
full_panel <- full_panel %>%
  group_by(iso3c) %>%
  arrange(iso3c, year) %>%
  mutate(
    lag_freefair = dplyr::lag(freefair, 1),
    lag_equallaw = dplyr::lag(equallaw, 1),
    lag_equalaccess = dplyr::lag(equalaccess, 1),
    lag_fdi_inflows = dplyr::lag(fdi_inflows, 1),
    lag_fdi_outflows = dplyr::lag(fdi_outflows, 1),
    lag_trade = dplyr::lag(trade, 1),
    lag_tariff_rate = dplyr::lag(tariff_rate, 1),
    lag_gdppc = dplyr::lag(gdppc, 1),
    lag_gdp_growth = dplyr::lag(gdp_growth, 1),
    lag_pop_size = dplyr::lag(pop_size, 1),
    lag_pop_growth = dplyr::lag(pop_growth, 1),
    lag_inflation = dplyr::lag(inflation, 1),
    lag2_freefair = dplyr::lag(freefair, 2),
    lag2_equallaw = dplyr::lag(equallaw, 2),
    lag2_equalaccess = dplyr::lag(equalaccess, 2),
  ) %>%
  ungroup()


## Descriptive Stats ----
# number of countries:
length(unique(full_panel$country))

# time horizon:
length(unique(full_panel$year))

# number of observations:
length((full_panel$country))

# number of least developed countries:
length(unique(full_panel$iso3c[full_panel$development  == "LDC"]))

# number of developing countries:
length(unique(full_panel$iso3c[full_panel$development  == "Developing"]))

# number of OECD countries:
length(unique(full_panel$iso3c[full_panel$development  == "OECD"]))

write.csv(full_panel, "data/full_panel.csv", row.names = FALSE)
