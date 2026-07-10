library(tidyverse)
library(sf)
library(censusapi)
library(tidycensus)
library(lubridate)
library(tigris)
library(httr)

options(tigris_use_cache = TRUE)

year_less_0 <- as.numeric(format(Sys.Date(), "%Y"))
year_less_1 <- as.numeric(format(Sys.Date(), "%Y"))-1
year_less_2 <- as.numeric(format(Sys.Date(), "%Y"))-2

#function to remove spaces from column titles because they will convert to 
#periods on import into the app, and this can lead to a mismatch between summary 
#data and var_config
make_safe_cols <- function(df){
  names(df) <- gsub(" ", "_", names(df)) 
  names(df) <- gsub("-", "..hyphen..", names(df), fixed = TRUE) 
  names(df) <- gsub("/", "..fslash..", names(df), fixed = TRUE) 
  names(df) <- gsub("(", "..oparen..", names(df), fixed = TRUE) 
  names(df) <- gsub(")", "..cparen..", names(df), fixed = TRUE) 
  names(df) <- gsub("+", "..plus..", names(df), fixed = TRUE) 
  names(df) <- gsub("<", "..lessthan..", names(df), fixed = TRUE) 
  names(df) <- gsub(">", "..greaterthan..", names(df), fixed = TRUE) 
  names(df) <- gsub("%", "..pcent..", names(df), fixed = TRUE) 
  names(df) <- gsub(":", "..colon..", names(df), fixed = TRUE) 
  return(df)
}

#______________________----
#GEO_CENSUS_TRACT----
##*base geo----
#PA tracts
tract_v10_pa <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "PA",
  county = c("Philadelphia", "Montgomery", "Bucks", "Delaware", "Chester"),
  year = 2010,
  geometry = TRUE
)

tract_v20_pa <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "PA",
  county = c("Philadelphia", "Montgomery", "Bucks", "Delaware", "Chester"),
  year = 2020,
  geometry = TRUE
)

#NJ tracts
tract_v10_nj <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NJ",
  county = c("Camden", "Burlington", "Gloucester", "Salem"),
  year = 2010,
  geometry = TRUE
)

tract_v20_nj <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NJ",
  county = c("Camden", "Burlington", "Gloucester", "Salem"),
  year = 2020,
  geometry = TRUE
)

#DE tracts 
tract_v10_de <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "DE",
  county = c("New Castle"),
  year = 2010,
  geometry = TRUE
)

tract_v20_de <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "DE",
  county = c("New Castle"),
  year = 2020,
  geometry = TRUE
)

#MD tracts
tract_v10_md <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "MD",
  county = c("Cecil"),
  year = 2010,
  geometry = TRUE
)

tract_v20_md <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "MD",
  county = c("Cecil"),
  year = 2020,
  geometry = TRUE
)

###combine/process----
tract_v10 <- bind_rows(tract_v10_pa, tract_v10_nj, tract_v10_de, tract_v10_md)
tract_v20 <- bind_rows(tract_v20_pa, tract_v20_nj, tract_v20_de, tract_v20_md)

tract_v10$region_id <- paste0("v10|", tract_v10$GEOID)
tract_v20$region_id <- paste0("v20|", tract_v20$GEOID)

process_tracts <- function(df) {
  df <- st_transform(df, crs = 4326)
  df$tract_id <- df$GEOID                                         
  df$state    <- str_sub(df$GEOID, 1, 2)
  df$county   <- str_sub(df$GEOID, 3, 5)
  df$tract    <- str_sub(df$GEOID, 6, 11)
  df$area     <- as.numeric(st_area(df$geometry)) * 0.000000386102 # sq meters -> sq miles
  df <- df %>% select(-NAME, -variable, -estimate, -moe, -GEOID)
  return(df)
}

tract_v10 <- process_tracts(tract_v10)
tract_v20 <- process_tracts(tract_v20)

geo_tract <- bind_rows(tract_v10, tract_v20)

st_write(geo_tract, "2_summary_data/geo_census_tract.geojson",  delete_dsn = TRUE)



##*ACS Data----
#years that you want to query ACS data for
years <- 2010:year_less_2

#df listing the state, and all counties numbers you are interested in querying
region_df <- tribble(
  ~state, ~county,
  "42",   "101",
  "42",   "091",
  "42",   "017",
  "42",   "045",
  "42",   "029",
  "34",   "007",
  "34",   "005",
  "34",   "015",
  "34",   "033",
  "10",   "003",
  "24",   "015"
)

#variable display name, the acs name, and the final category it belongs in
var_dict <- tribble(
  
  ~canonical_name,                              ~acs_var,              ~category,
  
  "total_population",                           "B01003_001E",         "demographics",
  "race..colon.._white_population",             "B02001_002E",         "demographics",
  "race..colon.._black_population",             "B02001_003E",         "demographics",
  "race..colon.._indigenous_population",        "B02001_004E",         "demographics",
  "race..colon.._asian_population",             "B02001_005E",         "demographics",
  "race..colon.._pacific_islander_population",  "B02001_006E",         "demographics",
  " race..colon.._other_population",            "B02001_007E",         "demographics",
  "race..colon.._multiracial_population",       "B02001_008E",         "demographics",
  " ethnicity..colon.._hispanic_population",    "B03003_003E",         "demographics",
  "ethnicity..colon.._non..hyphen..hispanic_population",
                                                "B03003_002E",         "demographics",
  "gender..colon.._male_population",            "B01001_002E",         "demographics",
  "gender..colon.._female_population",          "B01001_026E",         "demographics",
  "gender..colon.._female",                     "B01001_026E",         "demographics",
  "gender..colon.._male",                       "B01001_002E",         "demographics",
  "f_under_5",                                  "B01001_027E",         "demographics",
  "f_5_to_9",                                   "B01001_028E",         "demographics",
  "f_10_to_14",                                 "B01001_029E",         "demographics",
  "f_15_to_17",                                 "B01001_030E",         "demographics",
  "f_18_to_19",                                 "B01001_031E",         "demographics",
  "f_20",                                       "B01001_032E",         "demographics",
  "f_21",                                       "B01001_033E",         "demographics",
  "f_22_to_24",                                 "B01001_034E",         "demographics",
  "f_25_to_29",                                 "B01001_035E",         "demographics",
  "f_30_to_34",                                 "B01001_036E",         "demographics",
  "f_35_to_39",                                 "B01001_037E",         "demographics",
  "f_40_to_44",                                 "B01001_038E",         "demographics",
  "f_45_to_49",                                 "B01001_039E",         "demographics",
  "f_50_to_54",                                 "B01001_040E",         "demographics",
  "f_55_to_59",                                 "B01001_041E",         "demographics",
  "f_60_to_61",                                 "B01001_042E",         "demographics",
  "f_62_to_64",                                 "B01001_043E",         "demographics",
  "f_65_to_66",                                 "B01001_044E",         "demographics",
  "f_67_to_69",                                 "B01001_045E",         "demographics",
  "f_70_to_74",                                 "B01001_046E",         "demographics",
  "f_75_to_79",                                 "B01001_047E",         "demographics",
  "f_80_to_84",                                 "B01001_048E",         "demographics",
  "f_85_plus",                                  "B01001_049E",         "demographics",
  "m_under_5",                                  "B01001_003E",         "demographics",
  "m_5_to_9",                                   "B01001_004E",         "demographics",
  "m_10_to_14",                                 "B01001_005E",         "demographics",
  "m_15_to_17",                                 "B01001_006E",         "demographics",
  "m_18_to_19",                                 "B01001_007E",         "demographics",
  "m_20",                                       "B01001_008E",         "demographics",
  "m_21",                                       "B01001_009E",         "demographics",
  "m_22_to_24",                                 "B01001_010E",         "demographics",
  "m_25_to_29",                                 "B01001_011E",         "demographics",
  "m_30_to_34",                                 "B01001_012E",         "demographics",
  "m_35_to_39",                                 "B01001_013E",         "demographics",
  "m_40_to_44",                                 "B01001_014E",         "demographics",
  "m_45_to_49",                                 "B01001_015E",         "demographics",
  "m_50_to_54",                                 "B01001_016E",         "demographics",
  "m_55_to_59",                                 "B01001_017E",         "demographics",
  "m_60_to_61",                                 "B01001_018E",         "demographics",
  "m_62_to_64",                                 "B01001_019E",         "demographics",
  "m_65_to_66",                                 "B01001_020E",         "demographics",
  "m_67_to_69",                                 "B01001_021E",         "demographics",
  "m_70_to_74",                                 "B01001_022E",         "demographics",
  "m_75_to_79",                                 "B01001_023E",         "demographics",
  "m_80_to_84",                                 "B01001_024E",         "demographics",
  "m_85_plus",                                  "B01001_025E",         "demographics",
  
#______________________________________________________________________________________
  
  "total_pop_for_poverty",                      "B17001_001E",         "socioeconomics",
  "poverty..colon.._below_level",               "B17001_002E",         "socioeconomics",
  "poverty..colon.._at..fslash..above_level",   "B17001_003E",         "socioeconomics",
  "total_households_for_snap",                  "B22001_001E",         "socioeconomics",
  "snap..colon.._households_receiving",         "B22001_002E",         "socioeconomics",
  "snap..colon.._households_not_receiving",     "B22001_005E",         "socioeconomics",
  "total_pop_25..plus.._for_education",         "B15003_001E",         "socioeconomics",
  "education..colon.._high_school_diploma",     "B15003_017E",         "socioeconomics",
  "education..colon.._ged",                     "B15003_018E",         "socioeconomics",
  "education..colon.._some_college_less..hyphen..1yr",
                                                "B15003_019E",         "socioeconomics",
  "education..colon.._some_college_1yr..plus.._no_degree",
                                                "B15003_020E",         "socioeconomics",
  "education..colon.._associates_degree",       "B15003_021E",         "socioeconomics",
  "education..colon.._bachelors_degree",        "B15003_022E",         "socioeconomics",
  "education..colon.._masters_degree",          "B15003_023E",         "socioeconomics",
  "education..colon.._professional_degree",     "B15003_024E",         "socioeconomics",
  "education..colon.._doctorate_degree",        "B15003_025E",         "socioeconomics",
  
#______________________________________________________________________________________
  
  "median_household_income",                    "B19013_001E",         "labor",
  "total_civilian_pop_16..plus..",              "B23025_001E",         "labor",
  "in_labor_force..colon.._yes",                "B23025_002E",         "labor",
  "in_labor_force..colon.._no",                 "B23025_007E",         "labor",
  "employed..colon.._no",                       "B23025_005E",         "labor",
  "employed..colon.._yes",                      "B23025_004E",         "labor",
  "median_earnings",                            "B20004_001E",         "labor",
  
#______________________________________________________________________________________
  
  "total_workers_16..plus..",                   "B08301_001E",         "transportation",
  "commute..colon.._drove_alone",               "B08301_003E",         "transportation",
  "commute..colon.._carpooled",                 "B08301_004E",         "transportation",
  "commute..colon.._public_transit",            "B08301_010E",         "transportation",
  "commute..colon.._walked",                    "B08301_019E",         "transportation",
  "commute..colon.._other",                     "B08301_020E",         "transportation",
  "commute..colon.._worked_from_home",          "B08301_021E",         "transportation",
  
#______________________________________________________________________________________
  
  "median_home_value",                          "B25077_001E",         "housing",
  "median_gross_rent",                          "B25064_001E",         "housing",
  "housing_units..colon.._total",               "B25002_001E",         "housing",
  "housing_units..colon.._occupied",            "B25002_002E",         "housing",
  "housing_units..colon.._vacant",              "B25002_003E",         "housing",
  "housing_units..colon.._renter_occupied",     "B25070_001E",         "housing",
  "renters_cost_burden_30_34",                  "B25070_007E",         "housing",
  "renters_cost_burden_35_39",                  "B25070_008E",         "housing",
  "renters_cost_burden_40_49",                  "B25070_009E",         "housing",
  "renters_cost_burden_50plus",                 "B25070_010E",         "housing",
  "housing_units..colon.._owner_occupied",      "B25091_001E",         "housing",
  "owners_cost_burden_30_34",                   "B25091_008E",         "housing",
  "owners_cost_burden_35_39",                   "B25091_009E",         "housing",
  "owners_cost_burden_40_49",                   "B25091_010E",         "housing",
  "owners_cost_burden_50plus_mort",             "B25091_011E",         "housing",
  "owners_cost_burden_30_34_nomort",            "B25091_019E",         "housing",
  "owners_cost_burden_35_39_nomort",            "B25091_020E",         "housing",
  "owners_cost_burden_40_49_nomort",            "B25091_021E",         "housing",
  "owners_cost_burden_50plus_nomort",           "B25091_022E",         "housing"
)


#following steps create a years column in the var_dict df that lists the years each variable
#is included in ACS data for. This process happens because if a variable is not included in 
#a year and it is query this can cause the query to fail
check_var_exists <- function(var_name, vintage) {
  return(TRUE) 
}

var_dict_clean <- var_dict %>%
  mutate(clean_var = str_remove(acs_var, "E$"))

master_registry <- map_df(years, function(yr) {
  message("Downloading metadata for ", yr, "...")
  meta <- load_variables(year = yr, dataset = "acs5", cache = TRUE)
  meta %>% 
    select(name) %>% 
    mutate(year = yr, found=T)
})

availability_report <- crossing(var_dict_clean, year = years) %>%
  left_join(master_registry, by = c("clean_var" = "name", "year" = "year")) %>%
  mutate(is_available = !is.na(found)) %>%
  select(canonical_name, acs_var, year, is_available)

years_mapping <- availability_report %>%
  filter(is_available == TRUE) %>%
  group_by(acs_var) %>%
  summarize(years_available = list(year), .groups = "drop")

var_dict<- var_dict %>%
  left_join(years_mapping, by = "acs_var")

fetch_acs_year <- function(year, region_df, var_dict) {
  
  # Filter for variables available in that year
  vars_this_year <- var_dict %>%
    filter(map_lgl(years_available, ~ year %in% .x)) %>%
    pull(acs_var)
  
  acs_results <- pmap_dfr(region_df, function(state, county) {
    message("Fetching: State ", state, ", County ", county, ", Year ", year)
    
    getCensus(
      name     = "acs/acs5",
      vintage  = year,
      vars     = vars_this_year,
      region   = "tract:*",
      regionin = paste0("state:", state, " county:", county)
    )
  })
  
  # Assign the year and standardized GEOID
  acs_results %>%
    mutate(
      year = year,
      GEOID = paste0(state, county, tract)
    )
}

acs_all_raw <- map_dfr(years, ~ fetch_acs_year(.x, region_df, var_dict))

acs_all_raw <- acs_all_raw %>%
  mutate(region_id = ifelse(year < 2020, paste0("v10|", GEOID), paste0("v20|", GEOID)))

col_mapping <- setNames(var_dict$acs_var, var_dict$canonical_name)

acs_all <- acs_all_raw %>%
  rename(any_of(col_mapping))

categories <- unique(var_dict$category)

id_cols <- c("region_id", "year")

for (cat in categories) {
  vars_in_cat <- var_dict %>% 
    filter(category == cat) %>% 
    pull(canonical_name)

  df_name <- paste0("summary_acs_", cat)

  assign(df_name, acs_all %>% 
           select(any_of(c(id_cols, vars_in_cat))))
}

#aggregate columns for case by case situations
summary_acs_housing <- summary_acs_housing %>% 
  mutate(
    renters_cost_burdened_..oparen..30..pcent....plus.._of_income..cparen.. = 
      renters_cost_burden_30_34 + renters_cost_burden_35_39 + renters_cost_burden_40_49 + renters_cost_burden_50plus,
    
    renters_cost_burdened_..oparen..50..pcent....plus.._of_income..cparen.. = 
      renters_cost_burden_50plus,
    
    owners_cost_burdened_..oparen..30..pcent....plus.._of_income..cparen.. = 
      owners_cost_burden_30_34 + owners_cost_burden_35_39 + owners_cost_burden_40_49 + owners_cost_burden_50plus_mort + 
      owners_cost_burden_30_34_nomort + owners_cost_burden_35_39_nomort + owners_cost_burden_40_49_nomort + owners_cost_burden_50plus_nomort,
    
    owners_cost_burdened_..oparen..50..pcent....plus.._of_income..cparen.. = 
      owners_cost_burden_50plus_mort + owners_cost_burden_50plus_nomort,
  ) %>% select(
    -renters_cost_burden_30_34,-renters_cost_burden_35_39, -renters_cost_burden_40_49, -renters_cost_burden_50plus,
    -owners_cost_burden_30_34, -owners_cost_burden_35_39, -owners_cost_burden_40_49, -owners_cost_burden_50plus_mort, 
    -owners_cost_burden_30_34_nomort, -owners_cost_burden_35_39_nomort, -owners_cost_burden_40_49_nomort, -owners_cost_burden_50plus_nomort,
  )

summary_acs_demographics <- summary_acs_demographics %>% 
  mutate(
    age..colon.._..lessthan..18_population = 
      f_under_5 + f_5_to_9 + f_10_to_14 + f_15_to_17 + 
      m_under_5 + m_5_to_9 + m_10_to_14 + m_15_to_17,
    
    age..colon.._18..hyphen..39_population = 
      f_18_to_19 + f_20 + f_21 + f_22_to_24 + f_25_to_29 + f_30_to_34 + f_35_to_39 + 
      m_18_to_19 + m_20 + m_21 + m_22_to_24 + m_25_to_29 + m_30_to_34 + m_35_to_39,
    
    age..colon.._40..hyphen..59_population = 
      f_40_to_44 + f_45_to_49 + f_50_to_54 + f_55_to_59 + 
      m_40_to_44 + m_45_to_49 + m_50_to_54 + m_55_to_59,
    
    age..colon.._60..plus.._population = 
      f_60_to_61 + f_62_to_64 + f_65_to_66 + f_67_to_69 + f_70_to_74 + f_75_to_79 + f_80_to_84 + f_85_plus +
      m_60_to_61 + m_62_to_64 + m_65_to_66 + m_67_to_69 + m_70_to_74 + m_75_to_79 + m_80_to_84 + m_85_plus
  )%>% 
  select(
    -starts_with("f_"), -starts_with("m_")
    )

saveRDS(summary_acs_demographics,   "2_summary_data/summary_ACS_Demographics.rds")
saveRDS(summary_acs_socioeconomics,   "2_summary_data/summary_ACS_Socioeconomics.rds")
saveRDS(summary_acs_labor,   "2_summary_data/summary_ACS_Labor.rds")
saveRDS(summary_acs_transportation,   "2_summary_data/summary_ACS_Transportation.rds")
saveRDS(summary_acs_housing,   "2_summary_data/summary_ACS_Housing.rds")



##*Eviction Lab----
evict_phl  <- read.csv("1_raw_data/philadelphia_monthly_2020_2021.csv")
evict_wilm <- read.csv("1_raw_data/wilmington_monthly_2020_2021.csv")

evict <- bind_rows(evict_phl, evict_wilm)

evict <- evict %>%
  mutate(year = str_split_fixed(month, "/", 2)[,2]) %>%
  filter(year < year_less_0)

summary_evict_v20 <- evict %>% 
  group_by(GEOID, year) %>%
  summarise(residential_evictions_filed = sum(filings_2020, na.rm=T))%>%
  ungroup()

summary_evict_v20$year <- as.numeric(summary_evict_v20$year)

summary_evict_v20$region_id <- paste0("v20|", summary_evict_v20$GEOID)

summary_evict_v20 <- summary_evict_v20 %>% select(-GEOID)

saveRDS(summary_evict_v20, "2_summary_data/summary_Eviction_Lab.rds")

##*PPD Crime Incidents----
url <- "https://phl.carto.com/api/v2/sql"

fetch_year <- function(year) {
  
  current_offset <- 0
  rows_per_request <- 10000
  
  start_date <- paste0(year, "-01-01")
  end_date   <- paste0(year + 1, "-01-01")
  
  chunks_collected <- list()
  
  repeat {
    
    sql_query <- paste0(
      "SELECT objectid, dispatch_date, text_general_code, the_geom ",
      "FROM incidents_part1_part2 ",
      "WHERE dispatch_date >= '", start_date, "' ",
      "AND dispatch_date < '",   end_date,   "' ",
      "ORDER BY dispatch_date DESC ",
      "LIMIT ",  rows_per_request, " ",
      "OFFSET ", current_offset
    )
    
    response <- GET(url, query = list(q = sql_query, format = "geojson"))
    
    if (status_code(response) != 200) {
      print(paste0("API error for: ", year, " Status: ", status_code(response)))
      print(content(response, as = "text", encoding = "UTF-8"))
      break
    }
    
    response_text <- content(response, as = "text", encoding = "UTF-8")
    current_chunk <- st_read(response_text, quiet = TRUE)
    
    if (is.null(current_chunk) || nrow(current_chunk) == 0) break
    
    chunks_collected[[length(chunks_collected) + 1]] <- current_chunk
    current_offset <- current_offset + rows_per_request
    print(paste0("Got ", current_offset, " rows for ", year))
    
    if (nrow(current_chunk) < rows_per_request) break
    
    Sys.sleep(0.25)
  }
  
  bind_rows(chunks_collected)
}

all_years <- list()

for (year in 2010:year_less_1) {
  print(paste("Getting:", year))
  all_years[[as.character(year)]] <- fetch_year(year)
}

crime <- bind_rows(all_years)

crime <- st_transform(crime, crs = 4326)

crime$year <- year(crime$dispatch_date)

crime <- crime %>%
  filter(!st_is_empty(geometry))

crime_2010 <- crime %>%
  filter (year > 2009 & year < 2020)

crime_2020 <- crime %>%
  filter (year > 2019)

build_2010 <- st_join(crime_2010, tract_v10 %>% select(region_id), join = st_intersects)

build_2020 <- st_join(crime_2020, tract_v20 %>% select(region_id), join = st_intersects)

summary_crime_v10 <- build_2010 %>%
  st_drop_geometry() %>%
  group_by(region_id, year) %>%
  mutate(total_crimes = n()) %>%
  group_by(region_id, year, text_general_code, total_crimes) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pivot_wider(
    names_from  = text_general_code,
    values_from = count,
    values_fill = 0
  )

summary_crime_v20 <- build_2020 %>%
  st_drop_geometry() %>%
  group_by(region_id, year) %>%
  mutate(total_crimes = n()) %>%
  group_by(region_id, year, text_general_code, total_crimes) %>%
  summarise(count = n()) %>%
  ungroup %>%
  pivot_wider(
    names_from  = text_general_code,
    values_from = count,
    values_fill = 0
  )

summary_crime_v10$Part_1_Violent <- summary_crime_v10$`Homicide - Criminal` + #summary_crime_v20$`Homicide - Gross Negligence` is excluded here because it is not in the 2010 data
                          summary_crime_v10$Rape +
                          summary_crime_v10$`Robbery Firearm` +
                          summary_crime_v10$`Robbery No Firearm` +
                          summary_crime_v10$`Aggravated Assault Firearm` +
                          summary_crime_v10$`Aggravated Assault No Firearm`

summary_crime_v10$Part_1_Property <- summary_crime_v10$Arson +
                          summary_crime_v10$`Burglary Non-Residential` +
                          summary_crime_v10$`Burglary Residential` +
                          summary_crime_v10$`Motor Vehicle Theft` +
                          summary_crime_v10$`Theft from Vehicle` +
                          summary_crime_v10$Thefts
  
summary_crime_v20$Part_1_Violent <-summary_crime_v20$`Homicide - Gross Negligence` +
                         summary_crime_v20$`Homicide - Criminal` +
                         summary_crime_v20$Rape +
                         summary_crime_v20$`Robbery Firearm` +
                         summary_crime_v20$`Robbery No Firearm` +
                         summary_crime_v20$`Aggravated Assault Firearm` +
                         summary_crime_v20$`Aggravated Assault No Firearm`

summary_crime_v20$Part_1_Property <- summary_crime_v20$Arson +
                          summary_crime_v20$`Burglary Non-Residential` +
                          summary_crime_v20$`Burglary Residential` +
                          summary_crime_v20$`Motor Vehicle Theft` +
                          summary_crime_v20$`Theft from Vehicle` +
                          summary_crime_v20$Thefts

summary_crime_v10 <-make_safe_cols(summary_crime_v10)
summary_crime_v20 <-make_safe_cols(summary_crime_v20)

summary_crime <- bind_rows(summary_crime_v10, summary_crime_v20)

saveRDS(summary_crime, "2_summary_data/summary_PPD_Crime_Incidents.rds")

##*PPD Fatal Crashes----
sql_query <- paste0(
  "SELECT the_geom, year, date_, hit_____ru ", 
  "FROM fatal_crashes ",
  "ORDER BY date_ DESC"
)

response <- GET(url, query = list(q = sql_query, format = "geojson"))

if (status_code(response) == 200) {
  
  response_text <- content(response, as = "text", encoding = "UTF-8")
  fatal_crashes <- st_read(response_text, quiet = TRUE)
  
  print(paste("Success! Downloaded", nrow(fatal_crashes), "records."))
  
} else {
  
  print(paste("API Error:", status_code(response)))
  print(content(response, as = "text", encoding = "UTF-8")) 
  
}

fatal_crashes <- fatal_crashes %>%
  filter(year != "Active Investigation") %>%
  mutate(year = as.numeric(year))

fatal_crashes <- fatal_crashes %>%
  mutate(is_hit_run = ifelse(hit_____ru == "Yes", 1, 0))

crashes_v10 <- st_join(
  fatal_crashes %>% filter(year >= 2010 & year < 2020),
  tract_v10 %>% select(region_id), 
  join = st_intersects
)

crashes_v20 <- st_join(
  fatal_crashes %>% filter(year >= 2020 & year < year_less_0),
  tract_v20 %>% select(region_id), 
  join = st_intersects
)

summary_crashes <- function(data) {
  data %>%
    st_drop_geometry() %>%
    filter(!is.na(region_id)) %>%
    group_by(region_id, year) %>%
    summarise(
      total_fatal_crashes = n(),
      total_fatal_hit_and_runs = sum(is_hit_run, na.rm = TRUE),
      .groups = "drop"
    )
}

ppd_fatal_crash_reports_pre<- bind_rows(crashes_v10, scrashes_v20)

phl_geo <- geo_tract %>% filter (county == 101)

all_tracts <- unique(phl_geo$region_id)
all_years  <- unique(ppd_fatal_crash_reports_pre$year)

#master grid necessary to allow regions that had to crashes to appear as having no crashes, instead
#of just not being a part of the data
master_grid <- expand_grid(region_id = all_tracts, year = all_years)

master_grid <- master_grid %>%
  filter(
    (str_starts(region_id, fixed("v10|")) & year < 2020) |
      (str_starts(region_id, fixed("v20|")) & year > 2019)
  )

summary_ppd_fatal_crash_reports <- master_grid %>%
  left_join(ppd_fatal_crash_reports_pre, by = c("region_id", "year")) %>%
  mutate(
    total_fatal_crashes = replace_na(total_fatal_crashes, 0),
    total_fatal_hit_and_runs = replace_na(total_fatal_hit_and_runs, 0)
  )

saveRDS(summary_ppd_fatal_crash_reports, "2_summary_data/summary_PPD_Fatal_Crash_Reports.rds")

##*Properties----
# 
# prop <- st_read("1_raw_data/opa_properties_public.geojson")
# 
# prop_build <- st_join(prop, tract_v20 %>% select(region_id), join = st_within)
# 
# summary_prop_v20 <- prop_build %>%
#   st_drop_geometry() %>%
#   group_by(region_id) %>%
#   mutate(total_properties = n()) %>%
#   group_by(region_id, category_code_description, total_properties) %>%
#   summarise(count = n()) %>%
#   ungroup %>%
#   pivot_wider(
#     names_from  = category_code_description,
#     values_from = count,
#     values_fill = 0
#   )
# 
# summary_prop_v20$year <- 2026
# 
# names(summary_prop_v20) <- gsub(" ", "_", names(summary_prop_v20)) 
# names(summary_prop_v20) <- gsub("-", "..hyphen..", names(summary_prop_v20))
# names(summary_prop_v20) <- gsub(">", "..greaterthan..", names(summary_prop_v20))
# 
# summary_prop_v20 <- summary_prop_v20 %>% select(-'NA')
# 
# saveRDS(summary_prop_v20,  "2_summary_data/summary_Philadelphia_Properties.rds")
#

#______________________----
#GEO_ZIP_CODE----
##*base geo----

#clear global enviroment and reclaim memory
rm(list = ls())
gc()

year_less_0 <- as.numeric(format(Sys.Date(), "%Y"))
year_less_1 <- as.numeric(format(Sys.Date(), "%Y"))-1
year_less_2 <- as.numeric(format(Sys.Date(), "%Y"))-2

geo_zip <- zctas(year = 2024) %>%
  st_transform(crs = 4326)

geo_zip <- geo_zip %>%
  rename(region_id = GEOID20)

geo_zip <- geo_zip %>% select("region_id", "geometry")

##*Zillow Home Value Index----
zillow_fun <- function(df, bedrooms) {
  df_name <- deparse(substitute(df))
  
  df <- df %>%
    filter(
      (CountyName %in% c("Philadelphia County", "Montgomery County", 
                         "Bucks County", "Delaware County", "Chester County") 
       & StateName == "PA") |
        (CountyName %in% c("Camden County", "Burlington County", 
                           "Gloucester County", "Salem County") 
         & StateName == "NJ") |
        (CountyName == "New Castle County"
         & StateName == "DE") |
        (CountyName == "Cecil County"
         & StateName == "MD")
    )
  
  
  df$RegionName <- as.character(df$RegionName)
  
  df$RegionName <- ifelse(nchar(df$RegionName) == 4, paste0("0", df$RegionName), ifelse(
    nchar(df$RegionName) == 3, paste0("00", df$RegionName), df$RegionName
  ))
  
  df_yearly <- df %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "date_str",
      values_to = "value")
  
  df_yearly <- df_yearly  %>%
    mutate(
      date_str = str_remove(date_str, "^X"),
      date_str = str_replace_all(date_str, "\\.", "-"),
      date = ymd(date_str),
      year = year(date))
  
  df_summary <- df_yearly %>%
    group_by(RegionName, year) %>%
    summarize(
      avg_value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_summary <- df_summary %>%
    rename(!!sym(bedrooms) := avg_value)
  
  df_summary <- df_summary %>% rename(region_id = RegionName)
  
  df_summary[[bedrooms]] <- ifelse(is.nan(df_summary[[bedrooms]]), NA, df_summary[[bedrooms]])
  
  assign(paste0(df_name, "_summary"), df_summary, envir = globalenv())
}

z_bdrs_1 <- read.csv("1_raw_data/Zip_zhvi_bdrmcnt_1_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
z_bdrs_2 <- read.csv("1_raw_data/Zip_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
z_bdrs_3 <- read.csv("1_raw_data/Zip_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
z_bdrs_4 <- read.csv("1_raw_data/Zip_zhvi_bdrmcnt_4_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
z_bdrs_5 <- read.csv("1_raw_data/Zip_zhvi_bdrmcnt_5_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")

zillow_fun(z_bdrs_1, "Representative_One_Bedroom")
zillow_fun(z_bdrs_2, "Representative_Two_Bedroom")
zillow_fun(z_bdrs_3, "Representative_Three_Bedroom")
zillow_fun(z_bdrs_4, "Representative_Four_Bedroom")
zillow_fun(z_bdrs_5, "Representative_Five_Bedroom")

summary_zillow <- z_bdrs_1_summary %>%
  full_join(z_bdrs_2_summary, by = c("region_id", "year")) %>%
  full_join(z_bdrs_3_summary, by = c("region_id", "year")) %>%
  full_join(z_bdrs_4_summary, by = c("region_id", "year")) %>%
  full_join(z_bdrs_5_summary, by = c("region_id", "year")) %>%
  filter(year<year_less_0)

#Remove Unutilized Geoms
geo_zip$in_data <- ifelse(geo_zip$region_id %in% unique(summary_zillow$region_id),1,0)

geo_zip <- geo_zip %>% filter(in_data == 1) %>%
  select(-in_data)

geo_zip <- geo_zip %>%
  st_make_valid()

geo_zip$area <- as.numeric(st_area(geo_zip$geometry))*0.000000386102

saveRDS(summary_zillow,"2_summary_data/summary_Zillow_Home_Value_Index.rds")
st_write(geo_zip,   "2_summary_data/geo_zip_code.geojson",  delete_dsn = TRUE)

#______________________----
#GEO_DISTRICT----
##*base geo----

#clear global enviroment and reclaim memory
rm(list = ls())
gc()

year_less_0 <- as.numeric(format(Sys.Date(), "%Y"))
year_less_1 <- as.numeric(format(Sys.Date(), "%Y"))-1
year_less_2 <- as.numeric(format(Sys.Date(), "%Y"))-2

natality <- st_read("1_raw_data/Vital_Natality_PD.geojson")

natality <- natality %>%
  rename(region_id = GEOGRAPHY_NAME) %>%
  rename(year = YEAR)

natality <- natality %>% select(-RACE_ETHNICITY, 
                                -Shape__Area,
                                -Shape__Length,
                                -OBJECTID,
                                -GEOGRAPHY)

geo_district <- natality %>% 
  select(region_id, geometry) %>% 
  distinct() 

geo_district$area <- as.numeric(st_area(geo_district$geometry ))*0.000000386102

st_write(geo_district,   "2_summary_data/geo_district.geojson",  delete_dsn = TRUE)

##*Natality----
natality <- natality %>% filter(SEX == "All sexes")


natality$METRIC_VALUE <- ifelse(
  natality$QUALITY_FLAG %in% c("suppressed", "unreliable"),
  NA,
  natality$METRIC_VALUE
)

natality$METRIC_VALUE <- as.numeric(gsub("[^0-9.-]", "", natality$METRIC_VALUE))

summary_Natality <- natality %>% 
  st_drop_geometry() %>%
  pivot_wider(
    names_from  = METRIC_NAME,
    values_from = METRIC_VALUE
  )

summary_Natality <- summary_Natality %>% select(-AGE_CATEGORY, 
                                                -ESTIMATE_TYPE,
                                                -QUALITY_FLAG,
                                                -SEX
                                                )

saveRDS(summary_Natality,"2_summary_data/summary_Vital_Statistics_Natality.rds")