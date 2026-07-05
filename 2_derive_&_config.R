library(tidyverse)
library(sf)
library(tools)
library(readxl)
library(rmapshaper)

#______________________--------------------
#IMPORT & JOIN DATA-----------------
##*import--------------
summary_config <- read_excel("2_summary_config.xlsx")

geo_names <- unique(summary_config$geometry)

for(i in geo_names){
  file <- paste0("2_summary_data/", i, ".geojson")
  assign(i, st_read(file))
}

summary_names <- unique(summary_config$summary_name)

for(i in summary_names){
  file <- paste0("2_summary_data/", i, ".rds")
  assign(i, readRDS(file))
}

geom_groups <- summary_config %>%
  group_by(geometry) %>%
  summarise(summaries = list(summary_name), .groups = "drop")

##*join summary data by shared geom-----------
for (i in seq_len(nrow(geom_groups))) {
  geometry          <- geom_groups$geometry[[i]]
  app_summary_names <- geom_groups$summaries[[i]]
  app_name          <- gsub("geo_", "app_", geometry)
  
  combined_summary <- app_summary_names %>%
    map(get) %>%
    reduce(full_join, by = c("region_id", "year"), relationship = "one-to-one")
  
  geo_df <- get(geometry) %>% st_drop_geometry() %>% select(region_id, area) 
  
  result <- combined_summary %>%
    left_join(geo_df, by = "region_id")
  
  assign(app_name, result, envir = .GlobalEnv)
  
  geom_no_area <- get(geometry) %>% select(-area) 
  assign(geometry, geom_no_area, envir = .GlobalEnv)
}

#______________________--------------------
#VAR CONFIG / DERIVE---------------
var_config <- data.frame()

#add data to var_config df based on summary_config.xlsx
for (i in summary_config$summary_name) {
  df              <- get(i)
  formatter       <- summary_config$formatter[summary_config$summary_name == i]
  regions_covered <- summary_config$regions_covered[summary_config$summary_name == i]
  category        <- gsub("summary_", "", i)
  category        <- gsub("_", " ", category)
  metric_type     <- summary_config$metric_type[summary_config$summary_name == i]
  geo_df          <- summary_config$geometry[summary_config$summary_name == i]
  url             <- summary_config$url[summary_config$summary_name == i]
  description     <- summary_config$description[summary_config$summary_name == i]
  last_updated    <- summary_config$last_updated[summary_config$summary_name == i]
  rows <- tibble(
    var             = names(df),
    base_var        = names(df),
    category        = category,
    regions_covered = regions_covered,
    metric_type     = metric_type,
    geo_df          = geo_df,
    formatter       = formatter,
    url             = url,
    last_updated    = last_updated,
    description     = description)
  var_config <- rbind(var_config, rows)
}

var_config <- var_config %>%
  filter(!var %in% c("region_id", "year", "geometry"))

var_config <- var_config %>%
  mutate(
    range_min = NA,
    range_max = NA,
    label     = NA,
    palette   = NA
  )

##*parse derived/included cols----------
for (k in 1:5) {
  derived_col  <- paste0("derived_col_",  k)
  included_col <- paste0("included_cols_", k)
  summary_config[[derived_col]]  <- strsplit(as.character(summary_config[[derived_col]]),  ";")
  summary_config[[included_col]] <- strsplit(as.character(summary_config[[included_col]]), ";")
}

derive_metric <- function(j, i, var_config, included, summary_category, 
                          prefix, metric_type, formatter, 
                          divisor_col, multiplier, suppression_col, suppression_val) {
  
  if (!j %in% included) return(var_config)
  
  if (prefix == "pcent_pop_") print(paste0("deriving: ", j))
  
  new_var <- paste0(prefix, j)
  var_config$base_var[var_config$var == j] <- j
  
  app_df_name <- gsub("geo_", "app_", summary_config$geometry[summary_config$summary_name == i])
  geo_df_name <- summary_config$geometry[summary_config$summary_name == i]
  
  #update var_config for newly added var
  var_config <- var_config %>%
    add_row(
      var             = new_var,
      geo_df          = geo_df_name,
      base_var        = j,
      metric_type     = metric_type,
      category        = summary_category,
      regions_covered = var_config$regions_covered[var_config$var == j],
      url             = var_config$url[var_config$var == j],
      description     = var_config$description[var_config$var == j],
      last_updated    = var_config$last_updated[var_config$var == j],
      formatter       = formatter
    )
  
  df_to_manipulate <- get(app_df_name)
  
  #safety checks
  if (!j %in% names(df_to_manipulate) || 
      length(df_to_manipulate[[j]]) == 0 || 
      !is.numeric(df_to_manipulate[[j]])) {
    return(var_config)
  }
  
  #calculate the new metric
  print(paste0("calculating: ", new_var))
  print(paste0("divisor_col: ", divisor_col, " | nrow: ", nrow(df_to_manipulate)))
  print(paste0("divisor exists: ", divisor_col %in% names(df_to_manipulate)))
  print(paste0("j length: ", length(df_to_manipulate[[j]])))
  df_to_manipulate[[new_var]] <- (df_to_manipulate[[j]] / df_to_manipulate[[divisor_col]]) * multiplier
  
  df_to_manipulate[[new_var]][!is.finite(df_to_manipulate[[new_var]])] <- NA
  df_to_manipulate[[new_var]][df_to_manipulate[[suppression_col]] < suppression_val] <- NA
  
  assign(app_df_name, df_to_manipulate, envir = .GlobalEnv)
  
  return(var_config)
}

##*apply derive functions------------
for (i in summary_names) {
  summary_category       <- gsub("_", " ", gsub("summary_", "", i))
  filtered_df            <- var_config %>% filter(category == summary_category)
  current_summary_config <- summary_config %>% filter(summary_name == i)
  filtered_vars          <- filtered_df$var
  
  print(i)
  
  for (k in 1:5) {
    derived_col_k  <- paste0("derived_col_",  k)
    included_col_k <- paste0("included_cols_", k)
    derived  <- current_summary_config[[derived_col_k]][[1]]
    included <- current_summary_config[[included_col_k]][[1]]
    
    for (j in filtered_vars) {
  #custom derived functions are below. If you want to add a custom derived function, just copy and paste
  #an earlier row and append it to the end of the list
      if ("pcent_pop_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_pop_", metric_type = "% of Pop", formatter = "percent",
                                    divisor_col = "total_population", multiplier = 100, 
                                    suppression_col = "total_population", suppression_val = 100)
      }
      if ("pcent_hhs_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_hhs_", metric_type = "% of Hhs", formatter = "percent",
                                    divisor_col = "total_households", multiplier = 100, 
                                    suppression_col = "total_households", suppression_val = 100)
      }
      if ("per_1k_ppl_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "per_1k_ppl_", metric_type = "Per 1k Ppl", formatter = "count",
                                    divisor_col = "total_population", multiplier = 1000, 
                                    suppression_col = "total_population", suppression_val = 10)
      }
      if ("per_1k_renter_hhs_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "per_1k_renter_hhs_", metric_type = "Per 1k Renter Hhs", formatter = "count",
                                    divisor_col = "housing_units..colon.._renter_occupied", multiplier = 1000, 
                                    suppression_col = "housing_units..colon.._renter_occupied", suppression_val = 10)
      }
      if ("per_sq_mile_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "per_sq_mile_", metric_type = "Per Sq Mile", formatter = "count",
                                    divisor_col = "area", multiplier = 1, 
                                    suppression_col = "total_population", suppression_val = 10)
      }
      if ("pcent_of_pop_age_25_plus_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_of_pop_age_25_plus_", metric_type = "% of Pop Age 25+", formatter = "percent",
                                    divisor_col = "total_pop_25..plus.._for_education", multiplier = 100, 
                                    suppression_col = "total_population", suppression_val = 100)
      }
      if ("pcent_of_renters_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_of_renters_", metric_type = "% of Renters", formatter = "percent",
                                    divisor_col = "housing_units..colon.._renter_occupied", multiplier = 100, 
                                    suppression_col = "housing_units..colon.._renter_occupied", suppression_val = 100)
      }
    if ("pcent_of_owners_" %in% derived) {
      var_config <- derive_metric(j, i, var_config, included, summary_category,
                                  prefix = "pcent_of_owners_", metric_type = "% of Owners", formatter = "percent",
                                  divisor_col = "housing_units..colon.._owner_occupied", multiplier = 100, 
                                  suppression_col = "housing_units..colon.._owner_occupied", suppression_val = 100)
    }
      if ("pcent_workers_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_workers_16..plus..", metric_type = "% of Workers Age 16+", formatter = "percent",
                                    divisor_col = "total_workers_16..plus..", multiplier = 100, 
                                    suppression_col = "total_workers_16..plus..", suppression_val = 100)
      }
      if ("pcent_total_units_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_total_units_", metric_type = "% of Total Units", formatter = "percent",
                                    divisor_col = "housing_units..colon.._total", multiplier = 100, 
                                    suppression_col = "housing_units..colon.._total", suppression_val = 100)
      }
      if ("pcent_occupied_units_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_occupied_units_", metric_type = "% of Occupied Units", formatter = "percent",
                                    divisor_col = "housing_units..colon.._occupied", multiplier = 100, 
                                    suppression_col = "housing_units..colon.._occupied", suppression_val = 100)
      }
      if ("pcent_of_labor_force_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_of_labor_force_", metric_type = "% of Labor Force", formatter = "percent",
                                    divisor_col = "in_labor_force..colon.._yes", multiplier = 100, 
                                    suppression_col = "in_labor_force..colon.._yes", suppression_val = 100)
      }
      if ("pcent_of_pop_age_16_plus_" %in% derived) {
        var_config <- derive_metric(j, i, var_config, included, summary_category,
                                    prefix = "pcent_occupied_units_", metric_type = "% of Pop Age 16+", formatter = "percent",
                                    divisor_col = "total_civilian_pop_16..plus..", multiplier = 100, 
                                    suppression_col = "total_civilian_pop_16..plus..", suppression_val = 100)
      }
    }
  }
}

##*pal min/max vals----------
ceiling_max <- function(x) {
  x <- as.numeric(x)
  ifelse(x < 5, 5,
      ifelse(x < 10, x,
          ifelse(x < 100,    ceiling(x / 10)    * 10,
               ifelse(x < 200,    ceiling(x / 20)    * 20,
                      ifelse(x < 500,    ceiling(x / 50)    * 50,
                             ifelse(x < 1000,   ceiling(x / 100)   * 100,
                                    ifelse(x < 2000,   ceiling(x / 200)   * 200,
                                           ifelse(x < 5000,   ceiling(x / 500)   * 500,
                                                  ifelse(x < 50000,  ceiling(x / 1000)  * 1000,
                                                         ceiling(x / 25000) * 25000)))))))))
  }

for (i in seq_along(var_config$var)) {
  var <- var_config$var[i]
  var_config$range_min[i] <- 0
  app_df_name <- gsub("geo_", "app_", var_config$geo_df[i])
  app_df <- get(app_df_name)
  if (startsWith(var, "pcent_")) {
    var_config$range_max[i] <- 100
  } else {
    max_val <- max(app_df[[var]], na.rm = TRUE)
    var_config$range_max[i] <- ceiling_max(max_val)
  }
}

##*list active years------------
#this is the list of years the user will be able to select, and is based on all years 
#that have any data other than NA
var_config$years <- as.list(rep(NA, nrow(var_config)))

geoms <- as.character(geom_groups$geometry)
for (i in geoms) {
  df_string <- gsub("geo_", "app_", i)
  df <- get(df_string)
  vars <- intersect(names(df), var_config$var)
  for (j in vars) {
    if (!(j %in% c("region_id", "year", "geometry", "area")) & j %in% names(df)) {
      filtered <- df %>%
        select(all_of(j), "year") %>%
        filter(!is.na(.data[[j]]))
      active_years <- sort(unique(filtered$year))
      x <- which(var_config$var == j)
      if (length(x) == 0) next
      var_config$years[[x]] <- active_years
    }
  }
}

##*set labels----------------------
var_config$label <- gsub("_", " ", var_config$base_var)
var_config$label <- toTitleCase(tolower(var_config$label))
var_config$label <- gsub("..hyphen..",       "-", var_config$label)
var_config$label <- gsub("..pcent..",        "%", var_config$label)
var_config$label <- gsub("..colon..",        ":", var_config$label)
var_config$label <- gsub("..oparen..",       "(", var_config$label)
var_config$label <- gsub("..cparen..",       ")", var_config$label)
var_config$label <- gsub("..greaterthan..",  ">", var_config$label)
var_config$label <- gsub("..lessthan..",     "<", var_config$label)
var_config$label <- gsub("..fslash..",       "/", var_config$label)
var_config$label <- gsub("..plus..",         "+", var_config$label)

var_config$label[var_config$var == "Representative_One_Bedroom"]   <- "Representative 1 Bedroom"
var_config$label[var_config$var == "Representative_Two_Bedroom"]   <- "Representative 2 Bedroom"
var_config$label[var_config$var == "Representative_Three_Bedroom"] <- "Representative 3 Bedroom"
var_config$label[var_config$var == "Representative_Four_Bedroom"]  <- "Representative 4 Bedroom"
var_config$label[var_config$var == "Representative_Five_Bedroom"]  <- "Representative 5 Bedroom"

var_config$palette <- "Blues"

#______________________--------------------
#MANUAL OVERRIDES------------
rows <- which(grepl("Late or No Prenatal Care", var_config$label) & var_config$category == "Vital Statistics Natality")
var_config$label[rows]    <- "Late or No Prenatal Care"
var_config$base_var[rows] <- "count_late_or_no_prenatal_care"

rows <- which(grepl("Preterm Births", var_config$label) & var_config$category == "Vital Statistics Natality")
var_config$label[rows]    <- "Preterm Births"
var_config$base_var[rows] <- "count_preterm_births"

rows <- which(grepl("Low Birthweight Births", var_config$label) & var_config$category == "Vital Statistics Natality")
var_config$label[rows]    <- "Low Birthweight Births"
var_config$base_var[rows] <- "count_low_birthweight_births"

rows <- which(grepl("percent", var_config$var) & var_config$category == "Vital Statistics Natality")
var_config$formatter[rows]   <- "percent"
var_config$metric_type[rows] <- "% of Births"

rows <- which((var_config$label == "Count Births" | var_config$label == "Crude Birth Rate per 1000") & var_config$category == "Vital Statistics Natality")
var_config$label[rows]    <- "Births"
var_config$base_var[rows] <- "count_births"

rows <- which(var_config$var == "crude_birth_rate_per_1000" & var_config$category == "Vital Statistics Natality")
var_config$metric_type[rows] <- "Per 1k Ppl"

rows <- which((var_config$var == "median_home_value" | var_config$var == "median_gross_rent") & var_config$category == "ACS Housing")
var_config$formatter[rows]    <- "dollar"
var_config$metric_type[rows]  <- "Dollar Value"

rows <- which((var_config$var == "median_earnings" | 
               var_config$var == "median_earnings..colon.._male" |
               var_config$var == "median_earnings..colon.._female" |
               var_config$var == "median_household_income") &
               var_config$category == "ACS Labor")
var_config$formatter[rows]    <- "dollar"
var_config$metric_type[rows]  <- "Dollar Value"

#______________________--------------------
#SIMPLIFY GEOMS----------------
geo_census_tract <- ms_simplify(geo_census_tract, keep = 0.3, keep_shapes = TRUE)
geo_zip_code     <- ms_simplify(geo_zip_code,     keep = 0.5, keep_shapes = TRUE)

#______________________--------------------
#EXPORT-----------------
for (i in geo_names) {
  app_name <- gsub("geo_", "app_", i)
  saveRDS(get(app_name), paste0("4_app/", app_name, ".rds"))
  saveRDS(get(i),        paste0("4_app/", i, ".rds"))
}

saveRDS(var_config, "4_app/var_config.rds")
