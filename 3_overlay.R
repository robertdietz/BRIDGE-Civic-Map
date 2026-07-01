library(tidyverse)
library(sf)
library(tools)
library(readxl)
library(censusapi)
library(tidycensus)

options(tigris_use_cache = TRUE)

#OUTLINE REGION BORDERS-----------------
###RCOs-------------
rco_outline       <- st_read("3_overlay_data/rco_summary.geojson")
rco_outline       <- rco_outline %>% rename(refine = region_name)
rco_outline$category <- "City of Phl. RCOs"

###Council Districts--------------
council_outline   <- st_read("3_overlay_data/Council_Districts_2024.geojson")
council_outline   <- council_outline %>% rename(refine = DISTRICT)
council_outline$refine <- paste0("District ", council_outline$refine)
council_outline$category <- "City of Phl. Council Districts"

###Neighborhoods-------------
neighb_outline    <- st_read("3_overlay_data/philadelphia-neighborhoods.geojson")
neighb_outline    <- neighb_outline %>% rename(refine = LISTNAME)
neighb_outline$category    <- "City of Phl. Neighborhoods"

###Counties-------------------
counties_pa <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  county = c("Philadelphia", "Montgomery", "Bucks", "Delaware", "Chester"),
  year = 2020,
  geometry = TRUE,
  )%>%
  select(NAME, geometry)

counties_nj <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "NJ",
  county = c("Camden", "Burlington", "Gloucester", "Salem"),
  year = 2020,
  geometry = TRUE
)%>%
  select(NAME, geometry)

counties_de <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "DE",
  county = c("New Castle"),
  year = 2020,
  geometry = TRUE
)%>%
  select(NAME, geometry)

counties_md <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "MD",
  county = c("Cecil"),
  year = 2020,
  geometry = TRUE
)%>%
  select(NAME, geometry)

county_outline <- bind_rows(counties_de, counties_md, counties_nj, counties_pa)
county_outline$NAME <- ifelse(grepl(", Delaware", county_outline$NAME), 
                              paste0("DE: ", 
                              gsub(", Delaware", "", county_outline$NAME)), 
                              county_outline$NAME)

county_outline$NAME <- ifelse(grepl(", Pennsylvania", county_outline$NAME), 
                              paste0("PA: ", 
                              gsub(", Pennsylvania", "", county_outline$NAME)), 
                              county_outline$NAME)

county_outline$NAME <- ifelse(grepl(", Maryland", county_outline$NAME), 
                              paste0("MD: ", 
                              gsub(", Maryland", "", county_outline$NAME)), 
                              county_outline$NAME)

county_outline$NAME <- ifelse(grepl(", New Jersey", county_outline$NAME), 
                              paste0("NJ: ", 
                              gsub(", New Jersey", "", county_outline$NAME)), 
                              county_outline$NAME)

county_outline$category <- "Metro Area Counties"

county_outline <- county_outline %>% rename(refine = NAME)

county_outline <- st_transform(county_outline, crs = 4326)

###Combine-------------
region_outline    <- bind_rows(rco_outline, council_outline, neighb_outline, county_outline)

region_outline    <- region_outline %>% select(category, refine, geometry)

saveRDS(region_outline,  "4_app/overlay_region.rds")

#OUTLINE CIVIC INFRASTRUCTURE--------------
###Rail-------------
trolley_outline   <- st_read("3_overlay_data/Trolley_Lines.geojson")
trolley_outline$category <- "Rail: Trolley"

hs_outline        <- st_read("3_overlay_data/Highspeed_Lines.geojson")
hs_outline$category <- "Rail: High Speed"
hs_outline <- hs_outline %>% rename(Route_Name = Route)

regional_outline  <- st_read("3_overlay_data/Regional_Rail_Lines.geojson")
regional_outline$category <- "Rail: Regional"

transit_outline <- bind_rows(trolley_outline, hs_outline, regional_outline)

transit_outline <- transit_outline %>% 
  select(Route_Name, category, geometry)

transit_outline <- transit_outline %>%
  mutate(
    color = case_when(
      category == "Rail: Trolley"          ~ "#00990d",
      category == "Rail: High Speed"  ~ "#eb8500",
      category == "Rail: Regional"    ~ "#990091",
      TRUE                           ~ "gray"
    )
  )

transit_outline <- transit_outline %>% rename(name = Route_Name)

###Library-------------
point_library <- st_read("3_overlay_data/library_locations.geojson")

point_library <- st_transform(point_library, crs = 4326)

point_library <- point_library %>% select(building, geometry)

point_library$category <- "Library Branches"
point_library$size <- NA
point_library$color <- "#00990d"

point_library <- point_library %>% rename(name = building)

###Schools-------------
point_school <- st_read("3_overlay_data/Schools.geojson")

unique(point_school$type_specific)

point_school$enrollment[point_school$school_name == "THE LINC"] <- NA

point_school$size <- ifelse(point_school$enrollment <= 275, 1,
                     ifelse(point_school$enrollment <= 550, 2,
                     ifelse(point_school$enrollment <= 825, 3, 4)))

point_school$na_size <- ifelse(is.na(point_school$enrollment),1,0)

point_school$elem <- ifelse(grepl("ELEM", point_school$grade_level),1,0)
point_school$mid <- ifelse(grepl("MID", point_school$grade_level),1,0)
point_school$high <- ifelse(grepl("HIGH", point_school$grade_level),1,0)

point_school$size <- ifelse(is.na(point_school$enrollment),1,point_school$size)

point_school$school_name_label <- toTitleCase(tolower(point_school$school_name_label))
point_school <- point_school %>% rename(name = school_name_label)
# point_school$size <- paste0(point_school$grade_org, ", ", point_school$enrollment, ", ", point_school$type_specific)
point_school$size <- paste0("<b>Grade Level: </b>", point_school$grade_org, "<br><b>Type: </b>", str_to_title(point_school$type_specific))

point_school_type <- point_school
point_school_type$category <- paste0("Schools by Type: ", toTitleCase(tolower(point_school$type_specific)))

point_school_type$color <- ifelse(point_school_type$category == "Schools by Type: Archdiocese", "#00990d",
                             ifelse(point_school_type$category == "Schools by Type: Private", "#c8eb00",
                                    ifelse(point_school_type$category == "Schools by Type: District", "#eb8500",
                                           ifelse(point_school_type$category == "Schools by Type: Charter", "#eb1b00",
                                                  ifelse(point_school_type$category == "Schools by Type: Contracted" , "#990091",NA)))))

point_school_type <- point_school_type %>% select(name, geometry, category, size, color)

point_school_grade <- point_school
point_school_grade$color <- NA

point_school_grade <- point_school_grade %>%
  mutate(category = ifelse(elem == 1 & mid == 0 & high ==0, "Schools by Grade: Elementary",
                    ifelse(elem == 1 & mid == 1 & high ==0, "Schools by Grade: Elementary & Middle",
                    ifelse(elem == 1 & mid == 1 & high ==1, "Schools by Grade: Elementary, Middle & High",
                    ifelse(elem == 0 & mid == 1 & high ==0, "Schools by Grade: Middle",
                    ifelse(elem == 0 & mid == 1 & high ==1, "Schools by Grade: Middle & High",
                    ifelse(elem == 0 & mid == 0 & high ==1, "Schools by Grade: High", NA)))))))%>%
  mutate(
  color = case_when(
    category == "Schools by Grade: Elementary"                 ~ "#00990d",
    category == "Schools by Grade: Elementary & Middle"        ~ "#c8eb00",
    category == "Schools by Grade: Middle"                     ~ "#eb8500",
    category == "Schools by Grade: Middle & High"              ~ "#eb1b00",
    category == "Schools by Grade: High"                       ~ "#990091",
    category == "Schools by Grade: Elementary, Middle & High"  ~ "#8a8a8a",
    TRUE                                       ~ NA  
  )
)%>%  select(name, geometry, category, size, color)


###Combine-------------

civic_infras <- bind_rows(transit_outline, point_library, point_school_type, point_school_grade)

civic_infras <- civic_infras %>%
mutate(category = factor(category, levels = c(
  "Schools by Grade: Elementary",
  "Schools by Grade: Elementary & Middle",
  "Schools by Grade: Middle",
  "Schools by Grade: Middle & High",
  "Schools by Grade: High",
  "Schools by Grade: Elementary, Middle & High",
  "Rail: Trolley",                              
  "Rail: High Speed",                      
  "Rail: Regional",                             
  "Library Branches",
  "Schools by Type: Archdiocese",              
  "Schools by Type: Private",                   
  "Schools by Type: District",                
  "Schools by Type: Charter",                  
  "Schools by Type: Contracted"
))) %>%
  mutate(color = fct_reorder(factor(color), as.numeric(category)))

saveRDS(civic_infras,  "4_app/overlay_civic_infras.rds",)




class(civic_infras$category)


#graphic
# 
# w_philly <- rco_outline %>% 
#   filter(grepl("West Phil", refine))
# unique(w_philly$refine)
# 
# w_philly$color <- "red"
# 
# w_philly$color <- ifelse(w_philly$refine == "West Philly Plan + Preserve", "Purple",
#                   ifelse(w_philly$refine == "West Philadelphia Economic Development Council", "Orange",
#                   ifelse(w_philly$refine == "West Philly Together", "Green",
#                   ifelse(w_philly$refine == "West Philly United Neighbors", "Red",
#                   ifelse(w_philly$refine == "West Philadelphia Community Development Corporation", "Blue", "NA"))))
#                          )
# 
# 
# w_philly <- w_philly %>%
#   arrange(match(refine, c(
#     "West Philadelphia Community Development Corporation",  
#     "West Philly Together",
#     "West Philadelphia Economic Development Council",
#     "West Philly United Neighbors",
#     "West Philly Plan + Preserve"                          
#   )))
# 
# 
# 
# my_map <- leaflet(w_philly) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(
#     stroke = TRUE,
#     smoothFactor = 0.2,
#     fillColor = ~color,
#     color = ~color,
#     fillOpacity = .3,
#     weight = 1
#   ) %>%
#   addPolygons(
#     stroke = TRUE,
#     smoothFactor = 0.2,
#     fillColor = NA,
#     color = ~color,
#     opacity = 1,
#     fillOpacity = 0,
#     weight = 5
#   )
# 
# 
# library(htmlwidgets)
# library(webshot2)
# 
# # Save map as HTML first
# saveWidget(my_map, "map.html", selfcontained = TRUE)
# 
# # Then export to image
# webshot2::webshot(
#   "map.html", 
#   "map.png",
#   vwidth  = 3000,   # viewport width in pixels
#   vheight = 2000,   # viewport height in pixels
#   zoom    = 1      # pixel density multiplier — zoom = 3 gives you 5760 x 3240
# )
