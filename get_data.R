generate_quarter_list <- function() {
  quarter_list <- c()
  for (y in seq(2012, 2021, 1)) {
    for (q in seq(1, 4, 1)) {
      quarter_list <- append(quarter_list, paste(y, "-Q", q, sep = ""))
    }
  }
  
  return(quarter_list[seq(1, length(quarter_list) - 2)])
}

get_data <- function(quarter) {
  url <-
    paste(
      "https://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/....Q/all?startTime=",
      #"https://api.data.abs.gov.au/data/CPI/all?startPeriod=",
      quarter,
      "&endTime=",
      quarter,
      sep = ""
    )
  
  tf <- tempfile(tmpdir = tdir <- tempdir())
  download.file(url, tf)
  dataset <- readSDMX(tf, isURL = FALSE) %>%
    as.data.frame()
  return(dataset)
}

load_data <- function() {
  data <- readRDS("data/cpi_data.rds")
  return(data)
}

load_dsd <- function() {
  data <- readRDS("data/cpi_dsd.rds")
  return(data)
}


get_dsd <- function() {
  tf <- tempfile(tmpdir = tdir <- tempdir())
  download.file("https://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/CPI",
                tf)
  dsd <- readSDMX(tf, isURL = FALSE)
  return(dsd)
}

get_index <- function(dsd) {
  codes <- dsd %>%
    slot("codelists") %>%
    slot("codelists") %>%
    .[[3]] %>%
    .@Code
  
  index <- data.frame(
    indexID = sapply(codes, function(x)
      slot(x, "id")),
    parentID = sapply(codes, function(x)
      slot(x, "parentCode")),
    indexName = sapply(codes, function(x)
      slot(x, "label")$en)
  )
  
  index <- index %>%
    left_join(
      select(index, indexID, indexName) %>%
        rename(parentName = "indexName"),
      by = c("parentID" = "indexID")
    )
  return(index)
}

get_measure <- function(dsd) {
  codes <- dsd %>%
    slot("codelists") %>%
    slot("codelists") %>%
    .[[1]] %>%
    .@Code
  
  measure <- data.frame(
    measureID = sapply(codes, function(x)
      slot(x, "id")),
    measureName = sapply(codes, function(x)
      slot(x, "label")$en)
  )
  
  return(measure)
}

get_region <- function(dsd) {
  codes <- dsd %>%
    slot("codelists") %>%
    slot("codelists") %>%
    .[[2]] %>%
    .@Code
  
  region <- data.frame(
    regionID = sapply(codes, function(x)
      slot(x, "id")),
    regionName = sapply(codes, function(x)
      slot(x, "label")$en)
  )
  
  region$regionName[region$regionName == "Weighted average of eight capital cities"] <-
    "8 Caps"
  return(region)
}

get_tsest <- function(dsd) {
  codes <- dsd %>%
    slot("codelists") %>%
    slot("codelists") %>%
    .[[4]] %>%
    .@Code
  
  tsest <- data.frame(
    tsestID = sapply(codes, function(x)
      slot(x, "id")),
    tsestName = sapply(codes, function(x)
      slot(x, "label")$en)
  )
  
  return(tsest)
}

get_frequency <- function(dsd) {
  codes <- dsd %>%
    slot("codelists") %>%
    slot("codelists") %>%
    .[[5]] %>%
    .@Code
  
  frequency <- data.frame(
    frequencyID = sapply(codes, function(x)
      slot(x, "id")),
    frequencyName = sapply(codes, function(x)
      slot(x, "label")$en)
  )
  
  return(frequency)
}

recode_quarter <- function(quarterString) {
  year <- substr(quarterString, 1, 4)
  month <-
    str_pad(as.numeric(substr(quarterString, 7, 7)) * 3, 2, "left", "0")
  endDate <-
    as.Date(paste(year, month, "01", sep = "-")) + months(1) - 1
  return(endDate)
}

fix_rounding_issue <- function(data) {
  contribution2 <- data %>%
    left_join({
      data %>% group_by(parentID, obsTime, REGION) %>% summarise(obsValueTemp = sum(measureContribution),
                                                                 .groups = 'drop')
    },
    by = c(
      "INDEX" = "parentID",
      "obsTime" = "obsTime",
      "REGION" = "REGION"
    )) %>%
    mutate(obsValueFix = coalesce(obsValueTemp, measureContribution)) %>%
    select(-obsValueTemp)
  
  contribution3 <- contribution2 %>%
    left_join({
      contribution2 %>% group_by(parentID, obsTime, REGION) %>% summarise(obsValueTemp = sum(obsValueFix),
                                                                          .groups = 'drop')
    },
    by = c(
      "INDEX" = "parentID",
      "obsTime" = "obsTime",
      "REGION" = "REGION"
    )) %>%
    mutate(obsValueFix = coalesce(obsValueTemp, measureContribution)) %>%
    select(-obsValueTemp)
  
  contribution4 <- contribution3 %>%
    left_join({
      contribution3 %>% group_by(parentID, obsTime, REGION) %>% summarise(obsValueTemp = sum(obsValueFix),
                                                                          .groups = 'drop')
    },
    by = c(
      "INDEX" = "parentID",
      "obsTime" = "obsTime",
      "REGION" = "REGION"
    )) %>%
    mutate(obsValueFix = coalesce(obsValueTemp, measureContribution)) %>%
    select(-obsValueTemp)
  
  return(contribution4)
}

generate_map <- function() {
  lga_cities <-
    c(
      "Sydney (C)",
      "Melbourne (C)",
      "Brisbane (C)",
      "Perth (C)",
      "Adelaide (C)",
      "Hobart (C)",
      "Darwin (C)",
      "Unincorporated ACT"
    )
  
  cities_sf <- ozmap_data("abs_lga")
  st_agr(cities_sf) <- "constant"
  cities_sf <- cities_sf %>%
    filter(NAME %in% lga_cities) %>%
    st_transform(4326) %>% #ozmaps uses GDA94 projection
    st_centroid() %>%
    mutate(NAME = str_replace(NAME, fixed(" (C)"), ""))
  
  cities_sf[cities_sf$NAME == "Unincorporated ACT",]$NAME <-
    "Canberra"
  
  map <- leaflet(cities_sf) %>%
    addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
    addCircles(
      color = "green",
      radius = 1E5,
      highlightOptions = list(color = "white"),
      label = ~ NAME,
      labelOptions = labelOptions(
        noHide = T,
        textOnly = TRUE,
        direction = "topleft",
        style = list("color" = "white")
      )
    )
}

adjust_for_inflation <- function(data, quarter) {
  data <- data %>%
    arrange(INDEX, quarterEnd, REGION) %>%
    group_by(INDEX, REGION) %>%
    mutate(measureIndexBase = case_when(obsTime == quarter ~ measureIndex,
                                        TRUE ~ NA_real_)) %>%
    fill(measureIndexBase, .direction = "updown") %>%
    mutate(
      measureIndexAdjusted = case_when(
        obsTime == quarter ~ 100,
        TRUE ~ measureIndex / measureIndexBase * 100
      )
    ) %>%
    select(-measureIndexBase)
}

# saveRDS(data, file = "data/cpi_data.rds")
# saveRDS(get_dsd(), file = "data/cpi_dsd.rds")
