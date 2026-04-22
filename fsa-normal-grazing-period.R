library(tidyverse)
library(magrittr)
library(quarto)

# Load the census FIPS codes from several vintages
census <-
  dplyr::bind_rows(
    tigris::counties(cb = TRUE) %>%
      sf::st_drop_geometry(),
    tigris::counties(cb = TRUE, year = 2014) %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(
        tigris::states(cb = TRUE, year = 2014) %>%
          dplyr::transmute(STATEFP, STATE_NAME = NAME) %>%
          sf::st_drop_geometry()
      ) %>%
      dplyr::arrange(STATEFP, COUNTYFP)
  ) %>%
  dplyr::select(STATEFP, COUNTYFP, NAME, STATE_NAME) %>%
  dplyr::distinct() %>%
  tibble::as_tibble() %>%
  dplyr::arrange(STATEFP, COUNTYFP)

# Load the FSA dd17 Counties dataset, which includes the crosswalk between FSA codes and FIPS codes
fsa_counties_dd17 <-
  sf::read_sf(
    "/vsizip//vsicurl/https://sustainable-fsa.com/fsa-counties-dd17/FSA_Counties_dd17.gdb.zip"
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    `State FSA Code` = FSA_ST, 
    `County FSA Code` = stringr::str_trunc(FSA_STCOU, 3, side = "left", ellipsis = ""), 
    `FIPS State Code` = FIPSST, 
    `FIPS County Code` = FIPSCO
  ) %>%
  dplyr::arrange(`State FSA Code`,`County FSA Code`, `FIPS County Code`) %>%
  dplyr::distinct()

# FSA-defined Normal Grazing Periods
fsa_normal_grazing_period <-
  unzip(zipfile = "foia/2025-FSA-04691-F Bocinsky.zip",
        files = "LFP_NormalGrazingPeriodsReport20250416.xlsx",
        exdir = tempdir()) %>%
  readxl::read_excel() %>%
  dplyr::bind_rows(., 
    unzip(zipfile = "foia/2026-FSA-03465-F Bocinsky.zip",
          files = "2026-FSA-03465-F Bocinsky/LFP_NormalGrazingPeriodsReport20260422.xlsx",
          exdir = tempdir()) |>
      readxl::read_excel() |>
      dplyr::mutate(
        state_fsa_code = stringr::str_pad(state_fsa_code, width = 2, pad = "0"),
        county_fsa_code = stringr::str_pad(county_fsa_code, width = 3, pad = "0")
      ) |>
      magrittr::set_names(x = _, value = names(.))
  ) %>%
  # Some start and end dates are NA — remove
  dplyr::filter(!is.na(`Normal Grazing Period Start Date`)) %>%
  dplyr::left_join(fsa_counties_dd17,
                   relationship = "many-to-many") %>%
  dplyr::transmute(
    `FIPS State Code` = ifelse(!is.na(`FIPS State Code`), `FIPS State Code`, `State FSA Code`),
    `FIPS County Code` = ifelse(!is.na(`FIPS County Code`), `FIPS County Code`, `County FSA Code`),
    `Program Year`,
    `Pasture Type` = `Pasture Grazing Type`,
    `Grazing Period Start Date` =  lubridate::as_date(`Normal Grazing Period Start Date`),
    `Grazing Period End Date` = lubridate::as_date(`Normal Grazing Period End Date`)
  ) %>%
  dplyr::mutate(
    
    # Correct County Code for Oglala Lakota, SD
    `FIPS County Code` = 
      dplyr::case_when(
        `FIPS State Code` == "46" &
          `FIPS County Code` == "113" &
          `Program Year` > 2015 ~ "102",
        `FIPS State Code` == "46" &
          `FIPS County Code` == "102" &
          `Program Year` <= 2015 ~ "113",
        `FIPS State Code` == "02" &
          `FIPS County Code` == "270" &
          `Program Year` > 2015 ~ "158",
        `FIPS State Code` == "02" &
          `FIPS County Code` == "158" &
          `Program Year` <= 2015 ~ "270",
        .default = `FIPS County Code`
      ),
    
    # Correct name of "Full Season Improved Mixed Pasture" in certain records
    `Pasture Type` = 
      dplyr::replace_values(
        `Pasture Type`,
        "Full Season Improved Mixed Pastures" ~ 
          "Full Season Improved Mixed Pasture"),
    
    `Grazing Period Start Date` = 
      case_when(
        # Correct erroneous year in duplicated start date of two 2010 Utah records
        `Program Year` == 2010 & 
          `FIPS State Code` == "49" &
          `FIPS County Code` %in% c("031", "041") ~ 
          lubridate::as_date("2010-04-01"),
        
        # Correct erroneous Native Pasture duplicated start date in 2012 Kansas data (should be 2012-05-01)
        `Program Year` == 2012 & 
          `FIPS State Code` == "20" ~ 
          lubridate::as_date("2012-05-01"),
        
        # Correct erroneous Native Pasture duplicated start date in 2013 Kansas data (should be 2013-05-01)
        `Program Year` == 2013 &
          `FIPS State Code` == "20" ~ 
          lubridate::as_date("2013-05-01"),
        
        # Correct erroneous Native Pasture duplicated start date in 2014 Kansas data (should be 2014-05-01)
        `Program Year` == 2014 & 
          `FIPS State Code` == "20" ~ 
          lubridate::as_date("2014-05-01"),
        
        # Correct erroneous Forage Sorghum duplicated start date in 2016 Mississippi data (should be 2016-06-01)
        `Program Year` == 2016 & 
          `FIPS State Code` == "28" ~ 
          lubridate::as_date("2016-06-01"),
        
        .default = `Grazing Period Start Date`
      ),
    
    # Correct erroneous duplicated end date in 2016 (all forage types) in
    # Prairie County, MT data (should be 2016-12-01 based on surrounding counties)
    `Grazing Period End Date` = 
      case_when(
        `Program Year` == 2016 & 
          `FIPS State Code` == "30" &
          `FIPS County Code` == "079" ~ 
          lubridate::as_date("2016-12-01"),
        .default = `Grazing Period End Date`
      )
  ) %>%
  dplyr::distinct() %>%
  dplyr::group_by(`FIPS State Code`,
                  `FIPS County Code`,
                  `Pasture Type`,
                  `Program Year`) %>%
  dplyr::summarise(`Grazing Period Start Date` = min(`Grazing Period Start Date`),
                   `Grazing Period End Date` = max(`Grazing Period End Date`),
                   .groups = "drop") %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    census,
    by = c("FIPS State Code" = "STATEFP", "FIPS County Code" = "COUNTYFP"),
    relationship = "many-to-many"
  ) %>%
  dplyr::select(
    `FIPS State Code`,
    `FIPS County Code`,
    `FIPS State Name` = STATE_NAME,
    `FIPS County Name` = NAME,
    `Program Year`,
    `Pasture Type`,
    `Grazing Period Start Date`,
    `Grazing Period End Date`
  ) %>%
  dplyr::filter(!is.na(`Pasture Type`)) %T>%
  readr::write_csv("fsa-normal-grazing-period.csv")

## Render the interactive dashboard
quarto::quarto_render("fsa-normal-grazing-period.qmd")

## Render the README
quarto::quarto_render("README.Rmd", output_format = "md")
