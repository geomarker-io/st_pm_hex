library(magrittr)
library(tibble)
library(dplyr)
library(tidyr)

dl_unzip_and_read <- function(nei_ftp_url) {
  tmp_file <- tempfile()
  download.file(nei_ftp_url, tmp_file)
  unzip(tmp_file, exdir = basename(nei_ftp_url))
  on.exit(unlink(basename(nei_ftp_url), recursive = TRUE, force = TRUE))
  files <- list.files(basename(nei_ftp_url), pattern = "*.csv", full.names = TRUE)
  out <- readr::read_csv(files[1], comment = "#")
  return(out)
}

#### 2014 ####

d_nonpoint_2014 <-
 dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2014/data_summaries/2014v2/2014neiv2_nonpoint.zip")

d_nonpoint_2014 <- d_nonpoint_2014 %>%
  filter(pollutant_cd == "PM25-PRI") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_nonroad_2014 <-
  dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2014/data_summaries/2014v2/2014neiv2_nonroad_byregions.zip")

d_nonroad_2014 <- d_nonroad_2014 %>%
  filter(pollutant_cd == "PM25-PRI") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_onroad_2014 <-
  dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2014/data_summaries/2014v2/2014neiv2_onroad_byregions.zip")

d_onroad_2014 <- d_onroad_2014 %>%
  filter(pollutant_cd == "PM25-PRI") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_event_2014 <-
  dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2014/data_summaries/2014v2/2014neiv2_eventfire_countyscc.zip")

d_event_2014 <- d_event_2014 %>%
  filter(`pollutant code` == "PM25-PRI") %>%
  group_by(fips) %>%
  summarize(total_pm25 = sum(`total emissions`)) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_2014 <-
  bind_rows(
    d_nonroad_2014 %>% mutate(nei_year = "2014", eis = "nonroad"),
    d_onroad_2014 %>% mutate(nei_year = "2014", eis = "onroad"),
    d_nonpoint_2014 %>% mutate(nei_year = "2014", eis = "nonpoint"),
    d_event_2014 %>% mutate(nei_year = "2014", eis = "event")
  )

saveRDS(d_2014, "nei_2014_county.rds")

d_point_2014 <-
  dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2014/data_summaries/2014v2/2014neiv2_facility.zip")

d_point_2014 <- d_point_2014 %>%
  filter(pollutant_cd == "PM25-PRI") %>%
  transmute(
    lat = latitude_msr,
    lon = longitude_msr,
    total_emissions = total_emissions
  ) %>%
  na.omit() %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    mutate(
      nei_year = 2014,
      eis = "point"
    )

d_point_2014$h3 <- h3::geo_to_h3(d_point_2014, res = 8)

saveRDS(d_point_2014, "nei_2014_point.rds")

#### 2011 ####

d_nonpoint_2011 <-
 dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2011/data_summaries/2011v2/2011neiv2_nonpoint.zip")

d_nonpoint_2011 <- d_nonpoint_2011 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_nonroad_2011 <-
  dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2011/data_summaries/2011v2/2011neiv2_nonroad_byregions.zip")

d_nonroad_2011 <- d_nonroad_2011 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_onroad_2011 <-
  dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2011/data_summaries/2011v2/2011neiv2_onroad_byregions.zip")

d_onroad_2011 <- d_onroad_2011 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_event_2011 <-
  dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2011/data_summaries/2011v2/2011neiv2_eventfire_countyscc_caphap.zip")

d_event_2011 <- d_event_2011 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  rename(fips = state_and_county_fips_code) %>%
  group_by(fips) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_2011 <-
  bind_rows(
    d_nonroad_2011 %>% mutate(nei_year = "2011", eis = "nonroad"),
    d_onroad_2011 %>% mutate(nei_year = "2011", eis = "onroad"),
    d_nonpoint_2011 %>% mutate(nei_year = "2011", eis = "nonpoint"),
    d_event_2011 %>% mutate(nei_year = "2011", eis = "event")
  )

saveRDS(d_2011, "nei_2011_county.rds")

d_point_2011 <-
  dl_unzip_and_read("ftp://newftp.epa.gov/air/nei/2011/data_summaries/2011v2/2011neiv2_facility.zip")

d_point_2011 <- d_point_2011 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  transmute(
    lat = latitude_msr,
    lon = longitude_msr,
    total_emissions = total_emissions
  ) %>%
  na.omit() %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    mutate(
      nei_year = 2011,
      eis = "point"
    )

d_point_2011$h3 <- h3::geo_to_h3(d_point_2011, res = 8)

saveRDS(d_point_2011, "nei_2011_point.rds")

#### 2008 ####

# some files were downloaded manually and unzipped from same ftp site (b/c of inconsistent file naming schemes)

d_nonpoint_2008 <- readr::read_csv("2008NEIv3_nonpoint.csv", comment = "#")

d_nonpoint_2008 <- d_nonpoint_2008 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_nonroad_2008 <-
  purrr::map_dfr(c(
    "2008NEIv3_nonroad123.csv",
    "2008NEIv3_nonroad4.csv",
    "2008NEIv3_nonroad5.csv",
    "2008NEIv3_nonroad67.csv",
    "2008NEIv3_nonroad8910.csv"
  ), readr::read_csv, comment = "#", col_types = "ccccccccccccnc")

d_nonroad_2008 <- d_nonroad_2008 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_onroad_2008 <-
  purrr::map_dfr(c(
    "2008NEIv3_onroad123.csv",
    "2008NEIv3_onroad4.csv",
    "2008NEIv3_onroad5.csv",
    "2008NEIv3_onroad67.csv",
    "2008NEIv3_onroad8910.csv"
  ), readr::read_csv, comment = "#", col_types = "ccccccccccccnc")

d_onroad_2008 <- d_onroad_2008 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  group_by(state_and_county_fips_code) %>%
  summarize(total_pm25 = sum(total_emissions)) %>%
  rename(fips = state_and_county_fips_code) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

# event not avail for 2008

d_2008 <-
  bind_rows(
    d_nonroad_2008 %>% mutate(nei_year = "2008", eis = "nonroad"),
    d_onroad_2008 %>% mutate(nei_year = "2008", eis = "onroad"),
    d_nonpoint_2008 %>% mutate(nei_year = "2008", eis = "nonpoint")
  )

saveRDS(d_2008, "nei_2008_county.rds")

d_point_2008 <-
  readr::read_csv("2008neiv3_facility.csv", comment = "#")

d_point_2008 <- d_point_2008 %>%
  filter(description == "PM2.5 Primary (Filt + Cond)") %>%
  transmute(
    lat = latitude_msr,
    lon = longitude_msr,
    total_emissions = total_emissions
  ) %>%
  na.omit() %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    mutate(
      nei_year = 2008,
      eis = "point"
    )

d_point_2008$h3 <- h3::geo_to_h3(d_point_2008, res = 8)

saveRDS(d_point_2008, "nei_2008_point.rds")

#### 2017 ####

# only point, event, and nonroad avail right now

d_nonroad_2017 <-
  purrr::map_dfr(c(
    "nonroad_123.csv",
    "nonroad_4.csv",
    "nonroad_5.csv",
    "nonroad_67.csv",
    "nonroad_8910.csv"
  ), readr::read_csv, comment = "#", col_type = "cccccccccccccccncc")

d_nonroad_2017 <- d_nonroad_2017 %>%
  filter(`pollutant desc` == "PM2.5 Primary (Filt + Cond)") %>%
  rename(fips = `fips code`) %>%
  group_by(fips) %>%
  summarize(total_pm25 = sum(`total emissions`)) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_event_2017 <- readr::read_csv("esg_cty_scc_11808.csv")

d_event_2017 <- d_event_2017 %>%
  filter(`pollutant desc` == "PM2.5 Primary (Filt + Cond)") %>%
  rename(fips = `fips code`) %>%
  group_by(fips) %>%
  summarize(total_pm25 = sum(`total emissions`)) %>%
  mutate(fips = stringr::str_pad(fips, 5, "left", "0"))

d_2017 <-
  bind_rows(
    d_nonroad_2017 %>% mutate(nei_year = "2017", eis = "nonroad"),
    d_event_2017 %>% mutate(nei_year = "2017", eis = "event")
  )

saveRDS(d_2017, "nei_2017_county.rds")

d_point_2017 <-
  readr::read_csv("emis_sum_fac_11878.csv", comment = "#")

d_point_2017 <- d_point_2017 %>%
  filter(`pollutant desc` == "PM2.5 Primary (Filt + Cond)") %>%
  transmute(
    lat = `site latitude`,
    lon = `site longitude`,
    total_emissions = `total emissions`
  ) %>%
  na.omit() %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(
    nei_year = 2017,
    eis = "point"
  )

d_point_2017$h3 <- h3::geo_to_h3(d_point_2017, res = 8)

saveRDS(d_point_2017, "nei_2017_point.rds")

#### save all counties into one file

# merge to all counties in 50 states and make no emissions into zero, except for 2008 event, 2017 onroad, and 2017 nonpoint, which are truly missing

county_fips <-
  tigris::counties() %>%
  sf::st_as_sf() %>%
  filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>%
  sf::st_drop_geometry() %>%
  transmute(fips = GEOID)

all_county <-
  bind_rows(d_2008, d_2011, d_2014, d_2017) %>%
  pivot_wider(names_from = eis, values_from = total_pm25)

d_county <- left_join(county_fips, all_county, by = "fips") %>%
  as_tibble()

d_county <- replace_na(d_county, list(nonroad = 0, onroad = 0, nonpoint = 0, event = 0))

d_county[d_county$nei_year == "2008", "event"] <- NA
d_county[d_county$nei_year == "2017", "onroad"] <- NA
d_county[d_county$nei_year == "2017", "nonpoint"] <- NA

saveRDS(d_county, "nei_county_pm25.rds")

system("aws s3 cp nei_county_pm25.rds s3://geomarker/nei/nei_county_pm25.rds")

#### save all points into one file

rbind(d_point_2008, d_point_2011, d_point_2014, d_point_2017) %>%
  saveRDS("nei_point_pm25.rds")

system("aws s3 cp nei_point_pm25.rds s3://geomarker/nei/nei_point_pm25.rds")
