cong %>%
  mutate(county_ahcb = if_else(state == "MD" & county == "Montgomery" & is.na(county_ahcb), "mds_montgomery", county_ahcb)) %>%
  mutate(county_fips = if_else(state == "MD" & county == "Montgomery" & is.na(county_fips), 24031L, county_fips)) %>%
  mutate(county_ahcb = if_else(state == "MD" & county == "Prince George's" & is.na(county_ahcb), "mds_princegeorges", county_ahcb)) %>%
  mutate(county_fips = if_else(state == "MD" & county == "Prince George's" & is.na(county_fips), 24033L, county_fips)) %>%
  mutate(county_ahcb = if_else(state == "MA" & county == "Kennebec" & is.na(county_ahcb), "mes_kennebec", county_ahcb)) %>%
  mutate(county_fips = if_else(state == "MA" & county == "Kennebec" & is.na(county_fips), 23011L, county_fips))  -> cong

write_csv(cong, "congressional-counties.csv")
