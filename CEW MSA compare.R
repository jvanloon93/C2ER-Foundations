library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

Codes <- c("C1974", "C1206", "C3498", "C1446")

MSA_Names <- c("Denver-Aurora-Lakewood, CO MSA", "Atlanta-Sandy Springs-Roswell, GA MSA", 
               "Nashville-Davidson--Murfreesboro--Franklin, TN MSA", "Boston-Cambridge-Newton, MA-NH MSA") 

lst <- lapply(seq_along(Codes), function(i) qcew_api(year = 2018, qtr = "a", slice = "area", sliceCode = Codes[i]))

names(lst) <- MSA_Names

Name_df <- data.frame(Codes, MSA_Names)

Name_df <- rename(Name_df, area_fips = Codes)

df18 <- bind_rows(lst)

df18 <- df18 %>%
filter(agglvl_code == 44 & own_code == 5) %>%
group_by(industry_code) %>%
select(area_fips, industry_code, year, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) 

df18 <- left_join(df18, Name_df, by = "area_fips")

df18 <- left_join(df18, naics,  by = "industry_code")

ggplot(df, aes(x = industry_title, y = annual_avg_emplvl)) + geom_col() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + 
  facet_wrap(~ MSA_Names)

ggplot(df, aes(x = industry_title, y = annual_avg_estabs)) + geom_col() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + 
  facet_wrap(~ MSA_Names)

ggplot(df, aes(x = industry_title, y = avg_annual_pay)) + geom_col() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + 
  facet_wrap(~ MSA_Names)

lst15 <- lapply(seq_along(Codes), function(i) qcew_api(year = 2015, qtr = "a", slice = "area", sliceCode = Codes[i]))

df15 <- bind_rows(lst15)

df15 <- df15 %>%
  filter(agglvl_code == 44 & own_code == 5) %>%
  group_by(industry_code) %>%
  select(area_fips, industry_code, year, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) 

df15 <- left_join(df15, Name_df, by = "area_fips")

df15 <- left_join(df15, naics,  by = "industry_code")

write.csv(df18, "df18.csv")

write.csv(df15, "df15.csv")

us18 <- qcew_api(year = 2018, qtr = "a", slice = "area", sliceCode = "US000")

us15 <- qcew_api(year = 2015, qtr = "a", slice = "area", sliceCode = "US000") 

us <- bind_rows(us18, us15)

us <- us %>%
filter(agglvl_code == 14 & own_code == 5) %>%
  group_by(year) %>%
  summarise(NAICS_Emp = sum(annual_avg_emplvl), NAICS_Est = sum(annual_avg_estabs), NAICS_Wage = sum(total_annual_wages)) %>%
  mutate(Naics_Ann_Wage = NAICS_Wage/NAICS_Emp) 

write.csv(us, "us.csv")

us18 <- us18 %>%
  filter(own_code == 5) 

us18 <- left_join(us18, naics, by = "industry_code")

%>%
  group_by(year) %>%
  summarise(NAICS_Emp = sum(annual_avg_emplvl), NAICS_Est = sum(annual_avg_estabs), NAICS_Wage = sum(total_annual_wages)) %>%
  mutate(Naics_Ann_Wage = NAICS_Wage/NAICS_Emp) 