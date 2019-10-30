library(tidyverse)
library(tidycensus)

v <- load_variables(2018, "acs1") %>%
  rename(variable = name)

MSAs <- c("19740", "12060", "34980", "14460")

df <- get_acs("metropolitan statistical area/micropolitan statistical area",
              variables = c("B01001B_001", "B01001C_001", "B01001D_001","B01001E_001","B01001H_001", "B01001I_001"),
              year = 2018, 
              survey = "acs1")

df <- df %>%
filter(GEOID %in% MSAs) 
  
df <- left_join(df, v, by = "variable")

df <- df %>%
  filter(GEOID == 19740)

df <- rename(df, Racial_Identity = concept)

df$Racial_Identity <- str_replace(df$Racial_Identity, "SEX BY AGE", "")

ggplot(df, aes(x = "", y = estimate, fill = Racial_Identity)) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) + 
  scale_fill_brewer(palette="Blues")+
  theme_minimal()

Census_Pull <- data.frame(MSAs, MSA_Names) %>%
  rename(GEOID = MSAs)

Census_Pull$GEOID <- as.character(Census_Pull$GEOID)

PoP_over_25_vars <- function() {
  
  PoP_over_25_endings <- c()
  
  Male_var_endings <- 11:25 
  
  for (i in 1:length(Male_var_endings)) {
    new <- as.character(paste0("B01001_0", Male_var_endings[i]))
    PoP_over_25_endings <- c(PoP_over_25_endings, new)
  }
  
  female_var_endings <- 35:49
  
  for (i in 1:length(female_var_endings)) {
    new <- as.character(paste0("B01001_0", female_var_endings[i]))
    PoP_over_25_endings <- c(PoP_over_25_endings, new)
  }
  return(PoP_over_25_endings)
  
}

PoP_over_25_vars <- PoP_over_25_vars()

PoP_over_25 <- lapply(seq_along(PoP_over_25_vars), 
                      function(i) get_acs(
                        geography = "metropolitan statistical area/micropolitan statistical area",
                        variables = PoP_over_25_vars[i], 
                        year = 2018, 
                        survey = "acs1",
                        geometry = FALSE)) %>%
  bind_rows() %>%
  filter(GEOID %in% c(MSAs)) %>% 
  group_by(GEOID) %>% 
  summarise(Total_Pop_over_25 = sum(estimate)) %>%
  select(GEOID, Total_Pop_over_25)

Median_Household_income <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                                   variables = "B19013_001", 
                                   year = 2018,
                                   survey = "acs1",
                                   geometry = FALSE) %>%
  filter(GEOID %in% c(MSAs)) %>%
  rename(Median_Household_income = estimate) %>%
  select(GEOID, Median_Household_income)

Persons_below_poverty_line <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                                      variables = "B17001_002", 
                                      year = 2018, 
                                      survey = "acs1",
                                      geometry = FALSE) %>%
  filter(GEOID %in% c(MSAs)) %>%
  rename(Persons_below_poverty_line = estimate) %>%
  select(GEOID, Persons_below_poverty_line)

HS_Education_vars_over_25 <- c( "B15001_014", "B15001_015", "B15001_016", "B15001_017", "B15001_018", "B15001_022", "B15001_023", "B15001_024",
                                "B15001_025", "B15001_026", "B15001_030", "B15001_031", "B15001_032", "B15001_033", "B15001_034", "B15001_038",
                                "B15001_039", "B15001_040", "B15001_041", "B15001_042", "B15001_055", "B15001_056", "B15001_057", "B15001_058",
                                "B15001_059", "B15001_063", "B15001_064", "B15001_065", "B15001_066", "B15001_067", "B15001_071", "B15001_072",
                                "B15001_073", "B15001_074", "B15001_075", "B15001_079", "B15001_080", "B15001_081", "B15001_082", "B15001_083") 

HS_over_25 <- lapply(seq_along(HS_Education_vars_over_25), 
                     function(i) get_acs(
                       geography = "metropolitan statistical area/micropolitan statistical area",
                       variables = HS_Education_vars_over_25[i], 
                       year = 2018, 
                       survey = "acs1",
                       geometry = FALSE)) %>%
  bind_rows() %>%
  filter(GEOID %in% c(MSAs)) %>% 
  group_by(GEOID) %>% 
  summarise(HS_over_25 = sum(estimate)) %>%
  select(GEOID, HS_over_25)


Bachelors_over_25_vars <-  c( "B15001_017", "B15001_018", "B15001_025", "B15001_026", "B15001_033", "B15001_034", "B15001_041", "B15001_042", 
                              "B15001_058", "B15001_059", "B15001_066", "B15001_067", "B15001_074", "B15001_075", "B15001_082", "B15001_083") 

Bachelors_over_25 <- lapply(seq_along(Bachelors_over_25_vars), 
                            function(i) get_acs(
                              geography = "metropolitan statistical area/micropolitan statistical area",
                              variables = Bachelors_over_25_vars[i], 
                              year = 2018,
                              survey = "acs1",
                              geometry = FALSE)) %>%
  bind_rows() %>%
  filter(GEOID %in% c(MSAs)) %>% 
  group_by(GEOID) %>% 
  summarise(Bachelors_over_25 = sum(estimate)) %>%
  select(GEOID, Bachelors_over_25)


Census_Pull <- left_join(Census_Pull, PoP_over_25, by = "GEOID")

Census_Pull <- left_join(Census_Pull, HS_over_25, by = "GEOID")

Census_Pull <- left_join(Census_Pull, Bachelors_over_25, by = "GEOID")

Census_Pull <- left_join(Census_Pull, PoP_over_25, by = "GEOID")

Census_Pull <- Census_Pull %>%
  mutate(HS_percentage = HS_over_25 / Total_Pop_over_25, Bach_percentage = Bachelors_over_25/Total_Pop_over_25)

Census_Pull <- left_join(Census_Pull, Median_Household_income, by = "GEOID")

Census_Pull <- left_join(Census_Pull, Persons_below_poverty_line, by = "GEOID")
