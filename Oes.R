library(readr)
library(readxl)

oes <- read_excel("MSA_M2018_dl.xlsx")

MSAs <- c("19740", "12060", "34980", "71650")

oes$TOT_EMP <- as.numeric(oes$TOT_EMP)

oes <- oes %>%
  filter(AREA %in% MSAs & OCC_GROUP == "detailed") %>%
  arrange(desc(TOT_EMP)) 

oes[1:10,] %>%
  ggplot(aes(x = reorder(OCC_TITLE, -TOT_EMP), y = TOT_EMP)) + geom_col() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  

