library(tidyverse)

ircc <- read_csv('../results/ircc_service_info.csv') %>%
  select(-`...1`, -City, -Province)

healthline <- read_csv('../results/organization_service_info.csv')

ircc_peel <- filter(ircc, `In Peel` == "yes")

healthline_missing_cols <- setdiff(colnames(ircc %>% select(-`In Peel`)), colnames(healthline))

for (col in healthline_missing_cols) {
  healthline[[col]] <- rep(NA, nrow(healthline))
}

write_csv(healthline, 'combined.csv')
write_csv(ircc_peel, 'to_combine.csv')