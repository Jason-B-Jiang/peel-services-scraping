library(tidyverse)

full <- read_csv('temp/combined.csv') %>%
  select(Name,
         Website,
         Address,
         Email,
         Phone,
         Description,
         Fees,
         Eligibility,
         Languages,
         `Areas Served`,
         `Last Updated`,
         `Citizen test preparation`,
         `Help with daily life`,
         `Refugee services`,
         `Job language training`,
         `Job search support`,
         `LGBT services`,
         `Language assessment`,
         `Language training`,
         `Other services`,
         `Senior services`,
         `Women services`,
         `Youth services`) %>%
  group_by(Name, Address) %>%
  distinct(.keep_all = TRUE)

coordinates <- read_csv('../results/coordinates.csv') %>%
  select(-in_peel) %>%
  group_by(Name, Address) %>%
  distinct(.keep_all = TRUE)

combined <- coordinates %>%
  left_join(full, by = c('Name', 'Address'))

write_csv(combined, '../results/coordinates_and_info.csv')