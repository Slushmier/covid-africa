source('R//pull_covid_jhu_all.R')

african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso",
                       "Burundi", "Cabo Verde", "Cameroon", "Cape Verde",
                       "Central African Republic", "Chad",
                       "Congo (Brazzaville)",  "Cote d'Ivoire", 
                       "Democratic Republic of the Congo", "Djibouti", "Egypt",
                       "Equatorial Guinea", "Eswatini", "Ethiopia", "Eritrea",
                       "Gabon", "Gambia", "Gambia, The", "Ghana", "Guinea",
                       "Guinea-Bissau", "Ivory Coast", "Kenya", "Liberia",
                       "Libya", "Madagascar", "Malawi", "Mali", "Mayotte",
                       "Mauritania", "Mauritius","Mozambique","Morocco",
                       "Namibia", "Niger", "Nigeria", "Republic of the Congo",
                       "Reunion", "Rwanda", "Senegal", "Seychelles",
                       "Sierra Leone", "Somalia", "South Africa", "South Sudan",
                       "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda",
                       "Zambia", "Zimbabwe")

all_reports <- all_reports %>% mutate(Country_Region = as.character(Country_Region)) %>% 
  mutate(Country_Region = case_when(
                            Country_Region == "Cape Verde" ~ "Cabo Verde",
                            Country_Region ==  "Ivory Coast" ~ "Cote d'Ivoire",
                            Country_Region ==  "Congo (Brazzaville)" ~ "Republic of the Congo",
                            TRUE ~ Country_Region))

africa_reports <- all_reports %>%
  select(-Province_State) %>% 
  complete(Country_Region, date,
           fill = list(Confirmed = 0, Deaths = 0, Recovered = 0, Active = 0)) %>% 
  filter(Country_Region %in% african_countries)
  
write_csv(africa_reports, "Data\\africa_covid.csv")
