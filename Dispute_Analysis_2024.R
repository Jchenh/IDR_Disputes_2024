library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


#Read the 2024 combined csv 
dispute_2024 <- read.csv("payer_dispute_2024.csv")
head(dispute_2024, 10)

#Assign specialty based on CMS Codes
dispute_2024_specialty <- dispute_2024 %>% 
  mutate(specialty = case_when(
                                   Service.Code %in% 99281:99288 ~ "Emergency",
                                   Service.Code %in% 95700:96020 ~ "Neurology",
                                   Service.Code %in% 70010:79999 ~ "Radiology",
                                   Service.Code %in% 10004:69990 ~ "Surgery",
                                   Service.Code %in% 00100:01999 ~ "Anesthesia",
                                   Service.Code %in% 99291:99292 ~ "Critical care services",
                                   Service.Code %in% 93880:93998 ~ "Non-invasive vascular diagnostic studies",
                                   Service.Code %in% 99221:99239 ~ "Hospital inpatient services",
                                   Service.Code %in% 99466:99480 ~ "Inpatient neonatal intensive care",
                                   Service.Code %in% 99217:99226 ~ "Hospital observation services",
                                   Service.Code %in% 92920:93799 ~ "Cardiovascular procedures",
                                   Service.Code %in% 99151:99157 ~ "Moderate (conscious) sedation",
                                   Service.Code %in% 96360:96549 ~ "Other",
                                   Service.Code %in% 80047:89398 ~ "Pathology and lab",
                                   Service.Code %in% 99464:99465 ~ "Labor and delivery services"
                                 ))

#View the total number of disputes, not line of item
length(unique(dispute_2024_specialty$Dispute.Number))

#Calculate win rates of payers and providers
outcome <- dispute_2024_specialty %>% group_by(Payment.Determination.Outcome) %>% 
  summarise(count = n()) %>%
  mutate(percent = count/sum(count, na.rm=T)*100)

View(outcome)

#Calculate number of items by specialty
specialty_count <- dispute_2024_specialty %>% group_by(specialty) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

View(specialty_count)

#Calculate prevailing party offer as of % of QPA by specialty
price <- dispute_2024_specialty %>% group_by(specialty) %>%
  summarise(median_value = median(Prevailing.Party.Offer.as...of.QPA, na.rm=TRUE)) %>%
  arrange(desc(median_value))

View(price)

#A look at data on state level
state <- dispute_2024_specialty %>% group_by(Location.of.Service) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

View(state)

#Calculate number of items by place of service & outcomes of determinations
place <- dispute_2024_specialty %>% group_by(Place.of.Service.Code) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count, na.rm=T)*100) %>%
  mutate(percent = round(percent, digits = 2)) %>%
  arrange(desc(percent))

View(place)

basic <- dispute_2024_specialty %>% 
  group_by(Location.of.Service, Payment.Determination.Outcome) %>%
  summarise (count = n()) %>%
  mutate(percent = count/sum(count, na.rm=T))

View(basic)

#Take a look at surgery and spread it for visualization
surgery <- dispute_2024_specialty %>% 
  filter( specialty == "Surgery") %>%
  group_by(Location.of.Service, Payment.Determination.Outcome) %>%
  summarise (count = n()) %>%
  mutate(percent = count/sum(count, na.rm=T) * 100)

surgery_spread <- surgery %>% 
 select(-count) %>%
 spread(Location.of.Service, percent)

View(surgery_spread)

#Take a look at surgery and spread it for visualization
radiology <- dispute_2024_specialty %>% 
  filter(specialty == "Radiology") %>%
  group_by(Location.of.Service, Payment.Determination.Outcome) %>%
  summarise (count = n()) %>%
  mutate(percent = count/sum(count) * 100) %>%
  filter (sum(count) > 10)

radiology_spread <- radiology %>% 
  select(-count) %>%
  spread(Location.of.Service, percent)

View(radiology_spread)


#More analysis on the groups initiating disputes
library(stringr)

provider_group <- dispute_2024_specialty %>%
  group_by(Provider.Facility.Group.Name) %>%
  summarise(count = n())

View(provider_group)

#Take a closer look at HaloMD, using their email domain
filtered_data <- dispute_2024_specialty %>%
  filter(str_detect(Provider.Email.Domain, "halomd"))

View(filtered_data)

halomd <- filtered_data %>% 
  group_by(specialty) %>%
  summarise(median_value = median(Prevailing.Party.Offer.as...of.QPA, na.rm=TRUE)) %>%
  arrange(desc(median_value))

View(halomd)

halomd_rate <- filtered_data %>% 
  group_by(Payment.Determination.Outcome) %>% 
  summarise(count = n()) %>%
  mutate(percent = count/sum(count, na.rm=T))

View(halomd_rate)

halomd_insurer <- filtered_data %>% 
  group_by(Health.Plan.Issuer.Email.Domain) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

View(halomd_insurer)
