library(tidycensus)
library(tidyverse)

#Reading in the CSV file with all of the airport pairs
airports = read_csv("airport_pairs.csv")

#Filtering so it only includes those coming in or out of RDU
airports = airports %>% filter(origin == "RDU" | dest == "RDU")

#Sorting to find most popular non-stop destination, after filtering for 10,000 passengers

airports = airports %>% filter(passengers >= 10000) %>%
  arrange(dest, passengers)

#Found that the most popular destination from Raleigh is ATL, with 540190 passengers. 

#Now starting question 2, retrieving data from tidycensus

acs_vars = load_variables(2021, "acs5")
write_csv(acs_vars, "acsvars.csv")



demographics = get_acs(
  geography="cbsa",  
  variables=c(
    "total_pop"="B01003_001"
  ),
  year=2021,
  survey="acs5",
  output="wide",
)
#making characters so I can join them
airports$origin_cbsa = as.character(airports$origin_cbsa)
airports$dest_cbsa = as.character(airports$dest_cbsa)

#creating copies for join and made geo read as a character above. 
origin_pop = demographics %>% rename("origin_cbsa" = "GEOID") 
dest_pop = demographics %>% rename("dest_cbsa" = "GEOID") 

airport_demographics = left_join(airports, origin_pop, by="origin_cbsa")
airport_demographics = left_join(airports, dest_pop, by="dest_cbsa")

