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

