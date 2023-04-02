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
    "total_pop"="B01003_001",
    "income" = "B17001_002"
  ),
  year=2021,
  survey="acs5",
  output="wide",
)
#making characters so I can join them
demographics = demographics %>% mutate(percent_in_poverty = incomeE/total_popE)
airports$origin_cbsa = as.character(airports$origin_cbsa)
airports$dest_cbsa = as.character(airports$dest_cbsa)

#creating copies for join and made geo read as a character above. 
origin_pop = demographics %>% rename("origin_cbsa" = "GEOID", "origin_pop" = "total_popE", "origin_percent_in_poverty" = "percent_in_poverty") 
dest_pop = demographics %>% rename("dest_cbsa" = "GEOID", "dest_pop" = "total_popE", "dest_percent_in_poverty" = "percent_in_poverty") 

airport_demographics = left_join(airports, origin_pop, by="origin_cbsa") %>%
left_join(dest_pop, by="dest_cbsa")


#Creating a CBSA to CBSA dataset

CBSAs = airport_demographics %>% group_by(origin_cbsa, dest_cbsa) %>% summarize(total_passengers = sum(passengers))

#defining a metro as atleast 50,000 people- all of these airports are in metro areas

#creating models for each one of my variabels to make it easier to plot
origin_model = lm(passengers~origin_pop, airport_demographics)
summary(origin_model)

dest_model = lm(passengers~dest_pop, airport_demographics)
summary(dest_model)

dist_model = lm(passengers~distancemiles, airport_demographics)
summary(dist_model)

#origin plot
ggplot(airport_demographics, aes(x=origin_pop, y=passengers)) +
  geom_point(size=0.5) +
  geom_abline(slope = coef(origin_model)[[2]], intercept = coef(origin_model)[[1]])
#positive association
#dest plot
ggplot(airport_demographics, aes(x=dest_pop, y=passengers)) +
  geom_point(size=0.5)+
  geom_abline(slope = coef(dest_model)[[2]], intercept = coef(dest_model)[[1]])
#positive association
#distance plot
ggplot(airport_demographics, aes(x=distancemiles, y=passengers)) +
  geom_point(size=0.5)+
  geom_abline(slope = coef(dist_model)[[2]], intercept = coef(dist_model)[[1]])
#negative association


#extra credit- now will look at the income/poverty status of each origin and destination

origin_model_percent_in_poverty = lm(passengers~origin_percent_in_poverty, airport_demographics)
summary(origin_model_percent_in_poverty)

dest_model_percent_in_poverty = lm(passengers~dest_percent_in_poverty, airport_demographics)
summary(origin_model_percent_in_poverty)


#origin plot
ggplot(airport_demographics, aes(x=origin_percent_in_poverty, y=passengers)) +
  geom_point(size=0.5) +
  geom_abline(slope = coef(origin_model_percent_in_poverty)[[2]], intercept = coef(origin_model_percent_in_poverty)[[1]])
#positive association between the origin income and number of passengers
#dest plot
ggplot(airport_demographics, aes(x=dest_percent_in_poverty, y=passengers)) +
  geom_point(size=0.5) +
  geom_abline(slope = coef(dest_model_percent_in_poverty)[[2]], intercept = coef(dest_model_percent_in_poverty)[[1]])
              

#negative association between the destination percent in poverty and number of passengers

#this seems expected


#Question 3

#getting all airport data
airports_all = read_csv("airport_pairs.csv")

#repeating what I did above

#making characters so I can join them
airports_all$origin_cbsa = as.character(airports_all$origin_cbsa)
airports_all$dest_cbsa = as.character(airports_all$dest_cbsa)

#creating copies for join and made geo read as a character above. 
origin_pop = demographics %>% rename("origin_cbsa" = "GEOID", "origin_pop" = "total_popE", "origin_percent_in_poverty" = "percent_in_poverty") 
dest_pop = demographics %>% rename("dest_cbsa" = "GEOID", "dest_pop" = "total_popE", "dest_percent_in_poverty" = "percent_in_poverty") 

airport_demographics = left_join(airports_all, origin_pop, by="origin_cbsa") %>%
  left_join(dest_pop, by="dest_cbsa")


#Creating a CBSA to CBSA dataset

CBSAs = airport_demographics %>% group_by(origin_cbsa, dest_cbsa) #%>% #summarize(passengers = passengers, origin_percent_in_poverty = origin_percent_in_poverty, dest_percent_in_poverty = dest_percent_in_poverty, distancemiles = distancemiles, origin_pop = origin_pop, dest_pop = dest_pop)

CBSAmodel = lm(passengers ~ origin_pop + dest_pop + origin_percent_in_poverty + dest_percent_in_poverty + distancemiles, CBSAs)
summary(CBSAmodel)
 
#Question 4

potential_airports = tibble(
  origin_pop= 1391801,
  dest_pop=c(2493429, 863807, 382747, 3296317),
  origin_percent_in_poverty = 0.09118689,
  dest_percent_in_poverty = c(0.09721071, 0.18958286, 0.17658401, 0.10449784),
  distancemiles=c(2363, 1606, 496, 2193)
)

potential_airports$forecast_passengers = predict(CBSAmodel, potential_airports)
potential_airports


potential_airports = tibble(
  origin_pop= c(2493429, 863807, 382747, 3296317),
  dest_pop=1391801,
  origin_percent_in_poverty = 0.09118689,
  dest_percent_in_poverty = c(0.09721071, 0.18958286, 0.17658401, 0.10449784),
  distancemiles=c(2363, 1606, 496, 2193)
)

potential_airports$forecast_passengers = predict(CBSAmodel, potential_airports)
potential_airports

#I think these results make sense, they work essentially the same way in either direction. They do not seem nonsensical.
