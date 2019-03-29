Appendix A: Reproducible R Code
################### PUT ALL PRODUCTION CODE HERE #########################

################################
############ DATA ARCHITECTURE
################################
library(dplyr)
library(stringr)
library(logging)
library(ggplot2)
library(lubridate)
library(ggmap)
library(maps)
library(mapproj)
library(plotly)
library(reshape2)
library(sqldf)

### load data
load('~/Downloads/nps_subset.Rdata')
myData <- DT_exp_final_set

### make column names lowercase
names(myData) <- tolower(names(myData))
## replace spaces and hyphens in column names with underscores
names(myData) <- gsub('-| ', '_', names(myData))


### transform columns to dates
myData$check_in_date_c <- as.Date(myData$check_in_date_c)
myData$check_out_date_c <- as.Date(myData$check_out_date_c)
myData$arrival_date_r <- as.Date(myData$arrival_date_r)
myData$departure_date_r <- as.Date(myData$departure_date_r)
myData$e_delivereddate_i <- as.Date(myData$e_delivereddate_i)
myData$e_delivereddate_adj_i <- as.Date(myData$e_delivereddate_adj_i)
myData$response_date_h[which(myData$response_date_h == '')] <- NA
myData$response_date_h <- as.Date(myData$response_date_h)
myData$guest_checkin_date_h[which(myData$guest_checkin_date_h == '')] <- NA
myData$guest_checkin_date_h <- as.Date(myData$guest_checkin_date_h)
myData$guest_checkout_date_h[which(myData$guest_checkout_date_h == '')] <- NA
myData$guest_checkout_date_h <- as.Date(myData$guest_checkout_date_h)
myData$eff_date_cc <- as.Date(myData$eff_date_cc)

### create master columns
myData$m_check_in_date <- coalesce(myData$check_in_date_c, myData$arrival_date_r, myData$guest_checkin_date_h, myData$eff_date_cc)
myData$m_check_out_date <- coalesce(myData$check_out_date_c, myData$departure_date_r, myData$guest_checkout_date_h)
myData$m_length_of_stay <- coalesce(myData$length_of_stay_c, myData$length_stay_h)
myData$m_pov_code <- coalesce(myData$pov_code_c, myData$pov_h)
myData$m_guest_country <- coalesce(myData$guest_country_r, myData$e_country_i, myData$guest_country_h)
myData$m_guest_gender <- coalesce(myData$e_hy_gss_gender_i, myData$gender_h)
myData$m_survey_status <- coalesce(myData$status_h, myData$e_status_i)

## filter to just 2014 data
myData <- myData[year(myData$m_check_in_date) == '2014',]

## remove columns included in master cols
myData <- myData %>%
  select(-check_in_date_c, -arrival_date_r, -guest_checkin_date_h,
         -check_out_date_c, -departure_date_r, -guest_checkout_date_h,
         -length_of_stay_c, -length_stay_h, -pov_code_c, -pov_h,
         -guest_country_r, -e_country_i, -guest_country_h,
         -member_status_r, -e_hy_gss_tier_i, -gp_tier_h, - eff_date_cc,
         -status_h, -e_status_i, -e_hy_gss_gender_i, -gender_h)

######################################################
############ DATA TRANSFORMATIONS/MUNGING/CLEANING
#######################################################

## fix strings
myData$goldpassport_flg_r <- gsub('gp ', '', tolower(myData$goldpassport_flg_r))

### fix guest state format
myData$guest_state_h[grepl('[[:punct:]]| |[0-9]', toupper(myData$guest_state_h))] <- NA
myData$guest_state_h[which(myData$guest_state_h == '')] <- NA
### only keep US states
myData$guest_state_h[which(!(myData$guest_state_h %in% c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO','CT', 'DE',
                                                         'FL', 'GA', 'HI', 'ID', 'IL','IN', 'IA','KS','KY',
                                                         'LA','ME','MD', 'MA','MI', 'MN', 'MS', 'MO', 'MT',
                                                         'NE', 'NV','NH','NJ','NM','NY','NC','ND','OH','OK',
                                                         'OR','PA','RI','SC','SD','TN','TX','UT','VT','VA',
                                                         'WA','WV','WI','WY','DC')))] <- NA
## set all entries with US states to US country
myData$m_guest_country[which(is.na(myData$guest_state_h) == FALSE)] <- 'UNITED STATES'
## set bad/unknown values to NA
myData$m_guest_country[grepl('[0-9]|^[A-Z]{2}$', myData$m_guest_country)] <- NA
myData$m_guest_country[which(myData$m_guest_country %in% c('','$CO','MIA','NEW','MSP','XZJ'))] <- NA

myData$gp_tier <- tolower(myData$gp_tier)
myData$gp_tier[which(myData$gp_tier == 'ldia')] <- 'lifetime diamond'
myData$gp_tier[which(myData$gp_tier == 'diam')] <- 'diamond'
myData$gp_tier[which(myData$gp_tier == 'plat')] <- 'platinum'
myData$gp_tier[which(myData$gp_tier == 'card')] <- 'courtesy'
myData$gp_tier[which(myData$gp_tier == '')] <- NA

#set state names to lowercase
myData$state_pl <- tolower(myData$state_pl)
#Change likelihood_recommend_h to numeric
myData$likelihood_recommend_h <- as.numeric(myData$likelihood_recommend_h)
myData$nps_type[which(myData$nps_type == '')] <- NA
#Repeat customers are de facto Promoters for NPS type and de facto 9 for blank likelihood_recommend_h.  The following code converts any blanks for repeat customers to promoters.
#data frame for repeat customers with likelihood_recommend-h and nps_type
myData <- myData %>%
  group_by(cons_guest_id_c) %>%
  mutate(n = n(),
         nps_type = ifelse((n > 1) & is.na(nps_type), "Promoter", nps_type),
         likelihood_recommend_h = ifelse((n > 1) & is.na(likelihood_recommend_h), 9, likelihood_recommend_h)) %>%
  select(-n) %>%
  ungroup()

####################################
############ DESCRIPTIVE STATISTICS
####################################
myLocations <- myData %>%
  select(property_id_pl, property_longitude_pl,
         property_latitude_pl, hotel_name_long_pl) %>%
  group_by(property_id_pl) %>%
  mutate(number_of_surveys = n()) %>%
  distinct()

# getting the map
us_map2 <- get_map("united states", zoom = 4)
# plotting the map with some points on it
ggmap(us_map2) +
  geom_point(data = myLocations, aes(x = property_longitude_pl,
                                     y = property_latitude_pl,
                                     size = number_of_surveys))


myAvg <- myData %>%
  select(m_check_in_date, likelihood_recommend_h, brand_pl) %>%
  filter(is.na(likelihood_recommend_h) == FALSE) %>%
  group_by(m_check_in_date, brand_pl) %>%
  mutate(nps = mean(likelihood_recommend_h)) %>%
  select(-likelihood_recommend_h) %>%
  distinct()

ggplot(data = myAvg, aes(x = m_check_in_date, y = nps, colour = brand_pl)) +
  geom_line()

myCustomers <- myData %>%
  select(cons_guest_id_c, gp_tier, m_length_of_stay) %>%
  filter(is.na(cons_guest_id_c) == FALSE) %>%
  group_by(cons_guest_id_c) %>%
  mutate(number_of_stays = n(),
         avg_length_of_stay = mean(m_length_of_stay)) %>%
  select(-m_length_of_stay) %>%
  distinct()

myAvgTier <- myCustomers %>%
  group_by(gp_tier) %>%
  mutate(avg_stay = mean(number_of_stays)) %>%
  select(-cons_guest_id_c) %>%
  distinct()

ggplot(myCustomers, aes(x = gp_tier, y = avg_length_of_stay)) +
  geom_boxplot()

ggplot(myCustomers, aes(x = gp_tier, y = number_of_stays)) +
  geom_boxplot()

###################################
############ BUSINESS QUESTION 1
###################################
#check total promoters before considering repeat customers
totalPromoters <- length(myData$cons_guest_id_c[which(myData$likelihood_recommend_h >= 9)])
totalPromoters
#find repeat customers
myData$checkDupes <- (duplicated(myData$cons_guest_id_c, incomparables = FALSE))
#enter the value of FALSE in the checkDupes column for all NA guest id values that show up as repeat customers.
myData$checkDupes <- ifelse(is.na(myData$cons_guest_id_c) & myData$checkDupes == TRUE
                            ,myData$checkDupes == FALSE
                            ,myData$checkDupes)
#variable to identify where likelihood to recommend = NA
emptyLikelihood <- is.na(myData$likelihood_recommend_h)
#set repeat customer likelihood_recommend_h to 9 if blank
myData$likelihood_recommend_h <- ifelse(myData$checkDupes == TRUE & emptyLikelihood, 9, myData$likelihood_recommend_h)
#check total promoters after considering repeat customers
totalPromoters
#Brands graphed by amount of promoters
hotelByBrand <- ggplot(hotelRatings, aes(x = rownames(hotelRatings), y = hotelRatings$TRUE.))
hotelByBrand <- hotelByBrand + geom_point(aes(color = rownames(hotelRatings), size = 5))
hotelByBrand <- hotelByBrand + labs(x = 'Brands', y = 'Promoters')
hotelByBrand <- hotelByBrand + ggtitle("Promoters by Brand")
hotelByBrand
# Find Top Customer
#--
fTopCustomer <- function (pimyDataSet, pinoOfCustomer) {
  noOfCustomer <- pinoOfCustomer
  MyCompletedSurvey <- pimyDataSet[pimyDataSet$m_survey_status=="COMPLETED",]
  MyCompletedSurvey <- sqldf ("select cons_guest_id_c , count(1) surveycount
                              from MyCompletedSurvey
                              where cons_guest_id_c is not null
                              and cons_guest_id_c > 0
                              group by cons_guest_id_c
                              order by 2 desc" )
  myTopCustomer <- subset(myData, cons_guest_id_c %in% head(MyCompletedSurvey$cons_guest_id_c,n=pinoOfCustomer))
  return (myTopCustomer)
  
}
############ TOP CUSTOMER INFORMATION
################################
#-- Top Customer Call
myTopCustomer <- fTopCustomer ( myData , 25 )
#count business and leisure stays per top 25 customers
topCustomer.pov <- myTopCustomer %>%
  group_by(cons_guest_id_c) %>%
  summarise(business = length(m_pov_code[which(m_pov_code == 'BUSINESS')])
            ,leisure = length(m_pov_code[which(m_pov_code == 'LEISURE')]))
overall.pov <- myData %>%
  group_by(brand_pl) %>%
  summarise(business = length(m_pov_code[which(m_pov_code == 'BUSINESS')])
            ,leisure = length(m_pov_code[which(m_pov_code == 'LEISURE')]))
#Assign variable to get hotels listed by recommendation > 8
hotelRatings <- tapply(myData$likelihood_recommend_h,list(myData$brand_pl,myData$likelihood_recommend_h > 8), length)
hotelRatings

#convert top customer and overall customer data to plot together on bar graphs
topCust.long<-melt(topCustomer.pov,id.vars="cons_guest_id_c")
overall.long <- melt(overall.pov,id.vars="brand_pl")
#graph top 25 customer leisure vs business
topCustomer.pov_g <- ggplot(topCust.long, aes(x = as.character(cons_guest_id_c), y = value, fill = factor(variable)))
topCustomer.pov_g <- topCustomer.pov_g + geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = round(seq(min(topCust.long$value), max(topCust.long$value), by = 1)))
topCustomer.pov_g <- topCustomer.pov_g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
topCustomer.pov_g <- topCustomer.pov_g + ggtitle("Top 25 Customer Purpose of Visit")
topCustomer.pov_g
#graph top 25 customer leisure vs business
overall.pov_g <- ggplot(overall.long, aes(x = brand_pl, y = value, fill = factor(variable)))
overall.pov_g <- overall.pov_g + geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = round(seq(min(0), max(overall.long$value), by = 100000)))
overall.pov_g <- overall.pov_g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
overall.pov_g <- overall.pov_g + ggtitle("Overall Business vs. Leisure Travel")
overall.pov_g
#mean likelihood to recommend per top customer
topCustomer.avgRating <- tapply(myTopCustomer$likelihood_recommend_h, myTopCustomer$cons_guest_id_c, mean, na.rm = TRUE)
topCustomer.avgRating <- data.frame(topCustomer.avgRating)
#state of the top customers
topCustomer.state <- tapply(myTopCustomer$state_r,   myTopCustomer$cons_guest_id_c, unique)
# Scatter Plot
mySummary <- myData %>%
  group_by(brand_pl) %>%
  summarise(guest_count=length(cons_guest_id_c)
            ,completedcount=sum(ifelse(m_survey_status=="COMPLETED" , 1, 0)))
ggplot(mySummary, aes(x=guest_count, y=completedcount)) + geom_point(aes(size=completedcount, colour=brand_pl))
rm(mySummary)
############ MAPPING TOP CUSTOMERS
################################
#focus only on top 5 customers
myTopFiveCustomer <- fTopCustomer ( myData , 5)
# Function to generate basic simple US map as starting point
fmyUSMap <- function () {
  us <- map_data("state")
  g <- ggplot ( us , aes(map_id=region) )
  g <- g + geom_map( map=us, fill="white", color="black")
  g <- g + expand_limits(x=us$long, y=us$lat)
  g <- g + coord_map() + ggtitle("Hotels Frequented by Top Customers")
  return (g)
}
# Plot Top Customers
myTopFiveCustomer$region <- tolower(myTopFiveCustomer$state_pl)
g <- fmyUSMap()
g <- g + geom_point(data=myTopFiveCustomer
                    , aes(x=property_longitude_pl
                          ,y=property_latitude_pl
                          ,size=likelihood_recommend_h
                          ,colour=as.character(cons_guest_id_c)))
g
############ COMPARE COUNTRY REVENUE
################################
#Get mean revenue by country
medianRevenueByCountry <- sqldf("select m_guest_country,
                                mean(pms_total_rev_c) as 'mean_revenue'
                                median(pms_total_rev_c) as 'median_revenue'
                                from myData
                                group by m_guest_country
                                order by avg(pms_total_rev_c) desc")
#stays per country
staysPerCountry <-  myData %>%
  group_by(m_guest_country) %>%
  summarise(stays_by_country = length(m_guest_country))

#Get mean revenue by state
sqldf("select guest_state_h, avg(pms_total_rev_c) as 'mean_revenue' 
      from myData 
      group by guest_state_h 
      order by avg(pms_total_rev_c) desc")

#Get mean revenue by member status
sqldf("select gp_tier, avg(pms_total_rev_c) as 'mean_revenue' 
      from myData 
      group by gp_tier 
      order by avg(pms_total_rev_c) desc")

###################################
############ BUSINESS QUESTION 2
###################################
######################################
# amenitiesClean.R
# Clean and prepare the amenities portion
# of the Hyatt data set. This file should
# be sourced first!!!
# The tidyverse is a requirement.
######################################

library(tidyverse)

# Remove columns that are not needed for analyzing amenities
cleanAmenities <- function(amenitiesDF, dfType) {
  
  # Remove spaces, dashes and ampersands in column names, they cause parsing errors
  cols <- colnames(amenitiesDF)
  cols <- gsub(" ","_", cols)
  cols <- gsub("&", "and", cols)
  colnames(amenitiesDF) <- gsub("-","_", cols)
  
  # Add surveys where the guests gave an overall sat score
  # but did not provide a likelihood to recommend score
  amenities <- amenitiesDF %>%
    mutate(NPS_Type = replace(NPS_Type, NPS_Type == "" & Overall_Sat_H > 8, "Promoter"))
  
  amenities <- amenities %>%
    mutate(NPS_Type = replace(NPS_Type, NPS_Type == "" & Overall_Sat_H < 9, "Detractor"))
  
  # Required column lists
  if(dfType == "full") {
    # Include subset which will allow comparison with user feedback
    amenities <- amenities %>%
      select(CONS_GUEST_ID_C, ARRIVAL_DATE_R, POV_H, Likelihood_Recommend_H,
             Overall_Sat_H, Guest_Room_H, Tranquility_H, Condition_Hotel_H,
             Customer_SVC_H, Staff_Cared_H, Internet_Sat_H, Check_In_H, FandB_FREQ_H,
             FandB_Overall_Experience_H, Brand_PL, All_Suites_PL:Valet_Parking_PL, GP_Tier, NPS_Type)
  } else if(dfType == "nps") {
    # NPS type and amenities, remove surveys where an NPS type
    # does not exist (no likelihood to recommend value)
    amenities <- amenities %>%
      select(NPS_Type, All_Suites_PL:Valet_Parking_PL) %>%
      filter(NPS_Type != "")
    
    # Change Y's to 1 and N's to 0
    amenities <- amenities %>%
      mutate_at(.vars = vars(All_Suites_PL:Valet_Parking_PL),
                .funs = funs(ifelse(. == "Y", 1, 0)))
    
    # Change Promoter to 1 and Neutral/Detractor to 0
    # Change NPS to promoter = 1, everything else 0
    amenities <- amenities %>%
      mutate_at(.vars = vars(NPS_Type),
                .funs = funs(ifelse(. == "Promoter", 1, 0)))
    
    # Do not convert to factors
    
  } else if(dfType == "nps.factor") {
    # NPS type and amenities, remove surveys where an NPS type
    # does not exist (no likelihood to recommend value)
    amenities <- amenities %>%
      select(NPS_Type, All_Suites_PL:Valet_Parking_PL) %>%
      filter(NPS_Type != "")
    
    # Change Y's to 1 and N's to 0
    amenities <- amenities %>%
      mutate_at(.vars = vars(All_Suites_PL:Valet_Parking_PL),
                .funs = funs(ifelse(. == "Y", 1, 0)))
    
    # Change Promoter to 1 and Neutral/Detractor to 0
    # Change NPS to promoter = 1, everything else 0
    amenities <- amenities %>%
      mutate_at(.vars = vars(NPS_Type),
                .funs = funs(ifelse(. == "Promoter", 1, 0)))
    
    # Convert to factors
    cols <- colnames(amenities)
    amenities <- amenities %>%
      mutate_all(.funs = funs(factor(.)))
    
  } else if(dfType == "brand") {
    # Include the brand of the hotel and amenities
    amenities <- amenities %>%
      select(Property_ID_PL, Brand_PL, All_Suites_PL:Valet_Parking_PL)
    
    # Return only unique properties
    amenities <- amenities %>%
      unique()
  } else {
    # Otherwise, send back everything
    amenities <- amenities %>%
      select(CONS_GUEST_ID_C:NPS_Type)
  }
  
  return(amenities)
}

######################################
# amenitiesBrand.R
# Contains functions perform some descriptive
# statistics specifically for amenities.
######################################

# Property counts by brand
propCounts <- function(amenitiesArg) {
  
  # First, clean up the data set
  amenities <- cleanAmenities(amenitiesArg, "brand")
  
  # Get a count of the number of properties for each brand,
  # will be used to calculate percentages later
  countAmen <- amenities %>%
    select(Property_ID_PL, Brand_PL) %>%
    group_by(Brand_PL) %>%
    summarise(Count = n())
  
  # Convert to a data frame
  countAmen <- countAmen %>%
    as.data.frame()
  
  print(countAmen)
  return(countAmen)
}

# Amenities by Brand
brandAmenity <- function(amenitiesArg) {
  
  # First, get property counts
  countAmen <- propCounts(amenitiesArg)
  
  # Next, clean up the working data set
  amenities <- cleanAmenities(amenitiesArg, "brand")
  
  # Get column names without Brand and Property ID
  amenCols <- colnames(amenities)[-1]
  amenCols <- amenCols[-1]
  
  # Set up results data frame with a dummy entry
  results <- data.frame("junk", "more junk", as.numeric(1001101))
  names(results) <- c("Brand", "Amenity", "Prop_With")
  
  # Repeat for each amenity column
  for(colName in amenCols) {
    
    tibb <- amenities %>%
      group_by(Brand_PL) %>%
      filter_(paste(colName, "==", "'Y'")) %>%
      select(colName) %>%
      summarise(count = n())
    
    # Create a temp DF
    tmp <- data.frame(tibb[[1]], colName, tibb[[2]])
    names(tmp) <- c("Brand", "Amenity", "Prop_With")
    
    # And merge it with the results set
    results <- rbind(results, tmp)
  }
  
  ### Calculate Percentages
  
  # Remove dummy value from results DF
  results <- results[-1,]
  
  # Initialize the holding vector with a dummy value
  vec <- c(0)
  
  # Cycle through and add the number of total properties for each brand
  for(entry in results$Brand) {
    vec <- c(vec, countAmen$Count[which(countAmen$Brand_PL == entry)])
  }
  
  # Strip dummy value
  vec <- vec[-1]
  
  # Add to the results DF
  results$Total_Prop <- vec
  
  # Calculate the percentage using the mutate function
  results <- results %>%
    mutate(Pct_With = round((Prop_With / Total_Prop) * 100, 0))
  
  return(results)
}

######################################
# amenitiesChiSq.R
# Perform a Chi-Squared test on the amenities
# and NPS type to determine if the two are dependent
######################################

# Get the counts of promoters and detractors per amenity type
chiSqAmenity <- function(amenitiesArg) {
  
  # First, clean up the data set
  amenities <- cleanAmenities(amenitiesArg, "nps")
  
  # Get column names without NPS_Type
  amenCols <- colnames(amenities)[-1]
  
  # Set up results data frame
  results <- data.frame("junk", as.numeric(1001101))
  names(results) <- c("Amenity", "p.value")
  
  # Loop through all amenity columns
  for(colName in amenCols) {
    tibb <- amenities %>%
      select(c("NPS_Type", colName))
    #filter_(paste(colName, "!=", 0))
    
    print(table(tibb[[1]], tibb[[2]]))
    # Perform chi squared analysis
    chisq <- chisq.test(tibb[[1]], tibb[[2]], correct = FALSE)
    print(chisq)
    
    # Create a temp DF
    tmp <- data.frame(colName, chisq$p.value)
    names(tmp) <- c("Amenity", "p.value")
    
    # And merge it with the results set
    results <- rbind(results, tmp)
  }
  
  return(results[-1,])
}

######################################
# amenitiesKSVM.R
# Contains a function for creating a
# scalable vector machine model for the
# Hyatt amenities
######################################

library(kernlab)

# Create a KSVM for the amenities
svmAmenities <- function(amenitiesArg) {
  
  # First, clean up the data set
  amenities <- cleanAmenities(amenitiesArg, "nps.factor")
  
  ## Create training and testing data sets
  
  # Random index for picking surveys
  randIndex <- sample(1:dim(amenities)[1])
  
  # Set a cutpoint at 1/16 of the data set, larger values
  # create errors due to resource constraints
  cutpoint1_6 <- floor(dim(amenities)[1]/16)
  
  # Portion of the data set for training
  trainingAmenities <- amenities[randIndex[1:cutpoint1_6],]
  
  # Use the remainder of the data set for testing model out
  testingAmenities <- amenities[randIndex[(cutpoint1_6+1):length(randIndex)],]
  
  # Create the model using the training portion of the data set
  ksvmAmen <- ksvm(NPS_Type ~ ., data = trainingAmenities, kernel = "rbfdot",
                   kpar = "automatic", C = 10, cross = 10,
                   prob.model = FALSE)
  
  # What are the details of the model?
  print(ksvmAmen)
  
  # Run a prediction using the testing portion of the data set
  ksvmAmenPred <- predict(ksvmAmen, testingAmenities)
  
  # Determine if the model predicted the NPS type correctly
  ksvmAmenGood <- data.frame(testingAmenities[,1], ksvmAmenPred,
                             ifelse(testingAmenities[,1] == ksvmAmenPred, TRUE, FALSE))
  colnames(ksvmAmenGood) <- c("Test", "Pred", "Correct")
  
  # Calculate the percent correct
  ksvmAmenCorrect <- length(which(ksvmAmenGood$Test == ksvmAmenGood$Pred)) /
    dim(ksvmAmenGood)[1]
  
  # Present the results
  ksvmAmenCorrect <- round(ksvmAmenCorrect * 100, 2)
  print(sprintf("Percent good: %f", ksvmAmenCorrect))
  
  return(ksvmAmenGood)
}

######################################
# amenitiesAssoc.R
# Contains a function for running the
# Hyatt amenities through associative
# rule mining
######################################

library(arules)
library(arulesViz)

# Associative rule mining
assocAmenity <- function(amenitiesArg) {
  
  # First, clean up the data set, do not return factors
  amenities <- cleanAmenities(amenitiesArg, "nps")
  
  # Make it a tibble for tidyverse functions
  amenities <- as_tibble(amenities)
  
  # Run the amenities as a matrix through the apriori function
  # High support and confidence values were selected because
  # amenities tend to be clustered together by brand, making them
  # artificially high
  apAmen <- amenities %>%
    as.matrix() %>%
    apriori(parameter = list(supp = 0.25, conf = 0.5))
  
  # Create a scatter plot showing support vs confidence and
  # lift
  plot(apAmen)
  
  # Return the rules where NPS type is on the right hand side and
  # lift is greater than 1. From inspection of the results, the best
  # rules with NPS on rhs tend to have a lift of 1.00xxx
  return(subset(apAmen, subset = rhs %in% "NPS_Type" & lift > 1.00))
}

######################################
# amenitiesTest.R
# Run through descriptive analytics and
# call functions for models.
######################################

library(tidyverse)
library(ggplot2)
library(gplots)

workingSet <- DT_exp_final_set

# Basic stats, how many promoters and detractors?
proDet <- cleanAmenities(workingSet, "nps.factor")
print(sprintf("Valid surveys considered: %d", dim(proDet)[1]))
# Promoters / detractors
pro <- sum(ifelse(proDet$NPS_Type == 1, 1, 0))
print(sprintf("Promoters: %d", pro))
print(sprintf("Detractors: %d", ((dim(proDet)[1])-pro)))
print(sprintf("Promotor Percent: %f", 100 * (pro / (dim(proDet)[1]))))

# Color coding for plots
#             	Hyatt   	House   	Place     	Regency   	Andaz   	Grand   	Park
hyatt_colors <- c('Hyatt'=col2hex("darkblue"), 'Hyatt House'=col2hex("darkgreen"), 'Hyatt Place'=col2hex("darkorange"),
                  'Hyatt Regency'=col2hex("darkviolet"), Andaz=col2hex("darkred"), 'Grand Hyatt'=col2hex("red"),
                  'Park Hyatt'=col2hex("blue"))

# Property counts
propCount <- propCounts(workingSet)

print(sprintf("Total Number of Properties: %d", propCount))

# Plot the counts
ggplot(propCount, aes(x = Brand_PL, y = Count, fill = Brand_PL)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = hyatt_colors) +
  labs(x = "Hyatt Brand", fill = "Brand") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Number of Properties by Brand")

# Total number of amenities offered at each brand
AmenityCount <- c(16,24,16,18,27,24,22)
Brands <- c("Andaz", "Grand Hyatt", "Hyatt House", "Hyatt Place",
            "Hyatt Regency", "Hyatt", "Park Hyatt")
offered <- data.frame(Brands, AmenityCount)

# Plot the total amenities offered
ggplot(offered, aes(x = Brands, y = AmenityCount, fill = Brands)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = hyatt_colors) +
  labs(x = "Hyatt Brand", fill = "Brand") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Number of Amenities by Brand")

# Add the NPS ratio (promoter / non-promoter)
npsRatio <- c(2.79, 2.34, 2.84, 2.74, 1.87, 1.95, 3.73)
offered <- data.frame(offered, npsRatio)

# Add the percent offered, this indicates consistency across
# properties in a brand
pctOffered <- c(0.59, 0.58, 0.63, 0.39, 0.44, 0.29, 0.45)
offered <- data.frame(offered, pctOffered)

# Run linear models to confirm observations
lmCount <- lm(formula = npsRatio ~ AmenityCount, data = offered)
lmConsistency <- lm(formula = npsRatio ~ pctOffered, data = offered)

# Get amenity statistics grouped by brand
brands <- brandAmenity(workingSet)

# Sort alphabetically by Brand
brands <- brands %>%
  arrange(Brand)

print("Amenities by Brand: ")
print(brands)

# Pull individual Brands
andaz <- brands %>%
  filter(Brand == "Andaz")

hyatt <- brands %>%
  filter(Brand == "Hyatt")

hyatt_house <- brands %>%
  filter(Brand == "Hyatt House")

hyatt_place <- brands %>%
  filter(Brand == "Hyatt Place")

hyatt_regency <- brands %>%
  filter(Brand == "Hyatt Regency")

grand_hyatt <- brands %>%
  filter(Brand == "Grand Hyatt")

park_hyatt <- brands %>%
  filter(Brand == "Park Hyatt")

# Plot all
ggplot(brands, aes(x = Amenity, y = Pct_With, group = Brand, fill = Brand)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Amenity Availability by Brand") +
  ylab("Percent of Properties with Amenity") +
  scale_fill_manual(values = hyatt_colors) +
  ylim(0, 100)

# Plot Hyatt
ggplot(hyatt, aes(x = Amenity, y = Pct_With)) +
  geom_bar(stat = "identity", position = "dodge", color = "darkblue", fill = "darkblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Amenity Availability for Hyatt") +
  ylab("Percent of Hyatt Properties with Amenity") +
  theme(legend.position = "none") +
  ylim(0, 100)

# Plot Hyatt House
ggplot(hyatt_house, aes(x = Amenity, y = Pct_With)) +
  geom_bar(stat = "identity", position = "dodge", color = "darkgreen", fill = "darkgreen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Amenity Availability for Hyatt House") +
  ylab("Percent of Hyatt House Properties with Amenity") +
  theme(legend.position = "none") +
  ylim(0, 100)

# Plot Hyatt Place
ggplot(hyatt_place, aes(x = Amenity, y = Pct_With)) +
  geom_bar(stat = "identity", position = "dodge", color = "darkorange", fill = "darkorange") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Amenity Availability for Hyatt Place") +
  ylab("Percent of Hyatt Place Properties with Amenity") +
  theme(legend.position = "none") +
  ylim(0, 100)

# Plot Hyatt Regency
ggplot(hyatt_regency, aes(x = Amenity, y = Pct_With)) +
  geom_bar(stat = "identity", position = "dodge", color = "darkviolet", fill = "darkviolet") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Amenity Availability for Hyatt Regency") +
  ylab("Percent of Hyatt Regency Properties with Amenity") +
  theme(legend.position = "none") +
  ylim(0, 100)

# Plot Andaz
ggplot(andaz, aes(x = Amenity, y = Pct_With)) +
  geom_bar(stat = "identity", position = "dodge", color="darkred", fill = "darkred") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Amenity Availability for Andaz") +
  ylab("Percent of Andaz Properties with Amenity") +
  theme(legend.position = "none") +
  ylim(0, 100)

# Plot Grand Hyatt
ggplot(grand_hyatt, aes(x = Amenity, y = Pct_With)) +
  geom_bar(stat = "identity", position = "dodge", color = "red", fill = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Amenity Availability for Grand Hyatt") +
  ylab("Percent of Grand Hyatt Properties with Amenity") +
  theme(legend.position = "none") +
  ylim(0, 100)

# Plot Park Hyatt
ggplot(park_hyatt, aes(x = Amenity, y = Pct_With)) +
  geom_bar(stat = "identity", position = "dodge", color = "blue", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Amenity Availability for Park Hyatt") +
  ylab("Percent of Park Hyatt Properties with Amenity") +
  theme(legend.position = "none") +
  ylim(0, 100)

# Run a chi-squared test for independence
x.sq <- chiSqAmenity(workingSet)
# Filter p-value for 1% of better
x.sq <- x.sq %>%
  filter(p.value < 0.01)

# Associative Rules Mining
goodRules <- assocAmenity(workingSet)

# Print top rules
inspect(head(goodRules, n = 5, by = "confidence"))

# Create an SVM for amenities
svmResult <- svmAmenities(workingSet)



###################################
############ BUSINESS QUESTION 3
###################################
q3_dat <- myData %>%
  select(likelihood_recommend_h, `f&b_overall_experience_h`, guest_room_h, 
         tranquility_h, condition_hotel_h, customer_svc_h, staff_cared_h, internet_sat_h, 
         check_in_h, nt_rate_r)
q3_dat$nt_rate_r <- as.numeric(q3_dat$nt_rate_r)
q3_dat$likelihood_recommend_h[which(is.na(q3_dat$likelihood_recommend_h))] <- mean(q3_dat$likelihood_recommend_h[which(is.na(q3_dat$likelihood_recommend_h) == FALSE)])
q3_dat$`f&b_overall_experience_h`[which(is.na(q3_dat$`f&b_overall_experience_h`))] <- mean(q3_dat$`f&b_overall_experience_h`[which(is.na(q3_dat$`f&b_overall_experience_h`) == FALSE)])
q3_dat$guest_room_h[which(is.na(q3_dat$guest_room_h))] <- mean(q3_dat$guest_room_h[which(is.na(q3_dat$guest_room_h) == FALSE)])
q3_dat$tranquility_h[which(is.na(q3_dat$tranquility_h))] <- mean(q3_dat$tranquility_h[which(is.na(q3_dat$tranquility_h) == FALSE)])
q3_dat$condition_hotel_h[which(is.na(q3_dat$condition_hotel_h))] <- mean(q3_dat$condition_hotel_h[which(is.na(q3_dat$condition_hotel_h) == FALSE)])
q3_dat$customer_svc_h[which(is.na(q3_dat$customer_svc_h))] <- mean(q3_dat$customer_svc_h[which(is.na(q3_dat$customer_svc_h) == FALSE)])
q3_dat$staff_cared_h[which(is.na(q3_dat$staff_cared_h))] <- mean(q3_dat$staff_cared_h[which(is.na(q3_dat$staff_cared_h) == FALSE)])
q3_dat$internet_sat_h[which(is.na(q3_dat$internet_sat_h))] <- mean(q3_dat$internet_sat_h[which(is.na(q3_dat$internet_sat_h) == FALSE)])
q3_dat$check_in_h[which(is.na(q3_dat$check_in_h))] <- mean(q3_dat$check_in_h[which(is.na(q3_dat$check_in_h) == FALSE)])
q3_dat$nt_rate_r[which(is.na(q3_dat$nt_rate_r))] <- mean(q3_dat$nt_rate_r[which(is.na(q3_dat$nt_rate_r) == FALSE)])

# ---------- visualize data -----------

q3_dat_sample <- q3_dat[1:1000000,]

ggplot(q3_dat_sample, aes(x = `f&b_overall_experience_h`, y = likelihood_recommend_h)) + 
  geom_count()

ggplot(q3_dat_sample, aes(x = guest_room_h, y = likelihood_recommend_h)) + 
  geom_count()

ggplot(q3_dat_sample, aes(x = tranquility_h, y = likelihood_recommend_h)) + 
  geom_count()

ggplot(q3_dat_sample, aes(x = condition_hotel_h, y = likelihood_recommend_h)) + 
  geom_count()

ggplot(q3_dat_sample, aes(x = customer_svc_h, y = likelihood_recommend_h)) + 
  geom_count()

ggplot(q3_dat_sample, aes(x = internet_sat_h, y = likelihood_recommend_h)) + 
  geom_count()

ggplot(q3_dat_sample, aes(x = check_in_h, y = likelihood_recommend_h)) + 
  geom_count()

ggplot(q3_dat_sample %>% filter(nt_rate_r < 500), aes(x = nt_rate_r, y = likelihood_recommend_h)) + 
  geom_count()

q3_model <- lm(formula = likelihood_recommend_h ~ ., data = q3_dat)
summary(q3_model)

step(q3_model, data=q3_dat, direction="backward")

best_model <- lm(formula = likelihood_recommend_h ~ `f&b_overall_experience_h` + guest_room_h + 
                   tranquility_h + condition_hotel_h + customer_svc_h + staff_cared_h + 
                   internet_sat_h + nt_rate_r, data = q3_dat)
summary(best_model)
###################################
############ BUSINESS QUESTION 4
###################################
#build a column to count for surveys that have a likelihood to recommend even if not completed
myData$has_likelihood <- ifelse(myData$likelihood_recommend_h >= 1, 1, 0)
# Summary of specific columns having to do with regional information
mySummary <- myData %>%
  group_by(hotel_name_long_pl,brand_pl,us_region_pl, state_pl, city_pl, type_pl,location_pl, property_longitude_pl, property_latitude_pl) %>%
  summarise(   guest_count=length(cons_guest_id_c)
               , completedcount=sum(ifelse(m_survey_status=="COMPLETED" , 1, 0))
               , sumLikelihood=sum(ifelse(likelihood_recommend_h[which(likelihood_recommend_h >= 9)],1,0))
               , sumHasLikelihood = sum(has_likelihood, na.rm = TRUE)
               , promoterRatio=sumLikelihood/(sumHasLikelihood)
               , meanLikelihood=mean(likelihood_recommend_h,na.rm=TRUE)
               , totalGoodAmenities = rowSums(myHotelsNew[,26:54] == "Y")
  )
#-- PLOT INFORMATION FOR BEST HOTELS BY REGION PER TYPE --#
#subset of data that includes information for the best hotel per region
highestInRegion <- mySummary %>%
  group_by(us_region_pl, type_pl) %>%
  summarise(promoterRatio = max(promoterRatio))
#graph brand by type
brandByType <- myData %>%
  group_by(type_pl, brand_pl) %>%
  summarise(average_score = mean(likelihood_recommend_h, na.rm = TRUE))
brandByType.graph <- ggplot(brandByType, aes(x = brand_pl, y = average_score))
brandByType.graph <- brandByType.graph + geom_point(data = brandByType, aes(color = type_pl, size = 10))
brandByType.graph <- brandByType.graph + ggtitle("Brand By Type of Hotel")
brandByType.graph
#put all information from mySummary into highestInRegion to retain data for the subset of 24 hotels
highestInRegion <- merge(mySummary, highestInRegion, by = "promoterRatio")
highestInRegion$state_pl <- tolower(highestInRegion$state_pl)
#setting up information to plot to a map
us <- map_data("state")
#merge all data from US and highestInRegion
newData <- merge(us, highestInRegion, by.x = 'region', by.y='state_pl')
newData <- newData[,-21:-22]
#make a map that plots best hotels by type, location, and region without brand names
bestByRegion <- ggplot(us, aes(map_id=region))
bestByRegion <- bestByRegion + xlim(-125, -65)
bestByRegion <- bestByRegion + geom_map(map = us, fill = "white", color = "black")
bestByRegion <- bestByRegion + expand_limits(x=us$long, y = us$lat)
bestByRegion <- bestByRegion + coord_map() + ggtitle("Best by Region")
bestByRegion <- bestByRegion + geom_point(data=newData
                                          , aes(x = property_longitude_pl, y = property_latitude_pl, size=promoterRatio
                                                ,colour=type_pl.x
                                                ,shape = location_pl))
bestByRegion
#make a map that plots best hotels by type, location, and region with brand names
bestByRegion <- ggplot(us, aes(map_id=region))
bestByRegion <- bestByRegion + xlim(-125, -65)
bestByRegion <- bestByRegion + geom_map(map = us, fill = "white", color = "black")
bestByRegion <- bestByRegion + expand_limits(x=us$long, y = us$lat)
bestByRegion <- bestByRegion + coord_map() + ggtitle("Best by Region")
bestByRegion <- bestByRegion + geom_point(data=newData
                                          , aes(x = property_longitude_pl, y = property_latitude_pl, size=promoterRatio
                                                ,colour=type_pl.x
                                                ,shape = location_pl))
bestByRegion <- bestByRegion + geom_text(data = newData, aes(x = property_longitude_pl, y = property_latitude_pl, label = brand_pl))
bestByRegion
##FIND AMOUNT OF GOOD AMENITIES PER TOP HOTEL PER REGION
#extract good amenities for every hotel
myHotels <- unique(subset(myData, select=c(41:94)))
myHotels <- merge(myHotels,mySummary,by="hotel_name_long_pl")
#remove insignificant amenities
myHotels <- myHotels%>%
  select(-business_center_pl, -conference_pl, -elevators_pl, -pool_indoor_pl, -spa_pl, -`spa_f&b_offering_pl`)
#identify which columns represent amenities
colnames(myHotels[26:48])
#get a sum for good amenities per hotel
myHotels$totalAmenities <- rowSums(myHotels[,26:48] == "Y")
#fill in the amenity information on the newData data frame
newData <- merge(myHotels, newData, by = "hotel_name_long_pl")
#get amount of amenities per top hotel
amenityTable <- newData %>%
  group_by(type_pl.y, brand_pl.y, us_region_pl.y) %>%
  summarise(total_amenities = mean(totalAmenities))
#graph amenities per region per type of hotel
businessGraph <- ggplot(amenityTable, aes(x = type_pl.y, y = total_amenities))
businessGraph  <- businessGraph  + geom_point(data = amenityTable, aes(color = us_region_pl.y, size = 10))
businessGraph  <- businessGraph  + ggtitle("Amenities by Region")
businessGraph <- businessGraph + scale_y_continuous(breaks = round(seq(min(0), max(20), by = 1)))
businessGraph


