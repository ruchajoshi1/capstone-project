# Final R code

# Capstone Project - Maritime Data Intelligence System R code

## Data Wrangling

# Load all the packages used in the analysis
library("tidyverse")
library("lubridate")
library("caTools")

# Read the files
V_Fact_VoyPNL_File <- "F:\\Git\\capstone project\\dataSets\\V_Fact_VoyPNL.csv"
test_File <- "F:\\Git\\capstone project\\dataSets\\test.csv"
V_Dim_CargoAndGrade_File <- "F:\\Git\\capstone project\\dataSets\\V_Dim_CargoAndGrade.csv"
V_Dim_Vessel_File <- "F:\\Git\\capstone project\\dataSets\\V_Dim_Vessel.csv"
V_Dim_Voyage_File <- "F:\\Git\\capstone project\\dataSets\\V_Dim_Voyage.csv"

# Store the data into dataframe and check for blanks and NA
V_Fact_VoyPNL <- read.csv(V_Fact_VoyPNL_File, na.strings=c("","NA"), stringsAsFactors = FALSE)
test <- read.csv(test_File, na.strings=c("","NA"), stringsAsFactors = FALSE)
V_Dim_CargoAndGrade <- read.csv(V_Dim_CargoAndGrade_File, na.strings=c("","NA"), stringsAsFactors = FALSE)
V_Dim_Vessel <- read.csv(V_Dim_Vessel_File, na.strings=c("","NA"), stringsAsFactors = FALSE)
V_Dim_Voyage <- read.csv(V_Dim_Voyage_File, na.strings=c("","NA"), stringsAsFactors = FALSE)

#Look at the structure of the dataset
glimpse(V_Fact_VoyPNL)
glimpse(test)
glimpse(V_Dim_CargoAndGrade)
glimpse(V_Dim_Vessel)
glimpse(V_Dim_Voyage)

# removing records with Fkey_Dim_Voyage_Id = -1
V_Fact_VoyPNL <- filter(V_Fact_VoyPNL, V_Fact_VoyPNL$Fkey_Dim_Voyage_Id != "-1")
V_Dim_Vessel <- filter(V_Dim_Vessel, V_Dim_Vessel$Dim_Vessel_Id != "-1")

# get the voyage completion date into proper format.
V_Fact_VoyPNL$Month <- lubridate::month(ymd(V_Fact_VoyPNL$Fkey_DimTime_CompletedGMT),label=TRUE)
V_Fact_VoyPNL$Year <- lubridate::year(ymd(V_Fact_VoyPNL$Fkey_DimTime_CompletedGMT))

# considering data with only data_type = "A" and BatchId 
v_mod<- V_Fact_VoyPNL[V_Fact_VoyPNL$data_type=='A' & V_Fact_VoyPNL$BatchId==max(V_Fact_VoyPNL$BatchId),]

#combine similar vessel types into one category.
v_mod$vsl_type <- sub(pattern = "^HAN.*", replacement = "HANDYMAX", x = v_mod$vsl_type)
v_mod$vsl_type <- sub(pattern = "^MID.*", replacement = "MIDRANGE", x = v_mod$vsl_type)
v_mod$vsl_type <- sub(pattern = "^PAN.*", replacement = "PANAMAX", x = v_mod$vsl_type)
v_mod$vsl_type <- sub(pattern = "^AFR.*", replacement = "AFRAMAX", x = v_mod$vsl_type)
v_mod$vsl_type <- sub(pattern = "^LR1.*", replacement = "LR1", x = v_mod$vsl_type)

#Restrict the number of ports by matching them with areas
v_mod$lastDiscPort <- test$Region[match(v_mod$lastDiscPort,test$PortName)]
v_mod$firstLoadPort <- test$Region[match(v_mod$firstLoadPort,test$PortName)]

#Restrict the cargo types by matching them with Cargo text from the cargo table
v_mod$Cargo <- V_Dim_CargoAndGrade$CargoGrade_txt[match(v_mod$cargoShort,V_Dim_CargoAndGrade$Cargo_ShortName)]

# Join dwt column from V_Dim_Vessel table to the dataset
v_mod <- v_mod %>% left_join(V_Dim_Vessel[,c("Dim_Vessel_Id","dwt")], by=c("Fkey_Dim_Vessel_Id"="Dim_Vessel_Id"))

# Join estimate earnings and estimate voyage days columns from V_Dim_Voyage to the datset
v_mod <- left_join(v_mod,V_Dim_Voyage[,c("Dim_Voyage_Id","TCEquiv_Estimate_LatestDaySnapshot","TotalVoyageDays_Estimate_LatestDaySnapshot")],by = c("Fkey_Dim_Voyage_Id" = "Dim_Voyage_Id"))

# Calculate daily earnings for each voyage
v_mod <- filter(v_mod, (v_mod$TCEquiv_Estimate_LatestDaySnapshot != 0) || (v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot != 0))
v_mod <- filter(v_mod,  v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot >= 1)
v_mod$dailyrate_Est <- v_mod$TCEquiv_Estimate_LatestDaySnapshot / v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot

# Calculate the difference between estimated earnings and actual earnings
v_mod$diff_TCEquiv <- v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot - v_mod$tcEquv_Act

#Preliminary Exploration

#----------------------------
# calculate average earnings for each year for each vessel type
avgv2 <- aggregate(v_mod[,c("tcEquv_Act")],list(Year=v_mod$Year,vsl_type=v_mod$vsl_type), mean)
head(avgv2)

# plot average of each year for each type of vessel
avgv2yr <- avgv2[avgv2$Year <= 2019 & avgv2$Year >= 2010,]

ggplot(avgv2yr, aes(x=vsl_type,y=x)) +
  geom_bar(aes(fill = factor(vsl_type)),stat="identity")+
  facet_wrap(~Year, ncol = 2, nrow = 5) +
  theme(axis.text.x = element_text(angle=45,hjust = 1), plot.title = element_text(hjust = 0.5)) +     labs(title = "Average earnings vs Vessel type", x = "Vessel type", y = "Average earnings(USD)") +
  guides(fill=guide_legend(title = "Vessel Type"))   

#----------------------------
#calculate average earnings for each vessel type for all years
avgv3 <- aggregate(v_mod[,c("tcEquv_Act")],list(vsl_type=v_mod$vsl_type), mean)

# plot average for each vessel type for all years
ggplot(avgv3, aes(x=vsl_type,y=x)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=45,hjust = 1), plot.title = element_text(hjust = 0.5)) +     labs(title = "Average earnings vs Vessel type", x = "Vessel type", y = "Average earnings(USD)") 

#----------------------------
# sum of earnings for each year for each vessel type
sumv2 <- aggregate(v_mod[,c("tcEquv_Act")],list(Year=v_mod$Year,vsl_type=v_mod$vsl_type), sum)

# plot average of each year for each type of vessel
sumv2yr <- sumv2[sumv2$Year <= 2019 & sumv2$Year >= 2010,]

ggplot(sumv2yr, aes(x=vsl_type,y=x)) +
  geom_bar(aes(fill = factor(vsl_type)),stat="identity")+
  facet_wrap(~Year, ncol = 2, nrow = 5) +
  theme(axis.text.x = element_text(angle=45,hjust = 1), plot.title = element_text(hjust = 0.5)) +     labs(title = "Sum of earnings vs Vessel type", x = "Vessel type", y = "Sum of earnings(USD)") +
  guides(fill=guide_legend(title = "Vessel Type"))   

#----------------------------
# Calulate average for each trade area
avgv4 <- aggregate(v_mod[,c("tcEquv_Act")],list(trade_area=v_mod$tradeArea), mean)
avgv4 <- avgv4[order(-avgv4$x),]

avgv5 <- head(avgv4, n=50)

#plot average for top 50 trade area
ggplot(avgv5, aes(x=reorder(trade_area,-x),y=x)) +
  geom_bar(stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle=45,hjust = 1), plot.title = element_text(hjust = 0.5)) +     labs(title = "Average earnings vs Trade Area", x = "Trade Area", y = "Average earnings(USD)") 

#----------------------------
#sum of earnings for top 50 trade area
sumv4 <- aggregate(v_mod[,c("tcEquv_Act")],list(trade_area=v_mod$tradeArea), sum)
sumv4 <- sumv4[order(-sumv4$x),]

sumv5 <- head(sumv4, n=50)

#plot sum for top 50 trade area
ggplot(sumv5, aes(x=reorder(trade_area,-x),y=x)) +
  geom_bar(stat="identity",position = "dodge") +
  theme(axis.text.x = element_text(angle=45,hjust = 1), plot.title = element_text(hjust = 0.5)) +     labs(title = "Sum of earnings vs Trade Area", x = "Trade Area", y = "Sum of earnings(USD)") 

#----------------------------
#Sum of earnings for each Cargo type
sumv6 <- aggregate(v_mod[,c("tcEquv_Act")],list(cargo_type=v_mod$Cargo), sum)
sumv6 <- sumv6[order(-sumv6$x),]

#plot sum of earnings for each cargo type
ggplot(sumv6, aes(x=reorder(cargo_type,-x),y=x)) +
  geom_bar(stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle=45,hjust = 1), plot.title = element_text(hjust = 0.5)) +     labs(title = "Sum of earnings vs Cargo type", x = "Cargo type", y = "Sum of earnings(USD)") 

#----------------------------
#Calculate average monthly earnings for each month
avgv6 <- aggregate(v_mod[,c("tcEquv_Act")],list(Month=v_mod$Month), mean)
#avgv6 <- avgv6[order(-avgv4$x),]

#plot average monthly earnings for each month
ggplot(avgv6, aes(x=sort(Month),y=x)) +
  geom_bar(stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle=45,hjust = 1), plot.title = element_text(hjust = 0.5)) +     labs(title = "Average earnings vs Month", x = "Month", y = "Average earnings(USD)") 

# Machine Learning

# find the correlation 
#cor(v_mod$tcEquv_Act, v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot)
#cor(v_mod$tcEquv_Act, v_mod$Cargo_Lift_Act)

## Linear Regression 

#Split the data into training dataset and testing dataset with a 70/30 ratio.
# set the seed to make the partition reproduciable.
set.seed(40)
split = sample.split(v_mod$Fact_VoyPNL_Id, SplitRatio = 0.70)
v_mod_train = subset(v_mod, split == TRUE) 
v_mod_test = subset(v_mod, split == FALSE)

#Check the rows for training and testing the datasets
nrow(v_mod_train)
nrow(v_mod_test)

## Model 1

reg <- lm(v_mod_train$tcEquv_Act~v_mod_train$vsl_type + v_mod_train$TotalVoyageDays_Estimate_LatestDaySnapshot + v_mod_train$Cargo + v_mod_train$Cargo_Lift_Act + v_mod_train$Month + v_mod_train$lastDiscPort + v_mod_train$dwt)

summary(reg) 

plot(reg)

## Predict the data

earningspred <- predict(reg,v_mod_test)

actual_pred <- data.frame(cbind(actuals=v_mod_test, predicted=earningspred))

plot(predict(reg),v_mod_test$tcEquv_Act)
