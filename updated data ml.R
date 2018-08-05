# modified data with combined groups and apply regression
# Preparing Data
library("tidyverse")
library(lubridate)

#read all file
V_Fact_VoyPNL_File <- "F:\\Git\\capstone project\\dataSets\\V_Fact_VoyPNL.csv"
test_File <- "F:\\Git\\capstone project\\dataSets\\test.csv"
V_Dim_CargoAndGrade_File <- "F:\\Git\\capstone project\\dataSets\\V_Dim_CargoAndGrade.csv"
V_Dim_Vessel_File <- "F:\\Git\\capstone project\\dataSets\\V_Dim_Vessel.csv"
V_Dim_Voyage_File <- "F:\\Git\\capstone project\\dataSets\\V_Dim_Voyage.csv"

V_Fact_VoyPNL <- read.csv(V_Fact_VoyPNL_File, na.strings=c("","NA"), stringsAsFactors = FALSE)
test <- read.csv(test_File, na.strings=c("","NA"), stringsAsFactors = FALSE)
V_Dim_CargoAndGrade <- read.csv(V_Dim_CargoAndGrade_File, na.strings=c("","NA"), stringsAsFactors = FALSE)
V_Dim_Vessel <- read.csv(V_Dim_Vessel_File, na.strings=c("","NA"), stringsAsFactors = FALSE)
V_Dim_Voyage <- read.csv(V_Dim_Voyage_File, na.strings=c("","NA"), stringsAsFactors = FALSE)

# removing records with Fkey_Dim_Voyage_Id = -1
V_Fact_VoyPNL <- filter(V_Fact_VoyPNL, V_Fact_VoyPNL$Fkey_Dim_Voyage_Id != "-1")
V_Dim_Vessel <- filter(V_Dim_Vessel, V_Dim_Vessel$Dim_Vessel_Id != "-1")

# get the voyage completion date into proper format.
V_Fact_VoyPNL$Month <- lubridate::month(ymd(V_Fact_VoyPNL$Fkey_DimTime_CompletedGMT),label=TRUE)


# considering data with only data_type = "A" and BatchId = 96
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

# add dwt column from V_Dim_Vessel table to v_mod

#test1 <- left_join(v_mod,v_test, by = "Dim_Vessel_Id")

#test1 <- v_mod %>% select(V_Dim_Vessel,Dim_Vessel_Id,dwt) %>% left_join(by="Dim_Vessel_Id")

#test2<-v_mod %>% left_join(V_Dim_Vessel %>% select(Dim_Vessel_Id,dwt),by="Dim_Vessel_Id" )

#test3 <- V_Dim_Vessel %>% select(Dim_Vessel_Id,dwt) %>% right_join(v_mod,by="Dim_Vessel_Id")

v_mod <- v_mod %>% left_join(V_Dim_Vessel[,c("Dim_Vessel_Id","dwt")], by=c("Fkey_Dim_Vessel_Id"="Dim_Vessel_Id"))

#v_mod %>% left_join(v_test, by="Dim_Vessel_Id")

#head(test1)
#head(test1[,c("Dim_Vessel_Id","Cargo","dwt")])
#colnames(v_mod)
#head(v_mod$Fkey_Dim_Vessel_Id)
#head(v_mod[,c("Dim_Vessel_Id","Cargo")])

# add est earnings and est voyage days columns from V_Dim_Voyage to v_mod
v_mod <- left_join(v_mod,V_Dim_Voyage[,c("Dim_Voyage_Id","TCEquiv_Estimate_LatestDaySnapshot","TotalVoyageDays_Estimate_LatestDaySnapshot")],by = c("Fkey_Dim_Voyage_Id" = "Dim_Voyage_Id"))

#calculate daily earnings for each voyage

v_mod <- filter(v_mod, (v_mod$TCEquiv_Estimate_LatestDaySnapshot != 0) || (v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot != 0))
v_mod <- filter(v_mod,  v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot >= 1)
v_mod$dailyrate_Est <- v_mod$TCEquiv_Estimate_LatestDaySnapshot / v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot

v_mod$diff_TCEquiv <- v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot - v_mod$tcEquv_Act

#Regression for the complete data

write_csv(v_mod, "updated_data.csv")

reg <- lm(v_mod$tcEquv_Act~v_mod$vsl_type + v_mod$ttlVoyDays_Act + v_mod$Cargo + v_mod$Cargo_Lift_Act + v_mod$Month + v_mod$lastDiscPort + v_mod$dwt)

summary(reg) 

#coe_val <- summary(reg)$coefficients[,4]

#coe_val[coe_val < 0.001]

#plot the regression results
plot(reg)

#plot the prediction
plot(predict(reg),v_mod$tcEquv_Act)

#----------------------------------
# Regression for each vessel type

#---------- MIDRANGE -----------------
v_mr <- v_mod[v_mod$vsl_type=="MIDRANGE",]

summary(v_mr$ttlVoyDays_Act)

#remove outliers
v_mr1<-filter(v_mr,v_mr$ttlVoyDays_Act<=150)

ggplot(v_mr1,aes(x=ttlVoyDays_Act,y=tcEquv_Act)) +
  geom_point() +
  ggtitle(paste("MIDRANGE"))

reg_mr <- lm(v_mr1$tcEquv_Act~v_mr1$ttlVoyDays_Act + v_mr1$cargoShort+ v_mr1$tradeArea + v_mr1$Cargo_Lift_Act+ v_mr1$month + v_mr1$lastDiscPort2)

summary(reg_mr) # 0.1149 without lastDiscPort, 0.3722 with all discharge ports, 0.1367 with top few relative discharge ports

plot(reg_mr)

plot(predict(reg_mr),v_mr1$tcEquv_Act)

#head(predict(reg_mr))
#----------------------------------------
#---------- HANDYMAX -----------------
v_hm <- v_mod[v_mod$vsl_type=="HANDYMAX",]

summary(v_hm$ttlVoyDays_Act)

#remove outliers
v_hm1<-filter(v_hm,v_hm$ttlVoyDays_Act<=100)

ggplot(v_hm1,aes(x=ttlVoyDays_Act,y=tcEquv_Act)) +
  geom_point() +
  ggtitle(paste("HANDYMAX"))

reg_hm <- lm(v_hm1$tcEquv_Act~v_hm1$ttlVoyDays_Act + v_hm1$cargoShort+ v_hm1$tradeArea + v_hm1$Cargo_Lift_Act+ v_hm1$month + v_hm1$lastDiscPort2)

summary(reg_hm) # 0.198 without lastDiscPort, 0.3629 with all discharge ports, 0.2083 with top few relative discharge ports

plot(reg_hm)

plot(predict(reg_hm),v_hm1$tcEquv_Act)

#head(predict(reg_hm))
#------------------------------------------
#---------- AFRAMAX -----------------
v_am <- v_mod[v_mod$vsl_type=="AFRAMAX",]

summary(v_am$ttlVoyDays_Act)

#remove outliers
v_am1<-filter(v_am,v_am$ttlVoyDays_Act<=100)

ggplot(v_am1,aes(x=ttlVoyDays_Act,y=tcEquv_Act)) +
  geom_point() +
  ggtitle(paste("AFRAMAX"))

reg_am <- lm(v_am1$tcEquv_Act~v_am1$ttlVoyDays_Act + v_am1$cargoShort+ v_am1$tradeArea + v_am1$Cargo_Lift_Act+ v_am1$month + v_am1$lastDiscPort2)

summary(reg_am) # 0.2211 without lastDiscPort, 0.4425 with all discharge ports, none of these are present in this vsl_type with top few relative discharge ports

plot(reg_am)

plot(predict(reg_am),v_am1$tcEquv_Act)

#head(predict(reg_am))
#--------------------------------------
#---------- PANAMAX -----------------
v_pm <- v_mod[v_mod$vsl_type=="PANAMAX",]

summary(v_pm$ttlVoyDays_Act)

#remove outliers
v_pm1<-filter(v_pm,v_pm$ttlVoyDays_Act<=150)

ggplot(v_pm1,aes(x=ttlVoyDays_Act,y=tcEquv_Act)) +
  geom_point() +
  ggtitle(paste("PANAMAX"))

reg_pm <- lm(v_pm1$tcEquv_Act~v_pm1$ttlVoyDays_Act + v_pm1$cargoShort+ v_pm1$tradeArea + v_pm1$Cargo_Lift_Act+ v_pm1$month + v_pm1$lastDiscPort2)

summary(reg_pm) # 0.165 without lastDiscPort, 0.4093 with all discharge ports, 0.1696 with top few relative discharge ports

plot(reg_pm)

plot(predict(reg_pm),v_pm1$tcEquv_Act)

#---------------------------------------
# find the correlation 
cor(v_mod$tcEquv_Act, v_mod$TotalVoyageDays_Estimate_LatestDaySnapshot)
cor(v_mod$tcEquv_Act, v_mod$Cargo_Lift_Act)