# modified data with combined groups and apply regression

library("tidyverse")

#read all file
voypnlfile <- "F:\\Git\\capstone project\\dataSets\\V_Fact_VoyPNL.csv"

voypnl.data <- read.csv(voypnlfile, na.strings=c("","NA"), stringsAsFactors = FALSE)

# removing records with Fkey_Dim_Voyage_Id = -1
voypnl.data.clear <- filter(voypnl.data, voypnl.data$Fkey_Dim_Voyage_Id != "-1")

# get the voyage completion date into proper format.
voypnl.data.clear$dateformat <- as.Date(as.character(voypnl.data.clear$Fkey_DimTime_CompletedGMT),format="%Y%m%d")

#add new columns as day, month and year
voypnl.data.clear <- separate(data=voypnl.data.clear, 
                              col='dateformat', 
                              into=c('year', 'month','day'), 
                              sep = "-",
                              remove=TRUE)

# considering data with only data_type = "A" and BatchId = 96
v_mod<- voypnl.data.clear[voypnl.data.clear$data_type=='A' & voypnl.data.clear$BatchId=="96",]

#combine similar vessel types into one category.

v_mod$vsl_type <- sub(pattern = "^HAN.*", replacement = "HANDYMAX", x = v_mod$vsl_type)
v_mod$vsl_type <- sub(pattern = "^MID.*", replacement = "MIDRANGE", x = v_mod$vsl_type)
v_mod$vsl_type <- sub(pattern = "^PAN.*", replacement = "PANAMAX", x = v_mod$vsl_type)
v_mod$vsl_type <- sub(pattern = "^AFR.*", replacement = "AFRAMAX", x = v_mod$vsl_type)
v_mod$vsl_type <- sub(pattern = "^LR1.*", replacement = "LR1", x = v_mod$vsl_type)

# replace blank from last discharge port with navalue
sum(is.na(v_mod$lastDiscPort)) #285 values
v_mod$lastDiscPort[(is.na(v_mod$lastDiscPort))] <- "navalue"

# replace blank from trade area with navalue
sum(is.na(v_mod$tradeArea)) #905 values
v_mod$tradeArea[(is.na(v_mod$tradeArea))] <- "navalue"

# replace blank from cargo short with navalue
sum(is.na(v_mod$cargoShort)) #1239 values
v_mod$cargoShort[(is.na(v_mod$cargoShort))] <- "navalue"

# use top 20 trade areas and replace all other by othertradeArea
v_mod$tradeArea <- ifelse (v_mod$tradeArea %in% c("Transatlantic","Baltic Sea/ARA","Cross Med","navalue","FAR EAST","Black Sea/Med","Baltic Sea/Cont","ARA/WAF","USG/ECMEX","USG/Caribs","Baltic Sea/UK Cont","USA - TA","AG/FAR EAST","AG","FAR EAST/AUSTRALIA","USG/SOUTH AMERICA","Far East","WC America","Cross Caribs","Cont/ARA"), v_mod$tradeArea, v_mod$tradeArea <- "othertradeArea")


# use top 15 cargo short and replace all other with othercargoshort.
v_mod$cargoShort <- ifelse (v_mod$cargoShort %in% c("FO","CPP","ULSD","GASOLINE","NAPHTHA","navalue","GAS OIL","VGO","DPP","JET","CRUDE","MOGAS","UNLEADED GASOLINE","ULSD 10 PPM","JET A1"), v_mod$cargoShort, v_mod$cargoShort <- "othercargoshort")

# use top few last discharge ports and replace others with "other"
v_mod$lastDiscPort2 <- ifelse (v_mod$lastDiscPort %in% c("GLADSTONE","GUAYMAS","MUGARDOS","PUERTO SANDINO","SUNDSVALL"), v_mod$lastDiscPort, "others")

# to add month name to the month column

v_mod$month <- sub(pattern = "01", replacement = "January", x = v_mod$month)
v_mod$month <- sub(pattern = "02", replacement = "February", x = v_mod$month)
v_mod$month <- sub(pattern = "03", replacement = "March", x = v_mod$month)
v_mod$month <- sub(pattern = "04", replacement = "April", x = v_mod$month)
v_mod$month <- sub(pattern = "05", replacement = "May", x = v_mod$month)
v_mod$month <- sub(pattern = "06", replacement = "June", x = v_mod$month)
v_mod$month <- sub(pattern = "07", replacement = "July", x = v_mod$month)
v_mod$month <- sub(pattern = "08", replacement = "August", x = v_mod$month)
v_mod$month <- sub(pattern = "09", replacement = "September", x = v_mod$month)
v_mod$month <- sub(pattern = "10", replacement = "October", x = v_mod$month)
v_mod$month <- sub(pattern = "11", replacement = "November", x = v_mod$month)
v_mod$month <- sub(pattern = "12", replacement = "December", x = v_mod$month)

#calculate daily earnings for each voyage
v_mod <- filter(v_mod, (v_mod$tcEquv_Act != 0) || (v_mod$ttlVoyDays_Act != 0))
v_mod <- filter(v_mod,  v_mod$ttlVoyDays_Act >= 1)
v_mod$dailyrate <- v_mod$tcEquv_Act / v_mod$ttlVoyDays_Act

#Regression for the complete data

reg <- lm(v_mod$tcEquv_Act~v_mod$vsl_type + v_mod$ttlVoyDays_Act + v_mod$cargoShort + v_mod$tradeArea + v_mod$Cargo_Lift_Act + v_mod$month) # + v_mod$lastDiscPort)

summary(reg) # 0.1482 without lastDiscPort, 0.2779 with all lastDiscport, 0.1587 with selection of first few lastDiscport 

coe_val <- summary(reg)$coefficients[,4]

coe_val[coe_val < 0.001]

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
