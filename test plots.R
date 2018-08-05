# Read and plot

library("tidyverse")

#read all file

voyagefile <- "F:\\Git\\capstone project\\dataSets\\V_Dim_Voyage.csv"
voypnlfile <- "F:\\Git\\capstone project\\dataSets\\V_Fact_VoyPNL.csv"
portfile <- "F:\\Git\\capstone project\\dataSets\\V_Dim_Ports.csv"
vesselfile <- "F:\\Git\\capstone project\\dataSets\\V_Dim_Vessel.csv"

voyage.data <- read.csv(voyagefile, na.strings=c("","NA"), stringsAsFactors = FALSE)
#str(voyage.data)
#summary(voyage.data)

voypnl.data <- read.csv(voypnlfile, na.strings=c("","NA"), stringsAsFactors = FALSE)
#str(voypnl.data)
#summary(voypnl.data)

vessel.data <- read.csv(vesselfile, na.strings=c("","NA"), stringsAsFactors = FALSE)
#str(voypnl.data)
#summary(voypnl.data)

ports.data <- read.csv(portfile, na.strings=c("","NA"), stringsAsFactors = FALSE)
#glimpse(ports.data)
#summary(ports.data)

#colnames(vessel.data)
#colnames(voyage.data)
#colnames(voypnl.data)
#colnames(ports.data)

#"Fact_VoyPNL_Id" %in% colnames(vessel.data)
#"Fact_VoyPNL_Id" %in% colnames(voyage.data)
#"Fact_VoyPNL_Id" %in% colnames(voypnl.data)

#a <- colnames(voypnl.data)
#head(voypnl.data[,"Fact_VoyPNL_Id"])
#voypnl.data[(BatchId==96)&(data_type=="A")&(vsl_type=="AFRAMAX")&,]

#"Remarks" %in% colnames(vessel.data)

# removing records with Fkey_Dim_Voyage_Id = -1
#voypnl.data.clear <- voypnl.data[voypnl.data$Fkey_Dim_Voyage_Id != "-1",]
voypnl.data.clear <- filter(voypnl.data, voypnl.data$Fkey_Dim_Voyage_Id != "-1")
vessel.data.clear <- filter(vessel.data, vessel.data$Dim_Vessel_Id != "-1")
voyage.data.clear <- filter(voyage.data, voyage.data$Dim_Voyage_Id != "-1")
ports.data.clear <- filter(ports.data, ports.data$Dim_Ports_Id != "-1")

#glimpse(voypnl.data.clear)

  #filter(voypnl.data.clear, voypnl.data.clear$vsl_type == "HANDYMAX" & voypnl.data.clear$data_type == "A" & voypnl.data.clear$BatchId == "96")

#voypnl.data.clear.vsltype1 <- voypnl.data.clear[voypnl.data.clear$vsl_type == "HANDYMAX" & voypnl.data.clear$data_type == "A" & voypnl.data.clear$BatchId == "96",]

#ggplot(voypnl.data.clear, aes(x=Fkey_DimTime_CompletedGMT, y=tcEquv_Act, col=vsl_type)) +
#  geom_point()

#ggplot(voypnl.data.clear.vsltype1, aes(x=vsl_type, y=tcEquv_Act, col=vsl_type)) +
#  geom_point()

voypnl.data.clear$dateformat <- as.Date(as.character(voypnl.data.clear$Fkey_DimTime_CompletedGMT),format="%Y%m%d")

voypnl.data.clear <- separate(data=voypnl.data.clear, 
                         col='dateformat', 
                         into=c('year', 'month','day'), 
                         sep = "-",
                         remove=TRUE)

#voypnl.data.clear.vsltype1 <- voypnl.data.clear.vsltype1[order(voypnl.data.clear.vsltype1$dateformat),]

#ggplot(voypnl.data.clear, aes(x=year, y=tcEquv_Act)) +
#  geom_line()

v2<- voypnl.data.clear[voypnl.data.clear$data_type=='A' & voypnl.data.clear$BatchId=="96",]
#v2$year <- v2$Fkey_DimTime_CompletedGMT %/% 10000

head(v2$year)
head(v2$month)

#avgpnl <- v2 %>% group_by(vsl_type,year)

#%>% mean(tcEquv_Act, na.rm = TRUE)

#ggplot(avgpnl, aes(x=vsl_type, y = , col=year)) +
#  geom_bar()

# average of each year for each type of vessel
avgv2 <- aggregate(v2[,c("tcEquv_Act")],list(year=v2$year,vsl_type=v2$vsl_type), mean)
head(avgv2)

# plot average of each year for each type of vessel
ggplot(avgv2, aes(x=vsl_type,y=x)) +
  geom_bar(stat="identity")+
  facet_grid(.~year) +
  theme(axis.text.x = element_text(angle=45,hjust = 1))  

avgv2yr <- avgv2[avgv2$year <= 2019 & avgv2$year >= 2010,]

unique(avgv2yr$year)
ggplot(avgv2yr, aes(x=vsl_type,y=x)) +
  geom_bar(aes(fill = factor(vsl_type)),stat="identity")+
  facet_wrap(~year, ncol = 2, nrow = 5) +
  theme(axis.text.x = element_text(angle=45,hjust = 1))  

ggplot(avgv2, aes(x=vsl_type,y=x)) +
  geom_bar(aes(fill = factor(year)), stat="identity",position = "dodge")+
  facet_grid(~year) +
  theme(axis.text.x = element_text(angle=45,hjust = 1))  

ggplot(avgv2, aes(x=vsl_type,y=x)) +
  geom_bar(aes(fill = factor(vsl_type)), stat="identity",position = "dodge")+
  facet_grid(.~year) +
  theme(axis.text.x = element_text(angle=45,hjust = 1))  


#average for each vessel type for all years
avgv3 <- aggregate(v2[,c("tcEquv_Act")],list(vsl_type=v2$vsl_type), mean)

# plot average for each vessel type for all years
ggplot(avgv3, aes(x=vsl_type,y=x)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))  

pdf(file = "plots.pdf", paper = "a4")

# sum for each year for each vessel type
sumv2 <- aggregate(v2[,c("tcEquv_Act")],list(year=v2$year,vsl_type=v2$vsl_type), sum)

sumv2

# plot sum for each year for each vessel type
ggplot(sumv2, aes(x=vsl_type,y=x, fill = vsl_type)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45,hjust = 1))  

dev.off()
#glimpse(avgv2)
#v4 <- filter(v2, v2$vsl_type == "AFRAMAX")

#v3 <- filter(avgv2, avgv2$vsl_type == "AFRAMAX")
#head(v3)

#ggplot(v3, aes(x=year,y=x, fill = x)) +
#  geom_bar(stat="identity")
 # geom_bar(stat="identity",position = "dodge")
  
#ggplot(v4, aes(x=month, y=tcEquv_Act, fill=tcEquv_Act)) +
#  geom_bar(stat = "identity")

ggplot(v2, aes(x=month, y=tcEquv_Act)) +
    geom_boxplot()

ggplot(avgv2, aes(x=vsl_type,y=x)) +
  geom_bar(aes(fill = factor(vsl_type)), stat="identity",position = "dodge")+
  facet_grid(. ~ year) +
  theme(axis.text.x = element_text(angle=45,hjust = 1))  

sumplot <- function(yr)
{
  ggplot(sumv2[sumv2$year==yr,], aes(x=vsl_type,y=x)) +
    geom_bar(aes(fill = factor(vsl_type)), stat="identity",position = "dodge")+
    theme(axis.text.x = element_text(angle=45,hjust = 1)) + ggtitle(paste("sum for the year=", yr, sep=""))  
}
pdf(file = "plots_sum2.pdf", paper = "USr")
lapply(c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),sumplot)
dev.off()

#sumplot(2010)

avgplot <- function(yr)
{
  ggplot(avgv2[avgv2$year==yr,], aes(x=vsl_type,y=x)) +
    geom_bar(aes(fill = factor(vsl_type)), stat="identity",position = "dodge")+
    theme(axis.text.x = element_text(angle=45,hjust = 1)) + ggtitle(paste("avg for the year=", yr, sep=""))  
}
pdf(file = "plots_avg.pdf", paper = "USr")
lapply(c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),avgplot)

#myplot(2012)
dev.off()

# average for each trade area
avgv4 <- aggregate(v2[,c("tcEquv_Act")],list(trade_area=v2$tradeArea), mean)
avgv4 <- avgv4[order(-avgv4$x),]

write_csv(avgv4, "tradearea_avg.csv")

avgv5 <- head(avgv4, n=50)

#plot average for each trade area
ggplot(avgv5, aes(x=reorder(trade_area,-x),y=x)) +
  geom_bar(stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

#sum of earnings for top 50 trade area
sumv4 <- aggregate(v2[,c("tcEquv_Act")],list(trade_area=v2$tradeArea), sum)
sumv4 <- sumv4[order(-sumv4$x),]

write_csv(sumv4, "tradearea_sum.csv")

sumv5 <- head(sumv4, n=50)

#plot sum for top 50 trade area
ggplot(sumv5, aes(x=reorder(trade_area,-x),y=x)) +
  geom_bar(stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

#-------------------------------------

#sum of earnings for each cargoshort
sumv6 <- aggregate(v2[,c("tcEquv_Act")],list(cargo_type=v2$cargoShort), sum)
sumv6 <- sumv6[order(-sumv6$x),]

write_csv(sumv6, "cargotype_month_sum.csv")

sumv7 <- head(sumv6, n=20)

#plot sum for top 50 cargo type
ggplot(sumv7, aes(x=reorder(cargo_type,-x),y=x)) +
  geom_bar(stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle=45,hjust = 1))

#--------------------------------------
#calculate daily earning for each voyage
v2 <- filter(v2, (v2$tcEquv_Act != 0) || (v2$ttlVoyDays_Act != 0))
v2 <- filter(v2,  v2$ttlVoyDays_Act >= 1)
v2$dailyrate <- v2$tcEquv_Act / v2$ttlVoyDays_Act
summary(v2$ttlVoyDays_Act)
avgv6 <- aggregate(v2[,c("dailyrate")],list(vsl_type=v2$vsl_type), mean)  

head(avgv6)
avgv6 <- avgv6[order(-avgv6$x)]

#-----------------------------------
  v3 <- voyage.data.clear  
countv3 <- aggregate(v3[,c("Dim_Voyage_Id")],list(month=v3$CompleteGmtMonthNo), count)

#---------------------------------------
#calculate linear regression  
 
l1 <- lm(v2$dailyrate ~ v2$Cargo_Lift_Act) 
  
summary(l1)

ggplot(v2[v2$ttlVoyDays_Act<=100,],aes(x=ttlVoyDays_Act,y=tcEquv_Act)) +geom_point()
length(v2$ttlVoyDays_Act[v2$ttlVoyDays_Act<=100])
#11911
unique(v2$vsl_type)
v22 <- v2[v2$vsl_type=="MID RANGE",]
 v23<-filter(v22,v22$ttlVoyDays_Act<=150)
 summary(lm(v23$tcEquv_Act~v23$ttlVoyDays_Act))
#--------------------------------------------------------- 
# replace blank from last discharge port with navalue
 sum(is.na(v_mod$lastDiscPort)) #285 values
 v_mod$lastDiscPort[(is.na(v_mod$lastDiscPort))] <- "navalue"
 avg_ldp <- aggregate(v_mod[,c("tcEquv_Act")],list(lastDiscPort=v_mod$lastDiscPort), sum)
 avg_ldp <- avg_ldp[order(-avg_ldp$x),]
 
 #plot top 50 last discharge port 
 ggplot(head(avg_ldp, n=50), aes(x=reorder(lastDiscPort,-x),y=x)) +
   geom_bar(stat="identity",position = "dodge")+
   theme(axis.text.x = element_text(angle=45,hjust = 1))
 
 #write_csv(avg_ldp,"lastDiscPort_sum.csv")
 
#---------------------------------------------------------
 # replace blank from trade area with navalue
 sum(is.na(v_mod$tradeArea)) #905 values
 v_mod$tradeArea[(is.na(v_mod$tradeArea))] <- "navalue"
 avg_ta <- aggregate(v_mod[,c("tcEquv_Act")],list(trade_area=v_mod$tradeArea), sum)
 avg_ta <- avg_ta[order(-avg_ta$x),]
 
 #plot top 50 trade area 
 ggplot(head(avg_ta, n=50), aes(x=reorder(trade_area,-x),y=x)) +
   geom_bar(stat="identity",position = "dodge")+
   theme(axis.text.x = element_text(angle=45,hjust = 1))
 
 #write_csv(avg_ta,"tradeArea_sum.csv")
#-------------------------------------------------
 # replace blank from cargo short with navalue
 sum(is.na(v_mod$cargoShort)) #1239 values
 v_mod$cargoShort[(is.na(v_mod$cargoShort))] <- "navalue"
 avg_cs <- aggregate(v_mod[,c("tcEquv_Act")],list(cargo_short=v_mod$cargoShort), sum)
 avg_cs <- avg_cs[order(-avg_cs$x),]
 
 #plot top 50 cargo types 
 ggplot(head(avg_cs, n=50), aes(x=reorder(cargo_short,-x),y=x)) +
   geom_bar(stat="identity",position = "dodge")+
   theme(axis.text.x = element_text(angle=45,hjust = 1))
 
#write_csv(avg_cs,"cargoShort_sum.csv")
# -------------------------------------------------
   #check for NA values
   #sum(is.na(v_mod$vsl_type))
   #sum(is.na(v_mod$tradeArea))
   #sum(is.na(v_mod$cargoShort))
   #sum(is.na(v_mod$month))
   #sum(is.na(v_mod$lastDiscPort))
   
   #write modified data into file
   #write_csv(v_mod, "updated_voypnl.csv")
   
   # Write output into PDF
 
 #pdf(file = "regression_results.pdf", paper = "USr")
 #----------------------------------------------------
avg_dr <- aggregate(v_mod[,c("dailyrate")],list(vsl_type=v_mod$vsl_type), mean)
 avg_dr <- avg_dr [order(-avg_dr$x),]
 
 avg_dr
 
 #plot top 50 cargo types 
 ggplot(avg_dr, aes(x=reorder(vsl_type,-x),y=x)) +
   geom_bar(stat="identity",position = "dodge")+
   theme(axis.text.x = element_text(angle=45,hjust = 1))
 
 ----------------------------------------------------------
   geom_bar(position = position_fill(), colour = "black") +
   theme(axis.text.x = element_text(angle = 60, hjust=1), plot.title = element_text(hjust = 0.5)) + 
   labs(title = "Credit Limit Amount vs Default", x = "Limit Amount Bin", y = "Percentage") +
   guides(fill=guide_legend(title = "October Action")) +
   scale_y_continuous(labels = scales::percent) +
   geom_hline(data = hlines, aes(yintercept = baseline, colour = label)) + 
   scale_colour_manual(NULL, values = "green")
   
 
 