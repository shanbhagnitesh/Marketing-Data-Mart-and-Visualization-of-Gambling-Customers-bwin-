######### ONLINE GAMBLING PROJECT ##################################
####################################################################

####################################################################
################### PART 1: DATA CLEANING ##########################


#### Libraries Used :

if(!require("haven")) install.packages("haven");library("haven")
if(!require("dplyr")) install.packages("dplyr");library("dplyr")
if(!require("readr")) install.packages("readr");library("readr")
if(!require("lubridate")) install.packages("lubridate");library("lubridate")
if(!require("plyr")) install.packages("plyr");library("plyr")
if(!require("tidyr")) install.packages("tidyr");library("tidyr")
if(!require("readxl")) install.packages("readxl");library("readxl")


########################################################
################# Readind the Data #####################
########################################################

AnalyticDataInternetGambling <- read_sas("AnalyticDataInternetGambling.sas7bdat", NULL)

RawDataIDemographics <- read_sas("RawDataIDemographics.sas7bdat", NULL)

RawDataIIIPokerChipConversions <- read_sas("RawDataIIIPokerChipConversions.sas7bdat", NULL)

DailyAggregation <- read_sas("RawDataIIUserDailyAggregation.sas7bdat", NULL)


########################################################################################################
###################  RawDataIIIPokerChipConversions ####################################################
########################################################################################################



# cleaning the data to be able to make some analysis on the date and time of the transactions

RawDataIDemographics$FirstPay <- ymd(RawDataIDemographics$FirstPay)

RawDataIDemographics$FirstPay <- ymd(RawDataIDemographics$FirstPay)

RawDataIIIPokerChipConversions <- separate(RawDataIIIPokerChipConversions,TransDateTime, c("date", "time"),sep =" ")

RawDataIIIPokerChipConversions$date <- as.Date(RawDataIIIPokerChipConversions$date)

v <- merge(x = RawDataIIIPokerChipConversions, y = RawDataIDemographics[ , c("UserID", "FirstPay")], by = "UserID", all.x=TRUE)
RawDataIIIPokerChipConversions <- v %>% filter(date >= FirstPay & date <= as.Date("2005-09-30") & date >= as.Date("2005-02-01"))

v <- NULL

RawDataIIIPokerChipConversions$FirstPay <- NULL

RawDataIIIPokerChipConversions$TransAmount <- ifelse(RawDataIIIPokerChipConversions$TransAmount < 0 , 0, RawDataIIIPokerChipConversions$TransAmount)


RawDataIIIPokerChipConversions$TransTime <- format(as.POSIXct(RawDataIIIPokerChipConversions$TransTime,
                                                               format="%H:%M:%S"),"%H")

### 6 to 12 - Morning , 12 - 17 pm  Evening  -- 17 to 6 is Night

### Converting Time into Morning Afternoon and Evening to know what time of day did Transaction take place !!

RawDataIIIPokerChipConversions$MorningTrans <- 0
RawDataIIIPokerChipConversions$EveningTrans <- 0
RawDataIIIPokerChipConversions$NightTrans   <- 0

RawDataIIIPokerChipConversions$MorningTrans <- 
  ifelse(RawDataIIIPokerChipConversions$TransTime >= 6 & RawDataIIIPokerChipConversions$TransTime < 12, 1, 0)

RawDataIIIPokerChipConversions$EveningTrans <- 
  ifelse(RawDataIIIPokerChipConversions$TransTime >= 12 & RawDataIIIPokerChipConversions$TransTime < 17, 1, 0)

RawDataIIIPokerChipConversions$NightTrans <- 
  ifelse(RawDataIIIPokerChipConversions$MorningTrans == 0 & RawDataIIIPokerChipConversions$EveningTrans == 0, 1, 0)



########################################################################################################
## Distinguishing Buying and Selling

RawDataIIIPokerChipConversions$TransType <- 
  ifelse(RawDataIIIPokerChipConversions$TransType == 124, "Sell", "Buy")

# Finding which Day of the week 
RawDataIIIPokerChipConversions$Day <- weekdays(as.Date(RawDataIIIPokerChipConversions$TransDate))


#######################################################################################################

##### Following Variables will be Created as part of this Table :
# 1) : First Trans Date  -- His First Transaction Date
# 2) : Last Trans Date   -- Last Date of User's Transaction
# 3) : No_Of_Months_Played -- How many Different Months has the User Played
# 4) : Most_Played_Month -- Which month has the User played the most
# 5) : Most_Played_day -- Which Day has User Played the most
# 6) : No_Days_Played - How many different days has he played
# 7) : Total_Amount_Sold and Total_Amount Bought per User
# 8) : Mean and Max Amount Sold and Bought Per User 
# 9) : Morning_Trans = No of Transactions Done in the Morning
#10) : Evening_Trans = No of Transactions Done in the Evening
#11) : Night_Trans = No of Transactions Done in the Night
#12) : Amount_Sold/Bought_Last_Day = What was the amount that he sold / Bought on his last day
RawDataIIIPokerChipConversions <- as_tibble(RawDataIIIPokerChipConversions)

str(RawDataIIIPokerChipConversions)
RawDataIIIPokerChipConversions$UserID <- as.integer(RawDataIIIPokerChipConversions$UserID)

RawDataIIIPokerChipConversions <-ddply(RawDataIIIPokerChipConversions,.(UserID),summarize,
                                       First_TransDAte = min(TransDate),
                                       Last_TransDAte = max(TransDate),
                                       No_Months_Played = n_distinct(month(TransDate)),
                                       Most_Played_Month = names(which.max(table(month(TransDate)))),
                                       MostPlayed_Day = names(which.max(table(Day))),
                                       No_Days_Played = n_distinct(TransDate),
                                       Total_Amount_Sold = sum(TransAmount[TransType == "Sell"]),
                                       Total_Amount_Bought = sum(TransAmount[TransType == "Buy"]),
                                       Mean_Amount_Sold = mean(TransAmount[TransType == "Sell"]),
                                       Mean_Amount_Bought = mean(TransAmount[TransType == "Buy"]),
                                       Max_Amount_Sold = max(TransAmount[TransType == "Sell"]),
                                       Max_Amount_Bought = max(TransAmount[TransType == "Buy"]),
                                       Tot_Transaction_Morning = sum(MorningTrans),
                                       Tot_Transaction_Evening = sum(EveningTrans),
                                       Tot_Transc_Night = sum(NightTrans),
                                       Amount_Sold_Last_Day = sum(TransAmount[TransDate %in% max(TransDate) & TransType == "Sell"]),
                                       Amount_Bought_Last_Day = sum(TransAmount[TransDate %in% max(TransDate) & TransType == "Buy"])
                                       ,.drop = FALSE)



#########################################################################################################
################################# RawDataIDemographics ##################################################
#########################################################################################################

# reading the country, region and application to get the names into the basetable
country <- read_excel("country.xlsx")
region <- read_excel("region.xlsx")
application <- read_excel("application.xlsx")

#########################################################################################################

RawDataIDemographics <- RawDataIDemographics %>% filter(FirstPay >= as.Date("2005-02-01") & FirstPay <= as.Date("2005-09-30"))

#### Reading Country Language and Application Id Description instead of just Codes 
RawDataIDemographics <- merge(x = RawDataIDemographics, y = country, by = "Country", all.x = TRUE)
RawDataIDemographics <- merge(x = RawDataIDemographics, y = region, by = "Language", all.x = TRUE)
RawDataIDemographics <- merge(x = RawDataIDemographics, y = application,by = "ApplicationID", all.x = TRUE)

RawDataIDemographics$Gender <- ifelse(RawDataIDemographics$Gender == 0,"Female","Male" )


apply(RawDataIDemographics, 2, function(x) any(is.null(x)))



################################################################################
######################### AnalyticDataInternetGambling #########################
################################################################################



# cleaning the Analytic Data Internet Gambling

length(unique(AnalyticDataInternetGambling$USERID))

AnalyticDataInternetGambling <- merge(x = AnalyticDataInternetGambling, y = region, by = "Language", all.x = TRUE)

AnalyticDataInternetGambling <- merge(x = AnalyticDataInternetGambling, y = country, by = "Country", all.x = TRUE)


AnalyticDataInternetGambling <- merge(x = AnalyticDataInternetGambling, y = region, by = "Language", all.x = TRUE)


AnalyticDataInternetGambling$GENDER <- ifelse(AnalyticDataInternetGambling$GENDER == 0,"Female","Male" )

AnalyticDataInternetGambling$RegistrationDate <- as.Date(AnalyticDataInternetGambling$RegistrationDate)

######################################################################################################################

RawDataIIIPokerChipConversions$LengthOfRelation <- RawDataIIIPokerChipConversions$Last_TransDAte - RawDataIIIPokerChipConversions$First_TransDAte

days <- read_excel("days.xlsx")

RawDataIIIPokerChipConversions <- merge(x = RawDataIIIPokerChipConversions, y = days, by = "Most_Played_Month", all.x = TRUE)



RawDataIDemographics$Diff <- as.Date(RawDataIDemographics$FirstPay,"%Y%m%d")-as.Date(RawDataIDemographics$FirstAct,"%Y%m%d")


RawDataIDemographics$DaysToStartSP <- as.Date(RawDataIDemographics$FirstSp,"%Y%m%d")-as.Date(RawDataIDemographics$RegDate,"%Y-%m-%d")
RawDataIDemographics$DaysToStartCa <- as.Date(RawDataIDemographics$FirstCa,"%Y%m%d")-as.Date(RawDataIDemographics$RegDate,"%Y-%m-%d")
RawDataIDemographics$DaysToStartGa <- as.Date(RawDataIDemographics$FirstGa,"%Y%m%d")-as.Date(RawDataIDemographics$RegDate,"%Y-%m-%d")
RawDataIDemographics$DaysToStartPo <- as.Date(RawDataIDemographics$FirstPo,"%Y%m%d")-as.Date(RawDataIDemographics$RegDate,"%Y-%m-%d")
RawDataIDemographics$DaysToStartPo <- ifelse(is.na(RawDataIDemographics$DaysToStartPo),"Not Played",RawDataIDemographics$DaysToStartPo) 


####################################################################################################
##############################  Daily Aggregation ##################################################
####################################################################################################


#### User Daily Aggregation

DailyAggregation$Date <- ymd(DailyAggregation$Date)
DailyAggregation <- merge(x = DailyAggregation, 
                                        y = RawDataIDemographics[ , c("UserID", "FirstPay")], by = "UserID", all.x=TRUE)

v <- merge(x = DailyAggregation, y = RawDataIDemographics[ , c("UserID", "FirstPay")], by = "UserID", all.x=TRUE)

DailyAggregation <- v %>% filter(Date >= FirstPay & Date <= as.Date("2005-09-30") & Date >= as.Date("2005-02-01"))

v <- NULL

DailyAggregation$FirstPay <- NULL


#unique User Id
uniqueUserId <- unique(DailyAggregation$UserID)



# summary stats for Sports products (fixed odds, and live action)
DailyAggr_Sp <- DailyAggregation_clean %>% filter(ProductID == 1 | ProductID == 2)
DailyAggregation_Sp_clean<-ddply(DailyAggr_Sp,.(UserID),summarize,
                                 First_TransDAte_Sp = min(Date),
                                 Sum_Stakes_Sp = round(sum(Stakes),1), 
                                 Max_Stakes_Sp = max(Stakes), 
                                 Mean_Stakes_Sp=round(mean(Stakes),1),
                                 Sum_Winnings_Sp=round(sum(Winnings),1),
                                 Max_Winnings_Sp = max(Winnings),
                                 Mean_Winnings_Sp=round(mean(Winnings),1),
                                 Total_Bets_Sp= round(sum(Bets),1), 
                                 Length_Play_Sp= (max(Date)-min(Date)) ,
                                 LastDay_Sp = max(Date),
                                 No_Days_Played_Sp = n_distinct(Date),
                                 No_Months_Played_Sp = n_distinct(month(as.Date(Date,format="%Y%m%d"))),
                                 Most_Played_Month_Sp = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
                                 MostPlayed_Day_Sp = names(which.max(table(weekdays(as.Date(Date))))), 
                                 Stakes_Last_Day_Sp = sum(Stakes[Date %in% max(Date) ]),
                                 Winnings_Last_Day_Sp = sum(Winnings[Date %in% max(Date) ]),
                                 Bets_Last_Day_Sp = sum(Bets[Date %in% max(Date) ]),
                                 .drop = FALSE)






# No Poker Products

# summary stats for Casino sports book
DailyAggr_Ca <- DailyAggregation_clean %>% filter(ProductID == 4 | ProductID == 8)
DailyAggregation_Ca_clean<-ddply(DailyAggr_Ca,.(UserID),summarize,
                                 First_TransDAte_Ca = min(Date),
                                 Sum_Stakes_Ca = round(sum(Stakes),1), 
                                 Max_Stakes_Ca = max(Stakes), 
                                 Mean_Stakes_Ca=round(mean(Stakes),1),
                                 Sum_Winnings_Ca =round(sum(Winnings),1),
                                 Max_Winnings_Ca = max(Winnings),
                                 Mean_Winnings_Ca=round(mean(Winnings),1),
                                 Total_Bets_Ca= round(sum(Bets),1), 
                                 Length_Play_Ca= (max(Date)-min(Date)) ,
                                 LastDay_Ca = max(Date),
                                 No_Days_Played_Ca = n_distinct(Date),
                                 No_Months_Played_Ca = n_distinct(month(as.Date(Date,format="%Y%m%d"))),
                                 Most_Played_Month_Ca = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
                                 MostPlayed_Day_Ca = names(which.max(table(weekdays(as.Date(Date))))), 
                                 Stakes_Last_Day_Ca = sum(Stakes[Date %in% max(Date) ]),
                                 Winnings_Last_Day_Ca = sum(Winnings[Date %in% max(Date) ]),
                                 Bets_Last_Day_Ca = sum(Bets[Date %in% max(Date) ]),
                                 .drop = FALSE)

# summary stats for Games sports book
DailyAggr_Ga <- DailyAggregation_clean %>% filter(ProductID == 5 | ProductID == 6 | ProductID == 7)
DailyAggregation_Ga_clean<-ddply(DailyAggr_Ga,.(UserID),summarize,
                                 First_TransDAte_Ga = min(Date),
                                 Sum_Stakes_Ga = round(sum(Stakes),1), 
                                 Max_Stakes_Ga = max(Stakes), 
                                 Mean_Stakes_Ga=round(mean(Stakes),1),
                                 Sum_Winnings_Ga=round(sum(Winnings),1),
                                 Max_Winnings_Ga = max(Winnings),
                                 Mean_Winnings_Ga=round(mean(Winnings),1),
                                 Total_Bets_Ga= round(sum(Bets),1), 
                                 Length_Play_Ga= (max(Date)-min(Date)) ,
                                 LastDay_Ga = max(Date),
                                 No_Days_Played_Ga = n_distinct(Date),
                                 No_Months_Played_Ga = n_distinct(month(as.Date(Date,format="%Y%m%d"))),
                                 Most_Played_Month_Ga = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
                                 MostPlayed_Day_Ga = names(which.max(table(weekdays(as.Date(Date))))), 
                                 Stakes_Last_Day_Ga = sum(Stakes[Date %in% max(Date) ]),
                                 Winnings_Last_Day_Ga = sum(Winnings[Date %in% max(Date) ]),
                                 Bets_Last_Day_Ga = sum(Bets[Date %in% max(Date) ]),
                                 .drop = FALSE)



# Removing all the environment varibles apart from required



rm(list=setdiff(ls(), 
                c("AnalyticDataInternetGambling",
                  "RawDataIDemographics",
                  "RawDataIIIPokerChipConversions",
                  "DailyAggregation_Ca_clean",
                  "DailyAggregation_Ga_clean",
                  "DailyAggregation_Sp_clean")))

