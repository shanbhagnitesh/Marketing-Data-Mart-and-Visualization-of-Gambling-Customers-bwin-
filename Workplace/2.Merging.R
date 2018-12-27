#################################################################
################# Part 2: MERGING TABLES ########################
#################################################################



# We are keeping only the column of interest for the 4 categories of plays to construct the basetable

Sports <- DailyAggregation_Sp_clean[,c("UserID","First_TransDAte_Sp","Sum_Stakes_Sp","Sum_Winnings_Sp","Total_Bets_Sp",
                                       "Length_Play_Sp","LastDay_Sp","No_Days_Played_Sp","No_Months_Played_Sp","Most_Played_Month_Sp",
                                       "MostPlayed_Day_Sp")]


Casino <- DailyAggregation_Ca_clean[,c("UserID","First_TransDAte_Ca","Sum_Stakes_Ca","Sum_Winnings_Ca","Total_Bets_Ca",
                                      "Length_Play_Ca","LastDay_Ca","No_Days_Played_Ca","No_Months_Played_Ca","Most_Played_Month_Ca",
                                      "MostPlayed_Day_Ca")]

Games <- DailyAggregation_Ga_clean[,c("UserID","First_TransDAte_Ga","Sum_Stakes_Ga","Sum_Winnings_Ga","Total_Bets_Ga",
                                       "Length_Play_Ga","LastDay_Ga","No_Days_Played_Ga","No_Months_Played_Ga","Most_Played_Month_Ga",
                                       "MostPlayed_Day_Ga")]

Sports_Analytics <- AnalyticDataInternetGambling[,c("UserID","AGE","FOTotalStakes", "FOTotalWinnings", "FOTotalBets","FOTotalDaysActive",
                                                    "LATotalStakes","LATotalWinnings","LATotalBets","LATotalDaysActive")]


Poker <- RawDataIIIPokerChipConversions[,c("UserID","Last_TransDAte","MostPlayed_Day","No_Days_Played","Total_Amount_Sold", "Total_Amount_Bought","LengthOfRelation",	"Most_PlayedMonth")]


## Merging Sports with Sports Analytics to have an overall summary of Sports data (including FO and LA)

Sports_final <- merge(x=Sports_Analytics, y=Sports, by = "UserID", all.x = T)


########################## Final Basetable ####################################
##############################################################################

# Creating the profit made by online Website on each player that played Casinos
Casino$Profit_Ca <- Casino$Sum_Stakes_Ca - Casino$Sum_Winnings_Ca

# Creating the profit made by online Website on each player that played Games
Games$Profit_Ga <- Games$Sum_Stakes_Ga - Games$Sum_Winnings_Ga


# Creating the profit made by online Website on each player that played Poker
Poker$Profit_Po <- Poker$Total_Amount_Bought - Poker$Total_Amount_Sold


# Creating the profit made by online Website on each player that played Sports
Sports_final$Profit_Sp <-  Sports_final$Sum_Stakes_Sp - Sports_final$Sum_Winnings_Sp

Sports_final$Profit_FO <-  Sports_final$FOTotalStakes - Sports_final$FOTotalWinnings

Sports_final$Profit_LA <- Sports_final$LATotalStakes - Sports_final$LATotalWinnings

####################################################################################################################################################################################


###### CREATING THE MAIN BASETABLE

Basetable <- merge(x=RawDataIDemographics[,c(1,2,7,8,9,10,11)], y=Sports_final, by = "UserID", all.x = T)
Basetable <- merge(x=Basetable, y=Poker, by = "UserID", all.x = T)
Basetable <- merge(x=Basetable, y=Games, by = "UserID", all.x = T)
Basetable <- merge(x=Basetable, y=Casino, by = "UserID", all.x = T)
colnames(Basetable)


Basetable$TotalStakes <- Basetable$Sum_Stakes_Sp + Basetable$Sum_Stakes_Ga+Basetable$Sum_Stakes_Ca + Basetable$Total_Amount_Bought

Basetable$TotalWinnings <- Basetable$Sum_Winnings_Sp + Basetable$Sum_Winnings_Ga + Basetable$Sum_Winnings_Ca + Basetable$Total_Amount_Sold

#Reordering the columns

Basetable <- Basetable[,c("UserID","Gender","CountryName","Language Description","Application Description","AGE","RegDate",
                          "FOTotalBets","LATotalBets","Total_Bets_Sp", "Profit_FO","Profit_LA","Profit_Sp",
                          "FOTotalDaysActive","LATotalDaysActive","No_Days_Played_Sp",
                          "First_TransDAte_Sp","LastDay_Sp","Length_Play_Sp",
                          "No_Months_Played_Sp","Most_Played_Month_Sp","MostPlayed_Day_Sp",
                          "FirstPo","LastDay_Po","Length_Play_Po","No_Days_Played_Po",
                          "Most_Played_Day_Po","Most_Played_Month_Po", "Profit_Po",
                          "First_TransDAte_Ga","LastDay_Ga","Length_Play_Ga","No_Days_Played_Ga",
                          "No_Months_Played_Ga", "Most_Played_Month_Ga","MostPlayed_Day_Ga",
                          "Total_Bets_Ga", "Profit_Ga",
                          "First_TransDAte_Ca","LastDay_Ca","Length_Play_Ca", "No_Days_Played_Ca",
                          "No_Months_Played_Ca","Most_Played_Month_Ca","MostPlayed_Day_Ca",
                          "Total_Bets_Ca","Profit_Ca","TotalStakes","TotalWinnings")]       

# Transforming FirstPO as a Date
Basetable$FirstPo <- as.Date(Basetable$FirstPo, "%Y%m%d")                                                   

####### CREATING NEW VARIABLES FOR THE ANALYSIS

# GGR = Gross Gaming Revenue as the sum of all Profit made in total
Basetable$GGR <- Basetable$Profit_FO + Basetable$Profit_LA + Basetable$Profit_Po + Basetable$Profit_Ga + Basetable$Profit_Ca 

# The total bets placed for each players
Basetable$Total_BetsPlaced <- Basetable$Total_Bets_Ca + Basetable$Total_Bets_Ga + Basetable$Total_Bets_Sp

# Frequency = Total numbers of play during the length of subscription
Basetable$Frequency <- Basetable$No_Days_Played_Sp + Basetable$No_Days_Played_Po + Basetable$No_Days_Played_Ga + Basetable$No_Days_Played_Ca

# Last active date all of games
Basetable[, "Last_Active_Date"] <- as.Date(apply(Basetable[, c("LastDay_Sp", "LastDay_Ga", "LastDay_Po", "LastDay_Ca")], 1, max, na.rm=TRUE),"%Y-%m-%d")

# First active dare of all games
Basetable[, "First_Active_Date"] <- as.Date(apply(Basetable[, c("First_TransDAte_Sp", "FirstPo",  "First_TransDAte_Ga","First_TransDAte_Ca")], 1, min, na.rm=TRUE),"%Y-%m-%d")

# Total lenght of relationship of each player
Basetable$Total_Length_Relationship <-Basetable$Last_Active_Date - Basetable$First_Active_Date

# Last day of collected data
Basetable$Sep302005 <- as.Date("20050930","%Y%m%d")

# Customer loyalty or churn(depending on point of view) based on the last day of collected data and the last day of active play (0 is very loyal)
Basetable$Churn_Loyalty <- Basetable$Sep302005 - Basetable$Last_Active_Date
# Customer loyalty 
Basetable$Loyal <- ifelse(Basetable$Churn_Loyalty <= 61, 'High Loyalty',
                          ifelse((Basetable$Churn_Loyalty>61 &Basetable$Churn_Loyalty<=153),'Medium Loyalty',
                                 ifelse(Basetable$Churn_Loyalty>153,'Low Loyalty','Not Known')))


Basetable$Loyal <- factor(Basetable$Loyal, levels = c("High Loyalty","Medium Loyalty","Low Loyalty"), ordered=TRUE)
 

# Customer ARPU = Average Revenue per user for the casino which is the total profit made divided by the Frequency
Basetable$ARPU <- Basetable$GGR / Basetable$Frequency

Basetable$Gender <- as.factor(Basetable$Gender)

# Age group for users
Basetable$AGE <- ifelse(is.na(Basetable$AGE),median(Basetable$AGE,na.rm=TRUE),Basetable$AGE)
Basetable$Age_Group <- ifelse(Basetable$AGE <= 18, '< 18 Years old',
                          ifelse((Basetable$AGE >18 & Basetable$AGE <= 35),'18-35 Years old',
                                 ifelse((Basetable$AGE >35 & Basetable$AGE <= 55),'35-55 Years old',
                                        ifelse((Basetable$AGE > 55 & Basetable$AGE <= 75),'55-75 Years old',
                                              ifelse(Basetable$AGE > 75,'75+ Years old','Not Known')))))

Basetable$Age_Group <- as.factor(Basetable$Age_Group)
Basetable$CountryName <- as.factor(Basetable$CountryName)
Basetable$`Application Description` <- as.factor(Basetable$`Application Description`)

####### Creating columns for continents
continents <- read_excel("Continent.xlsx")
Basetable <- merge(x = Basetable, y = continents, by = "CountryName", all.x = TRUE)
Basetable$Continents <- as.factor(Basetable$Continents)


Basetable$ObservedFrequency <-ifelse(Basetable$Frequency < 75, 'Low',
                              ifelse((Basetable$Frequency >= 75 & Basetable$Frequency < 150),'Medium',
                              ifelse(Basetable$Frequency >= 150,'High','Not Known')))

Basetable$ObservedStakes <-ifelse(Basetable$TotalStakes < 1000, 'Low',
                          ifelse((Basetable$TotalStakes >= 1000 & Basetable$TotalStakes < 10000),'Medium',
                          ifelse(Basetable$TotalStakes >= 10000,'High','Not Known')))

Basetable$Ranked_Segment <- ifelse((Basetable$ObservedStakes == 'High' & Basetable$ObservedFrequency == 'High'),1,
                            ifelse((Basetable$ObservedStakes == 'High' & Basetable$ObservedFrequency == 'Medium'),2,
                            ifelse((Basetable$ObservedStakes == 'High' & Basetable$ObservedFrequency == 'Low'),3,
                            ifelse((Basetable$ObservedStakes == 'Medium' & Basetable$ObservedFrequency == 'High'),4,
                            ifelse((Basetable$ObservedStakes == 'Medium' & Basetable$ObservedFrequency == 'Medium'),5,
                            ifelse((Basetable$ObservedStakes == 'Medium' & Basetable$ObservedFrequency == 'Low'),6,7
                                          ))))))
                                   
Basetable$Profit_Margin <- ((Basetable$TotalStakes - Basetable$TotalWinnings) / Basetable$TotalStakes ) * 100 
Basetable$Profit_Margin <- replace(Basetable$Profit_Margin, is.na(Basetable$Profit_Margin), 0)

Basetable$LifetimeValue <- ( (Basetable$GGR) / Basetable$Frequency ) * (Basetable$Total_Length_Relationship +1)
Basetable$LifetimeValue <- ifelse(is.na(Basetable$LifetimeValue),0,Basetable$LifetimeValue)

Basetable$LifetimeValue <- as.double(Basetable$LifetimeValue)
  
  

### Reordering Columns For Better Readability of Basetable
colnames(Basetable)

col_order <- c("UserID","Gender","CountryName","Continents","Language Description","Application Description",
               "AGE","Age_Group","GGR","Ranked_Segment","Profit_Margin","LifetimeValue","Total_BetsPlaced","First_Active_Date","Last_Active_Date","Total_Length_Relationship",
               "Loyal","ARPU","ObservedFrequency","ObservedStakes","TotalStakes","TotalWinnings",
               "Profit_FO","Profit_LA","Profit_Sp","Profit_Ca","Profit_Ga","Profit_Po",
               "No_Days_Played_Sp","No_Days_Played_Ga","No_Days_Played_Po","No_Days_Played_Ca",
               "First_TransDAte_Sp","FirstPo","First_TransDAte_Ca","First_TransDAte_Ga",
               "LastDay_Sp","LastDay_Po","LastDay_Ga" ,"LastDay_Ca",
               "Length_Play_Sp","Length_Play_Ga","Length_Play_Ca","Length_Play_Po",
               "Most_Played_Month_Sp","Most_Played_Month_Po","Most_Played_Month_Ga","Most_Played_Month_Ca",
               "MostPlayed_Day_Sp","Most_Played_Day_Po","MostPlayed_Day_Ga","MostPlayed_Day_Ca",
               "LATotalBets","FOTotalBets","Total_Bets_Ga","Total_Bets_Ca",
               "No_Months_Played_Sp","No_Months_Played_Ca","No_Months_Played_Ga","Churn_Loyalty","Frequency")

Basetable <- Basetable[,col_order]
###################
rm(list=setdiff(ls(), 
                c("Basetable",
                  "Poker",
                  "Casino",
                  "Games",
                  "Sports_final")))

sum(Basetable$TotalStakes > 10000)
sum(Basetable$TotalStakes <= 10000 & Basetable$TotalStakes > 1000)
sum(Basetable$TotalStakes < 1000)

write.csv(Basetable, file = "Basetable.csv")
save(Basetable, file = "Basetable.RData")


