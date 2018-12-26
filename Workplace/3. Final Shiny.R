#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

load("Basetable.RData")

if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("DT")) install.packages("DT"); library("DT")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")
if(!require("shinyWidgets")) install.packages("shinyWidgets"); library("shinyWidgets")
if(!require("fBasics")) install.packages("fBasics"); library("fBasics")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("lumen"),
  # Application title
  titlePanel(titlePanel("Online Gambling")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
     
      
      checkboxGroupInput("gender","Select the gender:",
                         choices = levels(Basetable$Gender),
                          selected = unique(Basetable$Gender)),
      
       
      pickerInput("Application","Select the applications:",
                  choices = levels(Basetable$Application),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE),
                  selected = unique(Basetable$Application)),
      
      
      pickerInput("continents","Select the continents:",
                  choices = levels(Basetable$Continents),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE),
                  selected = unique(Basetable$Continents))
      
      
    ),
    
    
  
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  id= "tabsetpanel", 
                  tabPanel(title = "Geographical Presence of Customers",
                           plotOutput("geo")),
                  tabPanel(title = "Gamblers Profiles",
                           splitLayout(cellWidths = c("50%","50%"),plotOutput("hist"),plotOutput("hist2")),splitLayout(cellWidths = c("50%","50%"),plotOutput("hist3"),plotOutput("hist4"))),
                  tabPanel(title = "Segmentation of Customers",
                           plotOutput("segment1"),plotOutput("segment")),
                  tabPanel(title = "Summary Statistics",
                           br(),
                           br(),
                           h3("Descriptive Statistic of few Global Metrics",align = "center"),
                           br(),
                           tableOutput("stat")),
                  tabPanel(title = "Profits",
                           plotOutput("Pr"),plotOutput("Pr2")),
                  tabPanel(title = "Sports",
                           plotOutput("SP"),plotOutput("SP2")),
                  tabPanel(title = "Poker",
                           plotOutput("Po"),plotOutput("Po2")),
                  tabPanel(title = "Casino",
                           plotOutput("Ca"),plotOutput("Ca2")),
                  tabPanel(title = "Games",
                           plotOutput("Ga"),plotOutput("Ga2"))
                  
                 
                  
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$stat <- renderTable({
    # subset the basetable dataset to get the chosen Application  
    data <- subset(Basetable, Application %in% input$Application & Continents %in% input$continents & Gender %in% input$gender)
    options(scipen=999)
    
    sum_stats <- data[,c(9,12,13,18,21,22,61)]
    df1 <- round(basicStats(sum_stats)[c("Mean", "Stdev", "Median", "Minimum", "Maximum", "nobs"),],2)
    Sum_stats <- c("Mean","Stdev", "Median", "Minimum", "Maximum","nobs")
    cc <- cbind(Sum_stats, df1)
    
    })
  
  
  output$geo <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data <- subset(Basetable, Application %in% input$Application & Continents %in% input$continents & Gender %in% input$gender)
    
    # 
    data(wrld_simpl)
    myCountries = wrld_simpl@data$NAME %in% data$CountryName
    plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])
  })
  
  
  
    
  output$hist <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data <- subset(Basetable, Application %in% input$Application & Continents %in% input$continents & Gender %in% input$gender )
    
    # 
    hist(data$AGE,
         main ="Gamblers by Age", 
         ylab = "Number of gamblers",
         xlab = "Age")
  })
  
  output$hist2 <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    
    sport_plot<- sum(!is.na(data$First_TransDAte_Sp))
    games_plot <- sum(!is.na(data$First_TransDAte_Ga))
    poker_plot <- sum(!is.na(data$FirstPo))
    casino_plot <- sum(!is.na(data$First_TransDAte_Ca))
    vector <- c(sport_plot,games_plot,poker_plot,casino_plot)
    names(vector) <- c("Sport", "Games", "Poker", "Casino")
    bar <- barplot(vector,
                   names.arg = names(vector),
                   xlab = "Product Categories", 
                   ylab = "Number of Gamblers", 
                   col = c("#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9"),
                   main = "Gamblers per Product Categories")
    
    text(x = bar, y = vector, label = vector ,pos = 1, cex =1 , col = "black")
    
    
    })

  output$hist3 <- renderPlot({
    data1 <- subset(Basetable, Application %in% input$Application & Continents %in% input$continents & Gender %in% input$gender)  
    ggplot(subset(data1,!is.na(data1$Loyal)),aes(x=Loyal, fill=Loyal), na.rm = TRUE)  + geom_bar()  + ggtitle("Gamblers per Loyalty Class")+labs(x=" ",y="Numbers of Gamblers")
   
  
    
    })

  output$hist4 <- renderPlot({
    data1 = subset(Basetable, Application %in% input$Application & Continents %in% input$continents & Gender %in% input$gender)  
    ggplot(data = data1,aes(x=Language,fill= Language))  + geom_bar()  + ggtitle("Gamblers per Languages")+labs(x=" ",y="Numbers of Gamblers") + theme(axis.title.x=element_blank(),
                                                                                                                                                     axis.text.x=element_blank(),
                                                                                                                                                     axis.ticks.x=element_blank())
  })
  
    
  output$SP <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    # Pie Chart with Percentages
    Monday_Sp <- sum(data1$MostPlayed_Day_Sp == 'Monday', na.rm = TRUE)
    Tuesday_Sp <- sum(data1$MostPlayed_Day_Sp == 'Tuesday', na.rm = TRUE)
    Wednesday_Sp <- sum(data1$MostPlayed_Day_Sp == 'Wednesday', na.rm = TRUE)
    Thursday_Sp <- sum(data1$MostPlayed_Day_Sp == 'Thursday', na.rm = TRUE)
    Friday_Sp <- sum(data1$MostPlayed_Day_Sp == 'Friday', na.rm = TRUE)
    Saturday_Sp <- sum(data1$MostPlayed_Day_Sp == 'Saturday', na.rm = TRUE)
    Sunday_Sp <- sum(data1$MostPlayed_Day_Sp == 'Sunday', na.rm = TRUE)
    Preferred_Day_Sport <- c(Monday_Sp, Tuesday_Sp, Wednesday_Sp, Thursday_Sp, Friday_Sp, Saturday_Sp, Sunday_Sp)
    
    daysSp <- c(Monday_Sp, Tuesday_Sp, Wednesday_Sp, Thursday_Sp, Friday_Sp, Saturday_Sp, Sunday_Sp)
    lbls <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday")
    pct <- round(daysSp/sum(daysSp)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(Preferred_Day_Sport,labels = lbls, col=rainbow(length(lbls)),
        main="Preferred Day of Play for Sports Gamblers")
    
    
  })
  
  output$SP2 <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    Feb_Sp <- sum(data1$Most_Played_Month_Sp == 'Feb', na.rm = TRUE)
    Mar_Sp <- sum(data1$Most_Played_Month_Sp == 'Mar', na.rm = TRUE)
    April_Sp <- sum(data1$Most_Played_Month_Sp == 'April', na.rm = TRUE)
    May_Sp <- sum(data1$Most_Played_Month_Sp == 'May', na.rm = TRUE)
    June_Sp <- sum(data1$Most_Played_Month_Sp == 'June', na.rm = TRUE)
    July_Sp <- sum(data1$Most_Played_Month_Sp == 'July', na.rm = TRUE)
    Aug_Sp <- sum(data1$Most_Played_Month_Sp == 'Aug', na.rm = TRUE)
    Sep_Sp <- sum(data1$Most_Played_Month_Sp == 'Sep', na.rm = TRUE)
    Preferred_Month_Sport <- c(Feb_Sp, Mar_Sp, April_Sp, May_Sp, June_Sp, July_Sp, Aug_Sp, Sep_Sp)
    names(Preferred_Month_Sport) <- c("Feb", "Mar", "Apr","May","June","July","Aug","Sept")
    sp2 <- barplot(Preferred_Month_Sport, 
                   names.arg = names(Preferred_Month_Sport), 
                   xlab = " ", 
                   ylab = "Number of Players", 
                   col = rainbow(12),
                   main = "Preferred Month of Play for Sports Gamblers")
    text(x = sp2, y = Preferred_Month_Sport, label = Preferred_Month_Sport ,pos = 1, cex =1 , col = "black")
    
  })
  
  output$Po <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    Monday_Po <- sum(data1$Most_Played_Day_Po == 'Monday', na.rm = TRUE)
    Tuesday_Po <- sum(data1$Most_Played_Day_Po == 'Tuesday', na.rm = TRUE)
    Wednesday_Po <- sum(data1$Most_Played_Day_Po == 'Wednesday', na.rm = TRUE)
    Thursday_Po <- sum(data1$Most_Played_Day_Po == 'Thursday', na.rm = TRUE)
    Friday_Po <- sum(data1$Most_Played_Day_Po == 'Friday', na.rm = TRUE)
    Saturday_Po <- sum(data1$Most_Played_Day_Po == 'Saturday', na.rm = TRUE)
    Sunday_Po <- sum(data1$Most_Played_Day_Po == 'Sunday', na.rm = TRUE)
    Preferred_Day_Sport <- c(Monday_Po, Tuesday_Po, Wednesday_Po, Thursday_Po, Friday_Po, Saturday_Po, Sunday_Po)
    
    daysPo <- c(Monday_Po, Tuesday_Po, Wednesday_Po, Thursday_Po, Friday_Po, Saturday_Po, Sunday_Po)
    lbls <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday")
    pct <- round(daysPo/sum(daysPo)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(Preferred_Day_Sport,labels = lbls, col=rainbow(length(lbls)),
        main="Preferred Day of Play for Poker Gamblers")
    
  })
  
  output$Po2 <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    Feb_Po <- sum(data1$Most_Played_Month_Po == 'Feb', na.rm = TRUE)
    Mar_Po <- sum(data1$Most_Played_Month_Po == 'Mar', na.rm = TRUE)
    April_Po <- sum(data1$Most_Played_Month_Po == 'April', na.rm = TRUE)
    May_Po <- sum(data1$Most_Played_Month_Po == 'May', na.rm = TRUE)
    June_Po <- sum(data1$Most_Played_Month_Po == 'June', na.rm = TRUE)
    July_Po <- sum(data1$Most_Played_Month_Po == 'July', na.rm = TRUE)
    Aug_Po <- sum(data1$Most_Played_Month_Po == 'Aug', na.rm = TRUE)
    Sep_Po <- sum(data1$Most_Played_Month_Po == 'Sep', na.rm = TRUE)
    Preferred_Month_Poker <- c(Feb_Po, Mar_Po, April_Po, May_Po, June_Po, July_Po, Aug_Po, Sep_Po)
    names(Preferred_Month_Poker) <- c("Feb", "Mar", "Apr","May","June","July","Aug","Sept")
    Po2 <- barplot(Preferred_Month_Poker, 
                   names.arg = names(Preferred_Month_Poker), 
                   xlab = " ", 
                   ylab = "Number of Players", 
                   col = c("#E69F00", "#56B4E9", "#009E73","#FF3333","#FF9933","#FFFF33","#99FF33"),
                   main = "Preferred Month of Play for Poker Gamblers")
    text(x = Po2, y = Preferred_Month_Poker, label = Preferred_Month_Poker, pos = 1, cex = 1, col = "black")
    
  })
  
  
  output$Ca <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    Monday_Ca <- sum(data1$MostPlayed_Day_Ca == 'Monday', na.rm = TRUE)
    Tuesday_Ca <- sum(data1$MostPlayed_Day_Ca == 'Tuesday', na.rm = TRUE)
    Wednesday_Ca <- sum(data1$MostPlayed_Day_Ca == 'Wednesday', na.rm = TRUE)
    Thursday_Ca <- sum(data1$MostPlayed_Day_Ca == 'Thursday', na.rm = TRUE)
    Friday_Ca <- sum(data1$MostPlayed_Day_Ca == 'Friday', na.rm = TRUE)
    Saturday_Ca <- sum(data1$MostPlayed_Day_Ca == 'Saturday', na.rm = TRUE)
    Sunday_Ca <- sum(data1$MostPlayed_Day_Ca == 'Sunday', na.rm = TRUE)
    Preferred_Day_Ca <- c(Monday_Ca, Tuesday_Ca, Wednesday_Ca, Thursday_Ca, Friday_Ca, Saturday_Ca, Sunday_Ca)
    
    daysCa <- c(Monday_Ca, Tuesday_Ca, Wednesday_Ca, Thursday_Ca, Friday_Ca, Saturday_Ca, Sunday_Ca)
    lbls <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday")
    pct <- round(daysCa/sum(daysCa)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(Preferred_Day_Ca,labels = lbls, col=rainbow(length(lbls)),
        main="Preferred Day of Play for Casino Gamblers")
    
  })
  
  output$Ca2 <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    Feb_Ca <- sum(data1$Most_Played_Month_Ca == 'Feb', na.rm = TRUE)
    Mar_Ca <- sum(data1$Most_Played_Month_Ca == 'Mar', na.rm = TRUE)
    April_Ca <- sum(data1$Most_Played_Month_Ca == 'April', na.rm = TRUE)
    May_Ca <- sum(data1$Most_Played_Month_Ca == 'May', na.rm = TRUE)
    June_Ca <- sum(data1$Most_Played_Month_Ca == 'June', na.rm = TRUE)
    July_Ca <- sum(data1$Most_Played_Month_Ca == 'July', na.rm = TRUE)
    Aug_Ca <- sum(data1$Most_Played_Month_Ca == 'Aug', na.rm = TRUE)
    Sep_Ca <- sum(data1$Most_Played_Month_Ca == 'Sep', na.rm = TRUE)
    Preferred_Month_Ca <- c(Feb_Ca, Mar_Ca, April_Ca, May_Ca, June_Ca, July_Ca, Aug_Ca, Sep_Ca)
    names(Preferred_Month_Ca) <- c("Feb", "Mar", "Apr","May","June","July","Aug","Sept")
    Ca2 <- barplot(Preferred_Month_Ca, 
                   names.arg = names(Preferred_Month_Ca), 
                   xlab = " ", 
                   ylab = "Number of Players", 
                   col = c("#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9"),
                   main = "Preferred Month of Play for Casino Gamblers")
    text(x = Ca2, y = Preferred_Month_Ca, label = Preferred_Month_Ca, pos = 1 , Cas = 3, cex = 1, col = "black")
    
  })
  
  output$Ga <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    # Pie Chart with Percentages
    Monday_Ga <- sum(data1$MostPlayed_Day_Ga == 'Monday', na.rm = TRUE)
    Tuesday_Ga <- sum(data1$MostPlayed_Day_Ga == 'Tuesday', na.rm = TRUE)
    Wednesday_Ga <- sum(data1$MostPlayed_Day_Ga == 'Wednesday', na.rm = TRUE)
    Thursday_Ga <- sum(data1$MostPlayed_Day_Ga == 'Thursday', na.rm = TRUE)
    Friday_Ga <- sum(data1$MostPlayed_Day_Ga == 'Friday', na.rm = TRUE)
    Saturday_Ga <- sum(data1$MostPlayed_Day_Ga == 'Saturday', na.rm = TRUE)
    Sunday_Ga <- sum(data1$MostPlayed_Day_Ga == 'Sunday', na.rm = TRUE)
    Preferred_Day_Ga <- c(Monday_Ga, Tuesday_Ga, Wednesday_Ga, Thursday_Ga, Friday_Ga, Saturday_Ga, Sunday_Ga)
    
    daysGa <- c(Monday_Ga, Tuesday_Ga, Wednesday_Ga, Thursday_Ga, Friday_Ga, Saturday_Ga, Sunday_Ga)
    lbls <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday")
    pct <- round(daysGa/sum(daysGa)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(Preferred_Day_Ga,labels = lbls, col=rainbow(length(lbls)),
        main="Preferred Day of Play for Games Gamblers")
    
  })
  
  output$Ga2 <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    Feb_Ga <- sum(data1$Most_Played_Month_Ga == 'Feb', na.rm = TRUE)
    Mar_Ga <- sum(data1$Most_Played_Month_Ga == 'Mar', na.rm = TRUE)
    April_Ga <- sum(data1$Most_Played_Month_Ga == 'April', na.rm = TRUE)
    May_Ga <- sum(data1$Most_Played_Month_Ga == 'May', na.rm = TRUE)
    June_Ga <- sum(data1$Most_Played_Month_Ga == 'June', na.rm = TRUE)
    July_Ga <- sum(data1$Most_Played_Month_Ga == 'July', na.rm = TRUE)
    Aug_Ga <- sum(data1$Most_Played_Month_Ga == 'Aug', na.rm = TRUE)
    Sep_Ga <- sum(data1$Most_Played_Month_Ga == 'Sep', na.rm = TRUE)
    Preferred_Month_Gaker <- c(Feb_Ga, Mar_Ga, April_Ga, May_Ga, June_Ga, July_Ga, Aug_Ga, Sep_Ga)
    names(Preferred_Month_Gaker) <- c("Feb", "Mar", "Apr","May","June","July","Aug","Sept")
    Ca2 <- barplot(Preferred_Month_Gaker, 
                   names.arg = names(Preferred_Month_Gaker), 
                   xlab = " ", 
                   ylab = "Number of Players", 
                   col = c("#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9"),
                   main = "Preferred Month of Play for Games Gamblers")
    text(x = Ca2, y = Preferred_Month_Gaker, label = Preferred_Month_Gaker, pos = 1 , Cas = 3, cex = 1, col = "black")
    
  })
  
  output$Pr <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    Profit_Ca <- round(sum(data1$Profit_Ca, na.rm = TRUE),2)
    Profit_Fo <- round(sum(data1$Profit_FO, na.rm = TRUE),2)
    Profit_LA <- round(sum(data1$Profit_LA, na.rm = TRUE),2)
    Profit_Ga <- round(sum(data1$Profit_Ga, na.rm = TRUE),2)
    Profit_Po <- round(sum(data1$Profit_Po, na.rm = TRUE),2)
    
    Total_Profits <- c(Profit_Ca, Profit_Fo, Profit_LA, Profit_Ga, Profit_Po)
    
    names(Total_Profits) <- c("Casino", "Fixed Odds(Sports)", "Live Action(Sports)","Games","Poker")
    
    Pr2 <- barplot(Total_Profits, 
                   names.arg = names(Total_Profits), 
                   xlab = "Different Products Categories", 
                   ylab = "Total Profit", 
                   col = c("#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9"),
                   main = "Total Profits per Products Categories",
                   ylim = c(-990000,4000000))
    text(x = Pr2, y = Total_Profits, label = Total_Profits, pos = 1 ,Cas = 3, cex = 1, col = "black")
    

  })
  
  output$Pr2 <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
  
  Loyal_1 <- round(sum(data1[which(data1[,"Loyal"] == "High Loyalty"),"GGR"]),2)
  Loyal_2 <- round(sum(data1[which(data1[,"Loyal"] == "Medium Loyalty"),"GGR"]),2)
  Loyal_3 <- round(sum(data1[which(data1[,"Loyal"] == "Low Loyalty"),"GGR"]),2)
  Loyalty <- c(Loyal_1, Loyal_2, Loyal_3)
  names(Loyalty) <- c("High Loyalty","Medium Loyalty","Low Loyalty")
  
  Loy <- barplot(Loyalty,
                 names.arg = names(Loyalty), 
                 xlab = " ", 
                 ylab = "Total Profits (euros)", 
                 col=blues9[7:1],
                 main= "Total Profits per Classes of Loyalty")
  text(x = Loy, y = Loyalty, label = Loyalty, pos = 1 , Cas = 3, cex = 1, col = "black")
  
  
})
  
  output$segment1 <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
  
  Segment_1_p <- sum(data1$Ranked_Segment == 1)
  Segment_2_p <- sum(data1$Ranked_Segment == 2)
  Segment_3_p <- sum(data1$Ranked_Segment == 3)
  Segment_4_p <- sum(data1$Ranked_Segment == 4)
  Segment_5_p <- sum(data1$Ranked_Segment == 5)
  Segment_6_p <- sum(data1$Ranked_Segment == 6)
  Segment_7_p <- sum(data1$Ranked_Segment == 7)
  
  Segment_players_p <- c(Segment_1_p,Segment_2_p,Segment_3_p,Segment_4_p,Segment_5_p,Segment_6_p,Segment_7_p)
  names(Segment_players_p) <- c("1 - HM/HF", "2 - HM/MF", "3 - HM/LF", "4 - MM/HF","5 - MM/MF","6 - MM/LF","7 - LM")
  
  Pr_seg_p <- barplot(Segment_players_p,
                      names.arg =  names(Segment_players_p), 
                      xlab = "Ranked Segments", 
                      ylab = "No Gamblers per Segment", 
                      col=blues9[7:1],
                      main = "Number of Gamblers per Ranked Segment ",
                      ylim= c(0,40000))
  text(x = Pr_seg_p, y = Segment_players_p, label = Segment_players_p, pos = 3 ,Cas = 3, cex = 1, col = "black")
  
  
  
})
  
  
  output$segment <- renderPlot({
    # subset the basetable dataset to get the chosen Application  
    data1 <- subset(Basetable, Application %in% input$Application  & Continents %in% input$continents & Gender %in% input$gender)
    
    Segment_1 <- round(sum(data1[which(data1[,"Ranked_Segment"] == 1),"GGR"]),2)
    Segment_2 <- round(sum(data1[which(data1[,"Ranked_Segment"] == 2),"GGR"]),2)
    Segment_3 <- round(sum(data1[which(data1[,"Ranked_Segment"] == 3),"GGR"]),2)
    Segment_4 <- round(sum(data1[which(data1[,"Ranked_Segment"] == 4),"GGR"]),2)
    Segment_5 <- round(sum(data1[which(data1[,"Ranked_Segment"] == 5),"GGR"]),2)
    Segment_6 <- round(sum(data1[which(data1[,"Ranked_Segment"] == 6),"GGR"]),2)
    Segment_7 <- round(sum(data1[which(data1[,"Ranked_Segment"] == 7),"GGR"]),2)
    
    Segment_players <- c(Segment_1,Segment_2,Segment_3,Segment_4,Segment_5,Segment_6,Segment_7)
    names(Segment_players) <- c("1 - HM/HF", "2 - HM/MF", "3 - HM/LF", "4 - MM/HF","5 - MM/MF","6 - MM/LF","7 - LM")
    
    Pr_seg <- barplot(Segment_players,
                      names.arg = names(Segment_players), 
                      xlab = "Ranked Segments", 
                      ylab = "Total Profits (euros) per Segments", 
                      col=blues9[7:1],
                      main= "Total Profits (euros) per Ranked Segments")
    text(x = Pr_seg, y = Segment_players, label = Segment_players, pos = 1 , Cas = 3, cex = 1, col = "black")
    
    
  })

  }


# Run the application 
shinyApp(ui = ui, server = server)

