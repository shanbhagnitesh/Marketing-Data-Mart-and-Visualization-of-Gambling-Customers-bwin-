# Marketing-Data-Mart-and-Visualization-of-Gambling-Customers-bwin-
Bwin.com is an Online Based Gaming company and this Project aimed at Behavioral Analysis of 40k+ Users of Bwin and Marketing Datamart for and a Shiny app to visualize the Results.

# Keywords : R, Shiny, Statistics, Data Visualization, Marketing Analytics, Segmentation, LifetimeValue, ARPU,GGR,
# Profit Margin,Frequency, LiveAction,FixedOdds, Casino Boss Media and Casino Chartwell, Poker, SuperToto, GameVS and GamesBwin   

Quick Link to Shiny app : 

https://shanbhagnitesh.shinyapps.io/Workplace/

Quick Introduction to Some Important Variables to Understand the Project better :
(Detailed Explanation can be found in Markdown)

• GGR(Gross Gaming Revenue) - [Calculated] Calculated as the Total Profits to the Company. User
Stakes Minus the Total Winnings of the User from all Products

• Ranked Segment - [Calculated] Profitability rank of the segment to which the user belongs. See
Metrics section for details.

• Profit Margin - [Calculated] Total profit margin from the user for bwin.

• Lifetime Value (Indicative) - [Calculated] Descriptive approximate lifetime value of the user for
bwin over his active play period.

• Total Bets Placed - [Calculated] The Total Bets Placed by the User (excluding Poker)

• First&Last_Active_Date - [Calculated] The Very First & Last Transaction date of the User. Note
that it is after First Payment Date and this implies to all Dates as Before Payment Dates Transactions
have been deleted.

• Length of Relationship(In Days) - [Calculated] Last_Day - First_Day, which represents the total
number of days the user has been with the company in that time frame.

• Loyal - [Calculated] Loyalty was calculated as the Last_Day(Sep30) minus the Last Transaction Date
of the User. It would give an hint on whether the user is still with the company and loyal to the
company. These were also Divided into High / Medium and Low Loyalty Class.

• ARPU(Average Revenue Per User Per Login) - [Calculated] Total Profit Per User / Frequency

• Frequency - [Calculated] Total number of Logins. It was also divided into High, Medium and Low
Classes to indicate how frequent the Customer is. It is used as one of the measure to calculate the
Market Segments as explained below.

• Observed Stakes - [Calculated] Based on the Total Stakes of the User, Each user has been distinguished
into 3 Classes: High Stakes which indicates User’s Monetary Value, Medium Stakes and Low Stakes.
Stakes has been taken into consideration because Winnings depends on the User’s game and does not
give an indication about the User’s monetary value.

• Total Stakes/Winnings - [Calculated] Total Stakes and Total Winnings from all 4 Products has been
calculated

• Individual Profits Each Product - [Calculated] Indivdual Profits by each Product from the User.
