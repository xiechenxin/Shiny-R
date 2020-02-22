#add the required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(dplyr)
library(plotly)

#read in the datamart file
db <- read_csv("C:\\Users\\rsharma3\\Documents\\GitHub\\RGroupProject\\Marketing_Datamart_3.csv")

#convert categorical variables to factors
colsCONVERT <- c('COUNTRY', 'LANGUAGE', 'GENDER', 'FOFirstActiveWeekday', 'FOLastActiveWeekday', 
                 'LAFirstActiveWeekday', 'LALastActiveWeekday', 'Application', 'FirstPayWeekday',
                 'FirstActWeekday', 'FirstSpWeekday', 'FirstCaWeekday', 'FirstGaWeekday', 'FirstPoWeekday'
)
db[colsCONVERT] <- lapply(db[colsCONVERT], factor)

str(db)

#create the UI using dashboard template
ui <- dashboardPage(
  dashboardHeader(title='Marketing Datamart Dashboard'),
  dashboardSidebar(
    #put all the controls in the Dashboard Sidebar
    sidebarMenu(
      #put slider for average size of the bet, in Euros, made by the user over all his/her games 
      sliderInput('avgBetSLIDER', 'Average bet value',
                  value = min(db$Daily_Stakes_size),
                  min = min(db$Daily_Stakes_size),
                  max = max(db$Daily_Stakes_size),
                  step = 5
      ),
      #put slider for the total number of bets placed by the user over all his/her games
      sliderInput('totBetSLIDER', 'Total bets played',
                  value = min(db$Daily_Bets_SUM),
                  min = min(db$Daily_Bets_SUM),
                  max = max(db$Daily_Bets_SUM),
                  step = 50
      ),
      sliderInput('mostActiveSLIDER', 'Maximum Active Days',
                  value = min(db$maxActiveDays),
                  min = min(db$maxActiveDays),
                  max = max(db$maxActiveDays),
                  step = 10
      ),
      #sorts the whole dataset by GGR generated per user and concentrates on analysing only the top 10% 
      radioButtons('top10PROF',
                   'Top 10% customers by GGR',
                   c('All' = 1, 'Top 10%' = 0.1),
                   selected = 1
                   )
    )
  ),
  #put all the charts and graphs in the Dashboard body
  dashboardBody(
    #first row is for a snapshot of major performance metrics
    fluidRow(
      #info about how many customers is the dashboard showing at the moment
      valueBoxOutput('totUSERS', width=2),
      #info about how much percentage of customers is the dashboard showing at the moment
      valueBoxOutput('pctTotUSERS', width=2),
      # info about the total amount of stakes, in Euros, made by the above customers 
      valueBoxOutput('totSTAKES', width=2),
      # info about the stakes per bet, in Euros, made by the above customers 
      valueBoxOutput('avgSTAKES', width=2),
      # info about the total amount of profit/loss, i.e. the Gross Gaming Revenue bwin earned/lost, in Euros, on the above customers
      valueBoxOutput('totPROFITS', width=2),
      # info about the average Gross Gaming Revenue (GGR) per customer
      valueBoxOutput('ARPU', width=2)
    ),
    #row showing key demographic data that the marketing team can look at based on the filters in the Sidebar
    fluidRow(
      column(width = 3,
             plotly::plotlyOutput('gender')),
      column(width = 3,
             plotly::plotlyOutput('age')),
      column(width = 3,
             plotly::plotlyOutput('country')),
      column(width = 3,
             plotly::plotlyOutput('language'))
      #width of all columns should total 12
      
    ),
    #row showing key demographic data that the marketing team can look at based on the filters in the Sidebar
    fluidRow(
      column(width=3,
             plotly::plotlyOutput('convDelay')),
      column(width=3,
             plotly::plotlyOutput('actWeekday')),
      column(width=3,
             plotly::plotlyOutput('active30')),
      column(width=3,
             plotly::plotlyOutput('application'))
    )
  )
)

#define the server side code for object to be rendered on the ui dashboard 
server <- function(input, output, session){
  #render the first row of boxes containing key performance metrics
  #calculation for value box on total customers Dashboard shows at the given moment
  output$totUSERS <- renderValueBox({
      sel <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER) %>%
      nrow()
    valueBox(sel, 
             'nr of Customers',
             icon = icon('users'),
             color = 'purple'
    )
  })
  #calculation for value box on % of users Dashboard shows at the given moment
  output$pctTotUSERS <- renderValueBox({
    tot <- nrow(db)
    sel <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER) %>%
      nrow()
    valueBox(paste0(round(sel/tot*100,2),'%'), 
             '% of total customers',
             icon = icon('users'),
             color = 'purple'
    )
  })
  #calculation for value box on total stakes wagered in Euros
  output$totSTAKES <- renderValueBox({
    calcdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER)
    valueBox(paste0(round(sum(calcdb$Daily_Stakes_SUM)/1000,0), 'K'), 
             'Total Stakes Wagered (Turnover in Euros)',
             icon = icon('coins'),
             color = 'yellow'
    )
  })
  #calculation for value box on average bet size in Euros
  output$avgSTAKES <- renderValueBox({
    calcdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER)
    valueBox(paste0(round(mean(calcdb$Daily_Stakes_SUM),2)), 
             'Stakes per customer (in Euros)',
             icon = icon('coins'),
             color = 'yellow'
    )
  })
  #calculation for value box on Gross Gaming Revenues (Profits_SUM) in Euros
  output$totPROFITS <- renderValueBox({
    calcdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER) 
    valueBox(paste0(round(sum(calcdb$Daily_Profits_SUM)/1000,0), 'K'), 
             'Gross Gaming Revenues (in Euros)',
             icon = icon('euro'),
             color = 'green'
    )
  })
  #calculation for value box on Gross Gaming Revenues (Profits_SUM) per customer in Euros
  output$ARPU <- renderValueBox({
    calcdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER) 
    valueBox(paste0(round(mean(calcdb$Daily_Profits_SUM),2)), 
             'Revenue per Customer (in Euros)',
             icon = icon('euro'),
             color = 'green'
    )
  })
  #Plot showing gender distribution
  output$gender <- plotly::renderPlotly({
    plotdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER)
    ggplot(plotdb, aes(GENDER)) + 
      geom_bar(fill='steelblue') + 
      #geom_text(aes(label=scales::percent(..count../sum(..count..))), stat='count', vjust=1.6, color='black', size=3) + 
      theme_minimal() + 
      theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
      labs(title= "Gender Distribution", x="Gender", y= "nr of customers")
  })
  #Plot showing Top 10 languages
  output$language <- plotly::renderPlotly({
    plotdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER)  %>%
      filter(maxActiveDays>=input$mostActiveSLIDER) %>%
      count(LANGUAGE, sort=TRUE) %>%
      top_n(10)
    ggplot(plotdb, aes(x=reorder(LANGUAGE,n), y=n)) + 
      geom_bar(stat='identity', fill='orange') + 
      coord_flip() +
      theme_minimal()+ 
      theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
      labs(title="Top 10 Languages", x="Language", y="nr of customers")
  })
  #chart showing Top 10 Applications
  output$application <- plotly::renderPlotly({
    plotdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER)  %>%
      filter(maxActiveDays>=input$mostActiveSLIDER) %>%
      count(Application, sort=TRUE) %>%
      top_n(10)
    ggplot(plotdb, aes(x=reorder(Application,n), y=n)) + 
      geom_bar(stat='identity', fill='brown') + 
      coord_flip() +
      theme_minimal() + 
      theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
      labs(title="Top 10 Applications ", x="Application", y="nr of customers")
  })
  #Plot showing age distribution
  output$age <- plotly::renderPlotly({
    plotdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER)
    ggplot(data=plotdb, aes(AGE)) + 
      geom_histogram(fill='seagreen4') + 
      theme_minimal() + 
      theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
      labs(title="Age Distribution", x="Age", y="nr of customers")
  })
  #Plot showing Top 10 countries
  output$country <- plotly::renderPlotly({
    plotdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER) %>%
      count(COUNTRY, sort=TRUE) %>%
      top_n(10)
    ggplot(plotdb, aes(x=reorder(COUNTRY,n), y=n)) + 
      geom_bar(stat='identity', fill='purple4') + 
      coord_flip() +
      theme_minimal() +
      theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
      labs(title="Top 10 Countries", x="Country", y="nr of customers")
  })
  #chart showing delay in deposit payment from registration date 
  output$convDelay <- plotly::renderPlotly({
    plotdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER)
    ggplot(data=plotdb, aes(daydiff_FistPay_RegDate)) + 
      geom_histogram(fill='gold1') + 
      theme_minimal() + 
      theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
      labs(title="Conversion delay (in days)", x="Deposit date past registration date", y="nr of customers")
  })
  #chart showing most popular first weekday
  output$actWeekday <- plotly::renderPlotly({
    plotdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER)
    ggplot(plotdb, aes(FirstActWeekday)) + 
      geom_bar(fill='tomato') + 
      theme_minimal() + 
      theme(text = element_text(size=8), axis.text.x = element_text(angle=45, hjust=1)) + 
      labs(title= "Most popular first active playday", x="Weekday", y= "nr of customers")
  })
  #chart showing bets size of customers active in last 30 days
  output$active30 <- plotly::renderPlotly({
    plotdb <- db %>%
      top_n(as.numeric(input$top10PROF)*nrow(db), wt=Daily_Profits_SUM) %>%
      filter(Daily_Stakes_size>=input$avgBetSLIDER) %>%
      filter(Daily_Bets_SUM>=input$totBetSLIDER) %>%
      filter(maxActiveDays>=input$mostActiveSLIDER)
    ggplot(plotdb, aes(x=ActiveInLast30Days, y=Daily_Stakes_size)) +
      geom_point(fill='darkseagreen4') + 
      theme_minimal() +
      theme(text = element_text(size=8), axis.text.x = element_text(hjust=1)) + 
      labs(title="Recently Active Customers", x="Active in last 30 days", y="Avg Bet Value") #smaller bets size in recent active users
  })
  
}

shinyApp(ui = ui, server = server)

#############discarded plots not giving much info#######################################

# ggplot(db, aes(x=Daily_Stakes_size, y=Daily_Profits_size, color=GENDER)) +
#   geom_point()
# ggplot(db, aes(x=Daily_Stakes_SUM, y=Daily_Bets_SUM, color=GENDER)) +
#   geom_point()




