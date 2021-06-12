library(shiny)
library(shinydashboard)
library(plotly)
#library(tidyverse)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "Reni Kartika",
      dropdownMenuOutput("msg_menu"),
      dropdownMenuOutput("notif_menu"),
      dropdownMenu(
        type = "tasks",
        taskItem(
          text = "Look at your datacamp progress!",
          value = 77
        )
      )
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard-DWH", tabName = "DashboardDWH", 
                 icon = icon("dashboard"))
      )
      
    ),
    
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "DashboardDWH",
          h2("Dashboard E-Commerce (Assignment DWH)", 
             style="text-align:center;"),
          fluidRow(
            valueBoxOutput("totalProductOrderedBox"),
            valueBoxOutput("totalIncomeBox"),
            valueBoxOutput("totalUniqueUser")
          ),
          
          fluidRow(
            tabBox(
              title = "Summary",
              id = "summary1", height = "520px",
              tabPanel("Income", plotlyOutput("plot1")),
              tabPanel("Product", plotlyOutput("plot2"))
            ),
            box(
              title = "Top 3 Selling Product",
              box(
                width = "480px",solidHeader = TRUE,
                "1",
                h4(textOutput("top1Name")),
                h5(textOutput("top1Price")),
                h4(textOutput("top1Qty"))
              ),
              box(
                width = "480px",solidHeader = TRUE,
                "2",
                h4(textOutput("top2Name")),
                h5(textOutput("top2Price")),
                h4(textOutput("top2Qty"))
              ),
              box(
                width = "480px",solidHeader = TRUE,
                "3",
                h4(textOutput("top3Name")),
                h5(textOutput("top3Price")),
                h4(textOutput("top3Qty"))
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              height = "600px",
              box(
                width = 12,
                solidHeader = TRUE,
                dateRangeInput("dates", h3("Select Date Range"),
                               start = "2021-01-01")
              ),
              box(
                width = 12,
                solidHeader = TRUE,
                plotlyOutput("plot3")
              )
            )
          )
        )
      )
    )
  )
)