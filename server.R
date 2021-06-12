library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(zoo)
library(lubridate)

shinyServer(function(input, output, session){
  #total product yang kejual
  productSale <- read_csv("Product Sale Fact.csv")
  timeDimension <- read_csv("Time Dimension.csv")
  productDimension <- read_csv("Product Dimension.csv")
  userDimension <- read_csv("User Dimension.csv")
  
  totalProductOrdered <- productSale %>%
    summarize(sum(TotalProductOrdered))%>%
    as.data.frame()
  
  totalProductOrdered <- format(totalProductOrdered[1,1],
                                big.mark = ".")
  
  #total Income yang diperoleh
  totalIncome <- productSale %>%
    summarize(sum(TotalIncome))%>%
    as.data.frame()
  
  totalIncome <- format(totalIncome[1,1], big.mark = ".")
  
  #summary income
  d <- productSale%>%
    left_join(timeDimension, by="TimeID")%>%
    group_by(Month, Year)%>%
    summarize(TotalIncome = sum(TotalIncome),
              TotalProductOrdered = sum(TotalProductOrdered))%>%
    arrange(Year, Month)%>%
    mutate(yearMonth = paste0(Year, "-", sprintf("%02d", Month)),
           yearMonth = as.yearmon(yearMonth),
           TotalIncome = TotalIncome/10^6)
  
  spline_int <- as.data.frame(spline(d$yearMonth, d$TotalIncome))
  
  #summary product
  spline_int2 <- as.data.frame(spline(d$yearMonth, d$TotalProductOrdered))
  
  #top 3
  top3 <- productSale %>%
    left_join(productDimension, by = "ProductID")%>%
    group_by(ProductID)%>%
    summarize(TotalProductOrdered = sum(TotalProductOrdered))%>%
    arrange(desc(TotalProductOrdered))%>%
    head(3)%>%
    left_join(productDimension, by = "ProductID")%>%
    mutate(ProductPrice = paste0("Rp", format(ProductPrice, big.mark = ".", decimal.mark = ",")))%>%
    select(ProductID, ProductName, ProductPrice, ProductCategoryName, TotalProductOrdered)
  
  
  #HEADER MENU
  output$msg_menu <- renderMenu({
    pesan <- read_csv("pesan.csv")
    messages <- apply(pesan, 1, function(row) {
      messageItem(from = row[["from"]],
                  message = row[["message"]],
                  if(row[["gender"]] == "Female"){
                    icon = icon("female")
                  }else{
                    icon = icon("male")
                  },
                  href = row[["link"]])
    })
    dropdownMenu(type = "message", .list = messages)
  })
  
  output$notif_menu <- renderMenu({
    notif <- read_csv("notif.csv")
    notiff <- apply(notif, 1, function(row) {
      notificationItem(text = row[["text"]],
                  href = row[["link"]])
    })
    dropdownMenu(type = "notification", .list = notiff)
  })
  
  output$totalProductOrderedBox <- renderValueBox({
    valueBox(
      totalProductOrdered, "Total Product Ordered", icon = icon("box"),
      color = "yellow"
    )
  })
  
  output$totalIncomeBox <- renderValueBox({
    valueBox(
      paste0("Rp", totalIncome), "Total Income", icon = icon("money"),
      color = "teal"
    )
  })
  
  output$totalUniqueUser <- renderValueBox({
    valueBox(
      nrow(userDimension), "Total Unique User", icon = icon("user"),
      color = "olive"
    )
  })
  
  output$plot1 <- renderPlotly({
    ggplotly(
        ggplot()+
        geom_line(data = spline_int, aes(x = x, y = y),
                  col = "#D94B2B", show.legend = F, size = 2)+
        geom_area(data = spline_int, aes(x = x, y = y),
                  fill = "#D9BB96", show.legend = F)+
        labs(
          x = "Year",
          y = "Total Income (in Million Rupiah)"
        )+
        lims(y = c(0, 10^4))+
        theme_minimal()+
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())
    )
  })
  
  output$plot2 <- renderPlotly({
    ggplotly(
      ggplot()+
        geom_line(data = spline_int2, aes(x = x, y = y),
                  col = "#597A57", show.legend = F, size = 2)+
        geom_area(data = spline_int2, aes(x = x, y = y),
                  fill = "#91C78D", show.legend = F)+
        labs(
          x = "Year",
          y = "Total Products"
        )+
        lims(y = c(0, 4*10^4))+
        theme_minimal()+
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())
    )
  })
  
  output$top1Name <- renderText({
    paste(top3$ProductID[1], top3$ProductName[1])
  })
  
  output$top1Price <- renderText({
    top3$ProductPrice[1]
  })
  
  output$top1Qty <- renderText({
    paste(top3$TotalProductOrdered[1], "Products Ordered")
  })
  
  output$top2Name <- renderText({
    paste(top3$ProductID[2], top3$ProductName[2])
  })
  
  output$top2Price <- renderText({
    top3$ProductPrice[2]
  })
  
  output$top2Qty <- renderText({
    paste(top3$TotalProductOrdered[2], "Products Ordered")
  })
  
  output$top3Name <- renderText({
    paste(top3$ProductID[3], top3$ProductName[3])
  })
  
  output$top3Price <- renderText({
    top3$ProductPrice[3]
  })
  
  output$top3Qty <- renderText({
    paste(top3$TotalProductOrdered[3], "Products Ordered")
  })
  
  output$plot3 <- renderPlotly({
    selectedDf <- reactive({
      productSale %>%
        left_join(productDimension, by = "ProductID")%>%
        left_join(timeDimension, by = "TimeID")%>%
        filter(Date <= as.Date(input$dates[2]) & Date >= as.Date(input$dates[1]))%>%
        group_by(ProductCategoryName, Month, Year)%>%
        summarize(TotalProductOrdered = sum(TotalProductOrdered))%>%
        mutate(yearMonth = paste0(Year, "-", sprintf("%02d", Month)),
               yearMonth = as.yearmon(yearMonth))
    })
    
    
    ggplotly(
      selectedDf() %>%
        ggplot(aes(x = yearMonth, y = TotalProductOrdered,
                   fill = ProductCategoryName))+
        geom_col(position = "dodge")+
        labs(
          x = "Month Year",
          y = "Total Products"
        )+
        lims(y = c(0, 1.5*10^4))+
        theme_minimal()+
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())
    )
    
  })
  
})