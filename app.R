library(shiny)
library(jsonlite)
# library(rjson)
library(readr)
library(stringr)
require(reshape2)
require(reshape)
library(dplyr)
library(tidyverse)
require(reshape2)
library(readxl)
library(lubridate)
library(zoo)
library(shinyalert)
library(DT)
library(plotly)
library(kableExtra)
library(reactable)
library(glue)
library(shinyjs)
library(shinycssloaders)
library(data.table)
library(shinyWidgets)
library(stringi)
# source("DButils.R")
# source("functions.R")

ui <- fluidPage(includeCSS("www/design.css"),includeScript("www/design.js"),
                br(),

                # uiOutput("getCom"),
                div(id = 'innerC', tabsetPanel(id = "initial_Tabs",
                                               type = "tabs",
                                               tabPanel("Summary",   br(), fluidRow(
                                                 column(3,uiOutput("getDiff")),
                                                 column(3,uiOutput("getDHP")),
                                                 column(3,uiOutput("getEPS")),
                                                 column(3,uiOutput("getPrice")),
                                                 br()),
                                                 fluidRow(
                                                   column(3,uiOutput("getID")),
                                                   column(3,uiOutput("get5D"))),
                                                 fluidRow(column(4,uiOutput("profit")), actionButton("cwnt", "1D ↓ 5D ↓"),
                                                                                        actionButton("up1dt", "1D ↑ 5D ↓"),
                                                                                        actionButton("up5dt", "1D ↓ 5D ↑"),
                                                                                        actionButton("upt", "1D ↑ 5D ↑"),
                                                               #column(2,radioButtons("one_day","1D",choices = c("0","greater than 0","lesser than 0"),selected = NULL,inline = FALSE,width = NULL)),
                                                               #column(2,checkboxGroupInput("id", label = "id", choices = c("0","greater than 0","lesser than 0")))
                                                 ), br(), reactableOutput("details") %>% withSpinner(color="#0dc5c1")),
                                               tabPanel("Profit Prediction", br(),
                                                        fluidPage(fluidRow(span(column(5,(div(numericInput("investment", "Investment :", value = 100000)))),
                                                                                column(5,div(numericInput("returns", "No of days to get return :", value = 1, max = 10, min = 1))))),
                                                                  br(),br(),
                                                                  fluidRow(span(column(2,uiOutput("cl")),
                                                                                column(10, div(reactableOutput("prof_details")%>% withSpinner(color="#0dc5c1")))))
                                                                  # fluidRow(
                                                                  # column(6, br(), br(), br()
                                                                  #               , div(plotlyOutput("prof_plot")%>% withSpinner(color="#0dc5c1"))
                                                                  #               ),
                                                                  # column(12, div(reactableOutput("prof_details")%>% withSpinner(color="#0dc5c1"))))
                                                        )),
                                               tabPanel("Definitions", br(), tableOutput("def")),
                                               tabPanel("Buying Companies(WishList)", br(), reactableOutput("selected"), br(), br(), div(style = "margin-left:6vh", actionButton("dc", "Delete above Wishlisted Companies from database")))
                                               ,tabPanel("Investment Details", br(),
                                                         tableOutput("holdings"),
                                                         div(style="margin-left:0.5%", htmlOutput("hold_link")),
                                                         br(),br(),
                                                         tableOutput("inv"),
                                                         div(style="margin-left:0.5%", htmlOutput("inv_link")),
                                                         br(),br(),
                                                         tableOutput("statement"),
                                                         div(style="margin-left:0.5%", htmlOutput("sta_link"))),
                                               tabPanel("Profit/Loss", br(), plotlyOutput("plot_profit_loss",width = "400px")),
                ))
)

server <- function(input, output, session) {

  storage <- reactiveValues()
  storage$WishList <- data.frame()

  output$cl <- renderUI({
    clist <- sqlQuery(conn, paste0("select symbol, [5D], concat(symbol, ' (', round([5D],0), ')') as detailed  from Price_Table where symbol not in ('SINTEX.NS') order by [5D] desc"))
    div(style="margin-left:5%", checkboxGroupInput("companies_sel", "Companies",choices = clist$detailed, selected = clist$detailed[1:10]))
  })

  main_data <- sqlQuery(conn,paste0("select top 50 symbol,name,price,yearHighdayHigh,dayHigh,eps,marketCap,pe,[open],[volume],priceAvg50,priceAvg200,CONVERT(TEXT,description) as description,industry,sector,isin,isActivelyTrading,fullTimeEmployees,prevClose,low,[1D],[5D],date,floatShares,outstandingShares,pc,time from Price_Table"))

  #hidden_Columns <- c("symbol","eps", "marketCap", "pe", "description", "industry", "sector", "isin", "prevClose", "low", "open", "pc", "1D", "5D", "isActivelyTrading", "priceAvg50", "priceAvg200", "time", "date", "floatShares", "outstandingShares")

  main_data[['name']] <- sapply(paste0(
    main_data[['name']],
    "_",main_data[['isin']],
    "_",main_data[['pc']],
    "_",main_data[['priceAvg50']],
    "_",main_data[['priceAvg200']]),function(x) htmltools::HTML(unlist(strsplit(x, "_"))[1],
                                                                '<span class="a">','ISIN : ', unlist(strsplit(x, "_"))[2],'</span>&nbsp',
                                                                price_Change("1D", unlist(strsplit(x, "_"))[3]),
                                                                price_Change("5D", unlist(strsplit(x, "_"))[4]),
                                                                '<sup style="font-size: smaller">','50D avg : ', round(as.numeric(unlist(strsplit(x, "_"))[5]),2),'</sup>&nbsp',
                                                                '<sup style="font-size: smaller">',' /&nbsp&nbsp200D avg : ', round(as.numeric(unlist(strsplit(x, "_"))[6]),2),'</sup>&nbsp'
    )
  )
  # browser()
  main_data_nt <- main_data[(main_data$`1D` < 0 & main_data$`5D` < 0),]
  main_data_ut <- main_data[(main_data$`1D` > 0 & main_data$`5D` > 0),]
  main_data_u5dt <- main_data[(main_data$`1D` < 0 & main_data$`5D` > 0),]
  main_data_u1dt <- main_data[(main_data$`1D` > 0 & main_data$`5D` < 0),]

  output$details <- renderReactable(get_react_table(main_data))

  output$def <- renderTable({
    def_data <- sqlQuery(conn,paste0("select * from Defintions"))
    def_data
  })



  insert_Companies <- reactive({
    main_data[req(getReactableState("details"))$selected, c("symbol", "price")]
  })


  output$selected <- renderReactable({
    sqlQuery(conn,paste0("INSERT INTO companies_To_Invest (company,price,active) VALUES ('",insert_Companies()$symbol,"',",insert_Companies()$price, ",1)"))
    storage$WishList <- sqlQuery(conn,paste0("select distinct company, price from companies_To_Invest where active in (1)"))
    reactable(class = "rt",
              selection = "single",
              onClick = "select",
              storage$WishList,
              fullWidth = FALSE,
              outlined = TRUE, borderless = TRUE,
              columns = c(list(company = colDef(html = TRUE,width = 400))))
  })

  delete_Companies <- reactive({
    storage$WishList[req(getReactableState("selected"))$selected, "company"]
  })

  observeEvent(input$dc,{
    sqlQuery(conn,paste0("UPDATE companies_To_Invest SET active = 0 WHERE company in ('", delete_Companies(),"')"))
    storage$WishList <- sqlQuery(conn,paste0("select distinct company, price from companies_To_Invest where active in (1)"))
  })


  md <- main_data %>% filter(low > 0, marketCap > 0, volume > 0, isActivelyTrading == TRUE) %>% pull(symbol)
  hidden_Columns_Profit <- c("time")


  #main_data_selected <- sqlQuery(conn,paste0("select top 1 * from Profit_Table"))

  # main_data_selected <- sqlQuery(conn,paste0("exec sp_returns 3, 100000, ",input$companies_sel))
  # if(nrow(main_data_selected) > 1){
  # output$prof_plot <- renderPlotly({
  #   profit_function_plot(main_data_selected, symbol, open, input$returns, input$investment)
  # })

  # observe({
  #   print(paste0(input$companies_sel,collapse = ','))
  # })

  output$prof_details <- renderReactable({
    req(input$companies_sel)
    # browser()
    main_data_selected <- sqlQuery(conn,paste0("exec sp_returns ", input$returns, ",", input$investment,",'",paste0(gsub("\\(.*","",input$companies_sel),collapse = ','), "'"))
    # main_data_selected <- main_data_selected[order(main_data_selected$symbol, main_data_selected$date),]
    main_data_selected <- cast(main_data_selected , symbol~date, value = colnames(main_data_selected)[dim(main_data_selected)[2]])
    #browser()
    main_data_selected <- main_data_selected[c(1, dim(main_data_selected)[2]:c(dim(main_data_selected)[2] - 5))]

    reactable(class = "rt", rownames = F, as.data.frame(main_data_selected),pagination = FALSE,height = 500,striped = TRUE, highlight = TRUE,compact = TRUE,
              columns = c(list(symbol = colDef(html = TRUE,filterable = TRUE))
                          #,lapply(setNames(hidden_Columns_Profit, hidden_Columns_Profit), function(x){x = colDef(show =F)})
              ))
  })
  # }

browser()
  Statement <- read_excel("Statement/ledger-WMJ575.xlsx", range = "B15:H10000")
  final_Statement <- Statement[rowSums(is.na(Statement)) != ncol(Statement), ]
  total_Investment <- sum(final_Statement[final_Statement$`Voucher Type` == "Bank Receipts",]$Credit,na.rm = TRUE) - sum(final_Statement[final_Statement$`Voucher Type` == "Bank Payments",]$Debit,na.rm = TRUE)
  #Net_Balance <- data.frame(check.names = FALSE, Particulars = paste0('Net Profit(%)', " as of ", Sys.Date()), `Posting Date` =  NA, `Cost Center` = NA,  `Voucher Type` = NA, Debit = NA, Credit = NA, `Net Balance` = (final_Statement$`Net Balance`[final_Statement$Particulars == 'Closing Balance'] - 10000)*100/10000)
  #net <- rbind(final_Statement, Net_Balance)
  #netvalue <- data.frame(row.names = tail(net, n = 2)[['Particulars']], Value = round(tail(net, n = 2)[['Net Balance']],1))


  Investment_History <- as.data.frame(read_excel("Statement/tradebook-WMJ575-EQ.xlsx", range = "B15:M10000"))
  Investment_History <- Investment_History[rowSums(is.na(Investment_History)) != ncol(Investment_History),]
  Investment_History[Investment_History$Symbol == 'MRO-TEK-T', "Symbol"] <- "MRO-TEK"
  Investment_History <- as.data.frame(Investment_History[rowSums(is.na(Investment_History)) != ncol(Investment_History),])
  Investment_History$investment <- Investment_History$Quantity * Investment_History$Price
  Investment_History_v1 <- Investment_History %>% group_by(Symbol, `Trade Type`) %>% summarise(investment = sum(investment))
  Investment_History_v1 <- as.data.frame(Investment_History_v1)
  colnames(Investment_History_v1) <- c("Symbol", "TradeType", "investment")
  Investment_History_v1 <- cast(Investment_History_v1 , Symbol~TradeType, value = 'investment')
  Investment_History_v1$Status <- ifelse(is.na(Investment_History_v1$sell), "Not Sold","Sold")

  if(file.exists("Statement/holdings.csv")){
    holdings <- read_csv("Statement/holdings.csv")
  } else {
    holdings <- data.frame("Instrument" = "NA" , `Qty.` = 0,  `Avg. cost` = 0,  LTP = 0,  `Cur. val` = 0, `P&L` = 0, `Net chg.` = 0,  `Day chg.` = 0, check.names = FALSE)
  }

  holdings <- as.data.frame(holdings)
  colnames(holdings)[which(names(holdings) == "Instrument")] <- "Symbol"
  Investment_History_v1 <- left_join(Investment_History_v1, holdings[c('Symbol', 'Cur. val')], by='Symbol')


  #df <- data.frame(Symbol = c('A', 'B'), Status = c('Sold', 'Not Sold'), buy = c(1000,200), sell = c(200,NA), `Cur. val` = c(NA, 700), check.names = FALSE)
  net <- Investment_History_v1 %>% group_by(Status) %>% summarise(buy = sum(buy), sell = sum(sell, na.rm = TRUE), cur_val = sum(`Cur. val`,na.rm = TRUE))
  net <- as.data.frame(net)
  net$net_calculation <- ifelse(net$Status == "Sold", round((net$sell - net$buy)* 100/net$buy,2), round((net$cur_val - net$buy)* 100/net$buy,2))

  Investment_History_v1$profit <- ifelse(Investment_History_v1$Status == "Sold", round(((Investment_History_v1$sell - Investment_History_v1$buy)*100/Investment_History_v1$buy),1), 0)
  max_profit <- max(Investment_History_v1$profit,na.rm = TRUE)

  total_sold <- sum(Investment_History_v1[Investment_History_v1$Status == "Sold",]$sell)
  total_buy <- sum(Investment_History_v1[Investment_History_v1$Status == "Sold",]$buy)


  net_loss <- paste0("Net Profit : ", round((total_sold - total_buy),1), " : ", round(((total_sold - total_buy) * 100 / total_buy),2), "%")
  Realised <- net$net_calculation[net$Status == "Sold"]
  # if(net$Status == "Not Sold"){
  if("Not Sold" %in% net$Status){
    UnRealised <- net$net_calculation[net$Status == "Not Sold"]
    Current_Status <- round(((net$sell-net$buy)[net$Status == "Sold"] + (net$cur_val-net$buy)[net$Status == "Not Sold"]) * 100 / net$buy[net$Status == "Not Sold"],2)
  } else {
    UnRealised <- 0
    Current_Status <- Realised
  }

  Closing_Balance <- round(as.data.frame(final_Statement)['Net Balance'][as.data.frame(final_Statement)['Particulars'] == "Closing Balance"],2)


  if(Current_Status >= 0){
    col <- '#9add9a'
    til <- 'Net Profit'
  } else {
    col <- '#D22B2B'
    til <- 'Net Loss'
  }


  if(Realised >= 0){
    col_r <- '#9add9a'
    til_r <- 'Realised Profit'
  } else {
    col_r <- '#D22B2B'
    til_r <- 'Realised Loss'
  }

  if(UnRealised >= 0){
    col_ur <- '#9add9a'
    til_ur <- 'UnRealised Profit'
  } else {
    col_ur <- '#D22B2B'
    til_ur <- 'UnRealised Loss'
  }

  if(Closing_Balance >= 0){
    col_cb <- '#9add9a'
    til_cb <- 'Closing Balance'
  } else {
    col_cb <- '#D22B2B'
    til_cb <- 'Closing Balance'
  }
  days_cal <- round(as.double(difftime(Sys.Date(), "2024-04-29", units = "days")),0)

  # browser()
  output$profit <- renderUI({
    if(holdings$Qty. == 0 & holdings$`Cur. val` == 0 & holdings$LTP > 0){
      div(style="margin-left:2%", actionButton(inputId = "pro",
                                               HTML(paste0("One of the security is been sold today and hence the returns will be reflected tommorrow"))))
    } else {
      div(style="margin-left:2%", actionButton(inputId = "pro",
                                               HTML(paste0(
                                                 div(style="text-align:-webkit-left", til, ' : ',span(style=glue("color:{col};font-weight:800;"),Current_Status, " % "), tags$sub("(", til_r , " + ", til_ur, ")")),
                                                 div(style="text-align:-webkit-left", til_r, ' : ',span(style=glue("color:{col_r};font-weight:800;"),Realised, " % "), tags$sub(" in", days_cal, " days")),
                                                 div(style="text-align:-webkit-left", til_ur, ' : ',span(style=glue("color:{col_ur};font-weight:800;"),UnRealised, " % "), tags$sub(" in", days_cal, " days")),
                                                 div(style="text-align:-webkit-left", til_cb, ' : ',span(style=glue("color:{col_cb};font-weight:800;"),Closing_Balance, " Rs "), tags$sub(" in", days_cal, " days"))
                                               ))))
    }

  })



  output$inv <- renderTable({
    Investment_History
  }, caption = as.character(HTML('<span>TradeBook</span>&nbsp; <span id = "z" style="font-size:30px;-webkit-text-stroke-width:thick">&#8595;</span>
                                 <p id="demo"></p><script>var tit = document.getElementById("z");tit.onclick = displayDate;function displayDate() {document.getElementById("demo").innerHTML = Date();}</script>')), caption.placement = "top")

  output$statement <- renderTable({
    final_Statement
  }, caption = "Ledger", caption.placement = "top")

  output$holdings <- renderTable({
    holdings
  }, caption = "Holdings", caption.placement = "top")


  output$inv_link <- renderUI({
    tags$h5(paste0("Tradebook/Transaction Details : ", "https://console.zerodha.com/reports/tradebook"))
  })


  output$sta_link <- renderUI({
    tags$h5(paste0("Ledger Details : ", "https://console.zerodha.com/funds/statement"))
  })


  output$hold_link <- renderUI({
    tags$h5(paste0("Holdings : ", "https://kite.zerodha.com/holdings"))
  })

  observeEvent(input$pro, {
    updateTabsetPanel(session, inputId = "initial_Tabs", selected = "Investment Details")
  })

  # browser()
  output$plot_profit_loss <- renderPlotly({
    plot_ly(Investment_History_v1[Investment_History_v1$Status == "Sold",], x = ~Symbol, y = ~profit,
            type = 'bar', marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)',
                                                                               width = 1))) %>% layout(
                                                                                 paper_bgcolor='antiquewhite',
                                                                                 plot_bgcolor='antiquewhite')
    # %>%
    #   add_annotations(x= 0.5,
    #                   y= max_profit + 5,text = glue("<b>{net_loss}</b>"),showarrow = F)

  })

  dataModal2 <- function() {
    modalDialog(
      reactableOutput("lk"),footer = actionButton("cancel", "Cancel")
    )
  }


  observeEvent(input$cwnt,{
    showModal(dataModal2())
    output$lk <- renderReactable(get_react_table(main_data_nt[order(main_data_nt$`1D`, decreasing = TRUE),]))

  })

  observeEvent(input$up1dt,{
    showModal(dataModal2())
    output$lk <- renderReactable(get_react_table(main_data_u1dt[order(main_data_u1dt$`5D`, decreasing = FALSE),]))

  })

  observeEvent(input$up5dt,{
    showModal(dataModal2())
    output$lk <- renderReactable(get_react_table(main_data_u5dt[order(main_data_u5dt$`1D`, decreasing = FALSE),]))

  })

  observeEvent(input$upt,{
    showModal(dataModal2())
    output$lk <- renderReactable(get_react_table(main_data_ut[order(main_data_ut$`1D`, decreasing = TRUE),]))

  })


  observeEvent(input$cancel, {
    removeModal()
    output$lk <- renderReactable(NULL)
  })



}



shinyApp(ui, server)
