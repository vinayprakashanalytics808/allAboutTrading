library(RODBC)
library(rlang)
library(shiny)
library(jsonlite)
library(rjson)
library(stringr)
require(reshape2)
library(dplyr)
library(tidyverse)
require(reshape2)
library(readxl)
library(lubridate)
library(zoo)
library(shinyalert)
library(DT)
library(plotly)
# library(rhandsontable)
# library(kableExtra)
# library(reactable)
library(glue)
library(shinyjs)
library(shinycssloaders)
library(data.table)
# library(stringr)

## Connnection to sql server
conn <- odbcDriverConnect(connection = paste0("Driver={SQL Server Native client 11.0};server=localhost;database=Nifty;trusted_connection=yes;"))

get_Api <- sqlQuery(conn, "select distinct API from APIList")$API

update_Price_Table_to_sql <- function(r_DataFrame, table_Name){
  sqlQuery(conn,paste0("drop table ", table_Name))
  sqlSave(conn, r_DataFrame, tablename = table_Name,varTypes=c(description = "nvarchar(MAX)", name = "nvarchar(MAX)",time = "datetime2"),rownames = FALSE)
}

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

get_Links_int <- function(Description, from, to){
  com <- sqlQuery(conn,paste0("Select symbol, company_Name from companiesList_New ORDER BY slNo desc OFFSET ",from, " ROWS FETCH NEXT ", to, " ROWS ONLY"))
  #com <- sqlQuery(conn,paste0("Select symbol, company_Name from companiesList  where Location = 'India' ORDER BY slNo desc OFFSET ",from, " ROWS FETCH NEXT ", to, " ROWS ONLY"))
  #com_symbol <- capture.output(cat(com$symbol, sep = ","))
  com_symbol <- paste0(com$symbol,collapse = ",")
  Link <- sqlQuery(conn,paste0("select Link+API as Link from APIlist where [Description]  in (","'", Description, "'", ")"))
  return(gsub("companies_to_be_passed", com_symbol, Link))
  #return(str_replace_all(Link, "companies_to_be_passed", com_symbol))
}

main_Data_Get <- function(Description, from, to){
  datalist = list()
  for (j in c(1:13)){
    i <- from
    asd <- c(get_Links_int(Description, i, to))
    while (i < 6185) {
      asd <- c(asd, get_Links_int(Description, i, to))
      i <- i + to
    }
    asd <- asd[-1]

    datalist[[j]] <- rbindlist(lapply(rjson::fromJSON(file = asd[j]), nullToNA), fill = TRUE)
  }
  return(do.call(rbind, datalist))
}

main_data <- as.data.frame(main_Data_Get('To get latest day prices', from = 0, to = 499))
main_company_info <- as.data.frame(main_Data_Get('To get Company Information', from = 0, to = 499))
main_real_Time_price <- as.data.frame(main_Data_Get('To get Real Time Price', from = 0, to = 499))
main_Stock_price_Change <- as.data.frame(main_Data_Get('To get Stock Price Change', from = 0, to = 499))
#historical_Price <- lapply(rjson::fromJSON(file = gsub("2021-09-24", Sys.Date(), gsub("2021-08-24", Sys.Date()-20, get_Links('To get historical prices', TRUE)))), nullToNA) %>% pluck("historicalStockList") %>%  map_dfr(~ as_tibble(.x) %>% unnest_wider(historical))
main_data['yearHigh - dayHigh'] <- round(main_data['yearHigh'],0) - round(main_data['dayHigh'], 0)
main_data['eps'] <- round(main_data['eps'],2)

floating_Shares <-  as.data.frame(rbindlist(lapply(rjson::fromJSON(file = paste0('https://financialmodelingprep.com/api/v4/shares_float/all?apikey=',get_Api)), nullToNA), fill = TRUE))


company_info_Columns <- c('description', 'industry', 'sector', 'isin', 'isActivelyTrading', 'fullTimeEmployees')
real_Time_price_Columns <- c('prevClose', 'low')
Stock_price_Change_Columns <- c('1D', '5D')
floating_Shares_Columns <- c('date', 'floatShares', 'outstandingShares')

main_data <- inner_join(main_data, main_company_info[c(company_info_Columns, 'symbol')], by='symbol')
main_data <- inner_join(main_data, main_real_Time_price[c(real_Time_price_Columns, 'symbol')], by='symbol')
main_data <- inner_join(main_data, main_Stock_price_Change[c(Stock_price_Change_Columns, 'symbol')], by='symbol')
main_data <- inner_join(main_data, floating_Shares[c(floating_Shares_Columns, 'symbol')], by='symbol')

required_Columns <- c('symbol', 'name', 'price', 'yearHigh - dayHigh', 'dayHigh', 'eps', 'marketCap', 'pe', 'open', 'volume', 'priceAvg50', 'priceAvg200', company_info_Columns, real_Time_price_Columns, Stock_price_Change_Columns,floating_Shares_Columns)
hidden_Columns <- c("symbol", "marketCap", "pe", "description", "industry", "sector", "isin", "prevClose", "low", "open", "pc", "1D", "5D", "volume", "isActivelyTrading", "priceAvg50", "priceAvg200", "time", floating_Shares_Columns)

main_data <- main_data[required_Columns][order(main_data['yearHigh - dayHigh'], decreasing = FALSE),] %>% mutate_at(c('fullTimeEmployees'),
                                                                                                                 as.numeric) %>% filter(low > 0,
                                                                                                                                        marketCap > 0,
                                                                                                                                        volume > 0,
                                                                                                                                        isActivelyTrading == TRUE
                                                                                                                                        # between(`yearHigh - dayHigh`, input$getDiff_id[1], input$getDiff_id[2]),
                                                                                                                                        # between(dayHigh, input$getDHP_id[1], input$getDHP_id[2]),
                                                                                                                                        # between(`1D`, input$getID_id[1], input$getID_id[2]),
                                                                                                                                        # between(`5D`, input$get5D_id[1], input$get5D_id[2])
                                                                                                                 )

main_data$pc <- do.call(paste, c(main_data[c('1D', '5D')], sep = '_'))
main_data$time <- Sys.time()
update_Price_Table_to_sql(main_data, "Price_Table")
file.create(paste0(getwd(),"/","updated_Price_Table.txt"))
