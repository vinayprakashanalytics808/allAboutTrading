conn <- odbcDriverConnect(connection = paste0("Driver={SQL Server Native client 11.0};server=localhost;database=Nifty;trusted_connection=yes;"))

get_Api <- sqlQuery(conn, "select distinct API from APIList")$API


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}


## Round of 2
round_of_two <- function(a){
  return(round(a,2))
}

min_num <- function(a){
  return(round(min(a,na.rm = TRUE),2))
}

max_num <- function(a){
  return(round(max(a,na.rm = TRUE),2))
}


add_collapse_content <- function(x, id) {
  tagList(
    tags$button(
      "data-toggle" = "collapse",
      "data-target" = paste0("#", id),
      "More Info"
    ),
    div(
      "id" = id,
      "class" = "collapse",
      x
    )
  )  %>% as.character()
}


price_Change <- function(title, price){
  if (price > 0)
  {
    return(paste0('<span class="b" style="font-size: smaller">',title,' : ',round(as.numeric(price),2),'&nbsp<i></i></span>&nbsp'))
  } else
  {
    return(paste0('<span class="c" style="font-size: smaller">',title,' : ',round(as.numeric(price),2),'&nbsp<i></i></span>&nbsp'))
  }
}

# df$pc <- do.call(paste, c(df[Stock_price_Change_Columns], sep = '_'))

paste_function <- function(df, df_col = c(a, b)){
  for (i in df_col){
    return(substr(paste0(df,'$',df_col,collapse = ",", sep = ",'_'"), 1,nchar(paste0(df,'$',df_col,collapse = ",", sep = ",'_'"))-4))
  }
}



lag_function <- function(dataframe_to_call, category, open_price, number_of_days){
  ind = number_of_days:98
  category <- enquo(category)
  open_price <- enquo(open_price)
  varname <- paste("Day+", number_of_days , "_open", sep="")
  return(dataframe_to_call %>% group_by({{category}}) %>% mutate({{varname}} := c({{open_price}}[head(ind+1, n())], rep(NA, max(0, n() - length(ind)-1)))))
}

profit_function <- function(dataframe_to_call, category, open_price, number_of_days, profit){
  ind = number_of_days:98
  category <- enquo(category)
  open_price <- enquo(open_price)
  varname <- paste("profit for ", profit, " in ", format(number_of_days, scientific = FALSE), " days", sep = "")
  return(dataframe_to_call %>% group_by({{category}}) %>% mutate({{varname}} := round((profit * (close - c({{open_price}}[head(ind+1, n())], rep(NA, max(0, n() - length(ind)-1))))) / c({{open_price}}[head(ind+1, n())], rep(NA, max(0, n() - length(ind)-1))),0)))
}

back_ground = 'antiquewhite'
profit_function_plot <- function(dataframe_to_call, category, open_price, number_of_days, profit){
       ind = number_of_days:98
       category <- enquo(category)
       open_price <- enquo(open_price)
       varname <- paste("profit/share for ", profit, " in ", format(number_of_days, scientific = FALSE), " days", sep = "")
       new_data <- dataframe_to_call %>% group_by({{category}}) %>% mutate({{varname}} := (profit * (close - c({{open_price}}[head(ind+1, n())], rep(NA, max(0, n() - length(ind)-1))))) / c({{open_price}}[head(ind+1, n())], rep(NA, max(0, n() - length(ind)-1))))
       new_data <- new_data[complete.cases(new_data),]
       new_data$date <- as.Date(new_data$date)
       return(ggplotly(new_data %>% ggplot(aes(x=date,y=!!sym(names(.)[16])))+geom_hline(aes(yintercept=profit), linetype='dashed')+geom_line(aes(color=symbol))+theme(panel.background = element_rect(fill = back_ground),plot.background = element_rect(fill = back_ground),legend.background = element_rect(fill = back_ground))+ ggtitle(colnames(new_data)[16]) + theme(plot.title = element_text(color="black", size=12, face="bold.italic",hjust = 0.5), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())))
   }




hidden_Columns <- c("symbol", "eps", "marketCap", "pe", "description", "industry", "sector", "isin", "prevClose", "low", "open", "pc", "1D", "5D",  "isActivelyTrading", "priceAvg50", "priceAvg200", "time", "date", "floatShares", "outstandingShares")
get_react_table <- function(df){
  reactable(class = "rt", selection = "single" ,onClick = "select", striped = TRUE, highlight = TRUE,compact = TRUE,
          # defaultSelected = c(1),
          rownames = F, defaultPageSize = 15, columns = c(list(name = colDef(html = TRUE,width = 1000,
                                                                             filterable = TRUE)),lapply(setNames(hidden_Columns, hidden_Columns), function(x){x = colDef(show =F)})),
          defaultColDef = colDef(show = T), df, details = function(index) {
            htmltools::div(tags$style("table {
  font-family: arial, sans-serif;
  border-collapse: collapse;
  margin: 40px;
}

td, th {
  border: 1px solid #dddddd;
  text-align: left;
  padding: 2px;
}


tr:nth-child(even) {
}"),tags$table(tags$tr(
  tags$th('Symbol')
  ,tags$th('Sector')
  ,tags$th('Industry')
  ,tags$th('Company Info')
  ,tags$th('PE')
  ,tags$th('Market Cap')
  ,tags$th('prevClose')
  ,tags$th('eps')
  # ,tags$th('low')
  # ,tags$th('open')
)
,tags$tr(
  tags$td(df[index,][['symbol']])
  ,tags$td(df[index,][['sector']])
  ,tags$td(df[index,][['industry']])
  ,tags$td(df[index,][['description']])
  ,tags$td(df[index,][['pe']])
  ,tags$td(format(df[index,][['marketCap']],scientific = FALSE))
  ,tags$td(df[index,][['prevClose']])
  ,tags$td(df[index,][['eps']])
  # ,tags$td(df[index,][['low']])
  # ,tags$td(df[index,][['open']])
)))

          })
}
