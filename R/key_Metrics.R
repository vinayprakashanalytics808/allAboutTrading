measure <- "marketCap"
company <- "MUKKA.NS"



com <- sqlQuery(conn,paste0("Select symbol, company_Name from companiesList_New"))

key_metrics <- as.data.frame(rbindlist(lapply(rjson::fromJSON(file = paste0('https://financialmodelingprep.com/api/v3/key-metrics/',company,'?period=quarter&apikey=',get_Api)), nullToNA), fill = TRUE))
key_metrics$date <- as.Date(key_metrics$date)

#colSums(holdings==0)
