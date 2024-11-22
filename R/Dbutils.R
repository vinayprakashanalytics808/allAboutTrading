library(RODBC)
library(rlang)

## Connnection to sql server
conn <- odbcDriverConnect(connection = paste0("Driver={SQL Server Native client 11.0};server=localhost;database=Nifty;trusted_connection=yes;"))
