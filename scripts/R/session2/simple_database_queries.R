library(data.table)
library(sf)
library(RPostgreSQL)
library(magrittr)


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "Control_Test",
                 host = "10.12.50.107", port = 5432, 
                 user = 'covid_users', password = 'thissucks19')

report_data <- dbGetQuery(con, 'SELECT * FROM covid_data.dummy_table') %>% data.table

select_example <- dbGetQuery(con, "SELECT * FROM covid_data.dummy_table WHERE deaths > 100") %>% data.table

update_example <- dbGetQuery(con, "UPDATE covid_data.dummy_table SET combined_key = 'This is an update', recovered=1 WHERE country_region = 'Mainland China'") %>% data.table

insert_example <- dbExecute(con, "INSERT INTO covid_data.dummy_table (confirmed, deaths, recovered) VALUES (1000, 0, 100)")

# need permission to do this
delete_example <- dbGetQuery(con, "DELETE FROM covid_data.dummy_table where recovered = 100") %>% data.table

dbDisconnect(con)