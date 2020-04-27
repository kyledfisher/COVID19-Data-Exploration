library(magrittr)
library(xml2)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
library(utils)

insert_report_data <- function(con, missing_dates){
  
  trim<- function(x) return(tstrsplit(x, "\\s|[A-Z]", keep=1) %>% unlist)
  
  raw.path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
  
  missing_dates <- paste0(missing_dates, '.csv')
  data.dt <- lapply(missing_dates, function(x, raw.path) {
    tmp.dt <- read.csv(url(paste0(raw.path, x)))
    colnames(tmp.dt) <- colnames(tmp.dt) %>% gsub('ï..', '', .) %>% gsub('[.|\\/]', '_', .)
    
    tryCatch({
      tmp.dt$Last_Update <- tmp.dt$Last_Update %>% paste
      tmp.dt$Last_Update <- tmp.dt$Last_Update %>% 
        trim %>% 
        parse_date_time(orders=c('%m/%d/%y','%m/%d/%Y','%Y-%m-%d'))
      return(tmp.dt)},
      warning = function(w) {
        message(paste('Warning!  Check file:', x))
      },
      error = function(e) {
        message(paste('Error!  Check file', x))
      }
    )
  }, raw.path) %>% rbindlist(fill=TRUE) 
  
  data.dt$Last_Update <- data.dt$Last_Update %>% as.Date()
  if(grep('^lat$',names(data.dt),ignore.case=TRUE,value=FALSE) %>% length > 0){
    data.dt$Latitude <-data.dt[[grep('^lat$',names(data.dt),ignore.case=TRUE,value=TRUE)]]
  }
  if(grep('^long$',names(data.dt),ignore.case=TRUE,value=FALSE) %>% length > 0){
    data.dt$Longitude <-data.dt[[grep('^long$',names(data.dt),ignore.case=TRUE,value=TRUE)]]
  }
  
  colnames(data.dt) <- colnames(data.dt) %>% tolower
  
  table.template <- dbGetQuery(con, 'SELECT * FROM covid_data.report_data') %>% data.table %>% .[0,]
  table.template <- table.template[,-'id']
  
  rbind(table.template, data.dt, fill=TRUE)
  dbWriteTable(con, c('covid_data','report_data'), data.dt, append=TRUE, row.names=FALSE)
  
}

xml.path <- 'https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports'
report.file <- download_html(xml.path)
html.read <- read_html(report.file)
csv.list <- xml_text(html.read) %>% strsplit(split='\\n') %>% unlist

dates.list <- lapply(csv.list, function(x) {
  if (grepl(pattern='.csv', x, fixed=TRUE)) { 
    regex <- regexpr('\\d{2}-\\d{2}-\\d{4}.csv', x)
    return(substr(x, start=regex[[1]], stop=regex[[1]]+attr(regex, 'match.length')))
  }
}) %>% unlist 
dates.list <- dates.list %>% lapply(., function(x){gsub('*.csv$', '', x)}) %>% unlist

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "Control_Test",
                 host = "10.12.50.107", port = 5432, 
                 user = 'covid_users', password = 'thissucks19')
report_data <- dbGetQuery(con, 'SELECT last_update FROM covid_data.report_data')

missing_dates <- setdiff(dates.list %>% paste, format(report_data$last_update, "%m-%d-%Y") %>% paste %>% unique)
missing_dates <- dates.list[1:10]

if(length(missing_dates) > 0){
  insert_report_data(con, missing_dates)
}









