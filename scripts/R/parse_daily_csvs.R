# parse_daily_csvs.R
#
# Update data/ directory with newly added csvs from the main CSSE repo located 
# at https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports.
#
# Author: Z. Wallace
# Created: 3.29.20


library(magrittr)
library(xml2)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)

# Set paths 
git.path <- Sys.getenv('HOME')  # Where the base COVID19-Data-Exploration folder lives.

# Pull in list of daily data. 
xml.path <- 'https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports'
report.file <- download_html(xml.path)
html.read <- read_html(report.file)
csv.list <- xml_text(html.read) %>% strsplit(split='\\n') %>% unlist

# Parse html for the csv names
dates.list <- lapply(csv.list, function(x) {
    if (grepl(pattern='.csv', x, fixed=TRUE)) { 
        regex <- regexpr('\\d{2}-\\d{2}-\\d{4}.csv', x)
        return(substr(x, start=regex[[1]], stop=regex[[1]]+attr(regex, 'match.length')))
    }
})
dates.list[sapply(dates.list, is.null)] <- NULL
dates.list <- dates.list

# Fetch raw csv data
raw.path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
suppress <- lapply(dates.list, function(x, raw.path) {
    download.file(paste0(raw.path, x), paste0(git.path,'/Code/COVID19-Data-Exploration/data/',x))
}, raw.path)

# trim whitespace and any trailing digits
#trim<- function(x) return(sub('\\s\\d$', '', x))
trim<- function(x) return(tstrsplit(x, "\\s|[A-Z]", keep=1) %>%unlist)

# x <- dates.list[[1]]
data.dt <- lapply(dates.list, function(x) {
    
    # Fix column names and standardize date formats
    tmp.dt <- fread(paste0(git.path,'/Code/COVID19-Data-Exploration/data/',x)) %>% data.table
    colnames(tmp.dt) <- colnames(tmp.dt) %>% gsub('[ \\/]', '_', .)  # sub spaces and slashes for underscore
    # Fix date formatting for first 10 days
    tryCatch({
        tmp.dt$Last_Update <- tmp.dt$Last_Update %>% 
            trim %>% 
            parse_date_time(orders=c('%m/%d/%y','%m/%d/%Y','%Y-%m-%d'))# %>% 
            #substr(., 1, 10)
        return(tmp.dt)},
        warning = function(w) {
            message(paste('Warning!  Check file:', x))
        },
        error = function(e) {
            message(paste('Error!  Check file', x))
        }
    )
}) %>% rbindlist(fill=TRUE)

us_deaths.dt <- data.dt[data.dt$Country_Region=='US',] %>% group_by(Last_Update) %>% summarize(Deaths=max(Deaths)) %>% data.table
sum(us_deaths.dt$Deaths, na.rm=TRUE)

ggplot(data=us_deaths.dt) + 
    geom_line(mapping=aes(Last_Update))

