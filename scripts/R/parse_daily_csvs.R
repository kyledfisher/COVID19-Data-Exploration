# parse_daily_csvs.R
#
# Update data/ directory with newly added csvs from the main CSSE repo located 
# at https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports.
#
# Author: Z. Wallace
# Created: 3.29.20


library(magrittr)
library(xml2)
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

trim<- function(x) return(sub('\\s\\d$', '', x))

# x <- dates.list[[2]]
data.dt <- lapply(dates.list, function(x) {
    tmp.dt <- fread(paste0(git.path,'/Code/COVID19-Data-Exploration/data/',x)) %>% data.table
    
    # Fix formatting for first 10 days
    if (tstrsplit(x, split='\\.', keep=1) < '02-02-2020') {
        colnames(tmp.dt) <- colnames(tmp.dt) %>% gsub('[ \\/]', '_', .)  # sub spaces and slashes for underscore
        tryCatch(
            tmp.dt$Last_Update <- substr(tmp.dt$Last_Update, 1, 9) %>% 
                trim %>% 
                parse_date_time(orders='m/d/y') %>% 
                substr(., 1, 10),
            warning = function(w) {
                message(x)
            }
        )
    } else {
        # Truncate Last_Update to daily values
        tmp.dt$Last_Update <- substr(tmp.dt$Last_Update, 1, 10)
    }
    
    return (tmp.dt)
}) %>% rbindlist(fill=TRUE)

sum(data.dt[data.dt$Country_Region=='US', 'Deaths'], na.rm = TRUE)
data.dt%>% group_by(c(Country_Region, Deaths))

data.dt[which(data.dt$Deaths %>% is.na), 'Deaths'] <- 0
melted.dt <- data.dt[which(data.dt$`Country/Region`=='US'),]  %>%
    melt.data.table(id.vars='Last Update',measure.vars='Deaths', value.name='Deaths')

ggplot(data=melted.dt) + 
    geom_bar(mapping=aes(`Last Update`))
sum(melted.dt$Deaths)

