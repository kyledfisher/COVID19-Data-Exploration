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

# Choose country to look at
country.switch <- 'France'

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
curr_csvs.list <- list.files(paste0(git.path, '/Code/COVID19-Data-Exploration/data/'))

# Fetch raw csv data
raw.path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
suppress <- lapply(dates.list, function(x, raw.path, curr_csvs.list) {
    if (!(x %in% curr_csvs.list))  # Only download most recent csv
        download.file(paste0(raw.path, x), paste0(git.path,'/Code/COVID19-Data-Exploration/data/',x))
}, raw.path, curr_csvs.list)

# trim whitespace and any trailing digits
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
            parse_date_time(orders=c('%m/%d/%y','%m/%d/%Y','%Y-%m-%d'))
        return(tmp.dt)},
        warning = function(w) {
            message(paste('Warning!  Check file:', x))
        },
        error = function(e) {
            message(paste('Error!  Check file', x))
        }
    )
}) %>% rbindlist(fill=TRUE)

data.dt[data.dt$Recovered%>%is.na, 'Recovered'] <- 0
data.dt[data.dt$Deaths%>%is.na, 'Deaths'] <- 0
data.dt[data.dt$Confirmed%>%is.na, 'Confirmed'] <- 0


country_cases.dt <- data.dt[data.dt$Country_Region==country.switch,] %>% 
    group_by(Last_Update) %>% 
    summarize(Recovered=max(Recovered),Deaths=max(Deaths)) %>% 
    data.table

# Make separate data table for confirmed, since it can be an order of magnitude
# higher than recovered/death reports.
country_confirmed.dt <- data.dt[data.dt$Country_Region==country.switch,] %>% 
    group_by(Last_Update) %>% 
    summarize(Confirmed=max(Confirmed)) %>% data.table

melted.dt <- melt(country_confirmed.dt, id.vars='Last_Update', variable.name = 'Cases', 
                  value.name='Number.Reported')
ggplot(melted.dt, aes(x=Last_Update, y=Number.Reported, color=Cases)) +
    geom_point() + 
    geom_line()


melted.dt <- melt(country_cases.dt, id.vars='Last_Update', variable.name = 'Cases', 
                  value.name='Number.Reported')
ggplot(melted.dt, aes(x=Last_Update, y=Number.Reported, color=Cases)) +
    geom_point() + 
    geom_line()
