# parse_daily_csvs.R
#
# Update data/ directory with newly added csvs from the main CSSE repo located 
# at https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports.


file <- readLines(con='/Users/zach/Code/COVID19-Data-Exploration/data/csse_covid_19_daily_reports')
file[300]
grep('.csv', file, fixed=TRUE, value=TRUE)
