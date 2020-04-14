# This is where the data will be copied to.
git_path = "C:/Users/rensm/Documents/SURFDRIVE/Shared/Covid-19_data_and_plots"

# NB: Plots will be stored in the subfolder 'plots'

# The nubmer of days that is used to create the rolling average of new data coming in.
window_days = 5

## To do:
# - Create a new country called 'world', and let it run with the rest of the analyses.

# - Include the window size in the exported figure names.

# - Set the xlimit of the timeseries figures to start at 0

# - Add important dates for different countries (based on quarantine mesarues for example)
## Started to include deaths from elderly homes: France (06-apr), Belgium (06-apr)
################################################################################
################################################################################
################################################################################

# Clear the memory, except for user paramaters
rm(list = setdiff(ls(), c("git_path", "window_days")))

# these libraries are necessary for retrieving the data
library(readxl)
library(httr)

# these libraries are necessary for feature construction and plotting 
library(ggplot2)
library(dplyr)
library(RcppRoll)

################################################################################
################################################################################
################################################################################
# RETRIEVING DATA

# A function to check if the URL exists
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)
  
  if (is.null(res$result) || 
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(TRUE)
    
  } else {
    return(TRUE)
  }
  
}

#create the URL where the dataset is stored with automatic updates every day
most_recent_day = format(Sys.Date(), "%Y-%m-%d")
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", most_recent_day, ".xlsx", sep = "")

does_it_exist = url_exists(url)
if (! does_it_exist) {
  message(sprintf('<%s> did not exist, searching for earlier dates...',  most_recent_day))
}

while (! url_exists(url, quiet = TRUE)) {
  most_recent_day = format(as.Date(most_recent_day, format = '%Y-%m-%d') - 1, "%Y-%m-%d")
  message(sprintf('Looking for <%s>..', most_recent_day))
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", most_recent_day, ".xlsx", sep = "")
}
if (! does_it_exist) {
  message(sprintf('Continuing with <%s> as the most recent data available...',  most_recent_day))
}

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))


#read the Dataset sheet into “R”
newlocation = file.path(git_path, "archive")
newfile = sprintf("COVID-19-geographic-disbtribution-worldwide-%s.xlsx", most_recent_day)
dir.create(newlocation, showWarnings = FALSE)
success = file.copy(from=tf, to=file.path(newlocation,newfile), 
                    overwrite = TRUE, recursive = FALSE, 
                    copy.mode = TRUE)

if (success) {
  message(sprintf('Copied the latest data (from <%s>) to: %s', most_recent_day, newlocation))
} else {
  message('WARNING: Somehow, it failed... Check error log.')
}

################################################################################
################################################################################
################################################################################
# FEATURE CONSTRUCTION
data = read_excel(file.path(newlocation,newfile))

data[ ,"date"] = as.POSIXct(data$dateRep, format = '%Y-%m-%d %H:%M:%S') # get the date in the right format

data = data[order(data$date), ] # make sure it's sorted for cumulative measures

## Create 'world' as a country:
data_world = data.frame()
for (day in unique(data$date)) {
  # Combine all countries in one world
  
}

# Cases_on_an_international_conveyance_Japan is weird... negative cases, small population, few entries, not a real country. I'm omitting it.

# data[which.max(data$cumulative_deaths_norm), 'countriesAndTerritories']
# data[which.min(data$cases), 'countriesAndTerritories']
message('Removing <Cases_on_an_international_conveyance_Japan> as it has negative cases, few entries and is not a real country..')
data = data[data$countriesAndTerritories != "Cases_on_an_international_conveyance_Japan", ] 

## Feature construction
# Add cumulative counts
data = mutate(group_by(data,countriesAndTerritories), cumulative_deaths = cumsum(deaths))
data = mutate(group_by(data,countriesAndTerritories), cumulative_cases = cumsum(cases))
data = mutate(group_by(data,dateRep), cumulative_deaths_worldwide = cumsum(deaths))
data = mutate(group_by(data,dateRep), cumulative_cases_worldwide = cumsum(cases))

# Normalize per 100.000 capita
data = mutate(group_by(data,countriesAndTerritories), cumulative_deaths_norm = cumsum(deaths) / popData2018 * 100000)
data = mutate(group_by(data,countriesAndTerritories), cumulative_cases_norm = cumsum(cases) / popData2018 * 100000)

pop_by_country = data.frame()
idx = 0

data[,"day_n"] = as.integer(difftime(data$date, min(data$date), units = "days"))

with_cases = data[data$cases > 0, ]
data[,"day_n_since_first_case_worldwide"] = as.integer(difftime(data$date, min(with_cases$date), units = "days"))
with_deaths = data[data$deaths > 0, ]
data[,"day_n_since_first_death_worldwide"] = as.integer(difftime(data$date, min(with_deaths$date), units = "days"))


for (country in unique(data$countriesAndTerritories)) {
  idx = idx + 1
  pop_by_country[idx, 'country'] = country
  pop_by_country[idx, 'popData2018'] = max(data[data$countriesAndTerritories == country, 'popData2018'], na.rm=T)
  pop_by_country[idx, 'first_day_death'] = data[(data$countriesAndTerritories == country) & (data$deaths > 0),]$date[1]
  pop_by_country[idx, 'first_day_case'] = data[(data$countriesAndTerritories == country) & (data$cases > 0),]$date[1]
  
  cur_country = data[data$countriesAndTerritories == country, ]
  cur_country_idx = which(data$countriesAndTerritories == country)
  
  # Add day_count from first death and first case
  data[cur_country_idx, "day_n_since_first_death"] = as.integer(difftime(cur_country$date, pop_by_country[idx, 'first_day_death'], units = "days"))
  data[cur_country_idx, "day_n_since_first_case"] = as.integer(difftime(cur_country$date, pop_by_country[idx, 'first_day_case'], units = "days"))
  
  # pop_by_country[idx, 'first_day_death']
  
  for (i in 1:nrow(cur_country)) {
    cur_day = cur_country[i,]
    
    # within window
    within_window = cur_country[(cur_country$day_n > as.numeric(cur_day["day_n"] - window_days)) &  (cur_country$day_n <= as.numeric(cur_day["day_n"])), ]
    
    data[cur_country_idx[i], sprintf('avg_prev_%d_days_cases', window_days)] = sum(within_window$cases) / window_days
    data[cur_country_idx[i], sprintf('avg_prev_%d_days_deaths', window_days)] = sum(within_window$deaths) / window_days
    
    data[cur_country_idx[i], sprintf('avg_prev_%d_days_cases_norm', window_days)] = sum(within_window$cases) / window_days / pop_by_country[idx, 'popData2018'] * 100000
    data[cur_country_idx[i], sprintf('avg_prev_%d_days_deaths_norm', window_days)] = sum(within_window$deaths) / window_days / pop_by_country[idx, 'popData2018'] * 100000
    
    # Get the doubling rate
    data[cur_country_idx[i], sprintf('doubling_days_%d_rolling_deaths', window_days)] = cur_day$cumulative_deaths / (sum(within_window$deaths) / window_days)
    data[cur_country_idx[i], sprintf('doubling_days_%d_rolling_cases', window_days)] = cur_day$cumulative_cases / (sum(within_window$cases) / window_days)
    
  }
}

# get instantaneous doubling rate
data[,'doubling_days_deaths'] = data$cumulative_deaths / data$deaths
data[,'doubling_days_cases'] = data$cumulative_cases / data$cases

# Get rid of infinite values
data = do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),NA)))

# Store the enriched file
write.csv(data, file.path(git_path, 'enriched_up_to_date_data.csv'))

################################################################################
################################################################################
################################################################################
# PLOTTING

## These should now be all the features that exist. 
# [1] "dateRep"                           "day"                               "month"                             "year"                             
# [5] "cases"                             "deaths"                            "countriesAndTerritories"           "geoId"                            
# [9] "countryterritoryCode"              "popData2018"                       "date"                              "cumulative_deaths"                
# [13] "cumulative_cases"                  "cumulative_deaths_worldwide"       "cumulative_cases_worldwide"        "cumulative_deaths_norm"           
# [17] "cumulative_cases_norm"             "day_n"                             "day_n_since_first_case_worldwide"  "day_n_since_first_death_worldwide"
# [21] "day_n_since_first_death"           "day_n_since_first_case"            "avg_prev_5_days_cases"             "avg_prev_5_days_deaths"           
# [25] "avg_prev_5_days_cases_norm"        "avg_prev_5_days_deaths_norm"       "doubling_days_5_rolling_deaths"    "doubling_days_5_rolling_cases"    
# [29] "doubling_days_deaths"   "doubling_days_cases"

plot_them_all <- function(data, country_zoom = '', countries_of_interest = NULL, git_path = NULL) {
  
  # A bunch of interesting plots
  
  #############
  take_them_all = FALSE
  if (is.null(countries_of_interest)) {
    take_them_all = TRUE
    countries_of_interest = unique(data$countriesAndTerritories)
    # countries_of_interest = c("Netherlands",
    #                           "Portugal",
    #                           # "Greece",
    #                           # "New_Zealand",
    #                           # "Germany",
    #                           "Italy",
    #                           "Spain",
    #                           "United_States_of_America",
    #                           "United_Kingdom",
    #                           # "Belgium",
    #                           "China")
  }
  
  if (! country_zoom == '' && ! country_zoom %in% countries_of_interest) {
    # At the country zoom if its not in the list
    countries_of_interest = append(countries_of_interest, country_zoom)
  }
  
  data = data[order(data$date), ] # make sure it's sorted for cumulative measures
  data = do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),NA)))
  
  if (take_them_all) {
    sel = data
  } else {
    sel = data[data$countriesAndTerritories %in% countries_of_interest, ]
  }
  sel[, 'Country'] = sel$countriesAndTerritories
  
  if (country_zoom == '') {
    # file_path = sprintf('%s/plots', git_path)
    file_path = file.path(git_path, 'plots')
    max_data = sel
  } else {
    # file_path = sprintf('%s/plots/%s', git_path, country_zoom)
    file_path = file.path(git_path, 'plots', country_zoom)
    
    max_data = sel[sel$Country == country_zoom, ]
  }
  
  if (take_them_all) {
    file_path = file.path(file_path, 'all_countries')
  }
  
  dir.create(file_path, showWarnings = FALSE, recursive = T)
  
  message(sprintf('Exporting plots to <%s>..', file_path))
  
  max_y_val = max(max_data$avg_prev_5_days_deaths, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = avg_prev_5_days_deaths, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 deaths (per 5 days)",
         title = "Daily deaths w/ 5-day rolling average")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_deaths_5_rolling.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(max_data$avg_prev_5_days_deaths_norm, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = avg_prev_5_days_deaths_norm, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 deaths (per 5 days, normalized per 100,000)",
         title = "Daily deaths w/ 5-day rolling average normalized")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_deaths_5_rolling_normalized.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(max_data$cumulative_deaths_norm, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = cumulative_deaths_norm, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 deaths (cumulative, normalized per 100,000)",
         title = "Cumulative deaths (normalized per 100,000)")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_deaths_normalized.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(max_data$cumulative_cases, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = cumulative_cases, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 confirmed cases (accumulated)",
         title = "Total confirmed cases per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_cumulative_cases.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(max_data$cumulative_deaths, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = cumulative_deaths, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 deaths (accumulated)",
         title = "Total deaths per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_cumulative_deaths.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  
  ##############
  max_y_val = max(max_data$cumulative_deaths_norm, na.rm = T)
  max_x_val = max(max_data$cumulative_cases_norm, na.rm = T)
  
  p = ggplot(data = sel, aes(x = cumulative_cases_norm, y = cumulative_deaths_norm, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(xlim = c(0, max_x_val * 1.1), ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Covid-19 cases (cumulative, per 100.000)",
         y = "Covid-19 deaths  (cumulative, per 100.000)",
         title = "New cases vs new deaths")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('cumulative_deaths_norm_vs_cases_norm.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  
  ##############
  max_y_val = max(max_data$avg_prev_5_days_deaths, na.rm = T)
  max_x_val = max(max_data$avg_prev_5_days_cases, na.rm = T)
  
  p = ggplot(data = sel, aes(x = avg_prev_5_days_cases, y = avg_prev_5_days_deaths, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(xlim = c(0, max_x_val * 1.1), ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Covid-19 cases (avg per 5 days)",
         y = "Covid-19 deaths (avg per 5 days)",
         title = "New cases vs new deaths")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('avg5_deaths_vs_cases.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(max_data$cumulative_deaths, na.rm = T)
  max_x_val = max(max_data$avg_prev_5_days_cases, na.rm = T)
  
  p = ggplot(data = sel, aes(x = avg_prev_5_days_cases, y = cumulative_deaths, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(xlim = c(0, max_x_val * 1.1), ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Covid-19 cases (avg per 5 days)",
         y = "Covid-19 deaths (accumulated)",
         title = "New cases vs accumulated deaths")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('avg5_cumulative_deaths_vs_cases.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  
  ##############
  max_y_val = max(1/max_data$doubling_days_5_rolling_deaths, na.rm = T)
  min_y_val = min(1/max_data$doubling_days_5_rolling_deaths, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = 1/doubling_days_5_rolling_deaths, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(ylim = c(min_y_val - (min_y_val*.1), max_y_val)) +
    labs(x = "Days since first death in country",
         y = "1/Covid-19 deaths doubling-rate (rolling)",
         title = "1/Doubling rate per country")

  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_1div_death_doubling_rate_rolling.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(1/max_data$doubling_days_deaths, na.rm = T)
  min_y_val = min(1/max_data$doubling_days_deaths, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = 1/doubling_days_deaths, color = Country, shape = Country)) +
    geom_path() + geom_point() +coord_cartesian(ylim = c(min_y_val - (min_y_val*.1), max_y_val)) +
    labs(x = "Days since first death in country",
         y = "1/Covid-19 deaths doubling-rate (instantaneous)",
         title = "1/Doubling rate per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_1div_death_doubling_rate.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(1/max_data$doubling_days_5_rolling_cases, na.rm = T)
  min_y_val = min(1/max_data$doubling_days_5_rolling_cases, na.rm = T)
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = 1/doubling_days_5_rolling_cases, color = Country, shape = Country)) +
    geom_path() + geom_point() +coord_cartesian(ylim = c(min_y_val - (min_y_val*.1), max_y_val)) +
    labs(x = "Days since first death in country",
         y = "1/Covid-19 cases doubling-rate (rolling)",
         title = "1/Doubling rate per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_1div_case_doubling_rate_rolling.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(1/max_data$doubling_days_cases, na.rm = T)
  min_y_val = min(1/max_data$doubling_days_cases, na.rm = T)
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = 1/doubling_days_cases, color = Country, shape = Country)) +
    geom_path() + geom_point() +coord_cartesian(ylim = c(min_y_val - (min_y_val*.1), max_y_val)) +
    labs(x = "Days since first death in country",
         y = "1/Covid-19 cases doubling-rate (instantaneous)",
         title = "1/Doubling rate per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_1div_case_doubling_rate.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  
  
  
  ##############
  max_y_val = max(max_data$doubling_days_5_rolling_deaths, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = doubling_days_5_rolling_deaths, color = Country, shape = Country)) +
    geom_path() + geom_point() + coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 deaths doubling-rate (rolling)",
         title = "Doubling rate per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_death_doubling_rate_rolling.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(max_data$doubling_days_deaths, na.rm = T)
  
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = doubling_days_deaths, color = Country, shape = Country)) +
    geom_path() + geom_point() +coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 deaths doubling-rate (instantaneous)",
         title = "Doubling rate per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_death_doubling_rate.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(max_data$doubling_days_5_rolling_cases, na.rm = T)
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = doubling_days_5_rolling_cases, color = Country, shape = Country)) +
    geom_path() + geom_point() +coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 cases doubling-rate (rolling)",
         title = "Doubling rate per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_case_doubling_rate_rolling.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  ##############
  max_y_val = max(max_data$doubling_days_cases, na.rm = T)
  p = ggplot(data = sel, aes(x = day_n_since_first_death, y = doubling_days_cases, color = Country, shape = Country)) +
    geom_path() + geom_point() +coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
    labs(x = "Days since first death in country",
         y = "Covid-19 cases doubling-rate (instantaneous)",
         title = "Doubling rate per country")
  
  if (take_them_all) {
    p = p + theme(legend.position = "none")
  }
  
  ggsave('from_first_death_case_doubling_rate.png', plot = p, device = NULL, path = file_path,
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
}

# Define a limited list of countries of interest (for readability of the graph)

# Frankrijk --> since 5/apr/2020 there should be an increase in deaths as they changed the way they counted (by including nursery homes)
# Sweden --> interesting because 'no measures'
# Turkmenistan --> also interesting for taking no measures
# Russia --> interesting for falsely reporting the statistics
# Taiwan --> everything under control
# Turkey --> 10/11 apr 2020 last-minute lockdown (announced 2hr before starting) results in crowded streets/stores/supermarkets as a result of panick buying.

countries_of_interest = c("Netherlands",
                          # "Portugal",
                          "Sweden",
                          # "Greece",
                          # "New_Zealand",
                          # "Germany",
                          "Italy",
                          "Spain",
                          "United_States_of_America",
                          "United_Kingdom",
                          # "Belgium",
                          "China",
                          # "Austria",
                          "Taiwan")

for (country_zoom in countries_of_interest) {
  message(sprintf('Plotting zoomed in on %s...', country_zoom))
  plot_them_all(data,
                country_zoom = country_zoom,
                countries_of_interest = countries_of_interest,
                git_path = git_path
  )
}

# And plot them for everyone
message('Plotting without zoom, a selection of countries...')
plot_them_all(data,
              country_zoom = '',
              countries_of_interest = countries_of_interest,
              git_path = git_path)

message('Plotting without zoom, all countries...')
plot_them_all(data,
              country_zoom = '',
              countries_of_interest = NULL,
              git_path = git_path)



########################
# wip

library(plotly)
html_path = file.path(git_path, "html_plots")
dir.create(html_path, showWarnings = FALSE, recursive = T)

sel = data
sel[,'Country'] = sel$countriesAndTerritories

#############
p = ggplot(data = sel, aes(x = avg_prev_5_days_cases, y = cumulative_deaths, color = Country, shape = Country, label = Country)) +
  geom_path() + geom_point() + # coord_cartesian(xlim = c(0, max_x_val * 1.1), ylim = c(0, max_y_val * 1.1)) +
  labs(x = "Covid-19 cases (avg per 5 days)",
       y = "Covid-19 deaths (accumulated)",
       title = "New cases vs accumulated deaths") + theme(legend.position = "none")

htmlwidgets::saveWidget(ggplotly(p), file.path(html_path, "cases_vs_deaths.html"))

##############
p = ggplot(data = sel, aes(x = day_n_since_first_death, y = avg_prev_5_days_deaths_norm, color = Country, shape = Country)) +
  geom_path() + geom_point() +# coord_cartesian(ylim = c(0, max_y_val * 1.1)) +
  labs(x = "Days since first death in country",
       y = "Covid-19 deaths (per 5 days, normalized per 100,000)",
       title = "Daily deaths w/ 5-day rolling average normalized")

# plotly.offline.plot(ggplotly(p), filename='from_first_death_deaths_5_rolling_normalized.html', path = html_path) 
htmlwidgets::saveWidget(ggplotly(p), file.path(html_path, "from_first_death_deaths_5_rolling_normalized.html"))

################################################################################




p = ggplot(data = sel, aes(x = avg_prev_5_days_cases, y = cumulative_deaths, color = Country, shape = Country, label = Country)) +
  geom_path() + geom_point() + # coord_cartesian(xlim = c(0, max_x_val * 1.1), ylim = c(0, max_y_val * 1.1)) +
  labs(x = "Covid-19 cases (avg per 5 days)",
       y = "Covid-19 deaths (accumulated)",
       title = "New cases vs accumulated deaths") + theme(legend.position = "none")

fig <- ggplotly(p)

html_path = file.path(git_path, "html_plots")
dir.create(html_path, showWarnings = FALSE, recursive = T)


plotly.offline.plot(fig, filename='from_first_death_deaths_5_rolling_normalized.html', path = html_path) 


htmlwidgets::saveWidget(fig, "cases_vs_deaths.html", path = html_path)

if (take_them_all) {
  p = p + theme(legend.position = "none")
}

ggsave('avg5_cumulative_deaths_vs_cases.png', plot = p, device = NULL, path = git_path,
       scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)

