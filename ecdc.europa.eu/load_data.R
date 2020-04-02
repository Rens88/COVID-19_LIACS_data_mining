# This is where the data will be copied to.
git_path = "C:/Users/rensm/Documents/GitLiacs/Covid-19/ecdc.europa.eu/"

#these libraries are necessary
library(readxl)
library(httr)

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
newlocation = sprintf('%sCOVID-19-geographic-disbtribution-worldwide-%s.xlsx', git_path, most_recent_day)
success = file.copy(from=tf, to=newlocation, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

if (success) {
  message(sprintf('Copied the latest data (from <%s>) to: %s', most_recent_day, newlocation))
} else {
  message('WARNING: Somehow, it failed... Check error log.')
}



library(ggplot2)
library(dplyr)
library(RcppRoll)

## Interesting countries
# New_Zealand
# Germany
# Italy
# Spain
# United_States_of_America
# United_Kingdom
# Belgium
# China

window_days = 5

data = read_excel(newlocation)

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
  }
}


### Create a new country called 'world', and let it run with the rest of the analyses.
# word_wide_population = sum(pop_by_country, na.rm=T)

# 
# [1] "dateRep"                           "day"                               "month"                            
# [4] "year"                              "cases"                             "deaths"                           
# [7] "countriesAndTerritories"           "geoId"                             "countryterritoryCode"             
# [10] "popData2018"                       "date"                              "cumulative_deaths"                
# [13] "cumulative_cases"                  "cumulative_deaths_worldwide"       "cumulative_cases_worldwide"       
# [16] "cumulative_deaths_norm"            "cumulative_cases_norm"             "day_n"                            
# [19] "day_n_since_first_case_worldwide"  "day_n_since_first_death_worldwide" "day_n_since_first_death"          
# [22] "day_n_since_first_case"            "avg_prev_5_days_cases"             "avg_prev_5_days_deaths"   
# "avg_prev_5_days_cases_norm"             "avg_prev_5_days_deaths_norm"   
# 

countries_of_interest = c("Netherlands",
                          "Portugal",
                          # "Greece",
                          # "New_Zealand",
                          # "Germany",
                          "Italy",
                          "Spain",
                          "United_States_of_America",
                          "United_Kingdom",
                          # "Belgium",
                          "China")
data = data[order(data$date), ] # make sure it's sorted for cumulative measures

sel = data[data$countriesAndTerritories %in% countries_of_interest, ]
sel[, 'Country'] = sel$countriesAndTerritories
# 
# ggplot(data = sel, aes(x = day_n_since_first_case, y = avg_prev_5_days_cases, color = Country)) +
#   geom_line() +
#   labs(x = "Days since first case in country",
#        y = "Number of Covid-19 cases",
#        title = "Daily cases w/ 5-day rolling average")
# 
# ggplot(data = sel, aes(x = day_n_since_first_death, y = avg_prev_5_days_deaths, color = Country)) +
#   geom_line() +
#   labs(x = "Days since first death in country",
#        y = "Number of Covid-19 deaths",
#        title = "Daily deaths w/ 5-day rolling average")
# 
# ggplot(data = sel, aes(x = day_n_since_first_death, y = cumulative_deaths, color = Country)) +
#   geom_line() +
#   labs(x = "Days since first death in country",
#        y = "Cumulative umber of Covid-19 deaths",
#        title = "Total deaths per country")
# 
# 
# ggplot(data = sel, aes(x = day_n_since_first_case, y = avg_prev_5_days_cases_norm, color = Country)) +
#   geom_line() +
#   labs(x = "Days since first case in country",
#        y = "Number of Covid-19 cases (per 100.000)",
#        title = "Normarlized daily cases w/ 5-day rolling average")


#############
p = ggplot(data = sel, aes(x = day_n_since_first_death, y = avg_prev_5_days_deaths, color = Country, shape = Country)) +
  geom_line() + geom_point() +
  labs(x = "Days since first death in country",
       y = "Covid-19 deaths (per 5 days)",
       title = "Daily deaths w/ 5-day rolling average")

ggsave('from_first_death_deaths_5_rolling.png', plot = p, device = NULL, path = "C:/Users/rensm/Documents/GitLiacs/Covid-19/ecdc.europa.eu/plots",
       scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)

##############
p = ggplot(data = sel, aes(x = day_n_since_first_death, y = cumulative_cases, color = Country, shape = Country)) +
  geom_line() + geom_point() +
  labs(x = "Days since first death in country",
       y = "Covid-19 confirmed cases (accumulated)",
       title = "Total confirmed cases per country")
  
ggsave('from_first_death_cumulative_cases.png', plot = p, device = NULL, path = "C:/Users/rensm/Documents/GitLiacs/Covid-19/ecdc.europa.eu/plots",
         scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)

##############
p = ggplot(data = sel, aes(x = day_n_since_first_death, y = cumulative_deaths, color = Country, shape = Country)) +
  geom_line() + geom_point() +
  labs(x = "Days since first death in country",
       y = "Covid-19 deaths (accumulated)",
       title = "Total deaths per country")

ggsave('from_first_death_cumulative_deaths.png', plot = p, device = NULL, path = "C:/Users/rensm/Documents/GitLiacs/Covid-19/ecdc.europa.eu/plots",
       scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)


##############
p = ggplot(data = sel, aes(x = cumulative_cases_norm, y = cumulative_deaths_norm, color = Country, shape = Country)) +
  geom_line() + geom_point() +
  labs(x = "Covid-19 cases (cumulative, per 100.000)",
       y = "Covid-19 deaths  (cumulative, per 100.000)",
       title = "New cases vs new deaths")

ggsave('cumulative_deaths_norm_vs_cases_norm.png', plot = p, device = NULL, path = "C:/Users/rensm/Documents/GitLiacs/Covid-19/ecdc.europa.eu/plots",
       scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)


##############
p = ggplot(data = sel, aes(x = avg_prev_5_days_cases, y = avg_prev_5_days_deaths, color = Country, shape = Country)) +
  geom_line() + geom_point() +
  labs(x = "Covid-19 cases (avg per 5 days)",
       y = "Covid-19 deaths (avg per 5 days)",
       title = "New cases vs new deaths")

ggsave('avg5_deaths_vs_cases.png', plot = p, device = NULL, path = "C:/Users/rensm/Documents/GitLiacs/Covid-19/ecdc.europa.eu/plots",
       scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)

##############
p = ggplot(data = sel, aes(x = avg_prev_5_days_cases, y = cumulative_deaths, color = Country, shape = Country)) +
  geom_line() + geom_point() +
  labs(x = "Covid-19 cases (avg per 5 days)",
       y = "Covid-19 deaths (accumulated)",
       title = "New cases vs accumulated deaths")

ggsave('avg5_deaths_vs_cases.png', plot = p, device = NULL, path = "C:/Users/rensm/Documents/GitLiacs/Covid-19/ecdc.europa.eu/plots",
       scale = 1, width = 9.72, height = 6, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)



tmp = data[data$countriesAndTerritories %in% c('China'), ]

