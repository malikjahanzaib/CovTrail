#------------------------------------------------------
#------------------------------------------------------
# CovTrail
# Presented by: Jahanzaib Malik and Bhaktiben Patel
#------------------------------------------------------
#------------------------------------------------------

# IMPORT RAW DATA: Johns Hopkins Github data
confirmed_cases_raw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
str(confirmed_cases_raw) # Check latest date at the end of data
deaths_cases_raw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
str(deaths_cases_raw) # Check latest date at the end of data
recovered_cases_raw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
str(recovered_cases_raw) # Check latest date at the end of data
ts.confirmed_cases <- covid19.data(case = "ts-confirmed")
# View(confirmed_cases_raw)
# View(deaths_cases_raw)
# View(recovered_cases_raw)

# DATA CLEANING: To create countries level and global combined data
# Convert each data set from wide to long AND aggregate at countries level
library(tidyr)
library(dplyr)
library(covid19.analytics)

confirmed_cases <- confirmed_cases_raw %>% gather(key="date", value="confirmed_cases", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed_cases=sum(confirmed_cases))
deaths_cases <- deaths_cases_raw %>% gather(key="date", value="deaths_cases", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths_cases=sum(deaths_cases))
recovered_cases <- recovered_cases_raw %>% gather(key="date", value="recovered_cases", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered_cases=sum(recovered_cases))
summary(confirmed_cases)

# Final data: combine all three
countries <- full_join(confirmed_cases, deaths_cases) %>% full_join(recovered_cases)
# View(countries)

# Date variable
# Fix date variable and convert from character to date
str(countries) # check date character
countries$date <- countries$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
str(countries) # check date Date
# Create new variable: number of days
countries <- countries %>% group_by(Country.Region) %>% mutate(cumulative_confirmed_cases=cumsum(as.numeric(confirmed_cases)), days = date - first(date) + 1)

# Aggregate at global level
global <- countries %>% group_by(date) %>% summarize(confirmed_cases=sum(confirmed_cases), cumulative_confirmed_cases=sum(confirmed_cases), deaths_cases=sum(deaths_cases), recovered_cases=sum(recovered_cases)) %>% mutate(days = date - first(date) + 1)
# Extract specific countries: US 
# View(global)
us <- countries %>% filter(Country.Region=="US")

# SUMMARY STATISTICS
summary(countries)
by(countries$confirmed_cases, countries$Country.Region, summary)
by(countries$cumulative_confirmed_cases, countries$Country.Region, summary)
by(countries$deaths_cases, countries$Country.Region, summary)
by(countries$recovered_cases, countries$Country.Region, summary)
summary(global)
summary(us)

# GRAPHS
# Barchart of cases over time
library(ggplot2)
# global confirmed_cases
ggplot(global, aes(x=date, y=confirmed_cases)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

# US confirmed_cases
ggplot(us, aes(x=date, y=confirmed_cases)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in US", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

# global confirmed_cases, deaths_cases and recovered_cases
str(global)
global %>% gather("Type", "Cases", -c(date, days)) %>%
  ggplot(aes(x=date, y=Cases, colour=Type)) + geom_bar(stat="identity", width=0.2, fill="white") +
  theme_classic() +
  labs(title = "Covid-19 Global Cases", x= "Date", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.5))

# Line graph of cases over time
# global confirmed_cases
ggplot(global, aes(x=days, y=confirmed_cases)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))
# Ignore warning

# global confirmed_cases with counts in log10 scale
ggplot(global, aes(x=days, y=confirmed_cases)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

# global confirmed_cases, deaths_cases and recovered_cases
str(global)
global %>% gather("Type", "Cases", -c(date, days)) %>%
  ggplot(aes(x=days, y=Cases, colour=Type)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Cases", x= "Days", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.5))

# confirmed_cases by countries for select countries with counts in log10 scale
countries_selection <- countries %>% filter(Country.Region==c("US", "Italy", "China", "France", "United Kingdom", "Germany"))
ggplot(countries_selection, aes(x=days, y=confirmed_cases, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases by countries", x= "Days", y= "Daily confirmed cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

# Matrix of line graphs of confirmed_cases, deaths_cases and recovered_cases for select countries in log10 scale
str(countries_selection)
countries_selection %>% gather("Type", "Cases", -c(date, days, Country.Region)) %>%
  ggplot(aes(x=days, y=Cases, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Cases by countries", x= "Days", y= "Daily cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10") +
  facet_grid(rows=vars(Type))

#---------------------------------------

## Map
countries_total <- countries %>% group_by(Country.Region) %>% summarize(cumulative_confirmed_cases=sum(confirmed_cases), cumulative_deaths_cases=sum(deaths_cases), cumulative_recovered_cases=sum(recovered_cases))

# Basemap from package tmap
library(tmap)
data(World)
class(World)


countries_total$Country.Region[!countries_total$Country.Region %in% World$name]
list <- which(!countries_total$Country.Region %in% World$name)
countries_total$countries <- as.character(countries_total$Country.Region)
countries_total$countries[list] <-
  c("Andorra"                 ,           "Antigua and Barbuda"  ,           
    "Bahrain"                ,          "Barbados"         ,               
    "Bosnia and Herzegovina" ,         "Burma"             ,              
    "Cabo Verde"             ,        "Central African Republic" ,       
    "Comoros"                ,         "Congo (Brazzaville)"      ,       
    "Congo (Kinshasa)"       ,          "Czechia"                ,         
    "Diamond Princess"       ,          "Dominica"               ,         
    "Dominican Republic"     ,          "Equatorial Guinea"      ,         
    "Eswatini"               ,          "Grenada"                ,         
    "Holy See"               ,          "Kiribati"               ,         
    "Korea, South"           ,          "Laos"                   ,         
    "Liechtenstein"          ,          "Maldives"               ,         
    "Malta"                  ,          "Marshall Islands"       ,         
    "Mauritius"              ,          "Micronesia"             ,         
    "Monaco"                 ,          "MS Zaandam"             ,         
    "North Macedonia"        ,          "Palau"                  ,         
    "Saint Kitts and Nevis"  ,          "Saint Lucia"            ,         
    "Saint Vincent and the Grenadines" , "Samoa"                 ,          
    "San Marino"             ,          "Sao Tome and Principe"  ,         
    "Seychelles"             ,          "Singapore"              ,         
    "Solomon Islands"        ,          "South Sudan"            ,         
    "Summer Olympics 2020"   ,          "Taiwan*"                ,         
    "Tonga"                  ,          "US"                     ,
    "West Bank and Gaza"       ,        "Winter Olympics 2022"  )

countries_total$Country.Region[!countries_total$countries %in% World$name]
World$countries <- World$name
worldmap <- left_join(World, countries_total, by="countries")
worldmap$cumulative_confirmed_cases[is.na(worldmap$cumulative_confirmed_cases)] <- 0

# Map
ggplot(data = worldmap) + geom_sf(aes(fill=cumulative_confirmed_cases), color="black") +
  ggtitle("World Map of Confirmed Covid Cases",
          subtitle="") +
  theme_bw()

#--------------------------------------------

##SIR MODEL

#--------------------------------------------

# DATA FOR REGION: Ontario
ontario <- confirmed_cases_raw %>% filter(Province.State=="Ontario") %>% gather(key="date", value="confirmed_cases", -c(Country.Region, Province.State, Lat, Long)) %>%  mutate(cumulative_confirmed_cases=cumsum(confirmed_cases))
ontario$date <- ontario$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")

# SIR FUNCTION
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * I * S/N
    dI <- beta * I * S/N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# CREATE A VECTOR OF DAILY CUMULATIVE INCIDENCE NUMBERS OF CANADA FROM START DATE
infected <- ontario %>% filter(confirmed_cases>0) %>% pull(cumulative_confirmed_cases)

# Create an incrementing Day vector the same length as our cases vector
day <- 1:(length(infected))
N <- 14446515
# now specify initial values for S, I and R
init <- c(S = N - infected[1], I = infected[1], R = 0)

library(deSolve)

RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = day, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((infected - fit)^2)
}

# now find the values of beta and gamma that give the
# smallest RSS, which represents the best fit to the data.
# Start with values of 0.5 for each, and constrain them to
# the interval 0 to 1.0
optimization <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0,0), upper = c(1, 1))

# check for convergence
optimization$message

# Optimization Parameters
opt_par <- setNames(optimization$par, c("beta", "gamma"))
opt_par[1] = 0.2
opt_par[2] = 0.08

# Reproduction Number
R0 <- opt_par[1]/opt_par[2]
R0

# PREDICTION
# time in days for predictions
t <- 1:110
# get the fitted values from our SIR model
fittedcum <- data.frame(ode(y = init, times = t, func = SIR, parms = opt_par))
# add a Date column and join the observed incidence data
fittedcum <- fittedcum %>%
  mutate(date = as.Date("1/22/2020", "%m/%d/%y") + t - 1, province = "Ontario") %>%
  left_join(ontario %>% select(date, cumulative_confirmed_cases))

# plot the data
ggplot(fittedcum, aes(x = date)) +
  geom_line(aes(y = I), colour = "red") +
  geom_point(aes(y = cumulative_confirmed_cases), colour = "orange")


labs(y = "Cumulative incidence", x="Date",
     title = "COVID-19 fitted vs observed cumulative incidence, Ontario",
     subtitle = "(red=fitted incidence from SIR model, orange=observed incidence)")

# plot the data
ggplot(fittedcum, aes(x = date)) +
  geom_line(aes(y = I), colour = "red") +
  geom_line(aes(y = S), colour = "black") +
  geom_line(aes(y = R), colour = "green") +
  geom_point(aes(y = cumulative_confirmed_cases), colour = "orange") +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Persons", title = "COVID-19 fitted vs observed cumulative incidence, Ontario province") +
  scale_colour_manual(name = "",
                      values = c(red = "red", black = "black", green = "green", orange = "orange"),
                      labels = c("Susceptible", "recovered_cases", "Observed incidence", "Infectious")) +
  scale_y_continuous(trans="log10")


#generating the SIR Model for United States
par(mar=c(1,1,1,1))
options(scipen = 999)
generate.SIR.model(ts.confirmed_cases, 'US', tot.population = 329500000)

#--------------------------------------------

##SIR MODEL END

#--------------------------------------------
#############################################
#--------------------------------------------

##SVR MODEL

#--------------------------------------------
#Importing necessary libraries
library(e1071)
library(ggplot2)

# Importing the dataset
# dataset <- read.csv('C:/users/patel/Downloads/mydata.csv')
# dataset <- dataset[1:2]
# dim(dataset)

dataset <- confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
dataset <- dataset[2:3]
# %% [code]
# Fitting Support Vector Regression to the dataset
regressor <- svm(formula <- confirmed ~ ., data <- dataset, type <- 'eps-regression', kernel <- 'radial')

# %% [code]
# Predicting a new result
y_pred <- predict(regressor, data.frame(Level = 6.5))

# %% [code]
# visualize the Linear Regression 
ggplot() +
  geom_point(aes(x <- dataset$date, y <- dataset$confirmed), colour = 'red') +
  geom_line(aes(x <- dataset$date, y <- predict(regressor, confirmed)), colour = 'blue') +
  ggtitle('Support Vector Regression') +
  xlab('Days') +
  ylab('Confirmed Cases')           

# %% [code]
# visualize the smoother curve
x_grid = seq(min(dataset$date), max(dataset$date), 0.1)
ggplot() +
  geom_point(aes(x <- dataset$date, y <- dataset$date), colour = 'red') +
  geom_line(aes(x <- x_grid, y <- predict(regressor, data.frame(Level = x_grid))),colour = 'blue') +
  ggtitle('Support Vector Regression') +
  xlab('Days') +
  ylab('Confirmed Cases')

#--------------------------------------------

##SVR MODEL END

#--------------------------------------------

# ======================================================================================

