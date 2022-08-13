# load in data and libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(usmap)
library(janitor)
library(ggeasy)
library(gganimate)
library(transformr)
library(patchwork)
data <- read.csv(url("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"))

# Filter data
prison_pop <- select(data, aapi_prison_pop,
                           black_prison_pop,
                           latinx_prison_pop,
                           native_prison_pop,
                           other_race_prison_pop,
                           white_prison_pop)

prison_rate <- select(data, aapi_prison_pop_rate,
                            black_prison_pop_rate,
                            latinx_prison_pop_rate,
                            native_prison_pop_rate,
                            white_prison_pop_rate)

prison_pop_year <- select(data, year,
                          aapi_prison_pop,
                          black_prison_pop,
                          latinx_prison_pop,
                          native_prison_pop,
                          other_race_prison_pop,
                          white_prison_pop)

## Summary Information
# What is the average number of incarcerated people across race?
avg_incar <- function() {
  invar_pop <- summarize(prison_pop,
                         'White Population' = mean(white_prison_pop, na.rm=TRUE),
                         'Black Population' = mean(black_prison_pop, na.rm=TRUE),
                         'Latinx Population' = mean(latinx_prison_pop, na.rm=TRUE),
                         'Asian or Pacific Islander Population' = mean(aapi_prison_pop, na.rm=TRUE),
                         'Native American Population' = mean(native_prison_pop, na.rm=TRUE),
                         'Other Race Population' = mean(other_race_prison_pop, na.rm=TRUE))
  return(invar_pop)
}

# What is the highest prison population of incarcerated people across different races?
max_incar <- function() {
  invar_max <- summarize(prison_pop,
                         'White Population' = max(white_prison_pop, na.rm=TRUE),
                         'Black Population' = max(black_prison_pop, na.rm=TRUE),
                         'Latinx Population' = max(latinx_prison_pop, na.rm=TRUE),
                         'Asian or Pacific Islander Population' = max(aapi_prison_pop, na.rm=TRUE),
                         'Native American Population' = max(native_prison_pop, na.rm=TRUE),
                         'Other Race Population' = max(other_race_prison_pop, na.rm=TRUE))
  return(invar_max)
}

# What is the prison population change from 1970 and 2018 across different races?
dif_70_18 <- function() {
  years <- select(data, year,
                    black_prison_pop,
                    white_prison_pop)
  
  # Find population of 1970
  rate_70 <- filter(years, year == '1970')
  rate_70 <- summarize(rate_70,
                       'White Population' = sum(white_prison_pop, na.rm=TRUE),
                       'Black Population' = sum(black_prison_pop, na.rm=TRUE))
  
  # Find population of 2018
  rate_18 <- filter(years, year == '2021')
  rate_18 <- summarize(rate_18,
                       'White Population' = sum(white_prison_pop, na.rm=TRUE),
                       'Black Population' = sum(black_prison_pop, na.rm=TRUE))
  # Find difference
  difference <- rate_70 - rate_18
  return(difference)
}

# What is the average rate of incarcerated people across different races?
avg_rate <- function() {
  rate <- summarize(prison_rate,
                    'White Population Prison Rate' = mean(white_prison_pop_rate, na.rm=TRUE),
                    'Black Population Prison Rate' = mean(black_prison_pop_rate, na.rm=TRUE),
                    'Latinx Population Prison Rate' = mean(latinx_prison_pop_rate, na.rm=TRUE),
                    'Asian or Pacific Islander Population Prison Rate' = mean(aapi_prison_pop_rate, na.rm=TRUE),
                    'Native American Population Prison Rate' = mean(native_prison_pop_rate, na.rm=TRUE))
  return(rate)
}

# What is the highest rate of incarcerated people across different races?
max_rate <- function() {
  max_rate <- summarize(prison_rate,
                    'White Population Prison Rate' = max(white_prison_pop_rate, na.rm=TRUE),
                    'Black Population Prison Rate' = max(black_prison_pop_rate, na.rm=TRUE),
                    'Latinx Population Prison Rate' = max(latinx_prison_pop_rate, na.rm=TRUE),
                    'Asian or Pacific Islander Population Prison Rate' = max(aapi_prison_pop_rate, na.rm=TRUE),
                    'Native American Population Prison Rate' = max(native_prison_pop_rate, na.rm=TRUE))
  return(max_rate)
}


## Plot
plot_date <- function() {
  # Filter data
  prison_pop_year <- select(data, year,
                       aapi_prison_pop,
                       black_prison_pop,
                       latinx_prison_pop,
                       native_prison_pop,
                       other_race_prison_pop,
                       white_prison_pop)
  prison_pop_year <- prison_pop_year %>% group_by(year) %>% summarize(white = sum(white_prison_pop, na.rm=TRUE),
                                                   black = sum(black_prison_pop, na.rm=TRUE),
                                                   latinx = sum(latinx_prison_pop, na.rm=TRUE),
                                                   aapi = sum(aapi_prison_pop, na.rm=TRUE),
                                                   native = sum(native_prison_pop, na.rm=TRUE),
                                                   other = sum(other_race_prison_pop, na.rm=TRUE))
  prison_pop_year <- filter(prison_pop_year, year < '2017')
  # Plot data
  prison_pop_plot <- ggplot(prison_pop_year, aes(x=year)) + 
    geom_line(aes(y=white, color="White")) + 
    geom_line(aes(y=black, x=year, color="Black")) + 
    geom_line(aes(y=latinx, x=year, color="Latinx")) +
    geom_line(aes(y=aapi, x=year, color="Native American")) +
    geom_line(aes(y=native, x=year, color="Asian and Pacific Islander")) +
    geom_line(aes(y=other, x=year, color="Other")) +
    scale_color_manual(name = "Race",
                       values = c("White" = "blue",
                                  "Black" = "red",
                                  "Latinx" = "green",
                                  "Native American" = "purple",
                                  "Asian and Pacific Islander" = "orange",
                                  "Other" = "pink"))

  # Add titles
  prison_pop_plot <- prison_pop_plot + ggtitle("Change in Prison Population Over the Years by Race") +
    xlab("Year") + ylab("Population")
  
  return(prison_pop_plot)
}


## Map
map <- function() {
  # Filter data
  prison_pop_state <- select(data, state,
                             white_prison_pop_rate,
                             black_prison_pop_rate)
  prison_pop_state <- prison_pop_state %>% group_by(state) %>% summarize(
    white = mean(white_prison_pop_rate, na.rm=TRUE),
    black = mean(black_prison_pop_rate, na.rm=TRUE))
  
  # Load map
  plot_usmap()
  p1 <- plot_usmap(data = prison_pop_state, values = 'white', labels=FALSE) +
    labs(title = "Rate of Incarcerated White People") +
    scale_fill_continuous(low = "white", high ="darkblue", limits = c(0, 8000))
  p2 <- plot_usmap(data = prison_pop_state, values = 'black', labels=FALSE) +
    labs(title = "Rate of Incarcerated Black People") +
    scale_fill_continuous(low = "white", high ="darkblue", limits = c(0, 8000))
  mp <- p1 + p2
  return(mp)
}

map_detail <- function() {
  prison_pop_county <- filter(data, state=='NE')
  prison_pop_county <- select(prison_pop_county, fips,
                             white_prison_pop_rate,
                             black_prison_pop_rate)
  prison_pop_county <- prison_pop_county %>% group_by(fips) %>% summarize(
    white = mean(white_prison_pop_rate, na.rm=TRUE),
    black = mean(black_prison_pop_rate, na.rm=TRUE))
  
  p1 <- plot_usmap("counties", include = "NE", data = prison_pop_county, values = 'white', labels=FALSE) +
    labs(title = "Population of Incarcerated White People") +
    scale_fill_gradientn(colours = c("white", "orange", "red"),
                         values = scales::rescale(c(-0.5, -0.49, 0, 0.05, 0.5)), limits= c(0, 720000))
  p2 <- plot_usmap("counties", include = "NE", data = prison_pop_county, values = 'black', labels=FALSE) +
    labs(title = "Population of Incarcerated Black People") +
    scale_fill_gradientn(colours = c("white", "orange", "red"),
                         values = scales::rescale(c(-0.5, -0.49, 0, 0.05, 0.5)), limits= c(0, 720000))
  mp <- p1 + p2
  return(mp)
}

