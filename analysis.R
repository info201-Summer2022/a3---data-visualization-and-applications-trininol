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
  
  # Plot data
  prison_pop_plot <- ggplot(prison_pop_year, aes(x=year)) + 
    geom_line(aes(y=white_prison_pop, color="White"), alpha=0.5) + 
    geom_line(aes(y=black_prison_pop, color="Black"), alpha=0.5) + 
    geom_line(aes(y=latinx_prison_pop, color="Latinx"), alpha=0.5) +
    geom_line(aes(y=native_prison_pop, color="Native American"), alpha=0.5) +
    geom_line(aes(y=aapi_prison_pop, color="Asian and Pacific Islander"), alpha=0.5) +
    geom_line(aes(y=other_race_prison_pop, color="Other"), alpha=0.5) +
    scale_color_manual(name = "Race",
                       values = c("White" = "blue",
                                  "Black" = "red",
                                  "Latinx" = "green",
                                  "Native American" = "purple",
                                  "Asian and Pacific Islander" = "orange",
                                  "Other" = "pink"))

  # Add titles
  prison_pop_plot <- prison_pop_plot + ggtitle("Change in Prison Population Over the Years by Race") +
    xlab("Race") + ylab("Population")
  
  return(prison_pop_plot)
}


## Map
map <- function() {
  # Filter data
  prison_pop_state <- select(data, state,
                            aapi_prison_pop,
                            black_prison_pop,
                            latinx_prison_pop,
                            native_prison_pop,
                            other_race_prison_pop,
                            white_prison_pop)
  prison_pop_state <- prison_pop_state %>% group_by(state) %>% summarize(
                         white = max(white_prison_pop, na.rm=TRUE),
                         black = max(black_prison_pop, na.rm=TRUE),
                         latinx = max(latinx_prison_pop, na.rm=TRUE),
                         aapi = max(aapi_prison_pop, na.rm=TRUE),
                         native = max(native_prison_pop, na.rm=TRUE),
                         other = max(other_race_prison_pop, na.rm=TRUE))
  
  # Load map
  plot_usmap()
  p1 <- plot_usmap(data = prison_pop_state, values = 'white', labels=FALSE) +
                   labs(title = "Population of Incarcerated White People") +
                   scale_fill_continuous(low = "white", high ="darkblue", limits = c(0, 20000))
  p2 <- plot_usmap(data = prison_pop_state, values = 'black', labels=FALSE) +
                   labs(title = "Population of Incarcerated Black People") +
                   scale_fill_continuous(low = "white", high ="darkblue", limits = c(0, 20000))
  p3 <- plot_usmap(data = prison_pop_state, values = 'latinx', labels=FALSE) +
                   labs(title = "Population of Incarcerated Latinx People") +
                   scale_fill_continuous(low = "white", high ="darkblue", limits = c(0, 20000))
  p4 <- plot_usmap(data = prison_pop_state, values = 'aapi', labels=FALSE) +
                   labs(title = "Population of Incarcerated AAPI People") +
                   scale_fill_continuous(low = "white", high ="darkblue", limits = c(0, 20000))
  p5 <- plot_usmap(data = prison_pop_state, values = 'native', labels=FALSE) +
                   labs(title = "Population of Incarcerated Native American People") +
                   scale_fill_continuous(low = "white", high ="darkblue", limits = c(0, 20000))
  p6 <- plot_usmap(data = prison_pop_state, values = 'other', labels=FALSE) +
                   labs(title = "Population of Incarcerated Other People") +
                   scale_fill_continuous(low = "white", high ="darkblue", limits = c(0, 20000))
  mp <- p1 + p2 + p3 + p4 + p5 + p6
  return(mp)
}

