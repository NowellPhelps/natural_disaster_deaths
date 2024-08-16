# Nowell Phelps 
# Aug 15 2024
# JHU Biostat PhD Assessment - Data Analysis Question 1 Code

# -----------------------
# Main Script for task
# -----------------------

rm(list = ls())
library(tidyverse)
library(rvest)
library(stringr)
library(grDevices)
library(gridExtra)
setwd("~/Desktop/JHU_biostat_data_analysis_task/Question 1")
source("functions.R")

# Extract the html from webpage and inspect
url <- "https://en.wikipedia.org/wiki/List_of_natural_disasters_by_death_toll"
webpage <- read_html(url)
webpage |> html_elements("table")

# NB: the two tables we want are those with class="wikitable sortable mw-collapsible", i.e., the
# third and fourth elements of this list. This is from manual inspection of the class of the tables 
# in the page source code in browser, and leads to the [[3]] and [[4]] below. 

# Extract tables of interest
table_20 <- html_table(html_nodes(webpage, "table"), header = TRUE, fill = TRUE)[[3]]
table_21 <- html_table(html_nodes(webpage, "table"), header = TRUE, fill = TRUE)[[4]]

# Merge variables of interest from two tables
data <- table_20 %>% 
  select(c("Year", "Death toll", "Type", "Event")) %>%
  rbind(., table_21 %>% select(c("Year", "Death toll", "Type", "Event")))

# Convert death toll as instructed in questions, and clean disaster type categorization
data$n_deaths   <- sapply(data$'Death toll', FUN = function(X) clean_deathtoll(X))
data$type_clean <- sapply(data$Type, FUN = function(X) clean_distasterType(X))

data$type_clean <-factor(data$type_clean)

table(data$type_clean)

# Plot
fill_scale <- scale_fill_manual(values = c("Flood" = "blue",
                                           "Earthquake" = "black",
                                           "Limnic eruption" = "grey50",
                                           "Earthquake and tsunami" = "lightblue",
                                           "Tropical cyclone" = "purple",
                                           "Tropical cyclone and flood" = "purple4",
                                           "Heat wave" = "orange",
                                           "Avalanche" = "grey90" ,
                                           "Landslide" = "brown",
                                           "Volcanic eruption" = "red"),
                                name = "Disaster type")

p1 <- ggplot(data, aes(x = Year, y = n_deaths, fill = type_clean)) +
  geom_bar(stat = "identity", colour = "black", linewidth = 0.1) +
  fill_scale +
  theme_classic() +
  ggtitle("Figure 1: Number of deaths from deadliest natural disaster of each year of the 20th and 21st centuries") +
  scale_y_continuous(expand = expansion(0), 
                     breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000, 1750000, 2000000, 2250000),
                     limits = c(0, 2250000),
                     labels = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25)) +
  scale_x_continuous(expand = expansion(0),
                     breaks = c(1900, 1925, 1950, 1975, 2000, 2024)) +
  ylab("Deaths from deadliest natural disaster of year (millions)") +
  geom_hline(yintercept = 500000, linetype = "dashed", colour = "grey50") +
  xlab("Year") + 
  theme(panel.grid = element_blank()) 

# Plot adjusted data for disasters with < 500,000 deaths (see README file)
data_adj <- data
data_adj$n_deaths[which(data$Event == "1931 China floods")] <- 500000

p2 <- ggplot(data_adj, aes(x = Year, y = n_deaths, fill = type_clean)) +
  geom_bar(stat = "identity", colour = "black", linewidth = 0.1) +
  fill_scale +
  theme_classic() +
  scale_y_continuous(expand = expansion(0), 
                     breaks = c(0, 100, 200,300,400,500)*1000,
                     limits = c(0, 500000),
                     labels = c(0, 100,200,300,400,500)) +
  scale_x_continuous(expand = expansion(0),
                     breaks = c(1900, 1925, 1950, 1975, 2000, 2024))+
  ylab("Deaths from deadliest natural disaster of year (thousands)") +
  ggtitle("Figure 2: Number of deaths from deadliest natural disaster of each year of the 20th and 21st centuries, capped at 500,000")+
  xlab("Year") + 
  theme(panel.grid = element_blank())

pdf("Disaster plots.pdf", height = 8, width = 12, onefile=T)

grid.arrange(p1)
grid.arrange(p2)
  
dev.off()

