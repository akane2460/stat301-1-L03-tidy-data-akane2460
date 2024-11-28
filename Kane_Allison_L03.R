# L03-tidy-data ----
# Stat 301-1

# class notes
# tidy data = standard way of mapping meaning of data set to its structure
# standardizes everything

## load packages ----
library(tidyverse)
library(fivethirtyeight)

## datasets 
users_top7_2020 <- read.csv("data/users_top7_2020.csv")

## Exercises ----

### Ex 1 ----

# Recreate the plot below showing the change in cases over time using `table2`. What do you need to do first?

# first, need to tidy the data for the cases and population
table2_tidy <- table2 |> 
  pivot_wider(
    names_from = "type", 
    values_from = "count")

# plot
ex1_plot <-
  ggplot(data = table2_tidy, aes(x = year, y = cases)) +
    geom_line(aes(group = country), color = "grey50") +
    geom_point(aes(color = country))

ggsave(
  filename = "plots/ex1_plot.png",
  plot = ex1_plot,
  width = 6,
  units = "in"
)

### EX 2 ----

#load mm_data tibble
mm_data <- tribble(
  ~mm_type, ~blue, ~orange,	~green,	~yellow, ~brown, ~red, ~cyan_blue,
  "plain",  6,     18,      12,	    6,       7,	     7,    NA,
  "peanut", NA,	   11,	    9,	    1,	     12,	   8,    15
)

# tidy the mm_data with pivot_longer
mm_data_tidy <-
  mm_data |> 
    pivot_longer(
        cols = c("blue", "orange", "green", "yellow", "brown", "red", "cyan_blue"),
        names_to = "color",
        values_to = "count"
    ) |>
  na.omit()

mm_data_tidy

### EX 3----

table4a_tidy <- 
  table4a |> 
    pivot_longer(
      cols = c("1999", "2000"),
      names_to = "year",
      values_to = "cases"
    )

### EX 4----
# Use the `drinks` dataset and **only** the `pivot_longer` function to recreate the following:
drinks_tidy <-
  drinks |> 
    pivot_longer(
      cols = c("beer_servings", 
               "spirit_servings", 
               "wine_servings"),
      names_to = "type",
      values_to = "servings"
    )

### EX 5----

## dataset/table
people <- tribble(
  ~respondent_name,  ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

## updated data
people_updated <- tribble(
  ~respondent_name,  ~key,    ~value, ~time,
  #-----------------|--------|-------|------
  "Phillip Woods",   "age",       45,  "t1",
  "Phillip Woods",   "height",   186,  "t1",
  "Phillip Woods",   "age",       50,  "t2",
  "Jessica Cordero", "age",       37,  "t1",
  "Jessica Cordero", "height",   156,  "t1"
)


people_updated |>
  pivot_wider(
    names_from = key,
    values_from = value,
    )

## Case Study ----

users_top7_2020 <- read_csv('data/users_top7_2020.csv')

users_top7_2020_clean <-
  users_top7_2020 |>
    pivot_longer(
      cols = c(matches_1 : passes_12),
      names_to = "month_number",
      values_to = "count")

users_top7_2020_clean <- 
  users_top7_2020_clean |> 
    mutate(
      type = month_number
    )

# create variable type describing the action taken (pass, like or match)
users_top7_2020_clean$type <- 
  sub("_.*", "", users_top7_2020_clean$type)

# adjust variable month number to only include the number 
users_top7_2020_clean$month_number <- 
    sub(".*_", "", users_top7_2020_clean$month_number)

users_top7_2020_clean$month_number <- as.numeric(users_top7_2020_clean$month_number)

# Use an appropriate graph to visualize the matches, likes, and passes over time for each user. What insights and conclusions can you gain from this graph, if any.

top7_swipes_over_time <-
  ggplot(users_top7_2020_clean, aes(x = month_number, y = count)) +
    geom_point(aes(color = type)) +
    geom_line(aes(color = type)) +
    facet_wrap(~user_id) +
    scale_x_continuous(breaks = c(1:12)) +
    labs(
      title = "Activity of Top 7 Tinder Users in 2020 (Jan - Dec)",
      x = "Month",
      y = "Number of Swipes",
      color = "Type of Swipe",
      caption = "Source: Swipestats.io"
    )

ggsave(
  filename = "plots/top7_swipes_over_time.png",
  plot = top7_swipes_over_time,
  width = 12,
  units = "in"
)
  



