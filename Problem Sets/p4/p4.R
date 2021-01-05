# 1a
library(readr)
library(data.table)

movies = fread("https://raw.githubusercontent.com/hadley/ggplot2movies/master/data-raw/movies.csv")

system.time(readr::read_csv("https://raw.githubusercontent.com/hadley/ggplot2movies/master/data-raw/movies.csv"))
system.time(read.table("https://raw.githubusercontent.com/hadley/ggplot2movies/master/data-raw/movies.csv"))
system.time(fread("https://raw.githubusercontent.com/hadley/ggplot2movies/master/data-raw/movies.csv"))

# 1b

desc = fread("C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/p4/movies_description.csv")

# 2a

str(movies)
# 2b  - Check how this is done outside of base R-
tapply(movies$title, movies$mpaa, length)

# 3a
library(ggplot2)


ggplot(data=movies, aes(rating)) + geom_histogram(binwidth = 0.1)

# 3b

ggplot(data=movies, aes(budget)) + geom_histogram(binwidth = 10000000)

# 3c

pl <- ggplot(data=movies, aes(x= budget, y= rating)) + geom_point()

pl + scale_x_continuous(trans="log10")

lm(rating ~ budget,movies)

# 4a
library(magrittr)
library(dplyr)

movies %>%
  filter(budget == max(budget, na.rm = T)) %>%
  select(title)

# 4b
movies %>% 
  filter(year < 2000 & year > 1989) %>%
  arrange(desc(budget)) %>% 
  head(n= 3)
# 4c - Do this in base R -
library(plyr)

movies %>% 
  count(title) %>%
  filter(n == max(n))

# 5a
library(tibble)
movies %>%
  rowwise() %>%
  mutate(category_count = sum(Action, Animation, Comedy, Drama, Documentary, Romance, Short)) %>%
  filter(category_count == 1)

# 5b

movies_one_type = movies %>%
  rowwise() %>%
  mutate(category_count = sum(Action, Animation, Comedy, Drama, Documentary, Romance, Short)) %>%
  filter(category_count == 1)


library(tidyr)

gather(data = movies_one_type,
       key = "movie_type", 
       value = "type",
       Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  filter(type== 1) %>% 
  group_by(movie_type) %>%
  summarise(mean= mean(budget, na.rm= T), n = n()) %>% 
  filter(mean == min(mean))

# 5c
gather(data = movies_one_type,
       key = "movie_type", 
       value = "type",
       Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  filter(type== 1) %>% 
  group_by(movie_type) %>%
  summarise(mean= mean(budget, na.rm= T)) %>% 
  ggplot(aes(x=movie_type,y=mean)) + geom_bar(stat='identity')
  

# 6a
presidents = fread("C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/p4/presidents.csv")

# 6b
movies_plus= left_join(movies, presidents, by= "year")

# 6c
movies_plus %>% 
  group_by(party) %>% 
  summarise(mean = mean(rating, na.rm=T))


# 6d

movies_plus %>% 
  group_by(president) %>% 
  summarise(mean= mean(length), na.rm= T) %>%
  filter(mean==max(mean))
