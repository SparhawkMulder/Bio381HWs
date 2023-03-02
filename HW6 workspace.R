
# Top ---------------------------------------------------------------------


# general work document, to be copy-pasted and trashed later
# homework 6

library(tidyverse)
library(dplyr)


data(iris)
class(iris)
summary(iris)
length(iris) # 5
nrow(iris) # 150

# looks clean, so
irisBasic <- iris

# Iris1 with only virginica and versicolor, sepal lengths longer than 6cm, 
#   and widths longer than 2.5cm.

iris1 <- filter(irisBasic, Species==c("virginica","versicolor") &
                Sepal.Length>6.0 &
                Sepal.Width>2.5)
length(iris1)
nrow(iris1)


# Iris2, subset of iris1, only w/ columns for species, sepal lenth and sepal width

iris2 <- select(iris1, Species, Sepal.Length, Sepal.Width)
length(iris2)
nrow(iris2)

# Iris3, order iris2 from largest to smallest sepal length

iris3 <- arrange(iris2, desc(Sepal.Length))
head(iris3)


# Iris4, new column with sepal.area (width*length)

iris4 <- mutate(iris3, Sepal.Area=Sepal.Length*Sepal.Width)
length(iris4)
nrow(iris4)


# Iris 5, calculate avg sepal length, width, and sample size of iris4 dataframe

iris5 <- summarize(iris4, 
                   MeanLength=mean(Sepal.Length), 
                   MeanWidth=mean(Sepal.Width), 
                   Number=nrow(iris4))
                # SpeciesPresent= nrow(unique(iris4[Species])))
print(iris5)


# Iris 6, calculate the avg lengths, widths, and sample size for each species

iris6 <- summarize(iris4, 
                   MeanLength=mean(Sepal.Length), 
                   MeanWidth=mean(Sepal.Width), 
                   Number=nrow(iris4),
                   .by = Species)
print(iris6)


# GigaPipe ----------------------------------------------------------------


irisFinal <- iris %>%
  # filter(
           # Species==c("virginica","versicolor") &
           # Sepal.Length>6.0 &
           # Sepal.Width>2.5) %>%
  select(Species, Sepal.Length, Sepal.Width) %>%
  arrange(desc(Sepal.Length)) %>%
  mutate(Sepal.Area=Sepal.Length*Sepal.Width) %>%
  summarize(
            MeanLength=mean(Sepal.Length), 
            MeanWidth=mean(Sepal.Width), 
            Number=nrow(iris4),
            .by = Species)
print(irisFinal)



# 9. Longer data set. 3 columns. 
# Species = duh, 
# Measure = what measurement is it (sepal.length, etc)
# Value = value

irisFlip <- iris %>%
  select(Species, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
  pivot_longer(cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
               names_to = "Measure",
               values_to = "Value")
head(irisFlip)
