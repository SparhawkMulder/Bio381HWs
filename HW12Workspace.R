# Homework 12 Workspace
# Sparhawk Mulder

# Libraries -------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)


# Getting Data ----------------------------------------------------

Full <- read.csv("CleanedData.csv")


# Callbacks by photo ----------------------------------------------

# getting unique photos and rating organized
df <- dplyr::select(Full, photo, avg_rating) %>%
  arrange(photo) %>%
  unique()

# for loop vectors setup
Callbacks <- c()
Attempts <- c()
Photo <- c()

# getting callbacks, applications, and return rate per photo
for (i in 1:length(unique(Full$photo))) {
  Attempts <- c(Attempts,nrow(Full[Full$photo==i,]))
  Callbacks <- c(Callbacks,nrow(Full[Full$photo==i & Full$callback==1,]))
  Photo <- c(Photo,i)
}

# Build the Data Frame
CBDF <- data.frame(Photo,df$avg_rating, Attempts,Callbacks) %>%
  mutate(CBDF, Return = Callbacks/Attempts) %>%
  rename(Attractiveness = df.avg_rating)


# Graph: Simple Scatterplot

s <- ggplot(data=CBDF) +
  aes(x=Attractiveness, y=Return) +
  geom_point() +
  geom_text(label=Photo,
            nudge_x = 0.1, 
            size=2,
            check_overlap = T)

print(s)


# Scatterplot by gender: ----------------------------------------------

# manual Dataframe division
CBDF$Gender <- c(rep("Woman",32),rep("Man",32))


# scatterplot with color changes
s1 <- ggplot(data=CBDF) +
  aes(x=Attractiveness, y=Return,
      color=Gender,
      shape=Gender) +
  geom_point()

print(s1)

# Scatterplot with total applications as dot size ---------------------
s2 <- ggplot(data=CBDF) +
  aes(x=Attractiveness, y=Return,
      color=Gender,
      shape=Gender,
      size=Attempts,
      alpha=0.75) +
  geom_point()

print(s2)

ggsave(plot=s2,
       filename = "HCOL185LongAwaitedScatterplot.pdf")


# Mirrored Histogram of Ratings, by gender ----------------------------

# Setup simpler, more convenient dataframe:
AbyG <- data.frame(Female = CBDF$Attractiveness[1:32],
                   Male=CBDF$Attractiveness[33:64])
# this makes the mirroring a lot easier, I've found

  
# Junior Double Triple Histogram
h <- ggplot(data=AbyG)+
  aes(x=x)+
  geom_histogram( aes(x=Female,y=..density..),
                  binwidth = 0.25,
                  fill="pink",color="black") +
  # Men
  geom_histogram( aes(x=Male,y=-..density..),
                  binwidth=0.25,
                  fill="aquamarine",color="black")


# or a density funciton, same diff
d <- ggplot(data=AbyG)+
  aes(x=x)+
  geom_density( aes(x=Female,y=..density..),
                  fill="pink",color="black") +
  # Men
  geom_density( aes(x=Male,y=-..density..),
                  fill="aquamarine",color="black")

#print
print(h)
print(d)



ggsave(plot=h,
       filename = "HCol185Histogram.pdf")
