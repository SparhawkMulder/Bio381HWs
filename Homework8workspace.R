# Homework 8 Workspace

# Setup -----------------------------------------

library(tidyverse)
library(dplyr)
library(MASS)

# get data
z <- read.table("CleanedData.csv",header=TRUE,sep=",")

str(z)
summary(z)

# cleaning it for our simplified purposes!
z <- dplyr::select(z, photo, avg_rating, photo_gender)
z <- unique(z)
z <- arrange(z, photo)

# Splitting it by sex:

Men <- filter(z, photo_gender=="Male")
Women <- filter(z, photo_gender=="Female")


# Parameters for Normal -----------------------------------------

# Men
menPars <- fitdistr(Men$avg_rating,"normal")

menMean <- menPars$estimate["mean"]
menSD <- menPars$estimate["sd"]

# Women
womenPars <- fitdistr(Women$avg_rating,"normal")

womenMean <- womenPars$estimate["mean"]
womenSD <- womenPars$estimate["sd"]


# Randomized Normal Dataset(s) -----------------------------------------

MenRandAttr <- rnorm(100, mean=menMean, sd=menSD)
WomenRandAttr <- rnorm(100, mean=womenMean, sd=womenSD)
# these are only vectors

# Data frames
MenRandAttr <- data.frame(MenRandAttr) %>%
  rename(MenRandAttr,Attractiveness=MenRandAttr)

WomenRandAttr <- data.frame(WomenRandAttr) %>%
  rename(WomenRandAttr,Attractiveness=WomenRandAttr)

# ANOVA and analysis -----------------------------------------

hist(MenRandAttr[,1]) # basic little histogram to see, we'll make a better one later
hist(WomenRandAttr[,1])

ttest <- t.test(MenRandAttr[,1], y = WomenRandAttr[1], alternative="two.sided")
print(ttest)
# p.value = 0.471658


# Graph it -----------------------------------------

# ggplot histogram

ComparisonHist <- ggplot(data=WomenRandAttr, aes(x=Attractiveness)) +
  geom_histogram(fill="pink", color="black", alpha=0.60) +
  geom_histogram(data=MenRandAttr,
                  fill="lightblue", color="black", alpha=0.5)

ComparisonSmooth <- ggplot(data=WomenRandAttr, aes(x=Attractiveness)) +
  geom_density(fill="pink", color="black", alpha=0.60) +
  geom_density(data=MenRandAttr,
                 fill="lightblue", color="black", alpha=0.5)

print(ComparisonHist)
save(ComparisonHist,file="Comparison1.png")

print(ComparisonSmooth)
save(ComparisonSmooth,file="Comparison2.png")




# Adjusting Means for Significance -----------------------------------------

# by 0.5 units
WomenRandAttr <- rnorm(100, mean=(womenMean+0.5), sd=womenSD)

# to be 25% larger than the Men's
WomenRandAttr <- rnorm(100, mean=(menMean*1.25), sd=womenSD)
# We adjusted the MEN's mean and used that, so we can't accidentally make them MORE similar
# We also make it 25% larger, instead of using a flat number. This is more or less dramatic depending on what your units are (eg. pH behaves differently than Popularity)

WomenRandAttr <- rnorm(100, mean=(menMean+0.6), sd=womenSD)


# Adjusting Sample Size -----------------------------------------

# still using adjusted means, so that there's some significance
MenRandAttr <- rnorm(32, mean=menMean, sd=menSD)
WomenRandAttr <- rnorm(32, mean=(menMean+0.6), sd=womenSD)





