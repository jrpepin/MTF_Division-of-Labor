# Set up the data
setwd(repoDir) # This will set the working directory to the master project folder

## Load libraries
library("survey")
library("tableone")
library("nnet")
library("stargazer")
library("ggplot2")
library("cowplot")
library("ggeffects")
library("gtools")
library("directlabels")
library("formattable")

## Set as survey data
dataSvy <- svydesign(ids = ~1, weights = ~ weight, data = data)
summary(dataSvy)

## Number of respondents on each form (before sample restrictions)
sort(table(mtf$V1[mtf$V1 <= 2014]), ascending=T)

## Number of observations (unweighted)
table(data$racesex)

#####################################################################
# Appendix Table A
## Create Appendix Table A variable list
depVars <- c("hfw0", "hfwh", "hfwf", "hhwh",  "hhwf", "h0wf")

## Total Population (Weighted)
tabA_svy <- svyCreateTableOne(vars = depVars, data = dataSvy)
tabA_svy

## By RaceSex
tabA_svy <- svyCreateTableOne(vars = depVars, data = dataSvy, strata = c("racesex"), test = F)
tabA_svy

#####################################################################
# Table 1. Percentage Distribution of Independent Variables; 1976 - 2014

### Create Table 1 variable list
catVars <- c("momed", "momemp", "famstru", "religion",  "region")

### Total Population (Weighted)
table1_svy <- svyCreateTableOne(vars = catVars, data = dataSvy)
table1_svy

### By RaceSex
table1_svy <- svyCreateTableOne(vars = catVars, data = dataSvy, strata = c("racesex"), test = F)
table1_svy

#####################################################################
# Appendix Table B
## Number of missing
sum(complete.cases(miss)) # Observations without missing cases
nrow(miss) - sum(complete.cases(miss)) # Observations with missing cases

## The next three questions ask about your parents. If you were raised mostly by foster parents, stepparents, or others,
## answer for them. For example, if you have both a stepfather and a natural father, answer for the one that was the most
## important in raising you. What is the highest level of schooling your father completed?
table(miss$famstru, miss$momed, exclude=NULL )

### Step-by-step deletion
colSums(is.na(miss))

miss <- miss %>%  filter(!is.na(race))
miss <- miss %>%  filter(!is.na(gender))

miss <- miss %>%  filter(!is.na(hfw0))
miss <- miss %>%  filter(!is.na(hfwh))
miss <- miss %>%  filter(!is.na(hfwf))
miss <- miss %>%  filter(!is.na(hhwh))
miss <- miss %>%  filter(!is.na(hhwf))
miss <- miss %>%  filter(!is.na(h0wf))

miss <- miss %>%  filter(!is.na(momed))
miss <- miss %>%  filter(!is.na(momemp))

miss <- miss %>%  filter(!is.na(famstru))
miss <- miss %>%  filter(!is.na(religion))

# Given the large total sample size, we used listwise deletion to address missing cases.
miss <- na.omit(miss)

#####################################################################
# Figure 1. Young Adults' Expectations of the Division of Work and Family Labor in their Future Families 

## Create the multinomial models with ordinal year
data$year <- as.factor(data$year) 

## Specify reference levels
data$racesex  <- relevel(data$racesex,  ref = "White men")
data$momed    <- relevel(data$momed,    ref = "COMPLETED HIGH SCHOOL")
data$momemp   <- relevel(data$momemp,   ref = "YES, ALL OR NEARLY ALL OF THE TIME")
data$famstru  <- relevel(data$famstru,  ref = "Both Mother & Father")
data$religion <- relevel(data$religion, ref = "RARELY")
data$region   <- relevel(data$region,   ref = "Northeast")

## Run the models
m1 <- multinom(hfw0 ~ year + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
m2 <- multinom(hfwh ~ year + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
m3 <- multinom(hfwf ~ year + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
m4 <- multinom(hhwh ~ year + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
m5 <- multinom(h0wf ~ year + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
m6 <- multinom(hhwf ~ year + racesex + momed + momemp + famstru + religion + region, data, weights = weight)

## Create predicted probilities datesets
pm1   <- ggeffect(m1, terms = c("year"))
pm2   <- ggeffect(m2, terms = c("year"))
pm3   <- ggeffect(m3, terms = c("year"))
pm4   <- ggeffect(m4, terms = c("year"))
pm5   <- ggeffect(m5, terms = c("year"))
pm6   <- ggeffect(m6, terms = c("year"))

pm1$dol <- "hfw0"
pm2$dol <- "hfwh"
pm3$dol <- "hfwf"
pm4$dol <- "hhwh"
pm5$dol <- "h0wf"
pm6$dol <- "hhwf"

mdata = smartbind(pm1, pm2, pm3, pm4, pm5, pm6)
head(mdata)

## save dataset
write.csv(mdata, "figures/dol_Figure 1.csv")

# # If want to start from saved probability file.
mdata <-  read.csv("figures/dol_Figure 1.csv", header = TRUE)

## Prep dataset for graphing

### Readable labels for DOL
mdata$dol <- as.factor(mdata$dol)
levels(mdata$dol)[levels(mdata$dol)=="hfw0"] <- "Husband full-time; Wife at home"
levels(mdata$dol)[levels(mdata$dol)=="hfwh"] <- "Husband full-time; Wife part-time"
levels(mdata$dol)[levels(mdata$dol)=="hfwf"] <- "Both work full-time"
levels(mdata$dol)[levels(mdata$dol)=="hhwh"] <- "Both work part-time"
levels(mdata$dol)[levels(mdata$dol)=="hhwf"] <- "Husband part-time; Wife full-time"
levels(mdata$dol)[levels(mdata$dol)=="h0wf"] <- "Husband at home; Wife full-time"

mdata$dol    <- ordered(mdata$dol,   levels = c("Husband full-time; Wife at home", "Husband full-time; Wife part-time", "Both work full-time", 
                                                 "Both work part-time", "Husband part-time; Wife full-time", "Husband at home; Wife full-time"))
### Order the levels
colnames(mdata)[colnames(mdata)=="response.level"]                <- "level"
mdata$level                                                       <- as_factor(mdata$level)
levels(mdata$level)[levels(mdata$level)=="NOT.AT.ALL.ACCEPTABLE"] <- "NOT AT ALL ACCEPTABLE"
levels(mdata$level)[levels(mdata$level)=="SOMEWHAT.ACCEPTABLE"]   <- "SOMEWHAT ACCEPTABLE"

mdata$level <- ordered(mdata$level, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT ACCEPTABLE", "NOT AT ALL ACCEPTABLE"))

colnames(mdata)[colnames(mdata)=="x"] <- "year"

### Create type variable
mdata <- mdata %>%
  mutate(
    type = case_when(
      dol == "Husband full-time; Wife at home" |
      dol == "Husband full-time; Wife part-time"    ~ "TRADITIONAL",
      dol == "Both work full-time"               |
      dol == "Both work part-time"                  ~ "MATCHED",
      dol == "Husband at home; Wife full-time" |
      dol == "Husband part-time; Wife full-time"    ~ "GENDER ATYPICAL",     
      TRUE                                          ~  NA_character_ ))

### Create values for beginning and end points
mdata <- mdata %>%
  group_by(dol, level) %>%
  mutate(last_value  = last(scales::percent(predicted, accuracy =  1)),
         first_value = first(scales::percent(predicted, accuracy =  1))) 

## Make figure 1
fig1 <- mdata %>%
  filter(level != "SOMEWHAT ACCEPTABLE") %>%
  ggplot(aes(year, predicted, color = dol, ymin = conf.low, ymax = conf.high)) +
  facet_wrap( ~ level, ncol = 1) +
  geom_linerange(show.legend=FALSE, color = "grey") +
  geom_line(size = 1) +
  geom_text_repel(aes(label = dol), # This plots the dol labels on the right side without overlap.
                  data           = subset(mdata, level != "SOMEWHAT ACCEPTABLE" & year == 2014), # Only plot the labels 1 time
                  segment.colour = NA,
                  nudge_x        = 2018 - subset(mdata, level != "SOMEWHAT ACCEPTABLE" &  year == 2014)$year,
                  direction      = "y",
                  hjust          = 0,
                  size           = 3) +
  geom_text_repel(aes(label = last_value), # THis plots the 2014 proportions in line with the dol labels.
                  data           = subset(mdata, level != "SOMEWHAT ACCEPTABLE" &  year == 2014), # Only plot the labels 1 time
                  segment.colour = NA,
                  nudge_x        = 2015 - subset(mdata,  level != "SOMEWHAT ACCEPTABLE" & year == 2014)$year,
                  direction      = "y",
                  hjust          = 0,
                  size           = 3) +
  geom_dl(aes(label=first_value), method = list('first.bumpup', cex = .75)) + #This plots the 1976 proportions.
  coord_cartesian(xlim = c(1976, 2035), # This extendes the x-axis to make room for the dol labels.
                  ylim = c(0, 1),
                  clip = 'off') +   # This keeps the labels from disappearing
  geom_segment(aes(x=1976,xend=2014,y=0,yend=0),   color = "grey90") +
  geom_segment(aes(x=1976,xend=2014,y=.5,yend=.5), color = "grey90") +
  geom_segment(aes(x=1976,xend=2014,y=1,yend=1),   color = "grey90") +
  geom_vline(xintercept = c(1976, 1995, 2014),     color = "grey90") +
  scale_x_continuous(name = "", 
                     breaks   = c(1976, 1995, 2014), 
                     label    = c("1976", "1995", "2014"), 
                     position = "top") +
  scale_y_continuous(labels   = scales::percent_format(accuracy = 1), breaks = NULL) +
  theme_minimal() +
  theme(text             = element_text(size=12),
        legend.title     = element_blank(),
        strip.text.x     = element_text(angle = 0, face="bold", hjust = 0),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.placement  = "outside", 
        plot.margin = unit(c(.25,.5,1,1), "cm")) +
  labs(x = " ", y = " ", fill = "") +
  scale_colour_manual(values=c("#07d3ba", "#0392cf", "#f27777", "#efa937", "#e7298a", "#7570b3"))

fig1

ggsave("figures/dol_figure 1.png", fig1, height = 10, width = 8, dpi = 300)

#####################################################################
# Appendix Table C -- MULTINOMIALS
### https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/

## Ouput the data to pretty html files (for GitHub repo.)
### https://www.princeton.edu/~otorres/NiceOutputR.pdf
stargazer(m1, m2, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="data/mods1-2.html")
stargazer(m3, m4, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="data/mods3-4.html")
stargazer(m5, m6, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="data/mods5-6.html")

### Create csv files to copy/paste data into Excel tables for paper
setwd(file.path(outDir)) # Set the working-directory to the sub-folder where we will save the data output

# Model 1
m1odds <- exp(coef(m1)) # relative risk ratios
write.csv(m1odds, "m1.csv")

z1 <- summary(m1)$coefficients/summary(m1)$standard.errors
p1 <- (1 - pnorm(abs(z1), 0, 1)) * 2 # p values
write.csv(p1, "p1.csv")

# Model 2
m2odds <- exp(coef(m2))
write.csv(m2odds, "m2.csv")

z2 <- summary(m2)$coefficients/summary(m2)$standard.errors
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
write.csv(p2, "p2.csv")

# Model 3
m3odds <- exp(coef(m3))
write.csv(m3odds, "m3.csv")

z3 <- summary(m3)$coefficients/summary(m3)$standard.errors
p3 <- (1 - pnorm(abs(z3), 0, 1)) * 2
write.csv(p3, "p3.csv")

# Model 4
m4odds <- exp(coef(m4))
write.csv(m4odds, "m4.csv")

z4 <- summary(m4)$coefficients/summary(m4)$standard.errors
p4 <- (1 - pnorm(abs(z4), 0, 1)) * 2
write.csv(p4, "p4.csv")

# Model 5
m5odds <- exp(coef(m5))
write.csv(m5odds, "m5.csv")

z5 <- summary(m5)$coefficients/summary(m5)$standard.errors
p5 <- (1 - pnorm(abs(z5), 0, 1)) * 2
write.csv(p5, "p5.csv")

# Model 6
m6odds <- exp(coef(m6))
write.csv(m6odds, "m6.csv")

z6 <- summary(m6)$coefficients/summary(m6)$standard.errors
p6 <- (1 - pnorm(abs(z6), 0, 1)) * 2
write.csv(p6, "p6.csv")