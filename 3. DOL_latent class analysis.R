# Set up the data
setwd(repoDir) # This will set the working directory to the master project folder

# Load libraries
library("poLCA")
library("ztable")
library("forcats")
library("reshape2")
library("nnet")
library("ggplot2")
library("ggeffects")

##########################################################################################
# A. Conduct the LCA
##########################################################################################
## A1. Create the LCA models

# define function
f<-with(data, cbind(hfw0, hfwh, hfwf, hhwh, hhwf, h0wf)~1) 

# models with different number of groups without covariates:

 lc1<-poLCA(f, data=data, nclass=1, na.rm = FALSE, nrep=30, maxiter=100000) # Loglinear independence model.
 lc2<-poLCA(f, data=data, nclass=2, na.rm = FALSE, nrep=30, maxiter=100000)
 lc3<-poLCA(f, data=data, nclass=3, na.rm = FALSE, nrep=30, maxiter=100000)
 lc4<-poLCA(f, data=data, nclass=4, na.rm = FALSE, nrep=30, maxiter=100000)
 lc5<-poLCA(f, data=data, nclass=5, na.rm = FALSE, nrep=30, maxiter=100000)
 lc6<-poLCA(f, data=data, nclass=6, na.rm = FALSE, nrep=30, maxiter=100000) # The chosen model
 lc7<-poLCA(f, data=data, nclass=7, na.rm = FALSE, nrep=30, maxiter=100000)
 lc8<-poLCA(f, data=data, nclass=8, na.rm = FALSE, nrep=30, maxiter=100000)

##########################################################################################
## A2. Create fit statistics to compare models

 results <- data.frame(k_classes=c("1"),
                       log_likelihood=lc1$llik,
                       df = lc1$resid.df,
                       BIC=lc1$bic,
                       ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                       CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                       likelihood_ratio=lc1$Gsq)
 
 results$k_classes<-as.integer(results$k_classes)
 results[1,1]<-c("1")
 results[2,1]<-c("2")
 results[3,1]<-c("3")
 results[4,1]<-c("4")
 results[5,1]<-c("5")
 results[6,1]<-c("6")
 results[7,1]<-c("7")
 results[8,1]<-c("8")
 
 results[2,2]<-lc2$llik
 results[3,2]<-lc3$llik
 results[4,2]<-lc4$llik
 results[5,2]<-lc5$llik
 results[6,2]<-lc6$llik
 results[7,2]<-lc7$llik
 results[8,2]<-lc8$llik
 
 results[2,3]<-lc2$resid.df
 results[3,3]<-lc3$resid.df
 results[4,3]<-lc4$resid.df
 results[5,3]<-lc5$resid.df
 results[6,3]<-lc6$resid.df
 results[7,3]<-lc7$resid.df
 results[8,3]<-lc8$resid.df
 
 results[2,4]<-lc2$bic
 results[3,4]<-lc3$bic
 results[4,4]<-lc4$bic
 results[5,4]<-lc5$bic
 results[6,4]<-lc6$bic
 results[7,4]<-lc7$bic
 results[8,4]<-lc8$bic
 
 results[2,5]<-(-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) # abic
 results[3,5]<-(-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar) 
 results[4,5]<-(-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar) 
 results[5,5]<-(-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
 results[6,5]<-(-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)
 results[7,5]<-(-2*lc7$llik) + ((log((lc7$N + 2)/24)) * lc7$npar)
 results[8,5]<-(-2*lc8$llik) + ((log((lc8$N + 2)/24)) * lc8$npar)
 
 results[2,6]<- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) # caic
 results[3,6]<- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
 results[4,6]<- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
 results[5,6]<- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
 results[6,6]<- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))
 results[7,6]<- (-2*lc7$llik) + lc7$npar * (1 + log(lc7$N))
 results[8,6]<- (-2*lc8$llik) + lc8$npar * (1 + log(lc8$N))
 
 results[2,7]<-lc2$Gsq
 results[3,7]<-lc3$Gsq
 results[4,7]<-lc4$Gsq
 results[5,7]<-lc5$Gsq
 results[6,7]<-lc6$Gsq
 results[7,7]<-lc7$Gsq
 results[8,7]<-lc8$Gsq
 
#  calculate the Entropy (a pseudo-r-squared)
 # MIT license
 # Author: Daniel Oberski
 # Input: result of a poLCA model fit
 # Output: entropy R^2 statistic (Vermunt & Magidson, 2013, p. 71)
 # See: daob.nl/wp-content/uploads/2015/07/ESRA-course-slides.pdf
 # And: https://www.statisticalinnovations.com/wp-content/uploads/LGtecnical.pdf
 # And: https://gist.github.com/daob/c2b6d83815ddd57cde3cebfdc2c267b3
 
machine_tolerance <- sqrt(.Machine$double.eps)
 
entropy <- function(p) {
   p <- p[p > machine_tolerance] # since Lim_{p->0} p log(p) = 0
   sum(-p * log(p))
 }
 
 results$R2_entropy
 results[1,8]<-c("-")
 
 error_prior<-entropy(lc2$P) # class proportions model 2
 error_post<-mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
 results[2,8]<-round(((error_prior-error_post) / error_prior),3)
 
 error_prior<-entropy(lc3$P) #  class proportions model 3
 error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
 results[3,8]<-round(((error_prior-error_post) / error_prior),3)
 
 error_prior<-entropy(lc4$P) #  class proportions model 4
 error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
 results[4,8]<-round(((error_prior-error_post) / error_prior),3)
 
 error_prior<-entropy(lc5$P) #  class proportions model 5
 error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
 results[5,8]<-round(((error_prior-error_post) / error_prior),3)
 
 error_prior<-entropy(lc6$P) #  class proportions model 6
 error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
 results[6,8]<-round(((error_prior-error_post) / error_prior),3)
 
 error_prior<-entropy(lc7$P) #  class proportions model 7
 error_post<-mean(apply(lc7$posterior,1, entropy),na.rm = TRUE)
 results[7,8]<-round(((error_prior-error_post) / error_prior),3)
 
 error_prior<-entropy(lc8$P) #  class proportions model 8
 error_post<-mean(apply(lc8$posterior,1, entropy),na.rm = TRUE)
 results[8,8]<-round(((error_prior-error_post) / error_prior),3)
 
 # BIC % change ((2-1)/1)*100
 results[1,9]<-c("-")
 results[2,9]<- round(((lc2$bic -lc1$bic)/lc1$bic) * 100 ,3)
 results[3,9]<- round(((lc3$bic -lc2$bic)/lc2$bic) * 100 ,3)
 results[4,9]<- round(((lc4$bic -lc3$bic)/lc3$bic) * 100 ,3)
 results[5,9]<- round(((lc5$bic -lc4$bic)/lc4$bic) * 100 ,3)
 results[6,9]<- round(((lc6$bic -lc5$bic)/lc5$bic) * 100 ,3)
 results[7,9]<- round(((lc7$bic -lc6$bic)/lc6$bic) * 100 ,3)
 results[8,9]<- round(((lc8$bic -lc7$bic)/lc7$bic) * 100 ,3)
 
#  combining results to a dataframe
 colnames(results)<-c("k_classes","log-likelihood","resid. df","BIC","aBIC","cAIC","likelihood-ratio","Entropy", "BIC % Change")
 lca_results<-results
# 
# # Review results
ztable::ztable(lca_results)

setwd(outDir)
write_csv(lca_results, "Table 2.csv")
setwd(repoDir)

# ## Plot of LCA model comparison statistics
# # Order categories of results$model in order of appearance

results$k_classes <- as_factor(results$k_classes)

# #convert to long format
results2<-tidyr::gather(results,Kriterium,Guete,c("BIC", "cAIC", "likelihood-ratio"))
results2

# #plot
fit.plot<-ggplot(results2) + 
   geom_point(aes(x=k_classes,y=Guete),size=3) +
   geom_line(aes(k_classes, Guete, group = 1)) +
   theme_bw()+
   labs(x = "", y="", title = "") + 
   facet_grid(Kriterium ~. ,scales = "free") +
   theme_bw(base_size = 11, base_family = "") +   
   theme(panel.grid.major.x = element_blank() ,
         panel.grid.major.y = element_line(colour="grey", size=0.5),
         legend.title       = element_text(size = 11, face = 'bold'),
         axis.text          = element_text(size = 11),
         axis.title         = element_text(size = 11),
         legend.text        = element_text(size = 11),
         axis.line          = element_line(colour = "black"))
fit.plot
ggsave("figures/dol_figure A.png", fit.plot, width = 16, height = 12, units = "cm", dpi = 300)

##########################################################################################
## A3. Analysis of chosen LCA model -- 6 classes

### Proportion of people in each class
round(prop.table(table(lc6$predclass)),4)*100

## Compare classes with graphs
# Make a cleaner plot, converting a list to a DF with melt():
lcModelProbs <- melt(lc6$probs)
lcModelProbs$L1 <- as.factor(lcModelProbs$L1)

levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hfw0"] <- "H FT \nW Home"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hfwh"] <- "H FT \nW PT"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hfwf"] <- "Both \nFT"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hhwh"] <- "Both \nPT"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="h0wf"] <- "H Home \nW FT"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hhwf"] <- "H PT \nW FT"

levels(lcModelProbs$Var2)[levels(lcModelProbs$Var2)=="NOT AT ALL ACCEPTABLE"] <- "Not Acceptable"
levels(lcModelProbs$Var2)[levels(lcModelProbs$Var2)=="SOMEWHAT ACCEPTABLE"] <- "Somewhat Acceptable"
levels(lcModelProbs$Var2)[levels(lcModelProbs$Var2)=="ACCEPTABLE"] <- "Acceptable"  
levels(lcModelProbs$Var2)[levels(lcModelProbs$Var2)=="DESIRABLE"] <- "Desirable"

lcModelProbs$value <- round(lcModelProbs$value, 3)

# The classes come out in random order every time....... Use proporitons to identify the classes
round(prop.table(table(lc6$predclass)),4)*100

levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 1: "] <- "Neo-traditional (21%)" # 21%    
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 2: "] <- "Dual-earners (12%)" # 12% 
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 3: "] <- "Intensive Parents (15%)" # 15%   
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 4: "] <- "Strong Intensive Parents (3%)" # 3%
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 5: "] <- "Conventional Realists (23%)" # 23%
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 6: "] <- "Conventional (26%)" # 26%

## Save the data file
write.csv(lcModelProbs, "figures/dol_Figure 2.csv")

## If want to start from saved output file.
lcModelProbs <-  read.csv("figures/dol_Figure 2.csv", header = TRUE)

#########################################################################################
# A4. Figure 2 -- Latent Classes

## Order the classes, levels, and dol arrangements
lcModelProbs$Var1 <- factor(lcModelProbs$Var1, levels = c("Conventional (26%)", "Neo-traditional (21%)", "Conventional Realists (23%)", 
                                                          "Dual-earners (12%)", "Intensive Parents (15%)", "Strong Intensive Parents (3%)"))

lcModelProbs$Var2 <- ordered(lcModelProbs$Var2,  levels = c("Not Acceptable", "Somewhat Acceptable", "Acceptable", "Desirable"))

lcModelProbs$L1 <- ordered(lcModelProbs$L1, levels =c("H FT \nW Home", "H FT \nW PT", "Both \nFT", "Both \nPT", "H Home \nW FT", "H PT \nW FT"))

fig2 <- ggplot(lcModelProbs, aes(x = L1, y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Var1, ncol = 3) + 
  scale_fill_manual(values=c("#f27777", "#efa937", "#07d3ba", "#6399ab"), name = "") +
  theme_minimal() +
  theme(
    strip.text.x  = element_text(size = 11, face="bold"),
    axis.title    = element_text(size = 11), 
    axis.text     = element_text(size = 7), 
    plot.title    = element_text(size = 11, face = "bold"),
    legend.text   = element_text(size=8),
    legend.justification = "top",
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank()) +
  labs(x = "", y = "")
fig2

ggsave("figures/dol_figure 2.png", fig2, width = 24, height = 16, units = "cm", dpi = 300)

##########################################################################################
# B. Multinomial Analysis of LCA
##########################################################################################
## B1. Combine LCA class data with respondent demographic data

# Add probabilities to demo data
dat <- cbind(data, lc6$posterior)
dat$"1" <- round(dat$"1", 2)
dat$"2" <- round(dat$"2", 2)
dat$"3" <- round(dat$"3", 2)
dat$"4" <- round(dat$"4", 2)
dat$"5" <- round(dat$"5", 2)
dat$"6" <- round(dat$"6", 2)

# Renaming class columns
colnames(dat)[colnames(dat) == '1'] <- 'C1'
colnames(dat)[colnames(dat) == '2'] <- 'C2'
colnames(dat)[colnames(dat) == '3'] <- 'C3'
colnames(dat)[colnames(dat) == '4'] <- 'C4'
colnames(dat)[colnames(dat) == '5'] <- 'C5'
colnames(dat)[colnames(dat) == '6'] <- 'C6'

classes <- c("C1", "C2", "C3", "C4", "C5", "C6")
dat$class <- max.col(dat[classes], "first") #tie breakers go to first class
dat$class <- as.factor(dat$class)

#Rename classes -- same order as above -- combine Intensive Parents!
levels(dat$class)[levels(dat$class)=="1"] <- "Neo-traditional" 
levels(dat$class)[levels(dat$class)=="2"] <- "Dual-earners" 
levels(dat$class)[levels(dat$class)=="3"] <- "Intensive Parents" 
levels(dat$class)[levels(dat$class)=="4"] <- "Intensive Parents"
levels(dat$class)[levels(dat$class)=="5"] <- "Conventional Realists"
levels(dat$class)[levels(dat$class)=="6"] <- "Conventional"
dat$class <- factor(dat$class, levels = c("Conventional", "Neo-traditional", "Conventional Realists", 
                                          "Dual-earners", "Intensive Parents"))

dat$year <- as.factor(dat$year)
#########################################################################################
## B2. LCA Multinomial Logit (total sample pooled)

mn_all <- multinom(class ~ year + racesex + momed + momemp + famstru + religion + region, data = dat, weights = weight)

lcapp <- ggeffect(mn_all, terms = c("year"))

colnames(lcapp)[colnames(lcapp) == 'response.level'] <- 'class'
lcapp$class <- as.factor(lcapp$class)

levels(lcapp$class)[levels(lcapp$class)=="Conventional.Realists"] <- "Conventional Realists"
levels(lcapp$class)[levels(lcapp$class)=="Dual.earners"]          <- "Dual-earners"
levels(lcapp$class)[levels(lcapp$class)=="Intensive.Parents"]     <- "Intensive Parents"
levels(lcapp$class)[levels(lcapp$class)=="Neo.traditional"]       <- "Neo-traditional"

lcapp$class <- factor(lcapp$class, levels = c("Conventional", "Neo-traditional", "Conventional Realists", 
                                              "Dual-earners", "Intensive Parents"))
## Save the data as a csv file.
write.csv(lcapp, "figures/dol_figure C.csv")

## If want to start from saved output file.
lcapp <-  read.csv("figures/dol_Figure C.csv", header = TRUE)

#########################################################################################
## B3. Appendix Figure C
lcapp <- lcapp %>%
  group_by(class) %>%
  mutate(last_value = last(scales::percent(predicted, accuracy =  1))) 

lcapp <- lcapp %>%
  group_by(class) %>%
  mutate(first_value = first(scales::percent(predicted, accuracy =  1))) 

figC <- ggplot(lcapp, aes(x = x, y = predicted, color = class, ymin = conf.low, ymax = conf.high)) + 
  geom_linerange(show.legend=FALSE, color = "grey90") +
  geom_line(size=1.2) + 
  geom_text_repel(aes(label = class), # This plots the dol labels on the right side without overlap.
                  data           = subset(lcapp, x == "2014"), # Only plot the labels 1 time
                  segment.colour = NA,
                  nudge_x        = 2018 - subset(lcapp, x == "2014")$x,
                  direction      = "y",
                  hjust          = 0,
                  size           = 4) +
  geom_text_repel(aes(label = last_value), # THis plots the 2014 proportions in line with the dol labels.
                  data           = subset(lcapp, x == "2014"), # Only plot the labels 1 time
                  segment.colour = NA,
                  nudge_x        = 2015 - subset(lcapp, x == "2014")$x,
                  direction      = "y",
                  hjust          = 0,
                  size           = 3) +
  geom_dl(aes(label=first_value), method = list('first.bumpup', cex = .75)) +
  coord_cartesian(xlim = c(1976, 2035), # This extendes the x-axis to make room for the dol labels.
                  ylim = c(0, .60),
                  clip = 'off') +   # This keeps the labels from disappearing
  geom_segment(aes(x=1976, xend=2014, y=0,   yend=0),   color = "grey90") +
  geom_segment(aes(x=1976, xend=2014, y=.25, yend=.25), color = "grey90") +
  geom_segment(aes(x=1976, xend=2014, y=.5,  yend=.5),  color = "grey90") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = c(0, .25, .5)) +
  scale_x_continuous(breaks=c(1976, 2014), label = c("'76", "'14")) +
  theme_minimal() +
    theme(
    text                 = element_text(size=12),
    strip.text.x         = element_text(face="bold"),
    plot.title           = element_text(face = "bold"),
    legend.position      = "none",
    legend.title         = element_text(face = "bold"),
    panel.grid.minor     = element_blank(),
    panel.grid.major     = element_blank(),
    plot.margin          = unit(c(.25,.25,1,1), "cm")) +
  labs(x = "", y = "")

figC

ggsave("figures/dol_figure C.png", figC, height = 7, width = 8, dpi = 300)

##########################################################################################
## B4. LCA Multinomial Logit (Year & Race/Sex interaction)
mn <- multinom(class ~ year * racesex + momed + momemp + famstru + religion + region, data = dat, weights = weight)

mnodds <- coef(mn)
write.csv(mnodds, "data/mn.csv")

zmn <- summary(mn)$coefficients/summary(mn)$standard.errors
pmn <- (1 - pnorm(abs(zmn), 0, 1)) * 2
write.csv(pmn, "data/pmn.csv")

lcapp <- ggeffect(mn, terms = c("year[1976:2014]", "racesex"))

colnames(lcapp)[colnames(lcapp) == 'response.level'] <- 'class'
lcapp$class <- as_factor(lcapp$class)

levels(lcapp$class)[levels(lcapp$class)=="Conventional"]          <- "Conventional"
levels(lcapp$class)[levels(lcapp$class)=="Conventional.Realists"] <- "Conventional Realists"
levels(lcapp$class)[levels(lcapp$class)=="Dual.earners"]          <- "Dual-earners"
levels(lcapp$class)[levels(lcapp$class)=="Intensive.Parents"]     <- "Intensive Parents"
levels(lcapp$class)[levels(lcapp$class)=="Neo.traditional"]       <- "Neo-traditional"

write.csv(lcapp, "figures/dol_Figure 3.csv")

# # If want to start from saved output file.
lcapp <-  read.csv("figures/dol_Figure 3.csv", header = TRUE)

#########################################################################################
## B5. Figure 3 -- LCA by Race/Sex * Year

lcapp$class <- factor(lcapp$class, levels = c("Conventional", "Neo-traditional", "Conventional Realists", 
                                              "Dual-earners", "Intensive Parents"))

fig3 <- ggplot(lcapp, aes(x = x, y = predicted, colour = group, ymin = conf.low, ymax = conf.high)) + 
  geom_smooth(method = "loess", span = 0.5, se = FALSE, size=1.2) + 
  facet_wrap(~ class) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1),
                     breaks = c(0, .25, .5, .75)) +
  scale_colour_manual(values=c("#fdb863", "#e66101", "#b2abd2", "#5e3c99"), name = "Race & Gender") +
  theme_minimal() +
  theme(
    strip.text.x  = element_text(size = 10, face="bold"),
    plot.subtitle = element_text(size = 10),
    axis.title    = element_text(size = 10), 
    axis.text     = element_text(size = 10), 
    plot.title    = element_text(size = 10, face = "bold"),
    legend.position= c(0.9, 0.25),
    legend.text=element_text(size=10),
    legend.title=element_text(size=10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin=unit(c(.5,1,.5,1),"cm"),
    panel.spacing= unit(1.5, "lines")) +
  scale_x_discrete(limits=c(1976, 2014), label = c("'76", "'14")) +
  labs(x = "", y = "Predicted Probability of Class Membership \n")

fig3

ggsave("figures/dol_figure 3.png", fig3, width = 16.5, height = 14, units = "cm", dpi = 300)