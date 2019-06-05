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

## Create the LCA models

# define function
f<-with(data, cbind(hfw0, hfwh, hfwf, hhwh, hhwf, h0wf)~1) 

# models with different number of groups without covariates:

set.seed(01012) # Try to make the classes come out in the same order each time -- they still don't
 lc1<-poLCA(f, data=data, nclass=1, na.rm = FALSE, nrep=30, maxiter=100000) # Loglinear independence model.
 lc2<-poLCA(f, data=data, nclass=2, na.rm = FALSE, nrep=30, maxiter=100000)
 lc3<-poLCA(f, data=data, nclass=3, na.rm = FALSE, nrep=30, maxiter=100000)
 lc4<-poLCA(f, data=data, nclass=4, na.rm = FALSE, nrep=30, maxiter=100000)
 lc5<-poLCA(f, data=data, nclass=5, na.rm = FALSE, nrep=30, maxiter=100000)
 lc6<-poLCA(f, data=data, nclass=6, na.rm = FALSE, nrep=30, maxiter=100000) # The chosen model
 lc7<-poLCA(f, data=data, nclass=7, na.rm = FALSE, nrep=30, maxiter=100000)
 lc8<-poLCA(f, data=data, nclass=8, na.rm = FALSE, nrep=30, maxiter=100000)

## Testing if controlling for year makes a difference
 t<-with(data, cbind(hfw0, hfwh, hfwf, hhwh, hhwf, h0wf)~year)
 lc6t<-poLCA(t, data=data, nclass=3, na.rm = FALSE, nrep=30, maxiter=100000) 

## Creating Test statistics to compare models
 results <- data.frame(Model=c("Model 1"),
                       log_likelihood=lc1$llik,
                       df = lc1$resid.df,
                       BIC=lc1$bic,
                       ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                       CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                       likelihood_ratio=lc1$Gsq)
 
 results$Model<-as.integer(results$Model)
 results[1,1]<-c("Model 1")
 results[2,1]<-c("Model 2")
 results[3,1]<-c("Model 3")
 results[4,1]<-c("Model 4")
 results[5,1]<-c("Model 5")
 results[6,1]<-c("Model 6")
 results[7,1]<-c("Model 7")
 results[8,1]<-c("Model 8")
 
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
 entropy<-function (p) sum(-p*log(p))
 
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

#  combining results to a dataframe
 colnames(results)<-c("Model","log-likelihood","resid. df","BIC","aBIC","cAIC","likelihood-ratio","Entropy")
 lca_results<-results
# 
# # Review results
ztable::ztable(lca_results)
# 
# ## Plot of LCA model comparison statistics
# # Order categories of results$model in order of appearance

results$Model <- as_factor(results$Model) 
# 
# #convert to long format
results2<-tidyr::gather(results,Kriterium,Guete,4:7)
results2
# 
# #plot
fit.plot<-ggplot(results2) + 
   geom_point(aes(x=Model,y=Guete),size=3) +
   geom_line(aes(Model, Guete, group = 1)) +
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

ggsave("figures/dol_figure A.png", fit.plot, width = 16, height = 12, units = "cm", dpi = 300)

### Analysis of chosen LCA model -- 6 classes
## Proportion of people in each class
round(prop.table(table(lc6$predclass)),4)*100

## Compare classes with graphs
# Make a cleaner plot, converting a list to a DF with melt():
lcModelProbs <- melt(lc6$probs)
lcModelProbs$L1 <- as.factor(lcModelProbs$L1)

levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hfw0"] <- "H FT \nW Home"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hfwh"] <- "H FT \nW PT"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hfwf"] <- "Both FT"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hhwh"] <- "Both PT"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="h0wf"] <- "H Home \nW FT"
levels(lcModelProbs$L1)[levels(lcModelProbs$L1)=="hhwf"] <- "H PT \nW FT"
lcModelProbs$L1 <- ordered(lcModelProbs$L1, levels =c("H FT \nW Home", "H FT \nW PT", "Both FT", "Both PT", "H Home \nW FT", "H PT \nW FT"))

levels(lcModelProbs$Var2)[levels(lcModelProbs$Var2)=="NOT AT ALL ACCEPTABLE"] <- "Not Acceptable"
levels(lcModelProbs$Var2)[levels(lcModelProbs$Var2)=="SOMEWHAT ACCEPTABLE"] <- "Somewhat Acceptable"
levels(lcModelProbs$Var2)[levels(lcModelProbs$Var2)=="ACCEPTABLE"] <- "Acceptable"  
levels(lcModelProbs$Var2)[levels(lcModelProbs$Var2)=="DESIRABLE"] <- "Desirable"

lcModelProbs$Var2 <- ordered(lcModelProbs$Var2,  levels = c("Not Acceptable", "Somewhat Acceptable", "Acceptable", "Desirable"))

lcModelProbs$value <- round(lcModelProbs$value, 3)

# The classes come out in random order every time....... Use proporitons to identify the classes
round(prop.table(table(lc6$predclass)),4)*100

levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 1: "] <- "Conventional" # 26% 
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 2: "] <- "Conventional Realists" # 23%
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 3: "] <- "Intensive Parents" # 15%
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 4: "] <- "Dual-earners" # 12% 
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 5: "] <- "Neo-traditional" # 21% 
levels(lcModelProbs$Var1)[levels(lcModelProbs$Var1)=="class 6: "] <- "Strong Intensive Parents" # 3%
lcModelProbs$Var1 <- factor(lcModelProbs$Var1, levels = c("Conventional", "Neo-traditional", "Conventional Realists", 
                                          "Dual-earners", "Intensive Parents", "Strong Intensive Parents"))

write.csv(lcModelProbs, "figures/dol_Figure 3.csv")

# Figure 3 -- 6 latent classes

fig3 <- ggplot(lcModelProbs, aes(x = L1, y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Var1, ncol = 3) + 
  scale_fill_manual(values=c("#e7298a", "#7570b3", "#d95f02", "#1b9e77"), name = "") +
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
fig3

ggsave("figures/dol_figure 3.png", fig3, width = 24, height = 16, units = "cm", dpi = 300)

### Multinomial LCA

# Getting probabilities
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

#Rename classes -- same order as above
levels(dat$class)[levels(dat$class)=="1"] <- "Neo-traditional"
levels(dat$class)[levels(dat$class)=="2"] <- "Conventional Realists"
levels(dat$class)[levels(dat$class)=="3"] <- "Intensive Parents"
levels(dat$class)[levels(dat$class)=="4"] <- "Conventional" 
levels(dat$class)[levels(dat$class)=="5"] <- "Intensive Parents"
levels(dat$class)[levels(dat$class)=="6"] <- "Dual-earners"
dat$class <- factor(dat$class, levels = c("Conventional", "Neo-traditional", "Conventional Realists", 
                                          "Dual-earners", "Intensive Parents"))


## LCA Multinomial Logit

mn <- multinom(class ~ year * racesex + momed + momemp + famstru + religion + region, data = dat, weights = weight)

mnodds <- coef(mn)
write.csv(mnodds, "data/mn.csv")

zmn <- summary(mn)$coefficients/summary(mn)$standard.errors
pmn <- (1 - pnorm(abs(zmn), 0, 1)) * 2
write.csv(pmn, "data/pmn.csv")


lcapp <- ggpredict(mn, terms = c("year[1976:2014]", "racesex"))

colnames(lcapp)[colnames(lcapp) == 'response.level'] <- 'class'

lcapp$class <- factor(lcapp$class, levels = c("Conventional", "Neo-traditional", "Conventional Realists", 
                                          "Dual-earners", "Intensive Parents"))

write.csv(lcapp, "figures/dol_Figure 4.csv")


## Figure 4

fig4 <- ggplot(lcapp) + 
  geom_line(aes(x = x, y = predicted, colour = group), size=1.2) + 
  facet_wrap(~ class) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
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

fig4

ggsave("figures/dol_figure 4.png", fig4, width = 16.5, height = 14, units = "cm", dpi = 300)