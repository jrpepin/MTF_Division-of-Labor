# Set up the data
setwd(repoDir) # This will set the working directory to the master project folder

# Load libraries
library(nnet)
library(stargazer)
library(gtools)
library(ggeffects)
library(ggplot2)

## Create the multinomial models with interactions
med1 <- multinom(hfw0 ~ year * momed + racesex + momemp + famstru + religion + region, data, weights = weight)
med2 <- multinom(hfwh ~ year * momed + racesex + momemp + famstru + religion + region, data, weights = weight)
med3 <- multinom(hfwf ~ year * momed + racesex + momemp + famstru + religion + region, data, weights = weight)
med4 <- multinom(hhwh ~ year * momed + racesex + momemp + famstru + religion + region, data, weights = weight)
med5 <- multinom(h0wf ~ year * momed + racesex + momemp + famstru + religion + region, data, weights = weight)
med6 <- multinom(hhwf ~ year * momed + racesex + momemp + famstru + religion + region, data, weights = weight)

## Ouput the data to pretty html files (for GitHub repo.)
### https://www.princeton.edu/~otorres/NiceOutputR.pdf
stargazer(med1, med2, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="data/modsed1-2.html")
stargazer(med3, med4, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="data/modsed3-4.html")
stargazer(med5, med6, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="data/modsed5-6.html")

# Create predicted probilities datesets

pmed1   <- ggeffect(med1, terms = c("year[1976:2014]", "momed"))
pmed2   <- ggeffect(med2, terms = c("year[1976:2014]", "momed"))
pmed3   <- ggeffect(med3, terms = c("year[1976:2014]", "momed"))
pmed4   <- ggeffect(med4, terms = c("year[1976:2014]", "momed"))
pmed5   <- ggeffect(med5, terms = c("year[1976:2014]", "momed"))
pmed6   <- ggeffect(med6, terms = c("year[1976:2014]", "momed"))

pmed1$dol <- "hfw0"
pmed2$dol <- "hfwh"
pmed3$dol <- "hfwf"
pmed4$dol <- "hhwh"
pmed5$dol <- "h0wf"
pmed6$dol <- "hhwf"


meddata = smartbind(pmed1, pmed2, pmed3, pmed4, pmed5, pmed6)
head(meddata)

meddata$dol <- as.factor(meddata$dol)
levels(meddata$dol)[levels(meddata$dol)=="hfw0"] <- "Husband full-time;\n Wife at home"
levels(meddata$dol)[levels(meddata$dol)=="hfwh"] <- "Husband full-time;\n Wife part-time"
levels(meddata$dol)[levels(meddata$dol)=="hfwf"] <- "Both work full-time"
levels(meddata$dol)[levels(meddata$dol)=="hhwh"] <- "Both work part-time"
levels(meddata$dol)[levels(meddata$dol)=="hhwf"] <- "Husband part-time;\n Wife full-time"
levels(meddata$dol)[levels(meddata$dol)=="h0wf"] <- "Husband at home;\n Wife full-time"

meddata$dol    <- ordered(meddata$dol,   levels = c("Husband full-time;\n Wife at home", "Husband full-time;\n Wife part-time", "Both work full-time", 
                                                "Both work part-time", "Husband at home;\n Wife full-time", "Husband part-time;\n Wife full-time"))

colnames(meddata)[colnames(meddata)=="response.level"] <- "level"
meddata$level <- as_factor(meddata$level, ordered = TRUE)
levels(meddata$level)[levels(meddata$level)=="NOT.AT.ALL.ACCEPTABLE"] <- "NOT AT ALL\n ACCEPTABLE"
levels(meddata$level)[levels(meddata$level)=="SOMEWHAT.ACCEPTABLE"] <- "SOMEWHAT\n ACCEPTABLE"

meddata$level <- ordered(meddata$level, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT\n ACCEPTABLE",  "NOT AT ALL\n ACCEPTABLE"))

levels(meddata$group)[levels(meddata$group)=="LESS THAN HIGH SCHOOL"]                         <- "< HIGH SCHOOL"
levels(meddata$group)[levels(meddata$group)=="COMPLETED HIGH SCHOOL"]                         <- "HIGH SCHOOL"
levels(meddata$group)[levels(meddata$group)=="COMPLETED COLLEGE"]                             <- "COLLEGE"
levels(meddata$group)[levels(meddata$group)=="GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE"] <- "COLLEGE +"

meddata$group <- factor(meddata$group, levels = c("< HIGH SCHOOL", "HIGH SCHOOL", "SOME COLLEGE", 
                                                "COLLEGE", "COLLEGE +"))
colnames(meddata)[colnames(meddata)=="x"] <- "year"

write.csv(meddata, "figures/dol_Figure B.csv")

## Figure B
figB <- meddata %>%
  ggplot(aes(year, predicted, color = group, label = round(predicted, 1))) +
  geom_line(aes(linetype = group), size =1) +
  facet_grid(~ level ~ dol) +
  scale_linetype_manual(values=c("solid", "twodash", "longdash", "dotted", "dashed"))+
  theme_minimal() +
  scale_x_continuous(name = "", breaks = c(1976, 2014), label = c("'76", "'14")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x      = element_text(size = 9),
        strip.text.x     = element_text(size = 8, face = "bold"),
        strip.text.y     = element_text(size = 9, face = "bold"),
        axis.title       = element_text(size = 9), 
        axis.text        = element_text(size = 9), 
        legend.position  = 'top',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_blank(),
        panel.spacing = unit(1.25, "lines")) +
  labs(x = " ", y = "\n", fill = "") +
  guides(size=FALSE, linetype = FALSE) +
  scale_color_manual(values=c("#fdb863", "#e66101", "#008837",  "#b2abd2", "#5e3c99"), name = "Mothers' Education") +
  geom_hline(yintercept = .5,   color = "grey90") +
  geom_vline(xintercept = 1995, color = "grey90")

figB

ggsave("figures/dol_figure B.png", figB, width = 24, height = 16, units = "cm", dpi = 300)

## Appendix -- Education
mn_edu <- multinom(class ~ year * momed + racesex + momemp + famstru + religion + region, data = dat)

eduodds <- coef(mn_edu)
write.csv(eduodds, "mn_edu.csv")

zmnedu <- summary(mn_edu)$coefficients/summary(mn_edu)$standard.errors
pmnedu <- (1 - pnorm(abs(zmnedu), 0, 1)) * 2
write.csv(pmnedu, "pmn_edu.csv")


lcaedu <- ggpredict(mn_edu, terms = c("year[1976:2014]", "momed"))

colnames(lcaedu)[colnames(lcaedu) == 'response.level'] <- 'class'

lcaedu$class   <- as.factor(lcaedu$class)
lcaedu$class <- factor(lcaedu$class, levels = c("Conventional", "Neo-traditional", "Conventional Realists", 
                                              "Dual-earners", "Intensive Parents"))

levels(lcaedu$group)[levels(lcaedu$group)=="LESS THAN HIGH SCHOOL"]                         <- "< HIGH SCHOOL"
levels(lcaedu$group)[levels(lcaedu$group)=="COMPLETED HIGH SCHOOL"]                         <- "HIGH SCHOOL"
levels(lcaedu$group)[levels(lcaedu$group)=="COMPLETED COLLEGE"]                             <- "COLLEGE"
levels(lcaedu$group)[levels(lcaedu$group)=="GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE"] <- "COLLEGE +"

lcaedu$group <- factor(lcaedu$group, levels = c("< HIGH SCHOOL", "HIGH SCHOOL", "SOME COLLEGE", 
                                                "COLLEGE", "COLLEGE +"))


write.csv(lcaedu, "figures/dol_Figure C.csv")


## Figure C
figC <- ggplot(lcaedu) + 
  geom_line(aes(x = x, y = predicted, colour = group), size=1.5) + 
  facet_wrap(~ class) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values=c("#fdb863", "#e66101", "#404040",  "#b2abd2", "#5e3c99"), name = "Mom's Education") +
  theme_minimal() +
  theme(
    strip.text.x  = element_text(size = 10, face="bold"),
    plot.subtitle = element_text(size = 10),
    axis.title    = element_text(size = 10), 
    axis.text     = element_text(size = 10), 
    plot.title    = element_text(size = 10, face = "bold"),
    legend.position= c(0.94, 0.25),
    legend.text=element_text(size=10),
    legend.title=element_text(size=10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin=unit(c(.5,1,.5,1),"cm"),
    panel.spacing= unit(1.5, "lines")) +
  scale_x_discrete(limits=c(1976, 2014), label = c("'76", "'14")) +
  labs(x = "", y = "Predicted Probability of Class Membership \n")

ggsave("figures/dol_figure C.png", figC, width = 16.5, height = 14, units = "cm", dpi = 300)