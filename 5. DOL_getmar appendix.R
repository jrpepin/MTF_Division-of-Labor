library(nnet)
library(stargazer)

setwd("C:/Users/Joanna/Dropbox/Dernberger/Division of Labor/Tables")


## Create the multinomial models with interactions

mar1 <- multinom(hfw0 ~ year * getmar + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
mar2 <- multinom(hfwh ~ year * getmar + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
mar3 <- multinom(hfwf ~ year * getmar + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
mar4 <- multinom(hhwh ~ year * getmar + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
mar5 <- multinom(h0wf ~ year * getmar + racesex + momed + momemp + famstru + religion + region, data, weights = weight)
mar6 <- multinom(hhwf ~ year * getmar + racesex + momed + momemp + famstru + religion + region, data, weights = weight)

## Ouput the data to pretty html files (for GitHub repo.)
### https://www.princeton.edu/~otorres/NiceOutputR.pdf
stargazer(mar1, mar2, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="modsed1-2.html")
stargazer(mar3, mar4, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="modsed3-4.html")
stargazer(mar5, mar6, type="html", star.cutoffs = c(0.05, 0.01, 0.001), digits = 2, style = "asr", initial.zero = FALSE, out="modsed5-6.html")

# Create predicted probilities datesets
library(ggeffects)


pmar1   <- ggeffect(mar1, terms = c("year[1976,2014]", "getmar"))
pmar2   <- ggeffect(mar2, terms = c("year[1976,2014]", "getmar"))
pmar3   <- ggeffect(mar3, terms = c("year[1976,2014]", "getmar"))
pmar4   <- ggeffect(mar4, terms = c("year[1976,2014]", "getmar"))
pmar5   <- ggeffect(mar5, terms = c("year[1976,2014]", "getmar"))
pmar6   <- ggeffect(mar6, terms = c("year[1976,2014]", "getmar"))

pmar1$dol <- "hfw0"
pmar2$dol <- "hfwh"
pmar3$dol <- "hfwf"
pmar4$dol <- "hhwh"
pmar5$dol <- "h0wf"
pmar6$dol <- "hhwf"

library(gtools)
mardata = smartbind(pmar1, pmar2, pmar3, pmar4, pmar5, pmar6)
head(mardata)

mardata$dol <- as.factor(mardata$dol)
levels(mardata$dol)[levels(mardata$dol)=="hfw0"] <- "Husband full-time;\n Wife at home"
levels(mardata$dol)[levels(mardata$dol)=="hfwh"] <- "Husband full-time;\n Wife part-time"
levels(mardata$dol)[levels(mardata$dol)=="hfwf"] <- "Both work full-time"
levels(mardata$dol)[levels(mardata$dol)=="hhwh"] <- "Both work part-time"
levels(mardata$dol)[levels(mardata$dol)=="hhwf"] <- "Husband part-time;\n Wife full-time"
levels(mardata$dol)[levels(mardata$dol)=="h0wf"] <- "Husband at home;\n Wife full-time"

mardata$dol    <- ordered(mardata$dol,   levels = c("Husband full-time;\n Wife at home", "Husband full-time;\n Wife part-time", "Both work full-time", 
                                                    "Both work part-time", "Husband at home;\n Wife full-time", "Husband part-time;\n Wife full-time"))

colnames(mardata)[colnames(mardata)=="response.level"] <- "level"
mardata$level <- as_factor(mardata$level, ordered = TRUE)
levels(mardata$level)[levels(mardata$level)=="NOT.AT.ALL.ACCEPTABLE"] <- "NOT AT ALL\n ACCEPTABLE"
levels(mardata$level)[levels(mardata$level)=="SOMEWHAT.ACCEPTABLE"] <- "SOMEWHAT\n ACCEPTABLE"

mardata$level <- ordered(mardata$level, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT\n ACCEPTABLE",  "NOT AT ALL\n ACCEPTABLE"))

levels(mardata$group)[levels(mardata$group)=="AM ALREADY MARRIED"]      <- "ALREADY MARRIED"
levels(mardata$group)[levels(mardata$group)=="GETTING MARRIED"]         <- "WILL EVENTUALLY MARRY"
levels(mardata$group)[levels(mardata$group)=="I HAVE NO IDEA"]          <- "NOT SURE IF WILL MARRY"
levels(mardata$group)[levels(mardata$group)=="Missing"]                 <- "DATA MISSING"
levels(mardata$group)[levels(mardata$group)=="NOT GETTING MARRIED"]     <- "WILL NOT MARRY"


mardata$group <- factor(mardata$group, levels = c("WILL EVENTUALLY MARRY", "NOT SURE IF WILL MARRY", "WILL NOT MARRY", 
                                                  "ALREADY MARRIED", "DATA MISSING"))

setwd("C:/Users/Joanna/Dropbox/Dernberger/Division of Labor/Figures")
write.csv(mardata, "dol_Figure D.csv")

figE <- mardata %>%
  filter(level != "SOMEWHAT\n ACCEPTABLE" & 
           (dol   == "Husband full-time;\n Wife at home" | dol == "Both work full-time" | dol == "Husband at home;\n Wife full-time")) %>%
  ggplot(aes(x, predicted, color = group, label = round(predicted, 1))) +
  geom_point(aes(size = 10,  color = group)) +
  geom_line(aes(linetype = group)) +
  geom_text(check_overlap = TRUE, color = "white", size = 2) +
  facet_grid(~ level ~ dol) +
  theme_minimal() +
  theme(axis.text.x      = element_text(size = 9),
        strip.text.x     = element_text(size = 9, face = "bold"),
        strip.text.y     = element_text(size = 9, face = "bold"),
        axis.title       = element_text(size = 9), 
        axis.text        = element_text(size = 9), 
        plot.margin      = unit(c(.5,.5,.5,.5),"cm"),
        legend.position  = 'top',
        panel.grid.major = element_blank(),
        panel.border     = element_blank(),
        panel.spacing = unit(2, "lines")) +
  labs(x = " ", y = "\n", fill = "") +
  scale_color_manual(values=c("#fdb863", "#e66101", "#008837",  "#b2abd2", "#5e3c99"), name = "Marriage Plans") +
  guides(size=FALSE, linetype = FALSE) +
  scale_x_continuous(name = "", breaks = c(1976, 2014), label = c("'76", "'14")) +
  scale_y_continuous(breaks = NULL) +
  geom_hline(yintercept = .5, color = "grey90")

figE

ggsave("dol_figure E.png", figE, width = 18, height = 14, units = "cm", dpi = 300)



## Appendix -- Marriage Plans
mn_mar <- multinom(class ~ year * getmar + racesex + momed + momemp + famstru + religion + region, data = dat)

marodds <- coef(mn_mar)
write.csv(marodds, "mn_mar.csv")

zmnmar <- summary(mn_mar)$coefficients/summary(mn_mar)$standard.errors
pmnmar <- (1 - pnorm(abs(zmnedu), 0, 1)) * 2
write.csv(pmnmar, "pmn_mar.csv")

library(ggeffects)
lcamar <- ggpredict(mn_mar, terms = c("year[1976:2014]", "getmar"))

colnames(lcamar)[colnames(lcamar) == 'response.level'] <- 'class'

lcamar$class   <- as.factor(lcamar$class)
lcamar$class <- factor(lcamar$class, levels = c("Conventional", "Neo-traditional", "Conventional Realists", 
                                                "Dual-earners", "Intensive Parents"))

levels(lcamar$group)[levels(lcamar$group)=="AM ALREADY MARRIED"]      <- "ALREADY MARRIED"
levels(lcamar$group)[levels(lcamar$group)=="GETTING MARRIED"]         <- "WILL EVENTUALLY MARRY"
levels(lcamar$group)[levels(lcamar$group)=="I HAVE NO IDEA"]          <- "NOT SURE IF WILL MARRY"
levels(lcamar$group)[levels(lcamar$group)=="Missing"]                 <- "DATA MISSING"
levels(lcamar$group)[levels(lcamar$group)=="NOT GETTING MARRIED"]     <- "WILL NOT MARRY"

lcamar$group <- factor(lcamar$group, levels = c("WILL EVENTUALLY MARRY", "NOT SURE IF WILL MARRY", "WILL NOT MARRY", 
                                                  "ALREADY MARRIED", "DATA MISSING"))


setwd("C:/Users/Joanna/Dropbox/Dernberger/Division of Labor/Figures")
write.csv(lcamar, "dol_Figure F.csv")


## Figure F
library(ggplot2)

figF <- ggplot(lcamar) + 
  geom_line(aes(x = x, y = predicted, colour = group), size=1.5) + 
  facet_wrap(~ class) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values=c("#fdb863", "#e66101", "#404040",  "#b2abd2", "#5e3c99"), name = "Marriage Plans") +
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

ggsave("dol_figure F.png", figF, width = 16.5, height = 14, units = "cm", dpi = 300)
