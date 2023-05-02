library(tidyverse)
library(ISLR)
library(lme4)
library(sjPlot)
library(likert)
library(lmertest)

view_df(Wage, show.frq = TRUE, show.na = TRUE, show.prc = TRUE)
glimpse(Wage)

Wage %>% plot_frq(education)
p <- Wage %>% group_by(race) %>% plot_frq(education) %>% plot_grid()

save_plot(fig = p,
          filename = "Race_vs_education.jpg")

plot_grpfrq(
    Wage$education,
    Wage$jobclass
) + ggtitle('wages and jobclass comparison')

##THE PLOT WILL ALSO GIVE US THE CHI_SQUARE

plot_xtab(
     x=Wage$education,
     grp = Wage$jobclass,
     margin = "row",
     bar.pos = 'stack',
     coord.flip = TRUE,
     show.summary = TRUE
) 

#CROSS TABLES or PIVOT TABLES #THE TABLE WILL DISPLAY CHI_SQUARE ANALYSIS
# par(mfrow=c(1,2))
tab_xtab(
       var.row = Wage$education,
       var.col = Wage$jobclass,
       show.row.prc = TRUE
)

# Histogram

Wage %>%  group_by(jobclass) %>%  
      plot_frq(
          wage,
          type = "histogram", 
          show.mean = TRUE,
          normal.curve = TRUE
      ) %>% 
 plot_grid()

#PLOT_MODEL FUNCTION

m <- lm(wage~education, data=Wage)
plot_model(m, type = 'pred')
summary(m)

plot_model(m, show.values = TRUE, width=0.1)

tab_model(m,
      show.reflvl = TRUE,
      show.intercept = FALSE,
      p.style = "numeric_stars"
)

#SJPLOT work with almost all the models,

m.nb <- glmer.nb(
             age~wage*jobclass* health +(1|education), data=Wage
)

plot_model(m.nb, type = "int")[[4]]

plot_model(m.nb, type="pred", 
               terms=c("health","jobclass",
                       "wage[50,150,300]"))
