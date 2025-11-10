#############################################################################################################################
### Title: Electoral parity and violence
### Author: Stefan Stojkovic
## Jan 2025
## X - elect. parity/nationalism; Y - violence

# Clean env then call in the data cleaning script
rm(list = ls())
source("eda_inspect_tidy.R")



########################
# first only rad, hrss, lib and serb_fr
# then add competition, show that it works

# no need to control for vote share of radicals

# second, the same
# then add competition



sum(mydata$victim_serb, na.rm = T)
sum(mydata$victim_croat, na.rm = T)

table(mydata$ndh)
mydata$victim_41_43 <- mydata$total_civilian_deaths - mydata$y_1944 - mydata$y_1945

# take only ndh 1941-1943
mydata2 <- mydata %>% 
  filter(mydata$ndh == 1)
# mydata3 <- mydata %>% 
#   filter(mydata$ndh == 1 | mydata$ndh == 2)

sum(mydata$victim_serb)
sum(mydata2$victim_41_43)

summary(mydata2$victim_41_43)
summary(mydata2$victim_serb)
#table(mydata2$victim_41_43)
#mydata4 <- mydata[mydata$total_civilian_deaths > 0, ]




############## ZERO inflated regression models #################
mydata2$rad_natbloc_fr

mydata$comp_index_radNatbl_hrss
# (1) just two competition indexes, no controls
m_interact <- zeroinfl(victim_41_43 ~ hrss_fr+serb_fr+rad_natbloc_fr
                       + dem_agr_fr
                       , 
                       data = mydata2, dist = "negbin")
coeftest(m_interact, vcov = vcovHAC(m_interact))


###########################################
################ tryouts ##################



mydata2 <- mydata2 %>%
  mutate(
    democ_cat = cut(
      democ_fr,
      breaks = c(0, 0.01, 0.05, 1),
      labels = c("Low (0–0.01)", "Med (0.01–0.05)", "High (0.05–1)"),
      include.lowest = TRUE,
      right = FALSE
    ))

mydata2 <- mydata2 %>%
  mutate(
    democ_cat = cut(
      dem_agr_fr,
      breaks = c(0, 0.01, 0.07, 1),
      labels = c("Low", "med", "High"),
      include.lowest = TRUE,
      right = FALSE
    ))

# Check how many cases land in each bin:
table(mydata2$democ_cat)


# function to run pcor.test for one subgroup
pcor_by_group <- function(df) {
  # here we still partial out share_orthodox_serbs
  ctrls <- with(df, cbind(orth_fr, total_pop_scaled, turnout_fr))
  p <- pcor.test(
    x = df$comp_index_radNatbl_hrss,
    y = df$victim_41_43,
    z = ctrls,
    method = "pearson"
  )
  tibble(
    n          = nrow(df),
    r_partial  = p$estimate,
    p_value    = p$p.value
  )
}

mydata2 %>%
  group_by(democ_cat) %>%
  group_modify(~ pcor_by_group(.x)) %>%
  ungroup()


################

# get predicted victim counts across democ_cat:
preds <- ggpredict(m_interact, terms = "democ_cat")

# plot them:
plot(preds) +
  labs(
    x = "Democratic party share category",
    y = "Predicted # of victims",
    title = "Marginal effect of high vs. low dem_party on victims"
  )


###

preds <- ggpredict(m_interact, 
                   terms = c("comp_index_radNatbl_hrss [all]", 
                             "democ_cat"))
plot(preds) +
  labs(
    x = "Comp-Index",
    y = "Predicted # of victims",
    color = "Democratic\nshare bin",
    title = "Competition–Violence slopes by democ category"
  )

##############################
##############################
##############################



alpha <- m_interact$theta
cat("Alpha:", alpha, "\n")

y <- model.response( model.frame(m_interact) )
cat("Total observations: ", length(y), "\n")
sum(y == 0)
sum(y > 0)

ln_alpha <- log(alpha)
cat("LnAlpha:", ln_alpha, "\n")

n_obs <- nrow(model.frame(m_interact))
n_obs

sum(mydata$total_civilian_deaths == 0)
sum(mydata$total_civilian_deaths != 0)
mydata$comp_index_radNatbl_hrss

# (2) two competition indexes ADDED NOW individual party effects of Serbian parties
m_interact <- zeroinfl(victim_41_43 ~  comp_index_radNatbl_hrss + serb_fr + turnout_fr + total_pop_scaled
                       + dem_agr_fr + musl_fr
                       , 
                       data = mydata2, dist = "negbin")
coeftest(m_interact, vcov = vcovHAC(m_interact))
summary(m_interact)

m_int <- zeroinfl(
  victim_41_43 ~ comp_index_radNatbl_hrss * democ_cat
  + orth_fr + turnout_fr + total_pop_scaled,
  data = mydata2,
  dist = "negbin")
coeftest(m_int, vcov = vcovHAC(m_int))

preds <- ggpredict(
  m_int,
  terms = c("comp_index_radNatbl_hrss [all]", "democ_cat")
)

plot(preds) +
  labs(
    x = "Comp-Index",
    y = "Predicted # of victims",
    color = "Democ share bin",
    title = "How competition→violence slopes differ by democ_cat"
  )



  

summary(mydata2$comp_index_dem_hrss)
summary(mydata2$comp_index_radNatbl_hrss)

alpha <- m_interact$theta
cat("Alpha:", alpha, "\n")

ln_alpha <- log(alpha)
cat("LnAlpha:", ln_alpha, "\n")

sum(mydata$total_civilian_deaths == 0)
sum(mydata$total_civilian_deaths != 0)


# Your estimates (α ≈ 0.20 and Lnα ≈ –1.61) suggest that:
# There is moderate overdispersion in your data.
# The negative binomial model is appropriate because it can handle the extra variance present.

######### NOTE: in models with Serb_fr control, very negative correlation between turnout and victims
# and turnout was low where government repression was high!!!!!

# CONSIDER controlling for DALMACIJA, consider checking other parties at the time, peasant party above all and merge it with democrats!

# alternative model the one just below, all good with 3 above

m_interact <- glm.nb(victim_41_43 ~ comp_index_dem_hrss + comp_index_radNatbl_hrss + orth_fr
                         + county
                     , 
                     data = mydata2)
summary(m_interact)
m_interact <- glm.nb(victim_41_43 ~ comp_index_dem_hrss + comp_index_radNatbl_hrss + orth_fr + musl_fr + turnout_fr + total_pop_scaled
                       + democ_fr + rad_natbloc_fr
                         + county
                       , 
                       data = mydata2)
summary(m_interact)
nobs(m_interact)

# zeroinfl(victim_41_43 
#          ~ comp_index_radNatbl_hrss + comp_index_dem_hrss           # count part
#          | comp_index_radNatbl_hrss + comp_index_dem_hrss           # zero‐inflation part
#          , data = mydata2, dist = "negbin")

# # also check with
# m_interactglm <- glmmTMB(victim_croat ~ comp_index_dem_hrss + comp_index_rad_hrss + (1 | state), 
#                          data = mydata, family = nbinom2)
# summary(m_interactglm)

# m_interact2 <- glmmTMB(total_civilian_deaths ~ comp_index_dem_hrss + comp_index_rad_hrss +
#                          + orth_fr + turnout_fr + total_pop_scaled,
#                     #   + (1 | state), 
#                        data = mydata, family = nbinom2)
# summary(m_interact2)

# ROBUSTNESS 
mydata2$total_civilian_deaths
m_interact3 <- glmmTMB(total_civilian_deaths ~ comp_index_dem_hrss + comp_index_radNatbl_hrss 
                       + orth_fr + musl_fr + turnout_fr + total_pop_scaled
                       + democ_fr + rad_natbloc_fr
                       + factor(county), 
                       data = mydata2, family = nbinom2)
summary(m_interact3)
mydata$comp_index_dem_hrss
AIC(m_interact, m_interact3)

m_state_re2 <- glmmTMB(
  victim_41_43 ~ 
    comp_index_dem_hrss + comp_index_radNatbl_hrss + orth_fr + musl_fr + 
    turnout_fr + total_pop_scaled + democ_fr + rad_natbloc_fr +
    (1 | state),
  family = nbinom2(),
  data = mydata2
)
summary(m_state_re2)
nobs(m_state_re2)




######## Alternative models for plotting

check <- glm.nb(total_civilian_deaths ~ rad_natbloc_fr*hrss_fr + orth_fr + musl_fr*hrss_fr + turnout_fr + total_pop_scaled, 
                         data = mydata)
summary(check)
check2 <- glm.nb(total_civilian_deaths ~ democ_fr*hrss_fr + orth_fr + musl_fr*hrss_fr + turnout_fr 
                 + total_pop_scaled, 
                data = mydata)
summary(check2)

vif(check2)
cor.test(mydata$democ_fr, mydata$musl_fr)
cor.test(mydata$hrss_fr, mydata$musl_fr)

#anova(check, check2, test = "LRT")

############## ############## ############## ############## 
############## PLOT ############## 

# Calculate 25th, 50th, and 75th percentiles for hrss_fr
hrss_low  <- quantile(mydata$hrss_fr, 0.25, na.rm = TRUE)
hrss_med  <- quantile(mydata$hrss_fr, 0.50, na.rm = TRUE)
hrss_high <- quantile(mydata$hrss_fr, 0.75, na.rm = TRUE)

############################################
# 1. Liberal Model: democ_fr * hrss_fr (check2)
############################################

newdata_dem <- expand.grid(
  democ_fr = seq(min(mydata$democ_fr, na.rm = TRUE),
                 max(mydata$democ_fr, na.rm = TRUE),
                 length.out = 100),
  hrss_fr = c(hrss_low, hrss_med, hrss_high),
  orth_fr = mean(mydata$orth_fr, na.rm = TRUE),
  jmo_fr = mean(mydata$jmo_fr, na.rm = TRUE),
  turnout_fr = mean(mydata$turnout_fr, na.rm = TRUE),
  total_pop_scaled = mean(mydata$total_pop_scaled, na.rm = TRUE)
)

newdata_dem$hrss_label <- factor(newdata_dem$hrss_fr,
                                 levels = c(hrss_low, hrss_med, hrss_high),
                                 labels = c("Low HRSS (25th %ile)",
                                            "Median HRSS (50th %ile)",
                                            "High HRSS (75th %ile)"))

pred_dem <- predict(check2, newdata = newdata_dem, type = "link", se.fit = TRUE)
newdata_dem$fit   <- exp(pred_dem$fit)
newdata_dem$upper <- exp(pred_dem$fit + 1.96 * pred_dem$se.fit)
newdata_dem$lower <- exp(pred_dem$fit - 1.96 * pred_dem$se.fit)

############################################
# 2. Nationalist Model: rad_natbloc_fr + hrss_fr (check)
############################################

newdata_rad <- expand.grid(
  rad_natbloc_fr = seq(min(mydata$rad_natbloc_fr, na.rm = TRUE),
                       max(mydata$rad_natbloc_fr, na.rm = TRUE),
                       length.out = 100),
  hrss_fr = c(hrss_low, hrss_med, hrss_high),
  orth_fr = mean(mydata$orth_fr, na.rm = TRUE),
  jmo_fr = mean(mydata$jmo_fr, na.rm = TRUE),
  turnout_fr = mean(mydata$turnout_fr, na.rm = TRUE),
  total_pop_scaled = mean(mydata$total_pop_scaled, na.rm = TRUE)
)

newdata_rad$hrss_label <- factor(newdata_rad$hrss_fr,
                                 levels = c(hrss_low, hrss_med, hrss_high),
                                 labels = c("Low HRSS (25th %ile)",
                                            "Median HRSS (50th %ile)",
                                            "High HRSS (75th %ile)"))

pred_rad <- predict(check, newdata = newdata_rad, type = "link", se.fit = TRUE)
newdata_rad$fit   <- exp(pred_rad$fit)
newdata_rad$upper <- exp(pred_rad$fit + 1.96 * pred_rad$se.fit)
newdata_rad$lower <- exp(pred_rad$fit - 1.96 * pred_rad$se.fit)

############################################
# 3. Plotting with standardized y-axis and updated labels
############################################

text_size <- 16
y_limits <- c(0, 300)  # Standardized y-axis range for both plots

# Liberal Party Vote plot (democ_fr * hrss_fr)
p_dem <- ggplot(newdata_dem, aes(x = democ_fr, y = fit, color = hrss_label)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = hrss_label), alpha = 0.2, color = NA) +
  scale_y_continuous(limits = y_limits) +
  labs(x = "Liberal Party Vote",  # Removed variable name in parentheses
       y = "Predicted Civilian Deaths",
       color = "HRSS",
       fill = "HRSS") +
  theme_minimal(base_size = text_size) +
  theme(legend.position = "none")

# Serbian Nationalist Party Vote plot (rad_natbloc_fr + hrss_fr)
p_rad <- ggplot(newdata_rad, aes(x = rad_natbloc_fr, y = fit, color = hrss_label)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = hrss_label), alpha = 0.2, color = NA) +
  scale_y_continuous(limits = y_limits) +
  labs(x = "Serbian Nationalist Party Vote",  # Removed variable name in parentheses
       y = "Predicted Civilian Deaths",
       color = "HRSS",
       fill = "HRSS") +
  theme_minimal(base_size = text_size) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y  = element_blank(),  # Remove y-axis tick labels
        axis.ticks.y = element_blank())

# Arrange the two plots side by side
grid.arrange(p_dem, p_rad, ncol = 2)


# Compute correlations
cor_low <- cor(mydata$democ_fr[mydata$hrss_fr >= 0 & mydata$hrss_fr < 0.25], 
               mydata$total_civilian_deaths[mydata$hrss_fr >= 0 & mydata$hrss_fr < 0.25], 
               use = "complete.obs")

cor_mid <- cor(mydata$democ_fr[mydata$hrss_fr >= 0.25 & mydata$hrss_fr <= 0.75], 
               mydata$total_civilian_deaths[mydata$hrss_fr >= 0.25 & mydata$hrss_fr <= 0.75], 
               use = "complete.obs")

cor_high <- cor(mydata$democ_fr[mydata$hrss_fr > 0.75], 
                mydata$total_civilian_deaths[mydata$hrss_fr > 0.75], 
                use = "complete.obs")

# Print results
cor_low
cor_mid
cor_high


#### In this custom approach:
# expand.grid() creates a grid of predictor values. We fix the other covariates (like serb_fr, turnout_fr, and total_pop_scaled) at their mean values.
# predict() with type = "response" gives predictions on the original count scale.
# The final ggplot() call plots how predicted total civilian deaths vary with democ_fr for low and high levels of parity_index.




library(cowplot)

# Adjust text sizes globally (or for each plot) via base_size.
base_theme <- theme_minimal(base_size = 14)

# Create the nationalist (radical) plot with legend
p11_full <- ggplot(newdata, aes(x = rad_natbloc_fr, y = predicted, color = parity_label)) +
  geom_line(size = 1) +
  labs(x = "Serbian Nationalist Party Vote",
       y = "Predicted Civilian Deaths") +  # Removed color label here
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove y-axis label
        axis.text.y = element_blank(),   # Remove y-axis values
        axis.ticks.y = element_blank(),
        legend.position = "none")  # Remove legend

# Create the liberal plot without legend
p22_full <- ggplot(newdata2, aes(x = democ_fr, y = predicted, color = parity_label)) +
  geom_line(size = 1) +
  labs(x = "Serbian Liberal Party Vote",
       y = "Predicted Civilian Deaths") +
  base_theme +
  theme(legend.position = "none")

# Extract the legend from p11_full (temporarily setting legend.position)
legend <- get_legend(
  p11_full +
    theme(legend.position = "right")
)

# Now remove the legend from p11 for the main panel
p11_no_legend <- p11_full + theme(legend.position = "none")

# Arrange the two plots side by side with equal panel sizes
combined_plots <- plot_grid(p22_full, p11_no_legend, ncol = 2, align = 'v')

# Finally, add the common legend on the right
final_plot <- plot_grid(combined_plots, legend, rel_widths = c(1, 0.2))

final_plot





############### the rest below ##############








##################### plot



p1 <- ggplot(subset(mydata4, parity_cat == "Low" & rad_natbloc_fr > 0), 
             aes(x = rad_natbloc_fr, y = log_victims)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Low Parity: Interaction between Serbian Victims and Competition Index", 
       x = "Competition Index (Democratic)", 
       y = "Serbian Victims") +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, 8))

# p2 <- ggplot(subset(mydata, parity_category == "Medium"), 
#        aes(x = democ_fr, y = log_victims)) + 
#   geom_point(alpha = 0.5) + 
#   geom_smooth(method = "lm", se = FALSE, color = "blue") + 
#   labs(title = "Low Parity: Interaction between Serbian Victims and Competition Index", 
#        x = "Competition Index (Democratic)", y = "Serbian Victims") +
#   theme_minimal()

p3 <- ggplot(subset(mydata4, parity_cat == "High" & rad_natbloc_fr > 0), 
             aes(x = rad_natbloc_fr, y = log_victims)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Low Parity: Interaction between Serbian Victims and Competition Index", 
       x = "Competition Index (Democratic)", 
       y = "Serbian Victims") +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, 8))

#library(gridExtra)
grid.arrange(p1,p3, ncol = 2)


ggplot(subset(mydata), 
       aes(x = rad_natbloc_fr, y = log_victims)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Low Parity: Interaction between Serbian Victims and Competition Index", 
       x = "Competition Index (Democratic)", y = "Serbian Victims") +
  theme_minimal()










################# good #############
### ZINB models the main ones
## Result section part 1 (show both count and zero models):
# Finding 1: nationalist_nationalist competition increases serb victims
# Finding 2: liberal_nationalist competition does not
## Result section part 2: 
# Descriptive: Effects of parties on different levels of population parity

### Robustness:
# 1. Effects the same when only year 1 of war taken (to minimize war related factors)
# 2. Effects stay when NB model employed
# 3. Effects stay when full sample is employed
# Effects stay when control for state/region-level random effects







# ovaj prvi ispod ide u plot
m2hr <- glm.nb(croatian_victims ~ cro_fr + rad_natbloc_fr*parity_index +
                 ethn_frac + voted_1000+district,
               data = mydata)
coeftest(m2hr, vcov = vcovHC(m2hr, type = "HC3"))

m3hr <- glm.nb(croatian_victims ~ cro_fr + rad_natbloc_fr*parity_index + democ_fr
                 + ethn_frac + voted_1000 + district,
               data = mydata)
coeftest(m3hr, vcov = vcovHC(m3hr, type = "HC3"))

# hrvatska partija bez democ_fr kao kontrole ide u plot
m4hr <- glm.nb(croatian_victims ~ cro_fr+hrss_fr*parity_index + rad_natbloc_fr + democ_fr
               + ethn_frac + voted_1000+district,
               data = mydata)
coeftest(m4hr, vcov = vcovHC(m4hr, type = "HC3"))

# Calculate robust standard errors with coeftest
robust_m1hr <- coeftest(m1hr, vcov = vcovHC(m1hr, type = "HC3"))
robust_m2hr <- coeftest(m2hr, vcov = vcovHC(m2hr, type = "HC3"))
robust_m3hr <- coeftest(m3hr, vcov = vcovHC(m3hr, type = "HC3"))
robust_m4hr <- coeftest(m4hr, vcov = vcovHC(m4hr, type = "HC3"))

pseudo_r2hr <- c(
  pR2(m1hr)["r2ML"],
  pR2(m2hr)["r2ML"],
  pR2(m3hr)["r2ML"],
  pR2(m4hr)["r2ML"]
)

ll_hr <- c(
  pR2(m1hr)["llh"],
  pR2(m2hr)["llh"],
  pR2(m3hr)["llh"],
  pR2(m4hr)["llh"]
)

# Get number of observations for each model
n_obs <- c(nobs(m1hr), nobs(m2hr), nobs(m3hr), nobs(m4hr))

# table
outtt <- stargazer(robust_m1hr, robust_m2hr, robust_m3hr, robust_m4hr,
                   type="text",
                   title="Negative Binomial Models of Atrocities against Croatian Civilians",
                   style="ajps",
                   dep.var.labels.include = FALSE,
                   column.labels = c("(1)", "(2)", "(3)", "(4)"),
                   model.numbers = F,
                   keep.stat = c("n"),
                   digits = 2,
                   # Explicitly align labels for each model
                   covariate.labels = c("Share Croats",
                                        "Croat Nationalist Party",
                                        "Serb Liberal Party",
                                        "Parity",
                                        "Serb Nationalist Party",
                                        "Ethnic Fractionalization",
                                        "Total Population",
                                        "Liberal*Parity",
                                        "Serb Nationalist*Parity",
                                        "Croat Nationalist*Parity"
                                        
                   ),
                   omit = c("state"),
                   font.size = "small",
                   label = "",
                   star.char = c("+", "*", "**", "***"),
                   star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                   add.lines = list(
                     c("Region-fixed effects", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("N", n_obs),
                     c("Nagelkerke Pseudo R2", round(pseudo_r2hr, 3)),
                     c("Log-likelihood", round(ll_hr, 3))
                   )
)

### liberali smanjujuu zrtive, lib politike deluju dobro, srpski nacion
# povecavaju ejr ih ubijaju, hrv nac smanjuju jer ih stite??

m1 <- glm.nb(serb_victims ~ serb_fr + democ_fr*parity_index +rad_natbloc_fr
               + ethn_frac + voted_1000 + state,
               data = mydata)
coeftest(m1, vcov = vcovHC(m1, type = "HC3"))

m2 <- glm.nb(serb_victims ~ serb_fr + rad_natbloc_fr*parity_index +
               ethn_frac + voted_1000 + state,
               data = mydata)
coeftest(m2, vcov = vcovHC(m2, type = "HC3"))

m3 <- glm.nb(serb_victims ~ serb_fr + rad_natbloc_fr*parity_index + democ_fr
               + ethn_frac + voted_1000 + state,
               data = mydata)
coeftest(m3, vcov = vcovHC(m3, type = "HC3"))

m4 <- glm.nb(serb_victims ~ hrss_fr*parity_index + democ_fr+rad_natbloc_fr
               + ethn_frac + voted_1000 + state,
               data = mydata)
coeftest(m4, vcov = vcovHC(m4, type = "HC3"))


# Calculate robust standard errors with coeftest
robust_m1 <- coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
robust_m2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC3"))
robust_m3 <- coeftest(m3, vcov = vcovHC(m3, type = "HC3"))
robust_m4 <- coeftest(m4, vcov = vcovHC(m4, type = "HC3"))

pseudo_r2 <- c(
  pR2(m1)["r2ML"],
  pR2(m2)["r2ML"],
  pR2(m3)["r2ML"],
  pR2(m4)["r2ML"]
)

ll_hr <- c(
  pR2(m1)["llh"],
  pR2(m2)["llh"],
  pR2(m3)["llh"],
  pR2(m4)["llh"]
)

# Get number of observations for each model
n_obs <- c(nobs(m1), nobs(m2), nobs(m3), nobs(m4))

outtt <- stargazer(robust_m1, robust_m2, robust_m3, robust_m4,
                   type="text",
                   title="Negative Binomial Models of Atrocities against Serbian Civilians",
                   style="ajps",
                   dep.var.labels.include = FALSE,
                   column.labels = c("(1)", "(2)", "(3)", "(4)"),
                   model.numbers = F,
                   keep.stat = c("n"),
                   digits = 2,
                   # Explicitly align labels for each model
                   covariate.labels = c("Share Serbs",
                                        "Croat Nationalist Party",
                                        "Serb Liberal Party",
                                        "Serb Nationalist Party",
                                        "Parity",
                                        "Ethnic Fractionalization",
                                        "Total Population",
                                        "Liberal*Parity",
                                        "Serb Nationalist*Parity",
                                        "Croat Nationalist*Parity"
                                        
                   ),
                   omit = c("state"),
                   font.size = "small",
                   label = "",
                   star.char = c("+", "*", "**", "***"),
                   star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                   add.lines = list(
                     c("Region-fixed effects", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("N", n_obs),
                     c("Nagelkerke Pseudo R2", round(pseudo_r2hr, 3)),
                     c("Log-likelihood", round(ll_hr, 3))
                   )
)

# show marginal effects plot, either in robustness or below results

#cormat <- mydata %>% dplyr::select(hrss_fr, rad_natbloc_fr, democ_fr, parity_index,
                          #  ethn_frac, voted_1000)

#correlation_matrix <- cor(cormat)
#print(correlation_matrix, include.rownames = F, include.colnames = FALSE)


summary_table <- mydata %>%
  dplyr::select(hrss_fr, rad_natbloc_fr, democ_fr, serb_fr, cro_fr, muslim_fr, parity_index, ethn_frac) %>%
  summarise_all(list(
    N = ~sum(!is.na(.)),
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Pctl_25 = ~quantile(., probs = 0.25, na.rm = TRUE),
    Pctl_75 = ~quantile(., probs = 0.75, na.rm = TRUE),
    Max = ~max(., na.rm = TRUE)
  ))
print(summary_table)
summary(mydata$ethn_frac)
# Define variable names for the rows
var_names <- c("hrss_fr", "rad_natbloc_fr", "democ_fr", "serb_fr", "cro_fr", "muslim_fr", "parity_index", "ethn_frac")

# Create the formatted string for each variable row
formatted_table <- sapply(var_names, function(var) {
  # Create column names based on the variable name
  N_col <- paste0(var, "_N")
  Mean_col <- paste0(var, "_Mean")
  SD_col <- paste0(var, "_SD")
  Min_col <- paste0(var, "_Min")
  Pctl_25_col <- paste0(var, "_Pctl_25")
  Pctl_75_col <- paste0(var, "_Pctl_75")
  Max_col <- paste0(var, "_Max")


  # Retrieve the values for each statistic
  paste(var, "&", 
        summary_table[[N_col]], "&", 
        format(summary_table[[Mean_col]], digits = 3), "&", 
        format(summary_table[[SD_col]], digits = 3), "&", 
        summary_table[[Min_col]], "&", 
        summary_table[[Pctl_25_col]], "&", 
        summary_table[[Pctl_75_col]], "&", 
        summary_table[[Max_col]], "\\\\")
})

# Print each row of the table for copying to Overleaf
cat(formatted_table, sep = "\n")







###### 1. You should include county fixed effects if:
#   
# You suspect that unobserved county-level factors (e.g., county-specific institutions, historical grievances, local militias, policing structures) systematically influence violence but are not captured by your other controls.
# You want to isolate within-county variation in electoral parity and violence, meaning you're comparing municipalities within the same county rather than across counties.
# You believe that county-level differences do not directly explain the effect of electoral parity but may confound it.

# 👉 Interpretation with County FEs:
#   The coefficient on parity_index would reflect how differences in electoral parity within the same county affect political violence. The assumption is that county-wide factors that could influence violence are absorbed by the county fixed effects.
 
###### 2. When NOT to Control for County Fixed Effects
# You should not include county fixed effects if:
#   
#   You believe electoral parity's effect operates across counties, and controlling for counties would absorb meaningful variation.
# Counties themselves are meaningful units of analysis, and their characteristics interact with electoral parity in shaping violence.
# You are concerned about over-controlling, especially if there's limited within-county variation in electoral parity.
# Your number of counties is highly disproportionate to municipalities, leading to a risk of collinearity or losing statistical power.
# 👉 Interpretation without County FEs:
#   The coefficient on parity_index would reflect the general effect of electoral parity across municipalities, including cross-county variation. The assumption is that county-wide effects are either not strong enough to bias the results or are already controlled for through other variable
# 

##### 3. What About State-Level Controls?
#   Since there are only 4 states, including state-level controls makes sense, as they capture broader macro-political contexts. But state FEs (dummy variables for states) would be problematic because they absorb too much variation, given only 4 units.

#####  4. Recommendation
# If you want to generalize across municipalities and counties, don’t use county fixed effects.
# If you suspect county-level factors bias the estimates, use county fixed effects.
# If results change significantly between the models, that’s a hint that county-level factors matter, so a mixed-effects (hierarchical) model might be better.
# 👉 Try both models and compare the coefficients of parity_index:
#   
#   If the effect remains stable, county-level factors are likely not confounding.
# If the effect weakens or disappears, county differences were driving the association, and you should keep county FEs.

ggplot(mydata2, aes(x = total_civilian_deaths)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "",
    x = "Number of Civilian Deaths",
    y = "Number of Observations"
  ) +
  coord_cartesian(xlim = c(0, 55), 
                  ylim = c(0, 500)) +
  theme_minimal()

table(total_civilian_deaths)






#####################################################################################################################################################################
#######################################################
#######################################################

# 1. Create a 3‐level factor
mydata2$comp_cat_radNatbl_hrss <- with(mydata2,
                                       factor(
                                         ifelse(comp_index_radNatbl_hrss == 0, "None",
                                                ifelse(comp_index_radNatbl_hrss <= 0.756, "Low", "High")
                                         ),
                                         levels = c("None","Low","High")
                                       )
)

# Check distribution
table(mydata2$comp_cat_radNatbl_hrss)
prop.table(table(mydata2$comp_cat_radNatbl_hrss))

# 2. Re‐run your ZINB specifying the new categorical variable
m_interact_cat <- zeroinfl(
  victim_41_43 ~
    comp_index_dem_hrss +
    comp_cat_radNatbl_hrss +    # <-- categorical competition
    orth_fr + musl_fr + turnout_fr + total_pop_scaled +
    democ_fr + rad_natbloc_fr,
  data = mydata2,
  dist = "negbin"
)

# 3. Summarize
summary(m_interact_cat)
coeftest(m_interact_cat, vcov = vcovHAC(m_interact_cat))


######################################################

# 1) compute the 50th percentile among the non-zeros
M <- with(mydata2, 
          quantile(comp_index_dem_hrss[comp_index_dem_hrss > 0], .5)
)

# 2) build a 3-level factor
mydata2$comp_cat_dem_hrss <- with(mydata2,
                                  factor(
                                    ifelse(comp_index_dem_hrss == 0, "None",
                                           ifelse(comp_index_dem_hrss <= M, "Low", "High")
                                    ),
                                    levels = c("None","Low","High")
                                  )
)

# 3) inspect
table(mydata2$comp_cat_dem_hrss)
prop.table(table(mydata2$comp_cat_dem_hrss))

# 4) rerun your ZINB
m_interact_cat_dem <- zeroinfl(
  victim_41_43 ~
    comp_cat_dem_hrss +       # <-- categorical competition w/ liberals
    comp_index_radNatbl_hrss +  # (keep your previous nat-bloc factor too)
    orth_fr + musl_fr + turnout_fr + total_pop_scaled +
    democ_fr + rad_natbloc_fr,
  data = mydata2,
  dist = "negbin"
)

# 5) summary
summary(m_interact_cat_dem)
coeftest(m_interact_cat_dem, vcov = vcovHAC(m_interact_cat_dem))


##########################

install.packages("sphet")        # for conleySE
library(sphet)
# mydata2 needs columns munilat, munilong
conley_se <- conleySE(m_interact,
                      lat = mydata2$munilat,
                      lon = mydata2$munilong,
                      time = NULL,          # no time dimension
                      maxdist = 50*1000)    # 50 km cutoff
coeftest(m_interact, vcov = conley_se)


## To dos:
#long and lat kako je GPT rekao i radi spatially correlated SE