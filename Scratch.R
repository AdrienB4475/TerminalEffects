Terminal effects code I had no use for yet
# lact_dat1 <- bioseal %>%
#   filter(lactation_duration > 0) %>%
#   select(animalID,
#          bio_age,
#          lactation_duration,
#          year)

# 
# 
# lact_dat2 <- lact_dat %>% 
#   mutate(year_fct = factor(year), 
#          (lac_rescale = (lactation_duration - mean(lactation_duration))) / 10)
#   
# 
# #If it is binomial we can do .. but maybe not??
# lact_mod <- glmer.nb(
#   lac_rescale ~ bio_age + (1 | animalID),
#   data = lact_dat)
# 
# #control = glmerControl(optimizer = "bobyqa")
# 
# 
# #if it is continuos we can do 
# lact_mod <- lmer(
#   lac_rescale ~ bio_age + (1 | year),  # Fixed and random effects
#   data = lact_dat, 
#   control = lmerControl(optimizer = "bobyqa")
# )
# summary(lact_mod)
# 
# #going with the continuous we should be able to graph or calculate predicted values 
# lact_dat$predicted <- predict(lact_mod, type = "response")
# 
# #and then to graph 
# ggplot(lact_dat, aes(x = bio_age, y = lactation_duration)) +
#   geom_point(aes(color = animalID), alpha = 0.6) +  # Observed points
#   geom_smooth(method = "loess", se = FALSE) +
#   theme_minimal()


# bio_seal2 <- bioseal %>%
#   filter(bio_age < 1) %>%
#   group_by(age) %>%
#   mutate(avg_lact_dur = mean(lactation_duration, na.rm = TRUE)) %>%
#   ungroup() 


# 
# #Lactation Duration across ages
# ggplot(bio_seal2, aes(x = age , y = lactation_duration)) +
#          geom_point()+
#          geom_smooth(method = "lm" , se = TRUE)+
#   labs(title = "Lactation Duration Across Ages in Last Years", 
#        x = "age" , 
#        y = "Lactation Duration")
# 
# #Average Lactation Durate across Age
# ggplot(bio_seal2, aes(x = age , y = avg_lact_dur)) +
#          geom_point()+
#          geom_smooth(method = "loess" , se = TRUE)+
#   labs(title = "Lactation Duration Across Ages in Last Years", 
#        x = "age" , 
#        y = "Lactation Duration")




# # This is gamma
# mod1 <- glmer(lactation_duration ~ age + terminal:ageclass + (1 | year),
#               family = Gamma(link = "inverse"),
#               data = lact_dat, 
#               control = glmerControl(optimizer = "bobyqa"))


# mod3 <- glmer(
#   lactation_duration ~ age + terminal:ageclass + (1 | year),
#   family = Gamma(link = "inverse"),
#   data = lact_dat,
#   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))
# )

# neg.bin <- glmer.nb(
#   lactation_duration ~ age + terminal : ageclass + (1 | year),
#   family = Gamma(link = "inverse"),
#   data = lact_dat,
#   control = glmerControl(optimizer = "bobyqa"))

# plot(neg.bin)
# summary(neg.bin)

#Lets try new stuff
#negative binomial
# neg.binomial = glm(lactation_duration ~ age, data=lact_dat, family = quasipoisson())
# par(mfrow=c(2,2))  # Arrange multiple plots in a grid
# plot(neg.binomial)  # This will produce multiple diagnostic plots
# summary(neg.binomial)
# 
# neg.binomial = glm(lactation_duration ~ age10 + terminal : ageclass + (1| animalID) + (1| year),
#                    data=lact_dat, 
#                    family = quasipoisson()
#                      )

# summary(neg.binomial)$coefficients

#compare.fits(lactation_duration ~ age, data = lact_dat, poisson, neg.binomial, jitter = c(0, .1))
# neg.binomial <- glm(lactation_duration ~ age, data=lact_dat, family = quasipoisson())




# #modify bioseal to have lactation duration
# bioseal$firstobsbreed <- as.Date(bioseal$firstobsbreed, format = "%Y-%m-%d")
# bioseal$lastobsbreed <- as.Date(bioseal$lastobsbreed, format = "%Y-%m-%d")
# bioseal$lactation_duration <- as.numeric(difftime(bioseal$lastobsbreed, bioseal$firstobsbreed, units = "days"))
# #Random effect model



#Lets restart.. lets look at our first idea! How do mothers in their last year haul out? Early or later.. 

# ggplot(bioseal, aes(x = bio_age, y= firstobsbreeddoy))+
#          geom_point()+ 
#          geom_smooth(method = "loess") +
#          labs( 
#            title = "Do moms in their last year arrrive early?", 
#            x = "Years before death", 
#            y = "arrival time")
#            
# 
# #Then we wanted to see lactation duration
# ggplot(avg_lactation, aes(x= bio_age, y= avg_lact)) +
#          geom_point() +
#          geom_smooth(method = "loess") +
#          labs(
#            title ="lactation duration in ages",
#            x = "age", 
#            y = "lact_duration")



# #Calculate Average lac for bio age 
# avg_lactation <- bioseal %>%
#   group_by(bio_age) %>%
#   summarize(avg_lact = mean(lactation_duration, na.rm = TRUE))


#OK.. lets get the avg lact dur? then rerun code above
avg_lactation <- cleanseal %>%
  group_by(age) %>%
  summarize(avg_lact = mean(lactation_duration, na.rm = TRUE))
#YASS THAT WORKSSS SOOO NOW LETS PLOT AGAINST THE MALE V F LACT PERIOD FOR AGES IN LAST YEARS OF LIFE 
ggplot(avg_lactation, aes(x= age, y= avg_lact)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title ="lactation duration in ages",
    x = "age", 
    y = "lact_duration")

# ggplot(cleanseal, aes(x = Wt, y = age, color = pupsex)) +
#   geom_point(alpha = 0.5, size = 3) +  # Transparent points for less clutter
#   geom_smooth(method = "lm", se = FALSE) +  # Line of best fit 
#   labs(
#     title = "Relationship between Weight and Age by Pup Sex",
#     x = "Weight",
#     y = "Age"
#   ) +
#   theme_minimal()

# #Making a Bargraph to start Comparing Age to sex, bleh made before
# ggplot(age_sealgraph, aes(x = factor(age), y = percent, fill = pupsex)) +
#   geom_bar(stat = "identity", position = "stack") +  # Stack bars
#   labs(
#     title = "Percentage Distribution by Age and Pup Sex",
#     x = "Age",
#     y = "Percentage"
#   ) +
#   theme_minimal()

#Lets add to this by changing ages 18-21 as just old ages kinda how we did for age paper 

# age_sealgraph <- age_sealgraph %>% 
# filter(!(age %in% c(18, 19, 20, 21)))
# ggplot(age_sealgraph, aes(x = factor(age), y = percent, fill = pupsex)) +
#   geom_bar(stat = "identity", position = "stack") +
#   geom_hline(yintercept = 50, linetype = "dotted", color = "black", size = 1) +
#   labs(
#     title = "Percentage Distribution by Age and Pup Sex",
#     x = "Age",
#     y = "Percentage"
#   ) +
#   theme_minimal()



#Average weight in the years? 
#FIRST find average weights
# avg_weights <- cleanseal %>%
#   group_by(age) %>%
#    filter(!(age %in% c(18, 19, 20, 21))) %>%
#   summarise(avg_weight = mean(Wt, na.rm = TRUE))
# ggplot(avg_weights, aes(x = age, y = avg_weight)) +
#   geom_point() +
#   geom_smooth(method = "loess")




#plot predictions, raw data, and residuals (residuals separately - resid on y axis, predictors on the x axis)

#Plot 1:
#first left join the columns
# lact_dat <- lact_dat %>%
#   left_join(lac_pred_pop %>% select(age, terminal, predicted), by = c("age", "terminal"))

#Now calculate residuals
# lact_dat <- lact_dat %>%
#   mutate(residuals = lactation_duration - predicted)

#Need to add age class to lac_pred_pop
# lac_pred_pop <- lact_dat %>%
#   mutate(ageclass = ifelse(age >= maturity, "old", "young"))

# #predictions: 
# predictions <- predict(lac_mod, newdata = lac_pred_pop, re.form = NA, type = "response", se.fit = TRUE)
#Create confidence intervals:
# lac_pred_pop <- lac_pred_pop %>%
#   mutate(
#     predicted = predictions$fit,
#     se_fit = predictions$se.fit,
#     conf_lo = predicted - 1.96 * se_fit,  # Lower bound of 95% CI
#     conf_hi = predicted + 1.96 * se_fit   # Upper bound of 95% CI
#   )
