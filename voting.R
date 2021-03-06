require(foreign)
require(memisc)
require(dplyr)

uris <- as.data.frame(as.data.set(spss.system.file("~/Dropbox/Votes&Wind Data/FieldStudyUris/FieldStudyUris.sav")))
whitehall <- as.data.frame(as.data.set(spss.system.file("~/Dropbox/Votes&Wind Data/FieldStudyWhitehall/FieldStudyWhitehall.sav")))
rf_manipulate <- as.data.frame(as.data.set(spss.system.file("~/Dropbox/Votes&Wind Data/ManipulateRF/RegFocusManipulation_Votes.sav")))
manipulate_control <- as.data.frame(as.data.set(spss.system.file("~/Dropbox/Votes&Wind Data/ManipulationCheck/ManipulationRFonVoteswithControlsNov2014.sav")))
scottland <- as.data.frame(as.data.set(spss.system.file("~/Dropbox/Votes&Wind Data/Scotland_Data/Weather and Votes SPSS.sav")))
us_vote <- as.data.frame(as.data.set(spss.system.file("~/Dropbox/Votes&Wind Data/US_Data/US2008weatherandvotes.sav")))
voting_check <- as.data.frame(as.data.set(spss.system.file("~/Dropbox/Votes&Wind Data/VotingMaterialsCheck/VotingMaterialsCheck.sav")))

## PG 5
# equal variance test, which cant reject null hypothesis that the variance are the same
var.test(voting_check$si_assemblyman_safe_rf,
         voting_check$si_statesenator_promo_rf)

# since variance is equal then we pool the variance
t.test(voting_check$si_statesenator_promo_rf,
       voting_check$si_assemblyman_safe_rf, 
       var.equal = T)

# t-test without pooling variance
t.test(voting_check$si_assemblyman_safe_rf, 
       voting_check$si_statesenator_promo_rf)

# since cant reject the null hypothesis, so two sample are the same.


## PG 6
indoors <- whitehall %>% filter(filter_. == "Not Selected")
outdoors <- whitehall %>% filter(filter_. == "Selected")

# indoor 
table(indoors$vote, indoors$windy)

# contingency table 
tbl <- table(outdoors$vote, outdoors$windy)
print(tbl)

# percentage table
pct_tbl <- prop.table(tbl,2)
print(pct_tbl)

# 71% more like to vote no in windy day 
pct_tbl[1,1] / pct_tbl[1,2] - 1

# Chi-sqt test result shows that voting result is not indenpendent of wind condition
chisq.test(outdoors$vote, outdoors$windy, correct = F)

# just check to see if indoor case are the same, apparently with the indoor case 
# the voting result is indenpendent of the wind speed
chisq.test(indoors$vote, indoors$windy, correct = F)


## PG 7
# correlation 
cor(x = uris$rfscore, y = uris$windspeed)
# regression model
model_lm <-  lm(rfscore~windspeed, data = uris)
summary(model_lm)
# since rf score is not continuous using jitter plot with transparancy 
# show correlation
require(ggplot2)
ggplot(data = uris, aes(x=windspeed, y=rfscore)) + 
    geom_jitter(alpha= 0.5, 
                position = position_jitter(width = 0.3,
                                           height = 0.1))

## PG 8
tbl <- table(rf_manipulate$stdecision, rf_manipulate$regulatoryfocus)
prop.table(tbl,2)
chisq.test(tbl, correct = F)


## PG 9 scotland 
# Yes campaign
mean(voting_check$scotland_yes, na.rm = T)
sd(voting_check$scotland_yes, na.rm = T)
# No campaign
mean(voting_check$scotland_no, na.rm = T)
sd(voting_check$scotland_no, na.rm = T)

# equal variance test, which cant reject null hypothesis that the variance are the same
var.test(voting_check$scotland_yes,
         voting_check$scotland_no)

# since variance is equal then we pool the variance
t.test(voting_check$scotland_yes,
       voting_check$scotland_no, 
       var.equal = T)


# stepwise regression
require(MASS)
require(leaps)
scottland <- scottland %>% filter(council_name != "")
min_fit <- lm(percentage_no_votes ~ 1, data=scottland)
max_fit <- lm(percentage_no_votes ~ overall_turnout + cloudcoverelectionday_is0 + 
                  socialgrade_ab + religion_churchofscotland_percent + 
                  employment_parttime + nationalidentity_scotlandonly + 
                  householdcomposition_onepersonhousehold_65andunder + 
                  highestqualification_none + wind_speed_electionday,
              data=scottland)

step_result_scot <- step(min_fit, direction="both", 
                    scope=list(lower = min_fit, 
                               upper = max_fit))
step_result_scot$anova
summary(step_result_scot)

# correlation of windspeed of 2014/09/17 vs pct no vote
cor(x=scottland$wind_sep172014, scottland$percentage_no_votes, 
    use="complete")
cor.test(x=scottland$percentage_no_votes, scottland$wind_sep172014)

summary(lm(percentage_no_votes~wind_sep172014, data = scottland))


# correlation of wind speed vs likely hood of show up in voting booth
cor(x=scottland$wind_speed_electionday, scottland$overall_turnout, 
    use="complete")
cor.test(x=scottland$wind_speed_electionday, scottland$overall_turnout)
summary(lm(overall_turnout~wind_speed_electionday, data = scottland))

ggplot(data = scottland, aes(x=wind_speed_electionday, 
                             y = percentage_no_votes)) + 
        geom_point(alpha=0.5, size=5) + 
        stat_smooth(method = "lm")


### Last experiment windspeeed vs residual 
scotland_no_wind_mdl <- lm(percentage_no_votes ~ overall_turnout + cloudcoverelectionday_is0 + 
                               socialgrade_ab + religion_churchofscotland_percent + 
                               employment_parttime + nationalidentity_scotlandonly + 
                               householdcomposition_onepersonhousehold_65andunder + 
                               highestqualification_none,
                        data=scottland)
wind_speed_electionday
# seems other variables contains windspeed info, test is not significant
# not sure if this is what you wanted 
scotland_res_wind_df <- data.frame( res= scotland_no_wind_mdl$residuals, windspeed = scottland$wind_speed_electionday)
ggplot(scotland_res_wind_df, aes(x=windspeed, y = res)) + 
    geom_point() +
    geom_smooth(method="lm") 
summary(lm(res~windspeed, data = scotland_res_wind_df))



# pg 9 US vote
# Yes campaign
mean(voting_check$obama_rf, na.rm = T)
sd(voting_check$obama_rf, na.rm = T)
# No campaign
mean(voting_check$mccain_safe_rf, na.rm = T)
sd(voting_check$mccain_safe_rf, na.rm = T)

# equal variance test, which cant reject null hypothesis that the variance are the same
var.test(voting_check$obama_rf,
         voting_check$mccain_safe_rf)

# since variance is equal then we pool the variance
t.test(voting_check$scotland_yes,
       voting_check$scotland_no, 
       var.equal = T)

# regression
us_vote <- us_vote %>% filter(state != "")
row.names(us_vote) <- us_vote$state
min_fit_us <- lm(mccain_votes ~ 1, data=us_vote)
max_fit_us <- lm(mccain_votes ~ turnout_estimate + windspeednov2004 + 
                  windspeednov2005 + windspeednov2006 + windspeednov2007 + 
                  windspeednov2008 + americanindian + asian + black + 
                  hispaniclatino + nativehawaiian + mixedrace +
                  nonhispanicwhite + medianincome + education + uninsured +
                  windspeed_electionday,
              data=us_vote)

step_result_us <- step(min_fit_us, direction="both", 
                    scope=list(lower = min_fit_us, 
                               upper = max_fit_us))
step_result_us$anova
summary(step_result_us)

# showup rate correlation with windspeed
cor(us_vote$turnout_estimate, us_vote$windspeed_electionday)
cor.test(us_vote$turnout_estimate, us_vote$windspeed_electionday)
summary(lm(turnout_estimate~windspeed_electionday, us_vote))

# visualize mccain 
ggplot(data = us_vote, aes(x=windspeed_electionday, 
                             y = mccain_votes)) + 
    geom_point(alpha=0.5, size=5) + 
    stat_smooth(method = "lm") + 
    geom_text(aes(x = windspeed_electionday, 
                  y = mccain_votes + 0.01,
                  label = state))


ggplot(data = us_vote, aes(x=windspeed_electionday, 
                           y = obama_votes)) + 
    geom_point(alpha=0.5, size=5) + 
    stat_smooth(method = "lm")  



robust_fit <- rlm(mccain_votes~ uninsured + hispaniclatino + 
                      windspeed_electionday + turnout_estimate + 
                      asian + americanindian + nativehawaiian, 
                    data = us_vote)
summary(robust_fit)


# with HI removed
lev = hat(model.matrix(step_result_us))
us_lev <- data.frame(state=us_vote$state, leverage = lev)
ggplot(us_lev, aes(x=row.names(lev_df), y=leverage)) + 
    geom_text(aes(label = state))

cook <- cooks.distance(step_result_us)
us_cook <- data.frame(state=us_vote$state, cook_dist = cook)

ggplot(us_cook, aes(x=row.names(us_cook), y=cook_dist)) + 
    geom_text(aes(label = state))

# seems that removing HI & AK gives wind more importance hmmm...
us_hi <- us_vote %>% filter(!state %in% c("HI", "AK"))
min_fit_us_hi <- lm(mccain_votes ~ 1, data=us_hi)
max_fit_us_hi <- lm(mccain_votes ~ turnout_estimate + windspeednov2004 + 
                     windspeednov2005 + windspeednov2006 + windspeednov2007 + 
                     windspeednov2008 + americanindian + asian + black + 
                     hispaniclatino + nativehawaiian + mixedrace +
                     nonhispanicwhite + medianincome + education + uninsured +
                     windspeed_electionday,
                 data=us_hi)

step_result_us_hi <- step(min_fit_us_hi, direction="both", 
                       scope=list(lower = min_fit_us_hi, 
                                  upper = max_fit_us_hi))
step_result_us_hi$anova
summary(step_result_us_hi)
summary(step_result_us)

### Last experiment windspeeed vs residual 
no_wind_mdl <- lm(mccain_votes ~ turnout_estimate + windspeednov2004 + 
       windspeednov2005 + windspeednov2006 + windspeednov2007 + 
       windspeednov2008 + americanindian + asian + black + 
       hispaniclatino + nativehawaiian + mixedrace +
       nonhispanicwhite + medianincome + education + uninsured,
   data=us_hi)

# seems other variables contains windspeed info, test is not significant
# not sure if this is what you wanted 
res_wind_df <- data.frame( res= no_wind_mdl$residuals, windspeed = us_hi$windspeed_electionday)
ggplot(res_wind_df, aes(x=windspeed, y = res)) + 
    geom_point() +
    geom_smooth(method="lm") 
summary(lm(res~windspeed, data = res_wind_df))





## control all other wind info and regress vote against election day wind speed
wind_res <- lm(mccain_votes~windspeednovember3rd2004 + windspeednovember3rd2005 + 
                   windspeednovember3rd2006 + windspeednovember2rd2007 + 
                   windspeednovember3rd2008, 
           data = us_hi)$residuals
summary(lm(wind_res~us_hi$windspeed_electionday))
plot(lm(wind_res~us_hi$windspeed_electionday))
plot(us_hi$windspeed_electionday, wind_res)







####
plot(robust_fit)
plot(step_result_us)
require(car)
outlierTest(step_result_us)
qqPlot(step_result_us)
leveragePlots(step_result_us)

# Influential Observations
# added variable plots 
avPlots(step_result_us)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(us_vote)-length(step_result_us$coefficients)-2)) 
plot(step_result_us, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(step_result_us,	
              id.method="identify", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(step_result_us)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(step_result_us)


# Evaluate Nonlinearity
# component + residual plot 
crPlots(step_result_us)
# Ceres plots 
ceresPlots(step_result_us)

# Test for Autocorrelated Errors
durbinWatsonTest(step_result_us)