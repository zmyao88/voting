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

step_result <- step(min_fit, direction="both", 
                    scope=list(lower = min_fit, 
                               upper = max_fit))
step_result$anova
summary(step_result)

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
step_result$anova
summary(step_result_us)
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
