data(state)
statedata <- cbind(data.frame(state.x77),state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)
plot(statedata$x, statedata$y)
# highest average high school graduation rate of all the states in the region
tapply(statedata$HS.Grad, statedata$state.region, mean)
#boxplot of the murder rate by region 
boxplot(Murder ~ state.region, data = statedata)
# there is an outlier in the Northeast region of the boxplot you just generated. Which state does this correspond to?
tapply(statedata$Murder, statedata$state.region == "Northeast", max)
statedata$state.name[statedata$Murder == 10.9]
lifexpmodel <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(lifexpmodel)
plot(statedata$Income, statedata$Life.Exp)
lifexpmodel3 <- lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(lifexpmodel3)
lifexpmodel4 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(lifexpmodel4)
p <- predict(lifexpmodel4)
sort(predict(lifexpmodel4))
statedata$state.name[which.min(statedata$Life.Exp)]
statedata$state.name[which.max(statedata$Life.Exp)]
#which state do we make the smallest absolute error
p[which.min(abs(p - statedata$Life.Exp))]
which.min(abs(lifexpmodel4$residuals))
#which state do we make the largest absolute error
p[which.max(abs(p - statedata$Life.Exp))]
which.max(abs(lifexpmodel4$residuals))
