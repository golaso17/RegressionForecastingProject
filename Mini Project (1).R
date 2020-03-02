setwd("C:\\Users\\toekn\\Documents\\ISYE 4031")

input = read.table("Mini Project1.txt", header = TRUE, fill = TRUE)
data = subset(input, select = -c(1,2,3,5))
loc_data = subset(input, select = -c(1,2,5,19,20))
data

attach(data)
model1 = lm(GmSc~., data = data)
library(car)
vif(model)
mean(vif(model))
corr = cor(data); print(corr)
corrAbove50 = corr[corr>0.5]; print(corrAbove50)
maxCorr = max(corr[corr<1]); print(maxCorr)
plot()


plot(model)



install.packages("caroline")
library(caroline)
location_based = groupBy(df = loc_data, by = Opp, aggregation = 'mean', clmns = c(2), full.names = TRUE)
location_based
plot(location_based)


library(forecast)
correlation = corr(data)






full.lm = lm(GmSc ~ . , data=data)
min.lm = lm(GmSc ~ 1, data=data)

step_backward = step(full.lm, direction='backward')

step_forward = step(min.lm, list(upper=full.lm), direction='forward')

library(car)
install.packages('leaps')
library(leaps)
all_model = regsubsets(GmSc~., data = data, method = "exhaustive")
all_sum = summary(all_model)
Rsq = round(all_sum$rsq*100, digit=1)
adj_Rsq = round(all_sum$adjr2*100, digit=1)
Cp = round(all_sum$cp, digit=1)
SSE = all_sum$rss
k = as.numeric(rownames(all_sum$which))
n = all_model$nn
S = round(sqrt(all_sum$rss/(n-(k+1))), digit=2)

#Compute AIC
SSTO = sum((GmSc - mean(GmSc))^2);print(SSTO)
aic = round(2*(k+1)+n*log(SSE/n),digits=2);print(aic)

SSE = round(SSE,digits=2)
# Combine the measures with models
cbind(Rsq, adj_Rsq, Cp, S, SSE, aic, all_sum$outmat)
all_sum$outmat

model2 = lm(GmSc ~ Restaurants)
summary(model2)
plot(Restaurants, GmSc)
abline(model2)
vif(model2)


model3 = lm(GmSc ~ Restaurants + AvgTemp)
summary(model3)
i = 1:length(GmSc)
res = resid(model3)
plot(GmSc,res)
cor(Restaurants, AvgTemp)
vif(model3)

h1 = hatvalues(model1)
limit = 2 * (13 + 1)/length(GmSc)
h1[h1>limit]
h2 = hatvalues(model2)
limit = 2 * (13 + 1)/length(GmSc)
h2[h2>limit]
h3 = hatvalues(model3)
limit = 2 * (13 + 1)/length(GmSc)
h3[h3>limit]

t = qt(0.995, df = length(GmSc)-(13+2))
rstudent1 = rstudent(model1)
rstudent1[abs(rstudent1)>t]
rstudent2 = rstudent(model2)
rstudent2[abs(rstudent2)>t]
rstudent3 = rstudent(model3)
rstudent3[abs(rstudent3)>t]

f = qf(0.5, df1 = 13+1, df2 = length(GmSc)-(13+1))
cook1 = cooks.distance(model1)
cook1[cook1>f]
cook2 = cooks.distance(model2)
cook2[cook2>f]
cook3 = cooks.distance(model3)
cook3[cook3>f]

influence.measures(model1)
