library(corrplot)
library(DescTools)
data = ap_train

data$Gender = as.factor(data$Gender)
data$Customer.Type = as.factor(data$Customer.Type)
data$Type.of.Travel = as.factor(data$Type.of.Travel)
data$Class = as.factor(data$Class)
data$satisfaction = as.factor(data$satisfaction)

new_data = subset(data , select=-c(1:8,23:25))
colSums(is.na(new_data))
View(new_data)
new_data = na.omit(new_data)
colSums(is.na(new_data))

c = cor(new_data)
corrplot(c,method="circle")

library(psych)
# Kmo value is high inf=dicates the factor anlysis is good
KMO(new_data)

cortest.bartlett(new_data)
v=eigen(c)
plot(v$values,type="b")

#model
factanal(new_data,5)
f=factanal(new_data,5,rotation = "varimax")
f

#Diagramatic representation of factors
loads<-f$loadings
fa.diagram(loads)

#Factor score
out<-factanal(x=new_data,factors = 5,scores = "regression")
scores = out$scores
dim(scores)
data_frame = cbind(data,scores)
dim(data_frame)
View(data_frame)

boxplot(data_frame)

model=glm(data_frame$satisfaction~data_frame$Factor1+data_frame$Gender+data_frame$Customer.Type+data_frame$Age+data_frame$Type.of.Travel
          +data_frame$Class+data_frame$Departure.Delay.in.Minutes+data_frame$Arrival.Delay.in.Minutes+data_frame$Factor2+data_frame$Factor3
          +data_frame$Factor4+data_frame$Factor5,family = "binomial", data = data_frame)
summary(model)

Accuracydp=predict(model,type="response")
p = ifelse(Accuracydp > 1,2,1)
m = table(data_frame$satisfaction,p)
Accuracy=sum(diag(m)/sum(m))
print(Accuracy)



#on testing dataset
c = cor(new_data)
corrplot(c,method="circle")

library(psych)
# Kmo value is high inf=dicates the factor anlysis is good
KMO(new_data)

cortest.bartlett(new_data)
v=eigen(c)
plot(v$values,type="b")

#model
f=factanal(new_data,5,rotation = "varimax")
f

#Diagramatic representation of factors
loads<-f$loadings
fa.diagram(loads)

#Factor score
out<-factanal(x=new_data,factors = 5,scores = "regression")
scores = out$scores
dim(scores)
data_frame = cbind(data,scores)
dim(data_frame)

boxplot(data_frame)

model=glm(data_frame$satisfaction~data_frame$Factor1+data_frame$Gender+data_frame$Customer.Type+data_frame$Age
          +data_frame$Type.of.Travel+data_frame$Class+data_frame$Departure.Delay.in.Minutes+data_frame$Arrival.Delay.in.Minutes+
            data_frame$Factor2+data_frame$Factor3+data_frame$Factor4+data_frame$Factor5,family = "binomial", data = data_frame)
summary(model)

Accuracydp=predict(model,type="response")
p = ifelse(Accuracydp > 1,2,1)
m = table(data_frame$satisfaction,p)
Accuracy=sum(diag(m)/sum(m))
print(Accuracy)