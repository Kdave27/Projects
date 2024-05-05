#packages
#install.packages("DescTools")
library(DescTools)
library(psych)
#loading the data
data=Credit.Card.Defaulter.Prediction
#structure of data
str(data)
#summary statstic of data
summary(data)
describe(data)
#for checking duplicate rows
duplicate_rows=data[duplicated(data), ]
duplicate_rows

#data preprocessing
data$SEX=as.numeric(as.factor(data$SEX))
data$EDUCATION=as.numeric(as.factor(data$EDUCATION))
data$MARRIAGE=as.numeric(as.factor(data$MARRIAGE))
data$default=as.numeric(as.factor(data$default))
data$default <- ifelse(data$default >= 2, 1, 0)
str(data)
colSums(is.na(data))

#KMO test
KMO(data)

#Bartlett test
cortest.bartlett(data)

#selecting columns based on KMO test
new_data=subset(data , select=c(2,7:18))
str(new_data)
View(new_data)

#correlation plot
c=cor(new_data)
corrplot(c,method="circle")

#KMO on new data
KMO(new_data)
cortest.bartlett(new_data)

#Elbow method
v=eigen(c)
plot(v$values,type="b")
#factanal(new_data,2)

#factor analysis
f=factanal(new_data,2,rotation = "varimax")
f

#factor diagram
loads<-f$loadings
fa.diagram(loads)


#factor scores
out<-factanal(x=new_data,factors = 2,scores = "regression")
scores = out$scores
scores
dim(scores)

#combining factors with the dataset
data_frame = cbind(data,scores)
dim(data_frame)
View(data_frame)

#performing logistic regression
model=glm(data_frame$default~data_frame$Factor1+data_frame$Factor2+data_frame$PAY_AMT6+
            data_frame$PAY_AMT1+data_frame$PAY_AMT2+data_frame$PAY_AMT3+data_frame$PAY_AMT4+
            data_frame$PAY_AMT5+data_frame$SEX+data_frame$EDUCATION+data_frame$MARRIAGE,
            data_frame$AGE,family = "binomial", data = data_frame)
summary(model)



Accuracydp=predict(model,type="response")
p = ifelse(Accuracydp > 0.5,1,0)
m = table(data_frame$default,p)
print(m)
Accuracy=sum(diag(m)/sum(m))
print(Accuracy)

