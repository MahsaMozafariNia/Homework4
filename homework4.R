#1/9/2020

d=read.csv("D:/Old_Data/math/Data science toseeh/Files/bank.csv")
summary(d)
str(d)
dim(d)
help("sample")
s=sample(nrow(d),floor(0.2*nrow(d)),replace = FALSE)
d=d[,-12]
dtest=d[s,]
dtrain=d[-s,]
str(dtrain$day)

#we want to use dtrain to predict deposit and then compare it with deposit of dtest.
#first we should change day to factor. because it in int now.
dtrain$day=factor(dtrain$day)
levels(dtrain$day)
m=glm(deposit~.,family=binomial,data=dtrain)
summary(m)
stb=step(m,direction ="backward")
#mostaghel=paste("deposit~",paste(names(d)[-c(1,5,12,14,15,17)],collapse = "+"))
#stb=glm(mostaghel,family = binomial,data=dtrain)
summary(stb)
dtest$day=factor(dtest$day)
phat=predict(stb,dtest,type ="response")
head(phat,5)
help("predict")
yes_no=function(x){
  if (x>=0.5) {
    x="yes"
  }
  else{
    
    x="no"
  }
  return(x)
}
yhat=apply(as.matrix(phat), 1,yes_no)
head(yhat)
length(which(dtest$deposit==yhat))/length(dtest$deposit)


