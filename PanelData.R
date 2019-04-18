
#install.packages("epiDisplay")

data <- read.table("epilepsy.txt", header=TRUE, 
                   sep=" ",col.names = c("seizures" ,"id" ,"treat", "expind", "timeadj", "age"),
                   colClasses=c("integer","integer","factor","factor","integer","integer"))

#number of subjects
n<- length(unique(data$id))

#number of follow-up times
t<- dim( data[data$id==1,])[1]

#number of missing cases
missing <- colSums (is.na(data), na.rm = FALSE)
#checking is 99 or 999 were used for NA values
summary(data)

#adding time labels to data
time = rep(0:(t-1),times=n)
data$time= time

#creating a new variable, classification of age as 0 young, 1 old
data$age_class = ifelse(data$age<31,0,1)

#creating new variable, base number of seizures as base
base = data[data$expind==0,"seizures"]
data$base = rep(base,each=5)

#Mean and median of the response given time point
medsez=tapply(data$seizures,data$time,median)
medsez
meansez=tapply(data$seizures,data$time,mean)
meansez[1]=meansez[1]/4
meansez

#treatment data
data_treat = data[data$treat==1,]

# Mean of the treatment data 
treat_mean=tapply(data_treat$seizures,data_treat$time,mean)
treat_mean[1]=treat_mean[1]/4
treat_mean
treat_med=tapply(data_treat$seizures,data_treat$time,median)
treat_med

#placebo data
data_pla = data[data$treat==0,]

# Mean of the placebo data 
pla_mean=tapply(data_pla$seizures,data_pla$time,mean)
pla_mean[1]=pla_mean[1]/4
pla_mean
pla_med=tapply(data_pla$seizures,data_pla$time,median)
pla_med

#detecting outliers, it seems like id number 18 and 49 behaves as an outlier at all time points,
#but I didnt discard them in further analyis
data_new=data
data_new[data_new$time==0,]$seizures  = data[data$time==0,]$seizures /4
data_new_treat=data_new[data_new$treat==1,]
data_new_pla=data_new[data_new$treat==0,]
mean(data_new$seizures)+sd(data_new$seizures)*1.5
data_new[data_new$seizures>25,]
data_new_without =data_new[!data_new$id %in% c(18,49),]

#mean of the treatment data without outliers
data_new_without_treat = data_new_without[data_new_without$treat==1,]
new_without_treat_mean=tapply(data_new_without_treat$seizures,data_new_without_treat$time,mean,na.rm=T)
new_without_treat_mean
 
#mean of the placebo data without outliers
data_new_without_pla = data_new_without[data_new_without$treat==0,]
new_without_pla_mean=tapply(data_new_without_pla$seizures,data_new_without_pla$time,mean,na.rm=T)
new_without_pla_mean

#number of seizures through time
library(lattice)
trellis.device(color=T) 
histogram(~data$seizures|factor(data$time),xlab="Number of seizures") 

histogram(~data_new$seizures|factor(data_new$time),xlab="Number of seizures baseline adjusted") 

#box plots without outliers
boxplot(seizures~time,data=data_new, main = "All data", ylab = "seizures",
        xlab = "time", names = c(0,1, 2, 3, 4),ylim=c(0,75) )
boxplot(seizures~time,data=data_new_treat, main = "Treatment", ylab = "Seizures",
        xlab = "Time", names = c(0, 1, 2, 3, 4),ylim=c(0,75) )
boxplot(seizures~time,data=data_new_pla, main = "Placebo", ylab = "Seizures",
        xlab = "Time", names = c(0, 1, 2, 3, 4),ylim=c(0,75) )

## number of seizures according to age_class

trellis.device(color=T) 
histogram(~seizures|factor(age_class), data=data_new, breaks=seq(0, 110, 10),xlab="number of seizures by age_class") 

histogram(~seizures|factor(age_class),data=data_new_treat,breaks=seq(0, 110, 5),
          ylim= c(0,100),main="Number of seizures by age_class for the treatment group",
          xlab="Number of seizures" ) 
histogram(~seizures|factor(age_class),data=data_new_pla,breaks=seq(0, 110, 5),
          ylim= c(0,100),main="Number of seizures by age_class for the placebo group",
          xlab="Number of seizures" ) 

#age versus number of seizures

xyplot(seizures~age|time,data=data_new ,xlab="Age",
       ylab="Seizures",aspect=1,main="all data",
       panel=function(x, y){
         panel.xyplot(x, y)
         panel.loess(x, y, span=3)
       }
)

xyplot(seizures~age|time,data=data_new_treat ,xlab="Age",
       ylab="Seizures",aspect=1,main="Treatment group",
       panel=function(x, y){
         panel.xyplot(x, y)
         panel.loess(x, y, span=3)
       }
)

xyplot(seizures~age|time,data=data_new_pla ,xlab="Age",
       ylab="Seizures",aspect=1,main="Placebo group",
       panel=function(x, y){
         panel.xyplot(x, y)
         panel.loess(x, y, span=3) 
       }
)

#spagetti plots
library(lattice)
xyplot(seizures~time,data=data_new,type="l",groups=id,col="black",main="whole group")
xyplot(seizures~time,type="l",groups=id,data=data_new_treat,col="black",main="treatment",ylim=c(0,50))
xyplot(seizures~time,type="l",groups=id,data=data_new_pla,col="black",main="placebo",ylim=c(0,50))


library(epiDisplay)


followup.plot(data_new$id,data_new$time,
              data_new$seizures,line.col = "black",
              xlab="Time",ylab="Seizures")

followup.plot(data_new$id,data_new$time,
              data_new$seizures,by=data_new$age_class,
              xlab="Time",ylab="Number of seizures",ylim=c(0,40))

followup.plot(data_new_treat$id,data_new_treat$time,
              data_new_treat$seizures,by=data_new_treat$age_class,
              xlab="Time",ylab="Number of seizures",ylim=c(0,40),
              main="Treatment group")

followup.plot(data_new_pla$id,data_new_pla$time,
              data_new_pla$seizures,by=data_new_pla$age_class,
              xlab="Time",ylab="Number of seizures",ylim=c(0,40),
              main="Treatment group")
# I specifically chose y values not covering to whole range to be able to see 
#the overall tendency. Since the plot is already crowded,large y values are
#squeezing crowded area where most of the observations occupy 

data_main= data_new

### Draftman's display
# Producing matrix with rows as ids and columns as responses at different 
#time points
data.se=matrix(c(rep(0,length(unique(data_main$id))*(t))),ncol=t)

for(i in 1:(t))
  data.se[,i]=data_main[data_main$time==(i-1),]$seizures

dimnames(data.se)<-list(NULL,c("Baseline","Time=1","Time=2","Time=3","Time=4"))

head(data.se)
head(data)

splom(~jitter(data.se) )
#As can be seen here, responses at different time points are positively correlated

# variance covariance structure

cor(data.se,method="p") 
#Correlation doesnt seem to get less as time lag increases, so ar structure 
#doesnt seem appropriate
cov(data.se,m="p")

# autocorrelation for lag 1
new1=NULL
for(i in 1:4)
  new1=c(new1,data.se[,i])

new2=NULL
for(i in 2:5)
  new2=c(new2,data.se[,i])

ac1=cor(new1,new2)
ac1

# autocorrelation for lag 2
new1=NULL
for(i in 1:3)
  new1=c(new1,data.se[,i])

new2=NULL
for(i in 3:5)
  new2=c(new2,data.se[,i])

ac2=cor(new1,new2,use="pairwise.complete.obs")
ac2

#As expected there is high auto-correlation

#marginal models

#install.packages("geepack")
library(geepack)

model1 <- geeglm(seizures ~ age+ treat+time+base,
                data = data_main, id = id, family = poisson,
                corstr = "exchangeable")

summary(model1)


model2 <- geeglm(seizures ~ age+ treat+time+base+age*treat+base*treat+time*age+time*treat ,
                 data = data_main, id = id, family = poisson,
                 corstr = "exchangeable")

summary(model2)



model3 <-  geeglm(seizures ~ age+time+base+time*age ,
                  data = data_main, id = id, family = poisson,
                  corstr = "exchangeable")

summary(model3)

model4 <- geeglm(seizures ~ age+time+ treat+base+age*treat ,
                 data = data_main, id = id, family = poisson,
                 corstr = "exchangeable")

summary(model4)


model5 <- geeglm(seizures ~  age+ treat+base,
                 data = data_main, id = id, family = poisson,
                 corstr = "exchangeable")

summary(model5)

#unstructured
model6 <- geeglm(seizures ~ age+ treat+time+age*treat ,
                   data = data_main, id = id, family = poisson,
                   corstr = "unstructured")
summary(model6)

model7<- geeglm(seizures ~ age+ treat+base+treat*age,
                data = data_main, id = id, family = poisson,
                corstr = "exchangeable")
summary(model7)


data_main$seizures[data_main$timeadj==8 ]= data_main$seizures[data_main$timeadj==8 ]*4

model1int <- geeglm(seizures ~ age+ treat+time+base,
                 data = data_main, id = id, family = poisson,
                 corstr = "exchangeable")
summary(model1int)


model2int <- geeglm(seizures ~ age+ treat+time+base+age*treat+base*treat+time*age+time*treat ,
                 data = data_main, id = id, family = poisson,
                 corstr = "exchangeable")
summary(model2int)

model3int <-  geeglm(seizures ~ age+time+base+time*age ,
                  data = data_main, id = id, family = poisson,
                  corstr = "exchangeable")
summary(model3int)

model4int <- geeglm(seizures ~ age+time+ treat+base+age*treat ,
                 data = data_main, id = id, family = poisson,
                 corstr = "exchangeable")
summary(model4int)


model5int <- geeglm(seizures ~  age+ treat+base,
                 data = data_main, id = id, family = poisson,
                 corstr = "exchangeable")
summary(model5int)

#unstructured 
model6int <- geeglm(seizures ~ age+ treat+time+age*treat ,
                 data = data_main, id = id, family = poisson,
                 corstr = "unstructured")
summary(model6int)

model7int<- geeglm(seizures ~ age+ treat+base+treat*age,
                data = data_main, id = id, family = poisson,
                corstr = "exchangeable")
summary(model7int)

#using time adj too
model1intadj <- geeglm(seizures ~ age+ base+ timeadj,
                    data = data_main, id = id, family = poisson,
                    corstr = "exchangeable")
summary(model1intadj)

model2intadj <- geeglm(seizures ~ age+timeadj+ treat+time+base+age*treat+base*treat+time*age+time*treat ,
                    data = data_main, id = id, family = poisson,
                    corstr = "exchangeable")
summary(model2intadj)

model3intadj <-  geeglm(seizures ~ age+timeadj+time+base+time*age ,
                     data = data_main, id = id, family = poisson,
                     corstr = "exchangeable")
summary(model3intadj)

model4intadj <- geeglm(seizures ~ age+timeadj+time+ treat+base+age*treat ,
                    data = data_main, id = id, family = poisson,
                    corstr = "exchangeable")
summary(model4intadj)


model5intadj <- geeglm(seizures ~  age+timeadj+ treat+base,
                    data = data_main, id = id, family = poisson,
                    corstr = "exchangeable")
summary(model5intadj)

#unstructured
model6intadj <- geeglm(seizures ~ age+timeadj+ treat+time+age*treat ,
                    data = data_main, id = id, family = poisson,
                    corstr = "unstructured")
summary(model6intadj)

model7intadj<- geeglm(seizures ~ age+timeadj+ treat+base+treat*age,
                   data = data_main, id = id, family = poisson,
                   corstr = "exchangeable")

summary(model7intadj)
#install.packages("devtools")
#library(devtools)
#install_github(repo = "djhocking/qicpack/QICpack")
library(QICpack)
sapply(list( model1int, model2int, model3int,model4int, model1intadj,model3intadj   ),qic)

sum(model4int$residuals^2)
sum(model1intadj$residuals^2)
sum(model1int$residuals^2)
sum(model1$residuals^2)

model1intadj$fitted.values[1]

#transition models

data_new1 <- data_main[ data_main$time==2, -c(4,5,7,8) ]
colnames(data_new1)[1]="y"
data_new1_lag <- data_main[ data_main$time==1, -c(4,5,7,8) ]
data_new1 <- cbind(data_new1,data_new1_lag[,1] )
colnames(data_new1)[6]="ylag1"

data_new2 <- data_main[ data_main$time==3, -c(4,5,7,8) ]
colnames(data_new2)[1]="y"
data_new2_lag <- data_main[ data_main$time==2, -c(4,5,7,8) ]
data_new2 <- cbind(data_new2,data_new2_lag[,1] )
colnames(data_new2)[6]="ylag1"

data_new3 <- data_main[ data_main$time==4, -c(4,5,7,8) ]
colnames(data_new3)[1]="y"
data_new3_lag <- data_main[ data_main$time==3, -c(4,5,7,8) ]
data_new3 <- cbind(data_new3,data_new3_lag[,1] )
colnames(data_new3)[6]="ylag1"

data_trans <- rbind(data_new1,data_new2, data_new3)
dim(data_trans)

osub <- order(data_trans$id)
data_trans <- data_trans[osub,]

library(geepack)

#independence structure(with transition models this is the only corr structure we can use)
mdl_trans1 <- geeglm(y ~ treat + age +base+ ylag1,
                data = data_trans, id = id, family =poisson,
                corstr = "independence")
summary(mdl_trans1)

mdl_trans1.1 <- geeglm(y ~  age +base+ ylag1,
                  data = data_trans, id = id, family =poisson,
                  corstr = "independence")
summary(mdl_trans1.1)

anova(mdl_trans1,mdl_trans1.1) #p is high, so we can use the reduced the model

mdl_trans1.2 <- geeglm(y ~  base+ ylag1,
                       data = data_trans, id = id, family =poisson,
                       corstr = "independence")

summary(mdl_trans1.2)

anova(mdl_trans1.2,mdl_trans1.1) #p is high, so we can use the reduced the model

sum(mdl_trans1.1$residuals^2)
sum(mdl_trans1.2$residuals^2)

mdl_trans1.1$fitted.values[1]

#random effect models

# install.packages("lme4")
library(lme4)

#only random intercept
mdl_re1 <- lmer(seizures ~ treat + time + age + base +expind +
                     + (1 | id), data = data_main)
summary(mdl_re1)

mdl_re1.1 <- lmer(seizures ~ treat  + age + base +expind +
                  + (1 | id), data = data_main)
summary(mdl_re1.1)

anova(mdl_re1,mdl_re1.1)  # since p value is high, we choose the simpler model 1.1

mdl_re2 <- lmer(seizures ~ treat + time + age + base +expind +
                  + (time | id), data = data_main)

summary(mdl_re2)

#random intercept with random slope
mdl_re2.1 <- lmer(seizures ~ treat  + age + base +expind +
                  + (time | id), data = data_main)

summary(mdl_re2.1)
anova(mdl_re2,mdl_re2.1)  # since p value is high, we choose the simpler model 2.1

anova(mdl_re1.1,mdl_re2.1)  # since p value is small, we choose the more complex model 2.1

summary(mdl_re2.1)

fixef(mdl_re2.1)

ranef(mdl_re2.1)

estimate = fitted(mdl_re2.1)[1]

