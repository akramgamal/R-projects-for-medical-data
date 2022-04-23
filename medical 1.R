classes <- c("numeric","integer","factor", "character", "character", "integer", "factor", "factor","factor","factor","factor","factor","factor","factor")
medic <-read.table("MedicalAppointments.csv", header = TRUE, sep = ","
                   , row.names = NULL, colClasses =classes)
head(medic)

#2
table(medic$Neighbourhood)

#3
hist(medic$PatientId)

#4
par(mfrow=c(1,1))
plot(density(log2(medic$Age+1)),col=2)
lines(density(log2(medic$Age+1)),col=3)

#5
m <- table(complete.cases(medic))

names(which(colSums(is.na(medic)) > 0))

#6


table(medic$Handcap)
wrong1<-0
list1<-c()
for(i in 1:length(medic$Handcap)){
  if(as.numeric(as.character(medic$Handcap[i]))>1){
     wrong1=wrong1+1
     list1<-c(list1, as.numeric(as.character(medic$Handcap[i])))
     x<-unique(list1)
  }
}
print("Handcap")
print(wrong1)
print(x)
##################################
table(medic$Age)
list2<-c()
wrong2<-0
for(i in 1:length(medic$Age)){
  if(medic$Age[i]<0){
    wrong2=wrong2+1
    list2<-c(list2,medic$Age[i])
    y<-unique(list2)
  }
}
print("Age")
print(wrong2)
print(y)



#7

Mage=medic$Age[medic$Gender=="M"]
Fage=medic$Age[medic$Gender=="F"]
x=range(Mage,Fage)
plot(density(Mage),col="blue",xlim =x ,ylim=range(0,0.025), xlab = "ages",ylab = "frequency",main = "Distribution")
par(new=T)
plot(density(Fage),col="green",xlim =x,ylim=range(0,0.025), xlab = "ages",ylab = "frequency",main = "Distribution")

#8
medic$PatientId
mean(table(medic$PatientId[medic$No.show=="Yes"]))

#9
childrens<-c()
teens<-c()
adults<-c()
elder<-c()
for(i in 1:length(medic$Age)){
  if(medic$Age[i]>=0 &medic$Age[i]<=15 & medic$No.show[i]=="Yes"){
    childrens<-c(childrens,medic$Age[i])
  }else if(medic$Age[i]>=16 & medic$Age[i]<=21 & medic$No.show[i]=="Yes"){
    teens<-c(teens,medic$Age[i])
    
  }else if(medic$Age[i]>=22 & medic$Age[i]<=40 & medic$No.show[i]=="Yes"){
    adults<-c(adults,medic$Age[i])
  }else if(medic$Age[i]>=41 & medic$No.show[i]=="Yes"){
    elder<-c(elder,medic$Age[i])
  }
}

maximum<-c(length(childrens),length(teens),length(adults),length(elder))
print(max(maximum))

#10
childrens<-c()
teens<-c()
adults<-c()
elder<-c()
for(i in 1:length(medic$Age)){
  if(medic$Age[i]>=0 &medic$Age[i]<=15 & medic$No.show[i]=="No"){
    childrens<-c(childrens,medic$Age[i])
  }else if(medic$Age[i]>=16 & medic$Age[i]<=21 & medic$No.show[i]=="No"){
    teens<-c(teens,medic$Age[i])
    
  }else if(medic$Age[i]>=22 & medic$Age[i]<=40 & medic$No.show[i]=="No"){
    adults<-c(adults,medic$Age[i])
  }else if(medic$Age[i]>=41 & medic$No.show[i]=="No"){
    elder<-c(elder,medic$Age[i])
  }
}
maximum<-c(length(childrens),length(teens),length(adults),length(elder))
print(max(maximum))


#11
months<-c()
time<-medic$ScheduledDay[medic$No.show=="Yes"]
for(i in 1:length(time)){
  months<-c(months,as.integer(substr(medic$ScheduledDay[i],6,7)))
}
months
plot(density(months),col=2)

