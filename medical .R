#1
class(apColonData)
pdata=pData(apColonData)
edata=exprs(apColonData)
fdata = fData(apColonData)
class(pdata)
class(edata)
#1-a

class(pdata$filename)
class(pdata$DB_ID)
class(pdata$ExperimentID)
class(pdata$Tissue)
class(pdata$SubType)
class(pdata$ClinicalGroup)
class(pdata$Status)

#1-b

colnames(pdata)
rownames(pdata)

#1-c

summary(pdata$filename)
summary(pdata$DB_ID)
summary(pdata$ExperimentID)
summary(pdata$Tissue)
summary(pdata$SubType)
summary(pdata$ClinicalGroup)
summary(pdata$Status)

#1-d
table(pdata$Tissue)
table(pdata$SubType)
table(pdata$ClinicalGroup)
#NA frequency in clinical Group
length(pdata$ClinicalGroup[is.na(pdata$ClinicalGroup)])
table(pdata$Status)

#1-e
data1<-data.frame(edata[,1:10])
cov(data1)
cor(data1)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = cor(data1), col = palette, symm = TRUE)
#1-f
z1<-0
z2<-0
for(i in 1:length(colnames(edata))){
  if(colnames(edata)[i]=="GSM95478"){
    z1=i
  }else if(colnames(edata)[i]=="GSM95473"){
    z2=i
  }
}
print(z1)
print(z2)
plot(edata[,z1],edata[,z2])
abline(a=0,b=1,col="green")

#Q2
dim(edata)
colnames(edata)
hist(edata[,1])
hist(edata[,6])
edata_center <- edata-rowMeans(edata) #to make them center at zero
hist(edata_center[,6]) 
#all the previous was preproccessing
svd1<-svd(edata_center)
names(svd1)
plot(svd1$d^2/sum(svd1$d^2))#to get percentage

pc1<-prcomp(edata)
names(pc1)
plot(pc1$rotation[,1],svd1$v[,1])
edata_centered2 = t(t(edata) - colMeans(edata))
svd2 = svd(edata_centered2)
par(mfrow=c(1,1))
plot(pc1$rotation[,1],svd2$v[,1],col=2)

#Q3
Phenotypes <- as.factor(c(rep("Aries" ,29),rep("Taurus",24) , rep("Gemini",22),rep("Cancer",19),rep("Leo",21),rep("Virgo",18),rep("Libra",19),rep("Scorpio",20),rep("Sagittarius",23),rep("Capricorn",18),rep("Aquarius",20),rep("Pisces",23)))
h0=c(rep(1,12))
h0=h0/sum(h0)
h1=1-h0
chisq.test(table(Phenotypes),p=h0)
#Q4
selected_data<-edata[,1:10]
colnames(selected_data)
#selected_data = log2(selected_data + 1)
dist1 = dist(t(selected_data)) #samples distances between rows
hclust1 = hclust(dist1)
plot(hclust1,hang = -1) 

kmeans1 = kmeans(edata,centers=3)
table(kmeans1$cluster)
kmeans1$centers #centroid


