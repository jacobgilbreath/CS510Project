# Libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Coronavirus Data
d=read.csv("./Data/owid-covid-data.csv")
d= d %>%
  filter(location=="United States") %>%
  rename("Date"="date")

# Apple Stocks
apple=read.csv("./Data/AAPL.csv")
apple=apple[-(1:41),]
apple=apple %>%
  rename("AClose"="Close")

# Microsoft Stocks
msft=read.csv("./Data/MSFT.csv")
msft=msft[-(1:41),]
msft=msft %>%
  rename("MClose"="Close")

# Merge and Create New Data Frame of useful variables 
# (Date,new_cases,Closing Price)
x=1:212
ddd=merge(d,apple,by="Date")
ddd=merge(ddd,msft,by="Date")
nd=data.frame("Date"=ddd$Date,"new_cases"=ddd$new_cases,
              "AAPLClose"=ddd$AClose,"MSFTClose"=ddd$MClose,
              "n"=x)

# Changing to percent change (Degree of Freedom Reduced to 211)
appleu=nd$AAPLClose[2:212]
applel=nd$AAPLClose[1:211]
diffA=(appleu-applel)
percA=(diffA/applel)*100

msftu=nd$MSFTClose[2:212]
msftl=nd$MSFTClose[1:211]
diffM=(msftu-msftl)
percM=(diffM/msftl)*100

cu=nd$new_cases[2:212]
cl=nd$new_cases[1:211]
diffC=(cu-cl)
percC=(diffC/cl)*100
f=data.frame("msft"=percM,"aapl"=percA,"cases"=percC,"n"=1:211)
f=f[!is.infinite(rowSums(f)),]
f=f[-32,]

# Point Plots of Date vs. 
# Apple Close, Microsoft Close
fn=data.frame("n"=f$n,"aapl"=f$aapl,"msft"=f$msft)
fn=fn%>%
  melt(id.vars="n")
ggplot(fn)+geom_point(aes(x=n,y=value,color=variable))+
  geom_vline(xintercept=15,color="orange")+
  xlab("Date")+ylab("Percent Change")

fna=f[complete.cases(f),] # Remove NA rows

# Graph displaying Percentage of new cases in the US,
# Orange line represents the 1st confirmed case in US
ggplot(fna)+geom_point(aes(x=n,y=cases),color="Black")+
  geom_vline(xintercept=15,color="orange")+ylab("Percent Change in New Cases")+
  xlab("Date")

# Date vs Each Stock Linear regression
lmA=lm(aapl ~ cases,data=fna)
lmAS=summary(lmA)
Apred=lmAS$coefficients[1]+lmAS$coefficients[2]*fna$cases
Ares=fna$aapl-Apred
fna$Ares=Ares
# Adjusted R-Squared = 0.01537

lmM=lm(msft ~ cases,data=fna)
lmMS=summary(lmM)
Mpred=lmMS$coefficients[1]+lmMS$coefficients[2]*fna$cases
Mres=fna$msft-Mpred
fna$Mres=Mres
# Adjusted R-Squared = 0.001555

# Plot of Linear Relationship we are trying to model.
ggplot(fna,aes(x=cases))+
  geom_point(aes(y=msft),color="blue")+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  xlab("Percent Change in Cases") + ylab("Percent Microsoft Stock Change")

ggplot(fna,aes(x=cases))+geom_point(aes(y=aapl),color="red")+
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  xlab("Percent Change in Cases") + ylab("Percent Apple Stock Change")

# Residual Plots From Predicted (Red for Apple, Blue for Microsoft)
ggplot(fna,aes(x=n))+geom_point(aes(y=Mres),color="darkblue",size=0.75)+
  geom_point(aes(y=Ares),color="red",size=0.75)+
  geom_hline(yintercept = 0,color="black")+
  xlab("Days After Outbreak")+ylab("Residuals")

# From the models, there is no evidence to support that the Apple 
# and Microsoft closing stock prices were directly affected by only the
# COVID-19 outbreak. Although it was most likely a cause for the instability
# of the market, there must have been several other factors
# leading to the fall of the DOW. I would think a further study
# that includes other variables, such as travel restrictions or new 
# products released,  will provide better results with similar analyses.
