#set workspace
setwd("F:/EikDenYeoh/Documents/Research Paper/R")

#load read excel library
library(readxl)

library(dplyr)
library(xlsx)
library(ggplot2)
library(PerformanceAnalytics)
library(quantmod)
library(gridExtra)
library(grid)
library(astsa)
library(xts)
library(forecast)

#load all excel file
klcc <- read.csv(file="REITs/5235SS.KLCC.csv", header=TRUE, sep=",")
igbreit <- read.csv(file="REITs/5227.IGBREIT.csv", header=TRUE, sep=",")
pavreit <- read.csv(file="REITs/5212.PAVREIT.csv", header=TRUE, sep=",")
sunreit <- read.csv(file="REITs/5176.SUNREIT.csv", header=TRUE, sep=",")
cmmt <- read.csv(file="REITs/5180.CMMT.csv", header=TRUE, sep=",")
ytlreit <- read.csv(file="REITs/5109.YTLREIT.csv", header=TRUE, sep=",")
axreit <- read.csv(file="REITs/5106.AXREIT.csv", header=TRUE, sep=",")
mqreit <- read.csv(file="REITs/5123.MQREIT.csv", header=TRUE, sep=",")
alaqar <- read.csv(file="REITs/5116.ALAQAR.csv", header=TRUE, sep=",")
uoareit <- read.csv(file="REITs/5110.UOAREIT.csv", header=TRUE, sep=",")
hektar <- read.csv(file="REITs/5121.HEKTAR.csv", header=TRUE, sep=",")
alsreit <- read.csv(file="REITs/5269.ALSREIT.csv", header=TRUE, sep=",")
amfirst <- read.csv(file="REITs/5120.AMFIRST.csv", header=TRUE, sep=",")
arreit <- read.csv(file="REITs/5127.ARREIT.csv", header=TRUE, sep=",")
kipreit <- read.csv(file="REITs/5280.KIPREIT.csv", header=TRUE, sep=",")
twrreit <- read.csv(file="REITs/5111.TWRREIT.csv", header=TRUE, sep=",")
atrium <- read.csv(file="REITs/5130.ATRIUM.csv", header=TRUE, sep=",")
seal <- read.csv(file="REITs/4286.SEAL.csv", header=TRUE, sep=",")
ahp <- read.csv(file="REITs/4952.AHP.csv", header=TRUE, sep=",")

#format date
klcc$Date <- as.Date(klcc$Date)
igbreit$Date <- as.Date(igbreit$Date)
pavreit$Date <- as.Date(pavreit$Date)
sunreit$Date <- as.Date(sunreit$Date)
cmmt$Date <- as.Date(cmmt$Date)
ytlreit$Date <- as.Date(ytlreit$Date)
axreit$Date <- as.Date(axreit$Date)
mqreit$Date <- as.Date(mqreit$Date)
alaqar$Date <- as.Date(alaqar$Date)
uoareit$Date <- as.Date(uoareit$Date)
hektar$Date <- as.Date(hektar$Date)
alsreit$Date <- as.Date(alsreit$Date)
amfirst$Date <- as.Date(amfirst$Date)
arreit$Date <- as.Date(arreit$Date)
kipreit$Date <- as.Date(kipreit$Date)
twrreit$Date <- as.Date(twrreit$Date)
atrium$Date <- as.Date(atrium$Date)
seal$Date <- as.Date(seal$Date)
ahp$Date <- as.Date(ahp$Date)

#format double
klcc$Adj.Close <- as.double(levels(klcc$Adj.Close))[klcc$Adj.Close]
igbreit$Adj.Close <- as.double(levels(igbreit$Adj.Close))[igbreit$Adj.Close]
pavreit$Adj.Close <- as.double(levels(pavreit$Adj.Close))[pavreit$Adj.Close]
sunreit$Adj.Close <- as.double(levels(sunreit$Adj.Close))[sunreit$Adj.Close]
cmmt$Adj.Close <- as.double(levels(cmmt$Adj.Close))[cmmt$Adj.Close]
ytlreit$Adj.Close <- as.double(levels(ytlreit$Adj.Close))[ytlreit$Adj.Close]
axreit$Adj.Close <- as.double(levels(axreit$Adj.Close))[axreit$Adj.Close]
mqreit$Adj.Close <- as.double(levels(mqreit$Adj.Close))[mqreit$Adj.Close]
alaqar$Adj.Close <- as.double(levels(alaqar$Adj.Close))[alaqar$Adj.Close]
uoareit$Adj.Close <- as.double(levels(uoareit$Adj.Close))[uoareit$Adj.Close]
hektar$Adj.Close <- as.double(levels(hektar$Adj.Close))[hektar$Adj.Close]
alsreit$Adj.Close <- as.double(levels(alsreit$Adj.Close))[alsreit$Adj.Close]
amfirst$Adj.Close <- as.double(levels(amfirst$Adj.Close))[amfirst$Adj.Close]
arreit$Adj.Close <- as.double(levels(arreit$Adj.Close))[arreit$Adj.Close]
kipreit$Adj.Close <- as.double(levels(kipreit$Adj.Close))[kipreit$Adj.Close]
twrreit$Adj.Close <- as.double(levels(twrreit$Adj.Close))[twrreit$Adj.Close]
atrium$Adj.Close <- as.double(levels(atrium$Adj.Close))[atrium$Adj.Close]
seal$Adj.Close <- as.double(levels(seal$Adj.Close))[seal$Adj.Close]
ahp$Adj.Close <- as.double(levels(ahp$Adj.Close))[ahp$Adj.Close]

#Get Month
klcc$Month <- months(klcc$Date)
igbreit$Month <- months(igbreit$Date)
pavreit$Month <- months(pavreit$Date)
sunreit$Month <- months(sunreit$Date)
cmmt$Month <- months(cmmt$Date)
ytlreit$Month <- months(ytlreit$Date)
axreit$Month <- months(axreit$Date)
mqreit$Month <- months(mqreit$Date)
alaqar$Month <- months(alaqar$Date)
uoareit$Month <- months(uoareit$Date)
hektar$Month <- months(hektar$Date)
alsreit$Month <- months(alsreit$Date)
amfirst$Month <- months(amfirst$Date)
arreit$Month <- months(arreit$Date)
kipreit$Month <- months(kipreit$Date)
twrreit$Month <- months(twrreit$Date)
atrium$Month <- months(atrium$Date)
seal$Month <- months(seal$Date)
ahp$Month <- months(ahp$Date)

#Get Year
klcc$Year <- format(klcc$Date, format="%Y")
igbreit$Year <- format(igbreit$Date, format="%Y")
pavreit$Year <- format(pavreit$Date, format="%Y")
sunreit$Year <- format(sunreit$Date, format="%Y")
cmmt$Year <- format(cmmt$Date, format="%Y")
ytlreit$Year <- format(ytlreit$Date, format="%Y")
axreit$Year <- format(axreit$Date, format="%Y")
mqreit$Year <- format(mqreit$Date, format="%Y")
alaqar$Year <- format(alaqar$Date, format="%Y")
uoareit$Year <- format(uoareit$Date, format="%Y")
hektar$Year <- format(hektar$Date, format="%Y")
alsreit$Year <- format(alsreit$Date, format="%Y")
amfirst$Year <- format(amfirst$Date, format="%Y")
arreit$Year <- format(arreit$Date, format="%Y")
kipreit$Year <- format(kipreit$Date, format="%Y")
twrreit$Year <- format(twrreit$Date, format="%Y")
atrium$Year <- format(atrium$Date, format="%Y")
seal$Year <- format(seal$Date, format="%Y")
ahp$Year <- format(ahp$Date, format="%Y")

#aggregate on month, year and get mean
m_klcc <- aggregate(Adj.Close ~ Month + Year, klcc, mean)
m_igbreit <- aggregate(Adj.Close ~ Month + Year, igbreit, mean)
m_pavreit <- aggregate(Adj.Close ~ Month + Year, pavreit, mean)
m_sunreit <- aggregate(Adj.Close ~ Month + Year, sunreit, mean)
m_cmmt <- aggregate(Adj.Close ~ Month + Year, cmmt, mean)
m_ytlreit <- aggregate(Adj.Close ~ Month + Year, ytlreit, mean)
m_axreit <- aggregate(Adj.Close ~ Month + Year, axreit, mean)
m_mqreit <- aggregate(Adj.Close ~ Month + Year, mqreit, mean)
m_alaqar <- aggregate(Adj.Close ~ Month + Year, alaqar, mean)
m_uoareit <- aggregate(Adj.Close ~ Month + Year, uoareit, mean)
m_hektar <- aggregate(Adj.Close ~ Month + Year, hektar, mean)
m_alsreit <- aggregate(Adj.Close ~ Month + Year, alsreit, mean)
m_amfirst <- aggregate(Adj.Close ~ Month + Year, amfirst, mean)
m_arreit <- aggregate(Adj.Close ~ Month + Year, arreit, mean)
m_kipreit <- aggregate(Adj.Close ~ Month + Year, kipreit, mean)
m_twrreit <- aggregate(Adj.Close ~ Month + Year, twrreit, mean)
m_atrium <- aggregate(Adj.Close ~ Month + Year, atrium, mean)
m_seal <- aggregate(Adj.Close ~ Month + Year, seal, mean)
m_ahp <- aggregate(Adj.Close ~ Month + Year, ahp, mean)

#change column name
colnames(m_klcc) <- c("Month", "Year", "KLCC")
colnames(m_igbreit) <- c("Month", "Year", "IGBREIT")
colnames(m_pavreit) <- c("Month", "Year", "PAVREIT")
colnames(m_sunreit) <- c("Month", "Year", "SUNREIT")
colnames(m_cmmt) <- c("Month", "Year", "CMMT")
colnames(m_ytlreit) <- c("Month", "Year", "YTLREIT")
colnames(m_axreit) <- c("Month", "Year", "AXREIT")
colnames(m_mqreit) <- c("Month", "Year", "MQREIT")
colnames(m_alaqar) <- c("Month", "Year", "ALAQAR")
colnames(m_uoareit) <- c("Month", "Year", "UOAREIT")
colnames(m_hektar) <- c("Month", "Year", "HEKTAR")
colnames(m_alsreit) <- c("Month", "Year", "ALSREIT")
colnames(m_amfirst) <- c("Month", "Year", "AMFIRST")
colnames(m_arreit) <- c("Month", "Year", "ARREIT")
colnames(m_kipreit) <- c("Month", "Year", "KIPREIT")
colnames(m_twrreit) <- c("Month", "Year", "TWRREIT")
colnames(m_atrium) <- c("Month", "Year", "ATRIUM")
colnames(m_seal) <- c("Month", "Year", "SEAL")
colnames(m_ahp) <- c("Month", "Year", "AHP")

#merge data frame by month and year
m_reit<- merge(m_klcc, m_igbreit, by=c("Month", "Year"), all.y = TRUE)
m_reit<- merge(m_reit, m_pavreit, by=c("Month", "Year"), all.y=TRUE)
m_reit<- merge(m_reit, m_sunreit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_cmmt, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_ytlreit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_axreit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_mqreit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_alaqar, by=c("Month", "Year"), all.y=TRUE)
m_reit<- merge(m_reit, m_uoareit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_hektar, by=c("Month", "Year"))
m_reit<- merge(m_reit, m_alsreit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_amfirst, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_arreit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_kipreit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_twrreit, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_atrium, by=c("Month", "Year"), all.x=TRUE)
m_reit<- merge(m_reit, m_seal, by=c("Month", "Year"),all.x=TRUE)
m_reit<- merge(m_reit, m_ahp, by=c("Month", "Year"), all.x=TRUE)

#create date column 
ConvertDate <-paste(m_reit$Year,m_reit$Month, "01", sep=" ")
m_reit$Date <- as.Date(ConvertDate, format="%Y %B %d")

#sort by date
m_reit <- arrange(m_reit, Date)

#reorder column number
m_reit<-m_reit[c(22,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]

#convert zoo format for get the total of annual return
m_reit<- m_reit[,-c(2,3)]
zooreit<- read.zoo(m_reit)

#assign individua zoo format
zklcc <- zooreit[,1]
zklcc <- Return.calculate(zklcc)
rklcc <- table.AnnualizedReturns(na.omit(zklcc))

zigbreit <- zooreit[,2]
zigbreit <- Return.calculate(zigbreit)
rigbreit <- table.AnnualizedReturns(na.omit(zigbreit))

zpavreit <- zooreit[,3]
zpavreit <- Return.calculate(zpavreit)
rpavreit <- table.AnnualizedReturns(na.omit(zpavreit))

zsunreit <- zooreit[,4]
zsunreit <- Return.calculate(zsunreit)
rsunreit <- table.AnnualizedReturns(na.omit(zsunreit))

zcmmtreit <- zooreit[,5]
zcmmtreit <- Return.calculate(zcmmtreit)
rcmmt <- table.AnnualizedReturns(na.omit(zcmmtreit))

zytlreit <- zooreit[,6]
zytlreit <- Return.calculate(zytlreit)
rytlreit <- table.AnnualizedReturns(na.omit(zytlreit))

zaxreit <- zooreit[,7]
zaxreit <- Return.calculate(zaxreit)
raxreit <- table.AnnualizedReturns(na.omit(zaxreit))

zmqbreit <- zooreit[,8]
zmqbreit <- Return.calculate(zmqbreit)
rmqreit <- table.AnnualizedReturns(na.omit(zmqbreit))

zalaqar <- zooreit[,9]
zalaqar <- Return.calculate(zalaqar)
ralaqar <- table.AnnualizedReturns(na.omit(zalaqar))

zuoareit <- zooreit[,10]
zuoareit <- Return.calculate(zuoareit)
ruoareit <- table.AnnualizedReturns(na.omit(zuoareit))

zhektar <- zooreit[,11]
zhektar <- Return.calculate(zhektar)
rhektar <- table.AnnualizedReturns(na.omit(zhektar))

zalsreit <- zooreit[,12]
zalsreit <- Return.calculate(zalsreit)
ralsreit <- table.AnnualizedReturns(na.omit(zalsreit))

zamfirst <- zooreit[,13]
zamfirst <- Return.calculate(zamfirst)
ramfirst <- table.AnnualizedReturns(na.omit(zamfirst))

zarreit <- zooreit[,14]
zarreit <- Return.calculate(zarreit)
rarreit <- table.AnnualizedReturns(na.omit(zarreit))

zkipreit <- zooreit[,15]
zkipreit <- Return.calculate(zkipreit)
rkipreit <- table.AnnualizedReturns(na.omit(zkipreit))

ztwreit <- zooreit[,16]
ztwreit <- Return.calculate(ztwreit)
rtwreit <- table.AnnualizedReturns(na.omit(ztwreit))

zatrium <- zooreit[,17]
zatrium <- Return.calculate(zatrium)
ratrium <- table.AnnualizedReturns(na.omit(zatrium))

zseal <- zooreit[,18]
zseal <- Return.calculate(zseal)
rseal <- table.AnnualizedReturns(na.omit(zseal))

zahp <- zooreit[,19]
zahp <- Return.calculate(zahp)
rahp <- table.AnnualizedReturns(na.omit(zahp))

#merge table annulized return
mreits.annualReturn<- cbind(rklcc, rigbreit, rpavreit, rsunreit, rcmmt, rytlreit, raxreit, rmqreit, ralaqar, ruoareit,
                            rhektar,ralsreit,ramfirst,rarreit,rkipreit,rtwreit,ratrium,rseal,rahp)

#rotate horizontal to vertical
mreits.annualReturn_v<-t(mreits.annualReturn)

#change column name
colnames(m_klcc) <- c("Month", "Year", "Adj Price")
colnames(m_igbreit) <- c("Month", "Year", "Adj Price")
colnames(m_pavreit) <- c("Month", "Year", "Adj Price")
colnames(m_sunreit) <- c("Month", "Year", "Adj Price")
colnames(m_cmmt) <- c("Month", "Year", "Adj Price")
colnames(m_ytlreit) <- c("Month", "Year", "Adj Price")
colnames(m_axreit) <- c("Month", "Year", "Adj Price")
colnames(m_mqreit) <- c("Month", "Year", "Adj Price")
colnames(m_alaqar) <- c("Month", "Year", "Adj Price")
colnames(m_uoareit) <- c("Month", "Year", "Adj Price")
colnames(m_hektar) <- c("Month", "Year", "Adj Price")
colnames(m_alsreit) <- c("Month", "Year", "Adj Price")
colnames(m_amfirst) <- c("Month", "Year", "Adj Price")
colnames(m_arreit) <- c("Month", "Year", "Adj Price")
colnames(m_kipreit) <- c("Month", "Year", "Adj Price")
colnames(m_twrreit) <- c("Month", "Year", "Adj Price")
colnames(m_atrium) <- c("Month", "Year", "Adj Price")
colnames(m_seal) <- c("Month", "Year", "Adj Price")
colnames(m_ahp) <- c("Month", "Year", "Adj Price")

#create quote columns
m_klcc$Quote <- "KLCC"
m_igbreit$Quote <- "IGBREIT"
m_pavreit$Quote <- "PAVREIT"
m_sunreit$Quote <- "SUNREIT"
m_cmmt$Quote <- "CMMT"
m_ytlreit$Quote <- "YTLREIT"
m_axreit$Quote <- "AXREIT"
m_mqreit$Quote <- "MQREIT"
m_alaqar$Quote <- "ALAQAR"
m_uoareit$Quote <- "UOAREIT"
m_hektar$Quote <- "HEKTAR"
m_alsreit$Quote <- "ALSREIT"
m_amfirst$Quote <- "AMFIRST"
m_arreit$Quote <- "ARREIT"
m_kipreit$Quote <- "KIPREIT"
m_twrreit$Quote <- "TWRREIT"
m_atrium$Quote <- "ATRIUM"
m_seal$Quote <- "SEAL"
m_ahp$Quote <- "AHP"

#append row into 1 data frame
reit<- rbind(m_klcc, m_igbreit, m_pavreit, m_sunreit, m_cmmt, m_ytlreit, m_axreit, m_mqreit,
             m_alaqar, m_uoareit, m_hektar, m_alsreit,m_amfirst, m_arreit, m_kipreit, m_twrreit, m_atrium,
             m_seal, m_ahp)

#create date column for reit
ReitDate <-paste(reit$Year,reit$Month, "01", sep=" ")
reit$Date <- as.Date(ReitDate, format="%Y %B %d")

#reit sort by date
reit <- arrange(reit, Date)

#write multiple plot function 
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#plot graph
pklcc<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="KLCC"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#000000")
pigbreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="IGBREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#FC0303")
ppavreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="PAVREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#FCB803")
psunreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="SUNREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#0BB405")
pcmmt<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="CMMT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#05B4A7")
pytlreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="YTLREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#0567B4")
paxreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="AXREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#048EFB")
pmqreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="MQREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#7104FB")
palaqar<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="ALAQAR"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#CA04FB")
puoareit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="UOAREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#FB04CE")
phektar<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="HEKTAR"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#FB0465")
palsreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="ALSREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#716B6D")
pamfirst<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="AMFIRST"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#E54567")
parreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="ARREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#717AD9")
pkipreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="KIPREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#6DAACF")
ptwrreit<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="TWRREIT"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#5EE370")
patrium<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="ATRIUM"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#E3DB5E")
pseal<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="SEAL"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#F7C8A6")
pahp<-ggplot(reit, aes(Date, `Adj Price`, colour=Quote))+
  geom_line(data=subset(reit, Quote=="AHP"), size=2)+ 
  theme(legend.position="top")+ 
  scale_color_manual(values="#D87873")

#plot multiple plot
multiplot(pklcc, pigbreit, ppavreit, psunreit, pcmmt, pytlreit, paxreit, pmqreit, cols=4)

multiplot(palaqar,
          puoareit, phektar, palsreit, pamfirst, parreit, pkipreit, ptwrreit, cols=4)

multiplot(patrium, 
          pseal, pahp, cols=4)

#research topic for prediction
#preparing data for Machine learning
# 4.2 ARIMA Analysis for top 10 M-REITS

#Top 4 M-REITs with daily adjusted price
ytlreit_one<- na.omit(ytlreit[,c(1,6)])
ytlreit_one <- xts(x=ytlreit_one[,2], order.by=ytlreit_one[,"Date"])
sunreit_two<- na.omit(sunreit[,c(1,6)])
sunreit_two <- xts(x=sunreit_two[,2], order.by=sunreit_two[,"Date"])
mqreit_three<- na.omit(mqreit[,c(1,6)])
mqreit_three <- xts(x=mqreit_three[,2], order.by=mqreit_three[,"Date"])
atrium_four<-na.omit(atrium[,c(1,6)])
atrium_four <- xts(x=atrium_four[,2], order.by=atrium_four[,"Date"])

#diff is to detrended trend stationary data which is to understand the pattern with remove the trend data.
#Detrended price in stock is an indicator that attempts to eliminate the long term trends by displaced moving average
#which doesn't react to most current price action. 
par(mfcol=c(4,2))
plot(ytlreit_one)
plot(diff(ytlreit_one))

plot(sunreit_two)
plot(diff(sunreit_two))

plot(mqreit_three)
plot(diff(mqreit_three))

plot(atrium_four)
plot(diff(atrium_four))


#Next to find AR and MA model what best for choice on residual analysis 
#and diff detrended trend is fit to use that to identify model.
#This need to be test on different set of AR model and MA model 
#if manual test with auto.arima function, this could be find the minimun AIC result.
auto.arima(ytlreit_one)
sarima(ytlreit_one,2,1,2)

auto.arima(sunreit_two)
sarima(sunreit_two, 2,1,1)

auto.arima(mqreit_three)
sarima(mqreit_three, 0,1,1)

auto.arima(atrium_four)
sarima(atrium_four,2,1,2)

#forecast the data for 60 days ahead
par(mfcol=c(2,2))
ytlreit_pred<-sarima.for(ytlreit_one, n.ahead=60, 0,1,1)
sunreit_pred<-sarima.for(sunreit_two, n.ahead=60, 0,1,1)
mqreit_pred<-sarima.for(mqreit_three, n.ahead=60, 0,1,1)
atrium_pred<-sarima.for(atrium_four, n.ahead=60, 1,1,1)



