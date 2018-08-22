library(quantmod)
library(xts)
library(zoo)
library(timeSeries)
library(fPortfolio)
library(PortfolioAnalytics)
library(Hmisc)
library(PerformanceAnalytics)
library(openxlsx)

FVXriskfree <- read.xlsx("timeSeriesHistoric.xlsx",sheet = "Riskfree",colNames=T,rowNames=T)
FVXriskfree <- as.xts(FVXriskfree)

# amazon <- getSymbols("AMZN",src="av",auto.assign=F,return.class = "xts",adjusted=T,outputs.size="full",data.type="csv",periodicity = "weekly")
# amazon <- amazon["2013-05-17/2018-05-18"]
# amazon <- amazon[,c(4,5,7)]
# amazon$AMZN.Delta <- diff.xts(amazon[,1],lag=1,differences = 1,arithmetic = T,log=F,na.pad = T)
# amazon$AMZN.AM.Return <- returns(amazon[,2],method="discrete")
# amazon$AMZN.AM.ExcessReturn <- amazon[,5]-FVXriskfree[,2]
# amazon$AMZN.GM.Return <- returns(amazon[,2],method="continuous")
# amazon$AMZN.GM.ExcessReturn <- amazon[,7]-FVXriskfree[,2]
# write.zoo(amazon,file="amazon.csv",sep=",",index.name="Date")

amazon <- read.xlsx("timeSeriesHistoric.xlsx",sheet = "Amazon",colNames=T,rowNames=T)
amazon <- as.xts(amazon)

baidu <- read.xlsx("timeSeriesHistoric.xlsx",sheet = "Baidu",colNames=T,rowNames=T)
baidu <- as.xts(baidu)

disney <- read.xlsx("timeSeriesHistoric.xlsx",sheet = "Disney",colNames=T,rowNames=T)
disney <- as.xts(disney)

merck <- read.xlsx("timeSeriesHistoric.xlsx",sheet = "Merck",colNames=T,rowNames=T)
merck <- as.xts(merck)

google <- read.xlsx("timeSeriesHistoric.xlsx",sheet = "Google",colNames=T,rowNames=T)
google <- as.xts(google)

msciworld <- read.xlsx("timeSeriesHistoric.xlsx",sheet = "MSCI World",colNames=T,rowNames=T)
msciworld <- as.xts(msciworld)

regr.amazon <- summary(lm(amazon[,8] ~ msciworld[,6]))
regr.baidu  <- summary(lm(baidu[,8] ~ msciworld[,6]))
regr.disney <- summary(lm(disney[,8] ~ msciworld[,6]))
regr.google <- summary(lm(google[,8] ~ msciworld[,6]))
regr.merck  <- summary(lm(merck[,8] ~ msciworld[,6]))

riskfree <- c((mean(FVXriskfree[,2],na.rm=T)),mean.geometric(FVXriskfree[,2],na.rm=T))
names(riskfree) <- c("Arithmetic mean","Geometric mean")

stocks <- read.xlsx("Calculations.xlsx",sheet = "Stocks",colNames=T,rowNames=T)
stocks <- as.matrix(stocks)

 # stocks <- matrix(c(mean(amazon[,5],na.rm=T),mean(baidu[,5],na.rm=T),mean(disney[,5],na.rm=T),mean(google[,5],na.rm=T),mean(merck[,5],na.rm=T),mean(msciworld[,3],na.rm=T),
 #                    mean.geometric(amazon[,5]),mean.geometric(baidu[,5]),mean.geometric(disney[,5]),mean.geometric(google[,5]),mean.geometric(merck[,5]),mean.geometric(msciworld[,3]),
 #                    sd(amazon[,5],na.rm=T),sd(baidu[,5],na.rm=T),sd(disney[,5],na.rm=T),sd(google[,5],na.rm=T),sd(merck[,5],na.rm=T),sd(msciworld[,3],na.rm=T),
 #                    mean.geometric(amazon[,8])-regr.amazon$coefficients[2,1]*(mean.geometric(msciworld[,6])),mean.geometric(baidu[,8])-regr.baidu$coefficients[2,1]*(mean.geometric(msciworld[,6])),mean.geometric(disney[,8])-regr.disney$coefficients[2,1]*(mean.geometric(msciworld[,6])),mean.geometric(google[,8])-regr.google$coefficients[2,1]*(mean.geometric(msciworld[,6])),mean.geometric(merck[,8])-regr.merck$coefficients[2,1]*(mean.geometric(msciworld[,6])),mean.geometric(msciworld[,6])-1*(mean.geometric(msciworld[,6])),
 #                    regr.amazon$coefficients[2,1],regr.baidu$coefficients[2,1],regr.disney$coefficients[2,1],regr.google$coefficients[2,1],regr.merck$coefficients[2,1],1,
 #                    min(amazon[,5],na.rm=T),min(baidu[,5],na.rm=T),min(disney[,5],na.rm=T),min(google[,5],na.rm=T),min(merck[,5],na.rm=T),min(msciworld[,3],na.rm=T),
 #                    max(amazon[,5],na.rm=T),max(baidu[,5],na.rm=T),max(disney[,5],na.rm=T),max(google[,5],na.rm=T),max(merck[,5],na.rm=T),max(msciworld[,3],na.rm=T),
 #                    (mean(amazon[,5],na.rm=T)-riskfree[1])/sd(amazon[,5],na.rm=T),(mean(baidu[,5],na.rm=T)-riskfree[1])/sd(baidu[,5],na.rm=T),(mean(disney[,5],na.rm=T)-riskfree[1])/sd(disney[,5],na.rm=T),(mean(google[,5],na.rm=T)-riskfree[1])/sd(google[,5],na.rm=T),(mean(merck[,5],na.rm=T)-riskfree[1])/sd(merck[,5],na.rm=T),(mean(msciworld[,3],na.rm=T)-riskfree[1])/sd(msciworld[,3],na.rm=T),
 #                    (mean(amazon[,5],na.rm=T)-riskfree[1])/regr.amazon$coefficients[2,1],(mean(baidu[,5],na.rm=T)-riskfree[1])/regr.baidu$coefficients[2,1],(mean(disney[,5],na.rm=T)-riskfree[1])/regr.disney$coefficients[2,1],(mean(google[,5],na.rm=T)-riskfree[1])/regr.google$coefficients[2,1],(mean(merck[,5],na.rm=T)-riskfree[1])/regr.merck$coefficients[2,1],(mean(msciworld[,3],na.rm=T)-riskfree[1])/1,
 #                    sd(amazon[,5],na.rm=T)*sqrt(52),sd(baidu[,5],na.rm=T)*sqrt(52),sd(disney[,5],na.rm=T)*sqrt(52),sd(google[,5],na.rm=T)*sqrt(52),sd(merck[,5],na.rm=T)*sqrt(52),sd(msciworld[,3],na.rm=T)*sqrt(52),
 #                    (mean(amazon[,5],na.rm=T)+1)^52-1,(mean(baidu[,5],na.rm=T)+1)^52-1,(mean(disney[,5],na.rm=T)+1)^52-1,(mean(google[,5],na.rm=T)+1)^52-1,(mean(merck[,5],na.rm=T)+1)^52-1,(mean(msciworld[,3],na.rm=T)+1)^52-1,
 #                    basicStats(amazon[,7],ci=0.95)[15,],basicStats(baidu[,7],ci=0.95)[15,],basicStats(disney[,7],ci=0.95)[15,],basicStats(google[,7],ci=0.95)[15,],basicStats(merck[,7],ci=0.95)[15,],basicStats(msciworld[,5],ci=0.95)[15,],
 #                    basicStats(amazon[,7],ci=0.95)[16,],basicStats(baidu[,7],ci=0.95)[16,],basicStats(disney[,7],ci=0.95)[16,],basicStats(google[,7],ci=0.95)[16,],basicStats(merck[,7],ci=0.95)[16,],basicStats(msciworld[,5],ci=0.95)[16,],
 #                    regr.amazon[["r.squared"]],regr.baidu[["r.squared"]],regr.disney[["r.squared"]],regr.google[["r.squared"]],regr.merck[["r.squared"]],""),
 #                 nrow=14,ncol=6,byrow=T)
 #                    
 # dimnames(stocks) = list(c("\u03BC","Geometric Mean","\u03C3","Geometric \u237A","\u03B2","Min","Max","Sharpe Ratio","Treynor Ratio","Annualized \u03C3","Annualized GM","Skewness","Kurtosis","R Squared"),c("Amazon","Baidu","Disney","Google","Merck","MSCI World"))

returnStocks <- merge.xts(amazon[,5],baidu[,5],disney[,5],google[,5],merck[,5],msciworld[,3],all=T,fill=NA,suffixes=c("Amazon","Baidu","Disney","Google","Merck","MSCIWorld"),retside=T,retclass="xts",tzone=NULL,drop=NULL,check.names=NULL)
names(returnStocks) <- c("Amazon","Baidu","Disney","Google","Merck","MSCIWorld")
Cov_returns <- cov(returnStocks,y=NULL,use="complete.obs",method="pearson")
Cor_returns <- cor(returnStocks,y=NULL,use="complete.obs",method = "pearson")
returnStocksNA <- as.timeSeries(returnStocks[-1,])

# for the plot, the selected numbers make up
# 1. Efficient Frontier 2. Minimum risk portfolio 
# 3. Tangency Portfolio  4. Risk/Return of each single asset
# 8. Sharpe Ratio

Spec <- portfolioSpec()
Spec@portfolio[["riskFreeRate"]]<-riskfree[1]

effFrontier <- portfolioFrontier(returnStocksNA,spec=Spec,constraints="LongOnly",include.mvl=T)
maxSR <- tangencyPortfolio(returnStocksNA,spec=Spec,constraints = "LongOnly")
minVar <- minvariancePortfolio(returnStocksNA,spec=Spec,constraints="LongOnly")


frontierweights <- getWeights(effFrontier)
risk_return <- frontierPoints(effFrontier,frontier = "both",return="mean",risk="Sigma")

Portfolio_returns <- Return.portfolio(returnStocks,weights=getWeights(maxSR),geometric =F,wealth.index = F,contribution=T)
Portfolio_returns$Portfolio_GR <- Return.portfolio(returnStocks,weights=getWeights(maxSR),geometric =T,wealth.index = F,contribution=T)
Portfolio_returns <- Portfolio_returns[,c(1,8,2,3,4,5,6,7)]
colnames(Portfolio_returns) <- c("Portfolio_AR","Portfolio_GR","Amazon","Baidu","Disney","Google","Merck","MSCI World")
regr.portfolio <- summary(lm((Portfolio_returns[,2]-FVXriskfree[,2])~msciworld[,6]))
regr.minVar <- summary(lm((Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1]-FVXriskfree[,2])~msciworld[,6]))

Portfolio <- matrix(c(maxSR@portfolio@portfolio[["targetReturn"]][["mean"]],minVar@portfolio@portfolio[["targetReturn"]][["mean"]],
                      mean.geometric(Portfolio_returns[,2]),mean.geometric(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1]),
                      maxSR@portfolio@portfolio[["targetRisk"]][["Sigma"]],minVar@portfolio@portfolio[["targetRisk"]][["Sigma"]],
                      mean.geometric(Portfolio_returns[,2]-FVXriskfree[,2])-mean.geometric(msciworld[,6])*regr.portfolio$coefficients[2,1],
                      mean.geometric(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1]-FVXriskfree[,2])-mean.geometric(msciworld[,6])*regr.minVar$coefficients[2,1],
                      regr.portfolio$coefficients[2,1],regr.minVar$coefficients[2,1],
                      min(Portfolio_returns[,1]),min(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =F,wealth.index = F,contribution=T)[,1]),
                      max(Portfolio_returns[,1]),max(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =F,wealth.index = F,contribution=T)[,1]),
                      mean(Portfolio_returns[,1]-FVXriskfree[,2])/maxSR@portfolio@portfolio[["targetRisk"]][["Sigma"]],mean(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =F,wealth.index = F,contribution=T)[,1]-FVXriskfree[,2])/minVar@portfolio@portfolio[["targetRisk"]][["Sigma"]],
                      mean(Portfolio_returns[,1]-FVXriskfree[,2])/regr.portfolio$coefficients[2,1],mean(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =F,wealth.index = F,contribution=T)[,1]-FVXriskfree[,2])/regr.minVar$coefficients[2,1],
                      maxSR@portfolio@portfolio[["targetRisk"]][["Sigma"]]*sqrt(52),minVar@portfolio@portfolio[["targetRisk"]][["Sigma"]]*sqrt(52),
                      (maxSR@portfolio@portfolio[["targetReturn"]][["mean"]]+1)^52-1,(minVar@portfolio@portfolio[["targetReturn"]][["mean"]]+1)^52-1,
                      basicStats(Portfolio[,2],ci=0.95)[15,],basicStats(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1],ci=0.95)[15,],
                      basicStats(Portfolio[,2],ci=0.95)[16,],basicStats(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1],ci=0.95)[16,],
                      regr.portfolio[["r.squared"]],regr.minVar[["r.squared"]]),
                    nrow=14,ncol=2,byrow=T)

dimnames(Portfolio) <- list(c("\u03BC","Geometric Mean","\u03C3","Geometric \u237A","\u03B2","Min","Max","Sharpe Ratio","Treynor Ratio","Annualized \u03C3","Annualized GM","Skewness","Kurtosis","R Squared"),c("Tangential","Minimum Variance"))

.pardefault <- par()

# MSCI Plot

par(mfrow = c(2,1),
    oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
par(bty="l")
chart.Histogram(msciworld[,5],breaks="scott",probability=T,p=0.99,main="Histogram MSCI",xlim=c(-0.07,0.07),ylim=c(0,30),xlab="",
                methods=c("add.normal"),colorset=c(rgb(0,0.6,0.6,0.6),"cyan",rgb(0,0,1,0.7)),border.col=rgb(0,0,1,0.5),xaxis=T,yaxis=T)
par(bty="n")
boxplot(coredata(msciworld[,5]),names="MSCI",ylim=c(-0.07,0.07),
        las=1,col=rgb(0,0,1,0.7),horizontal=T,xaxt="n")
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="HistBox MSCI.png",res=400)


qqnorm(as.numeric(msciworld[,5]),plot.it=T,main="Normal Q-Q Plot MSCI",ylab="Returns GM",cex.lab=1.3,mgp=c(2.5,0.6,0))
panel.first=grid(ny = NULL,nx = NA, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F)
qqline(msciworld[,5],distribution = qnorm,qtype=8,col=2,lwd=1)
legend(-2.5,0.04, bty="n",legend=c(paste("Skewness:",basicStats(msciworld[,5],ci=0.95)[15,]),paste("Kurtosis:    ",basicStats(msciworld[,5],ci=0.95)[16,])),
       col=1, pch=14,cex=1.0)
dev.print(png,width=2700,height=2700,units="px",filename="Normal Q-Q MSCI.png",res=500)

  
# Amazon plot

plot(as.timeSeries.xts(msciworld[,6]),as.timeSeries.xts(amazon[,8]),main="Regression slope (Amazon ~ MSCI World)",
    ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
    pch=20,type="p",col="darkblue",bty="l",ylim=c(0,0.05),xlim=c(0,0.02),xaxs="i",yaxs="i",
    mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(amazon[,8]~msciworld[,6]),col="red")
text(0.015,0.01,pos=3,offset=1.5,label=bquote(beta==.(round(stocks[5,"Amazon"],4))),col="red",cex=1)
text(0.015,0.01,pos=3,label=bquote(R^2==.(round(regr.amazon[["r.squared"]],4))),col="red",cex=1)
arrows(0.0001,0,x1=0.0001,y1=regr.amazon[["coefficients"]][1,1],length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(0.0005,0.5*regr.amazon[["coefficients"]][1,1],label=bquote(alpha[italic(J)]),col="orange")
dev.print(png,width=2700,height=1350,units="px",filename="Regression Amazon.png",res=350)


plot(c(stocks[3,1],stocks[3,6]),c(stocks[1,1],stocks[1,6]),
     panel.first=grid(nx = 18, ny = 10, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     main="Risk-Return plot (weekly AM)",las=1,type="p",xlab=(bquote("Risk"~sigma)),ylab=(bquote("Return"~mu)),bty="l",pch=6,cex.axis=0.85,col="red4",
     xlim=c(0,0.05),ylim=c(0,0.01),xaxs="i",yaxs="i",mgp=c(2.8,0.7,0),cex.lab=1.3)
text(stocks[3,c(1,6)],stocks[1,c(1,6)],label=colnames(stocks[,c(1,6)]),pos=2,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="R-R Amazon.png",res=400)

plot(stocks[5,c(1,6)],stocks[2,c(1,6)],panel.first=grid(nx = 18, ny = 8, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     xlim=c(0,1.8),ylim=c(0,0.008),xaxs="i",yaxs="i",bty="l",
     xlab=bquote(beta),ylab="Return",main="Security Market Line",pch=15,col="blue",
     mgp=c(2.8,0.6,0),las=1,cex.lab=1.2)
minor.tick(nx=5, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
abline(riskfree[1],stocks[2,6]-riskfree[1])
arrows(stocks[5,"Amazon"],riskfree[1]+stocks[5,"Amazon"]*(stocks[2,6]-riskfree[1]),x1=stocks[5,"Amazon"],y1=stocks[2,"Amazon"]-0.00005,length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(stocks[5,"Amazon"],(riskfree[1]+stocks[5,"Amazon"]*(stocks[2,6]-riskfree[1])+
                           (stocks[2,"Amazon"]-(riskfree[1]+stocks[5,"Amazon"]*(stocks[2,6]-riskfree[1])))*0.5-0.00005),label=bquote(alpha),col="orange",pos=4,cex=1.1)
text(stocks[5,"Amazon"],stocks[2,"Amazon"],label="Amazon",pos=2,cex=0.8)
text(stocks[5,6],stocks[2,6],label="MSCI World",pos=1,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="SML Amazon.png",res=400)


par(mfrow = c(2,1),
    oma = c(0,3.9,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
par(bty="l")
chart.Histogram(msciworld[,5],breaks="scott",probability=T,p=0.99,main="Histogram MSCI~Amazon",xlim=c(-0.1,0.1),ylim=c(0,30),xlab="",
                methods=c("add.normal"),colorset=c(rgb(0,0.6,0.6,0.6),"cyan",rgb(0,0,1,0.7)),border.col=rgb(0,0,1,0.5),xaxis=T,yaxis=T)
par(bty="l")
chart.Histogram(amazon[,7],breaks="fd",add=T,
                methods=c("add.normal"),colorset=c(rgb(1,0.3,0,0.4),"orange",rgb(1,0,0,0.7)),border.col=rgb(1,0,0,0.5),
                probability=T,p=0.99,note.lines=0,note.color="black")
legend(-0.09,25 , bty="n",legend=c("~MSCI", "~Amazon"),
       col=c(rgb(0,0.6,0.6),rgb(1,0.3,0)), pch=15,cex=0.9)
par(bty="n")
boxplot(coredata(merge.xts(amazon[,7],msciworld[,5])),names=c("Amazon","MSCI"),ylim=c(-0.1,0.1),
        las=1,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horizontal=T,xaxt="n")
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="HistBox Amazon.png",res=400)


qqnorm(as.numeric(amazon[,7]),plot.it=T,main="Normal Q-Q Plot Amazon",ylab="Returns GM",cex.lab=1.3,mgp=c(2.5,0.6,0))
panel.first=grid(ny = NULL,nx = NA, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F)
qqline(amazon[,7],distribution = qnorm,qtype=8,col=2,lwd=1)
legend(-2,0.15, bty="n",legend=c(paste("Skewness:",basicStats(amazon[,7],ci=0.95)[15,]),paste("Kurtosis:    ",basicStats(amazon[,7],ci=0.95)[16,])),
       col=1, pch=14,cex=1.0)
dev.print(png,width=2700,height=2700,units="px",filename="Normal Q-Q Amazon.png",res=500)


# Baidu Plot

plot(as.timeSeries.xts(msciworld[,6]),as.timeSeries.xts(baidu[,8]),main="Regression slope (Baidu ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="darkblue",bty="l",ylim=c(0,0.05),xlim=c(0,0.02),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(baidu[,8]~msciworld[,6]),col="red")
text(0.015,0.01,pos=3,offset=1.5,label=bquote(beta==.(round(stocks[5,"Baidu"],4))),col="red",cex=1)
text(0.015,0.01,pos=3,label=bquote(R^2==.(round(regr.baidu[["r.squared"]],4))),col="red",cex=1)
arrows(0.0001,0,x1=0.0001,y1=regr.baidu[["coefficients"]][1,1],length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(0.0005,0.5*regr.baidu[["coefficients"]][1,1],label=bquote(alpha[italic(J)]),col="orange")
dev.print(png,width=2700,height=1350,units="px",filename="Regression Baidu.png",res=350)

plot(c(stocks[3,"Baidu"],stocks[3,6]),c(stocks[1,"Baidu"],stocks[1,6]),
     panel.first=grid(nx = 20, ny = 10, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     main="Risk-Return plot (weekly)",las=1,type="p",xlab=(bquote("Risk"~sigma)),ylab=(bquote("Return"~mu)),bty="l",pch=6,cex.axis=0.85,col="red4",
     xlim=c(0,0.05),ylim=c(0,0.01),xaxs="i",yaxs="i",mgp=c(2.8,0.7,0),cex.lab=1.3)
text(stocks[3,c(2,6)],stocks[1,c(2,6)],label=colnames(stocks[,c(2,6)]),pos=2,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="R-R Baidu.png",res=400)

plot(stocks[5,c(2,6)],stocks[2,c(2,6)],panel.first=grid(nx = 18, ny = 8, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     xlim=c(0,1.8),ylim=c(0,0.008),xaxs="i",yaxs="i",bty="l",
     xlab=bquote(beta),ylab="Return",main="Security Market Line",pch=15,col="blue",
     mgp=c(2.8,0.6,0),las=1,cex.lab=1.2)
minor.tick(nx=5, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
abline(riskfree[1],stocks[2,6]-riskfree[1])
arrows(stocks[5,"Baidu"],riskfree[1]+stocks[5,"Baidu"]*(stocks[2,6]-riskfree[1]),x1=stocks[5,"Baidu"],y1=stocks[2,"Baidu"]-0.00005,length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(stocks[5,"Baidu"],(riskfree[1]+stocks[5,"Baidu"]*(stocks[2,6]-riskfree[1])+
                          0.5*(stocks[2,"Baidu"]-(riskfree[1]+stocks[5,"Baidu"]*(stocks[2,6]-riskfree[1])))-0.00005),label=bquote(alpha[italic(J)]),col="orange",pos=4,cex=1.1)
text(stocks[5,"Baidu"],stocks[2,"Baidu"],label="Baidu",pos=2,cex=0.8)
text(stocks[5,6],stocks[2,6],label="MSCI World",pos=1,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="SML Baidu.png",res=400)

par(mfrow = c(2,1),
    oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
par(bty="l")
chart.Histogram(msciworld[,5],breaks="Scott",probability=T,p=0.99,main="Histogram MSCI~Baidu",xlim=c(-0.15,0.15),ylim=c(0,30),xlab="",
                methods=c("add.normal"),colorset=c(rgb(0,0.6,0.6,0.6),"cyan",rgb(0,0,1,0.7)),border.col=rgb(0,0,1,0.5),xaxis=T,yaxis=T)
par(bty="l")
chart.Histogram(baidu[,7],breaks="FD",add=T,
                methods=c("add.normal"),colorset=c(rgb(1,0.3,0,0.4),"orange",rgb(1,0,0,0.7)),border.col=rgb(1,0,0,0.5),
                probability=T,p=0.99,note.lines=0,note.color="black")
legend(-0.14,25 , bty="n",legend=c("~MSCI", "~Baidu"),
       col=c(rgb(0,0.6,0.6),rgb(1,0.3,0)), pch=15,cex=0.9)
par(bty="n")
boxplot(coredata(merge.xts(baidu[,7],msciworld[,5])),names=c("Baidu","MSCI"),ylim=c(-0.15,0.15),
        las=1,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horizontal=T,xaxt="n")
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="HistBox Baidu.png",res=400)


qqnorm(as.numeric(baidu[,7]),plot.it=T,main="Normal Q-Q Plot Baidu",ylab="Returns GM",cex.lab=1.3,mgp=c(2.5,0.6,0))
panel.first=grid(ny = NULL,nx = NA, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F)
qqline(baidu[,7],distribution = qnorm,qtype=8,col=2,lwd=1)
legend(-2,0.15, bty="n",legend=c(paste("Skewness:",basicStats(baidu[,7],ci=0.95)[15,]),paste("Kurtosis:    ",basicStats(baidu[,7],ci=0.95)[16,])),
       col=1, pch=14,cex=1.0)
dev.print(png,width=2700,height=2700,units="px",filename="Normal Q-Q Baidu.png",res=500)


# Disney plot

plot(as.timeSeries.xts(msciworld[,6]),as.timeSeries.xts(disney[,8]),main="Regression slope (Disney ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="darkblue",bty="l",ylim=c(0,0.05),xlim=c(0,0.02),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(disney[,8]~msciworld[,6]),col="red")
text(0.015,0.01,pos=3,offset=-0.4,label=bquote(beta==.(round(stocks[5,"Disney"],4))),col="red",cex=1)
text(0.015,0.01,pos=3,offset=-1.5,label=bquote(R^2==.(round(regr.disney[["r.squared"]],4))),col="red",cex=1)
arrows(0.0001,0,x1=0.0001,y1=regr.disney[["coefficients"]][1,1],length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(0.0005,0.5*regr.disney[["coefficients"]][1,1],label=bquote(alpha[italic(J)]),offset=1.5,col="orange")
dev.print(png,width=2700,height=1350,units="px",filename="Regression Disney.png",res=350)


plot(c(stocks[3,"Disney"],stocks[3,6]),c(stocks[1,"Disney"],stocks[1,6]),
     panel.first=grid(nx = 20, ny = 10, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     main="Risk-Return plot (weekly)",las=1,type="p",xlab=(bquote("Risk"~sigma)),ylab=(bquote("Return"~mu)),bty="l",pch=6,cex.axis=0.85,col="red4",
     xlim=c(0,0.05),ylim=c(0,0.01),xaxs="i",yaxs="i",mgp=c(2.8,0.7,0),cex.lab=1.3)
text(stocks[3,c(3,6)],stocks[2,c(3,6)],label=colnames(stocks[,c(3,6)]),pos=2,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="R-R Disney.png",res=400)


plot(stocks[5,c(3,6)],stocks[2,c(3,6)],panel.first=grid(nx = 18, ny = 8, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     xlim=c(0,1.8),ylim=c(0,0.008),xaxs="i",yaxs="i",bty="l",
     xlab=bquote(beta),ylab="Return",main="Security Market Line",pch=15,col="blue",
     mgp=c(2.8,0.6,0),las=1,cex.lab=1.2)
minor.tick(nx=5, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
abline(riskfree[1],stocks[2,6]-riskfree[1])
arrows(stocks[5,"Disney"],riskfree[1]+stocks[5,"Disney"]*(stocks[2,6]-riskfree[1]),x1=stocks[5,"Disney"],y1=stocks[2,"Disney"]-0.00005,length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(stocks[5,"Disney"],(riskfree[1]+stocks[5,"Disney"]*(stocks[2,6]-riskfree[1])+
                          (stocks[2,"Disney"]-(riskfree[1]+stocks[5,"Disney"]*(stocks[2,6]-riskfree[1])))*0.5-0.00005),label=bquote(alpha[italic(J)]),col="orange",pos=4,cex=1.1)
text(stocks[5,"Disney"],stocks[2,"Disney"],label="Disney",pos=2,cex=0.8)
text(stocks[5,6],stocks[2,6],label="MSCI World",pos=1,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="SML Disney.png",res=400)


par(mfrow = c(2,1),
    oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
par(bty="l")
chart.Histogram(msciworld[,5],breaks="Scott",probability=T,p=0.99,main="Histogram MSCI~Disney",xlim=c(-0.10,0.10),ylim=c(0,30),xlab="",
                methods=c("add.normal"),colorset=c(rgb(0,0.6,0.6,0.6),"cyan",rgb(0,0,1,0.7)),border.col=rgb(0,0,1,0.5),xaxis=T,yaxis=T)
par(bty="l")
chart.Histogram(disney[,7],breaks="FD",add=T,
                methods=c("add.normal"),colorset=c(rgb(1,0.3,0,0.4),"orange",rgb(1,0,0,0.7)),border.col=rgb(1,0,0,0.5),
                probability=T,p=0.99,note.lines=0,note.color="black")
legend(-0.09,25 , bty="n",legend=c("~MSCI", "~Disney"),
       col=c(rgb(0,0.6,0.6),rgb(1,0.3,0)), pch=15,cex=0.9)
par(bty="n")
boxplot(coredata(merge.xts(disney[,7],msciworld[,5])),names=c("Disney","MSCI"),ylim=c(-0.1,0.1),
        las=1,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horizontal=T,xaxt="n")
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="HistBox Disney.png",res=400)

qqnorm(as.numeric(disney[,7]),plot.it=T,main="Normal Q-Q Plot Disney",ylab="Returns GM",cex.lab=1.3,mgp=c(2.5,0.6,0))
panel.first=grid(ny = NULL,nx = NA, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F)
qqline(disney[,7],distribution = qnorm,qtype=8,col=2,lwd=1)
legend(-2,0.09, bty="n",legend=c(paste("Skewness:",basicStats(disney[,7],ci=0.95)[15,]),paste("Kurtosis:    ",basicStats(disney[,7],ci=0.95)[16,])),
       col=1, pch=14,cex=1)
dev.print(png,width=2700,height=2700,units="px",filename="Normal Q-Q Disney.png",res=500)

# Google plot

plot(as.timeSeries.xts(msciworld[,6]),as.timeSeries.xts(google[,8]),main="Regression slope (Google ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="darkblue",bty="l",ylim=c(0,0.05),xlim=c(0,0.02),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(google[,8]~msciworld[,6]),col="red")
text(0.015,0.01,pos=3,offset=1.5,label=bquote(beta==.(round(stocks[5,"Google"],4))),col="red",cex=1)
text(0.015,0.01,pos=3,label=bquote(R^2==.(round(regr.google[["r.squared"]],4))),col="red",cex=1)
arrows(0.0001,0,x1=0.0001,y1=regr.google[["coefficients"]][1,1],length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(0.0005,0.5*regr.google[["coefficients"]][1,1],label=bquote(alpha[italic(J)]),col="orange")
dev.print(png,width=2700,height=1350,units="px",filename="Regression Google.png",res=350)

plot(c(stocks[3,"Google"],stocks[3,6]),c(stocks[1,"Google"],stocks[1,6]),
     panel.first=grid(nx = 18, ny = 10, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     main="Risk-Return plot (weekly)",las=1,type="p",xlab=(bquote("Risk"~sigma)),ylab=(bquote("Return"~mu)),bty="l",pch=6,cex.axis=0.85,col="red4",
     xlim=c(0,0.05),ylim=c(0,0.01),xaxs="i",yaxs="i",mgp=c(2.8,0.7,0),cex.lab=1.3)
text(stocks[3,c(4,6)],stocks[1,c(4,6)],label=colnames(stocks[,c(4,6)]),pos=2,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="R-R Google.png",res=400)

plot(stocks[5,c(4,6)],stocks[2,c(4,6)],panel.first=grid(nx = 18, ny = 8, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     xlim=c(0,1.8),ylim=c(0,0.008),xaxs="i",yaxs="i",bty="l",
     xlab=bquote(beta),ylab="Return",main="Security Market Line",pch=15,col="blue",
     mgp=c(2.8,0.6,0),las=1,cex.lab=1.2)
minor.tick(nx=5, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
abline(riskfree[1],stocks[2,6]-riskfree[1])
arrows(stocks[5,"Google"],riskfree[1]+stocks[5,"Google"]*(stocks[2,6]-riskfree[1]),x1=stocks[5,"Google"],y1=stocks[2,"Google"]-0.00005,length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(stocks[5,"Google"],(riskfree[1]+stocks[5,"Google"]*(stocks[2,6]-riskfree[1])+
                           (stocks[2,"Google"]-(riskfree[1]+stocks[5,"Google"]*(stocks[2,6]-riskfree[1])))*0.5-0.00005),label=bquote(alpha[italic(J)]),col="orange",pos=4,cex=1.1)
text(stocks[5,"Google"],stocks[1,"Google"],label="Google",pos=2,cex=0.8)
text(stocks[5,6],stocks[2,6],label="MSCI World",pos=1,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="SML Google.png",res=400)

par(mfrow = c(2,1),
    oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
par(bty="l")
chart.Histogram(msciworld[,5],breaks="Scott",probability=T,p=0.99,main="Histogram MSCI~Google",xlim=c(-0.1,0.1),ylim=c(0,30),xlab="",
                methods=c("add.normal"),colorset=c(rgb(0,0.6,0.6,0.6),"cyan",rgb(0,0,1,0.7)),border.col=rgb(0,0,1,0.5),xaxis=T,yaxis=T)
par(bty="l")
chart.Histogram(google[,7],breaks="FD",add=T,
                methods=c("add.normal"),colorset=c(rgb(1,0.3,0,0.4),"orange",rgb(1,0,0,0.7)),border.col=rgb(1,0,0,0.5),
                probability=T,p=0.99,note.lines=0,note.color="black")
legend(-0.09,25 , bty="n",legend=c("~MSCI", "~Google"),
       col=c(rgb(0,0.6,0.6),rgb(1,0.3,0)), pch=15,cex=0.9)
par(bty="n")
boxplot(coredata(merge.xts(google[,7],msciworld[,5])),names=c("Google","MSCI"),ylim=c(-0.1,0.1),
        las=1,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horizontal=T,xaxt="n")
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="HistBox Google.png",res=400)


qqnorm(as.numeric(google[,7]),plot.it=T,main="Normal Q-Q Plot Google",ylab="Returns GM",cex.lab=1.3,mgp=c(2.5,0.6,0))
panel.first=grid(ny = NULL,nx = NA, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F)
qqline(google[,7],distribution = qnorm,qtype=8,col=2,lwd=1)
legend(-2,0.15, bty="n",legend=c(paste("Skewness:",basicStats(google[,7],ci=0.95)[15,]),paste("Kurtosis:    ",basicStats(google[,7],ci=0.95)[16,])),
       col=1, pch=14,cex=1)
dev.print(png,width=2700,height=2700,units="px",filename="Normal Q-Q Google.png",res=500)


# Merck Plot

plot(as.timeSeries.xts(msciworld[,6]),as.timeSeries.xts(merck[,8]),main="Regression slope (Merck ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="darkblue",bty="l",ylim=c(0,0.05),xlim=c(0,0.02),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(merck[,8]~msciworld[,6]),col="red")
text(0.015,0.01,pos=3,offset=-0.5,label=bquote(beta==.(round(stocks[5,"Merck"],4))),col="red",cex=1)
text(0.015,0.01,pos=3,offset=-1.5,label=bquote(R^2==.(round(regr.merck[["r.squared"]],4))),col="red",cex=1)
arrows(0.0001,0,x1=0.0001,y1=regr.merck[["coefficients"]][1,1],length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(0.0005,2*regr.merck[["coefficients"]][1,1],label=bquote(alpha[italic(J)]),offset=1.5,col="orange")
dev.print(png,width=2700,height=1350,units="px",filename="Regression Merck.png",res=350)

plot(c(stocks[3,"Merck"],stocks[3,6]),c(stocks[1,"Merck"],stocks[1,6]),
     panel.first=grid(nx = 20, ny = 10, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     main="Risk-Return plot (weekly)",las=1,type="p",xlab=(bquote("Risk"~sigma)),ylab=(bquote("Return"~mu)),bty="l",pch=6,cex.axis=0.85,col="red4",
     xlim=c(0,0.05),ylim=c(0,0.01),xaxs="i",yaxs="i",mgp=c(2.8,0.7,0),cex.lab=1.3)
text(stocks[3,c(5,6)],stocks[1,c(5,6)],label=colnames(stocks[,c(5,6)]),pos=2,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="R-R Merck.png",res=400)

plot(stocks[5,c(5,6)],stocks[2,c(5,6)],panel.first=grid(nx = 18, ny = 8, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     xlim=c(0,1.8),ylim=c(0,0.008),xaxs="i",yaxs="i",bty="l",
     xlab=bquote(beta),ylab="Return",main="Security Market Line",pch=15,col="blue",
     mgp=c(2.8,0.6,0),las=1,cex.lab=1.2)
minor.tick(nx=5, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
abline(riskfree[1],stocks[2,6]-riskfree[1])
arrows(stocks[5,"Merck"],riskfree[1]+stocks[5,"Merck"]*(stocks[2,6]-riskfree[1]),x1=stocks[5,"Merck"],y1=stocks[2,"Merck"]-0.00005,length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(stocks[5,"Merck"],(riskfree[1]+stocks[5,"Merck"]*(stocks[2,6]-riskfree[1])+
                           (stocks[2,"Merck"]-(riskfree[1]+stocks[5,"Merck"]*(stocks[2,6]-riskfree[1])))*0.5-0.00005),label=bquote(alpha[italic(J)]),col="orange",pos=4,cex=1.1)
text(stocks[5,"Merck"],stocks[2,"Merck"],label="Merck",pos=2,cex=0.8)
text(stocks[5,6],stocks[2,6],label="MSCI World",pos=1,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="SML Merck.png",res=400)

par(mfrow = c(2,1),
    oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
par(bty="l")
chart.Histogram(msciworld[,5],breaks="Scott",probability=T,p=0.99,main="Histogram MSCI~Merck",xlim=c(-0.07,0.07),ylim=c(0,30),xlab="",
                methods=c("add.normal"),colorset=c(rgb(0,0.6,0.6,0.6),"cyan",rgb(0,0,1,0.7)),border.col=rgb(0,0,1,0.5),xaxis=T,yaxis=T)
par(bty="l")
chart.Histogram(merck[,7],breaks="FD",add=T,
                methods=c("add.normal"),colorset=c(rgb(1,0.3,0,0.4),"orange",rgb(1,0,0,0.7)),border.col=rgb(1,0,0,0.5),
                probability=T,p=0.99,note.lines=0,note.color="black")
legend(-0.06,25 , bty="n",legend=c("~MSCI", "~Merck"),
       col=c(rgb(0,0.6,0.6),rgb(1,0.3,0)), pch=15,cex=0.9)
par(bty="n")
boxplot(coredata(merge.xts(merck[,7],msciworld[,5])),names=c("Merck","MSCI"),ylim=c(-0.07,0.07),
        las=1,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horizontal=T,xaxt="n")
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="HistBox Merck.png",res=400)

qqnorm(as.numeric(merck[,7]),plot.it=T,main="Normal Q-Q Plot Merck",ylab="Returns GM",cex.lab=1.3,mgp=c(2.5,0.6,0))
panel.first=grid(ny = NULL,nx = NA, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F)
qqline(merck[,7],distribution = qnorm,qtype=8,col=2,lwd=1)
legend(-2,0.08, bty="n",legend=c(paste("Skewness:",basicStats(merck[,7],ci=0.95)[15,]),paste("Kurtosis:    ",basicStats(merck[,7],ci=0.95)[16,])),
       col=1, pch=14,cex=1)
dev.print(png,width=2700,height=2700,units="px",filename="Normal Q-Q Merck.png",res=500)

# Minimum Variance Portfolio plot

par(oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
weightsPie(minVar,labels=T,box=F,col=color,cex=2)
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="Weightpie minVar.png",res=350)

plot(as.timeSeries.xts(msciworld[,6]),as.timeSeries.xts(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1]-FVXriskfree[,2]),main="Regression slope (MinVar Portfolio ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="darkblue",bty="l",ylim=c(0,0.025),xlim=c(0,0.025),xaxs="i",yaxs="i",
     mgp=c(2.5,0.6,0),las=1,cex.lab=1.3)
abline(lm((Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1]-FVXriskfree[,2])~msciworld[,6]),col="red")
text(0.018,0.01,pos=3,offset=1.5,label=bquote(beta==.(round(regr.minVar$coefficients[2,1],4))),col="red",cex=1)
text(0.018,0.01,pos=3,label=bquote(R^2==.(round(regr.minVar[["r.squared"]],4))),col="red",cex=1)
arrows(0.0001,0,x1=0.0001,y1=regr.minVar[["coefficients"]][1,1],length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(0.0005,0.5*regr.minVar[["coefficients"]][1,1],label=bquote(alpha[italic(J)]),col="orange")
dev.print(png,width=2700,height=1350,units="px",filename="Regression minVar.png",res=350)

plot(c(Portfolio[3,2],stocks[3,6]),c(Portfolio[1,2],stocks[1,6]),
     panel.first=grid(nx = 20, ny = 10, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F),
     main="Risk-Return plot (weekly)",las=1,type="p",xlab=(bquote("Risk"~sigma)),ylab=(bquote("Return"~mu)),bty="l",pch=6,cex.axis=0.85,col="red4",
     xlim=c(0,0.04),ylim=c(0,0.004),xaxs="i",yaxs="i",mgp=c(2.8,0.7,0),cex.lab=1.3)
text(Portfolio[3,2]+0.0025,Portfolio[1,2],label="MinVar Portfolio",pos=3,cex=0.8)
text(stocks[3,6]-0.0025,stocks[1,6],label="MSCI World",pos=1,cex=0.8)
dev.print(png,width=2700,height=1350,units="px",filename="R-R minVar.png",res=400)


par(mfrow = c(2,1),
    oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
par(bty="l")
chart.Histogram(msciworld[,5],breaks="Scott",probability=T,p=0.99,main="Histogram MSCI~ MinVar Portfolio",xlim=c(-0.1,0.1),ylim=c(0,40),xlab="",
                methods=c("add.normal"),colorset=c(rgb(0,0.6,0.6,0.6),"cyan",rgb(0,0,1,0.7)),border.col=rgb(0,0,1,0.5),xaxis=T,yaxis=T)
chart.Histogram(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1],
                breaks="Scott",add=T,methods=c("add.normal"),colorset=c(rgb(1,0.3,0,0.4),"orange",rgb(1,0,0,0.7)),border.col=rgb(1,0,0,0.5),
                probability=T,p=0.99,note.lines=0,note.color="black")
legend(-0.1,25 , bty="n",legend=c("~MSCI", "~MinVar"),
       col=c(rgb(0,0.6,0.6),rgb(1,0.3,0)), pch=15,cex=0.9)
par(bty="n")
boxplot(coredata(merge.xts(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1],msciworld[,5])),
        names=c("MinVar","MSCI"),ylim=c(-0.1,0.1),las=1,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horizontal=T,xaxt="n")
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="HistBox minVar.png",res=400)

qqnorm(as.numeric(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1]),plot.it=T,main="Normal Q-Q Plot MinVar",ylab="Returns GM",cex.lab=1.3,mgp=c(2.5,0.6,0))
panel.first=grid(ny = NULL,nx = NA, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F)
qqline(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1],distribution = qnorm,qtype=8,col=2,lwd=1)
legend(-2.5,0.04, bty="n",legend=c(paste("Skewness:",basicStats(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1],ci=0.95)[15,]),
                                 paste("Kurtosis:    ",basicStats(Return.portfolio(returnStocks,weights=getWeights(minVar),geometric =T,wealth.index = F,contribution=T)[,1],ci=0.95)[16,])),
       col=1, pch=14,cex=1)
dev.print(png,width=2700,height=2700,units="px",filename="Normal Q-Q minVar.png",res=500)

# Tangential Portfolio plot

color <- c("yellow","blue","black","green","cyan",118)

par(oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
weightsPie(maxSR,labels=T,box=F,col=color)
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="Weightpie Portfolio.png",res=350)

plot(as.timeSeries.xts(msciworld[,6]),as.timeSeries.xts(Portfolio_returns[,2]-FVXriskfree[,2]),main="Regression slope (Portfolio ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="darkblue",bty="l",ylim=c(0,0.05),xlim=c(0,0.02),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm((Portfolio_returns[,2]-FVXriskfree[,2])~msciworld[,6]),col="red")
text(0.015,0.01,pos=3,offset=1.5,label=bquote(beta==.(round(regr.portfolio$coefficients[2,1],4))),col="red",cex=1)
text(0.015,0.01,pos=3,label=bquote(R^2==.(round(regr.portfolio[["r.squared"]],4))),col="red",cex=1)
arrows(0.0001,0,x1=0.0001,y1=regr.portfolio[["coefficients"]][1,1],length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(0.0005,0.5*regr.portfolio[["coefficients"]][1,1],label=bquote(alpha[italic(J)]),col="orange")
dev.print(png,width=2700,height=1350,units="px",filename="Regression Portfolio.png",res=350)

plot(stocks[1,]~stocks[3,],
     xlim=c(0,0.05),ylim=c(0,0.008),xaxs="i",yaxs="i",
     type="p",bty="l",main="Capital Market Line",xlab=expression(paste("Target Risk ",sigma)),ylab=expression(paste("Target ",mu)),las={1},
     col=color,pch=19,cex=1,cex.lab=1.3,par(mgp=c(2.7,0.6,0)))
grid(nx = 20, ny = 8, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = F)
minor.tick(nx=4, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
plot(maxSR,c(3))
text(stocks[1,c(1,3:6)]~stocks[3,c(1,3:6)], labels = colnames(stocks[1:2,c(1,3:6)]), pos = 4,cex=0.7)
text(stocks[1,2]~stocks[3,2], labels = "Baidu", pos = 2,cex=0.7)
text(Portfolio[3,1],
     Portfolio[1,1],
     labels="Tangency point",pos=2,cex=0.7)
points(Portfolio[8,2],
       Portfolio[7,2],pch=18,col="orangered1")
text(Portfolio[3,2],
     Portfolio[1,2],
     labels="MVP",pos=2,cex=0.7,col="red")
dev.print(png,width=2700,height=1350,units="px",filename="CML Portfolio.png",res=400)

plot(c(stocks[5,6],Portfolio[5,1]),
     c(stocks[2,6],Portfolio[2,1]),panel.first=c(abline(riskfree[1],stocks[2,6]-riskfree[1]),
      grid(nx = 18, ny = 8, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = F)),
     type="p",main="Security Market Line",xlab=bquote(beta),ylab="Return",cex.lab=1.2,
     xlim=c(0,1.8),ylim=c(0,0.008),xaxs="i",yaxs="i",las=1,pch=10,col="blue",bty="l",cex=1.2,mgp=c(2.8,0.6,0),
     width=4,height=3)
minor.tick(nx=5, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
text(stocks[5,6],stocks[1,6],label="MSCI World",pos=1,cex=0.8)
text(Portfolio[5,1],Portfolio[2,1],label="Portfolio",pos=2,cex=0.8)
arrows(Portfolio[5,1],riskfree[1]+Portfolio[5,1]*(stocks[2,6]-riskfree[1]),x1=Portfolio[5,1],y1=Portfolio[2,1]-0.00005,length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(Portfolio[5,1],(riskfree[1]+Portfolio[5,1]*(stocks[2,6]-riskfree[1])+
    (Portfolio[2,1]-(riskfree[1]+Portfolio[5,1]*(stocks[2,6]-riskfree[1])))*0.5-0.00005),label=bquote(alpha[italic(J)]),col="orange",pos=4,cex=1.1)
dev.print(png,width=2700,height=1350,units="px",filename="SML Portfolio.png",res=400)

par(mfrow = c(2,1),
    oma = c(0,3.5,0.5,0) + 0.1,
    mar = c(0,0,1.5,1) + 0.1)
par(bty="l")
chart.Histogram(msciworld[,5],breaks="scott",probability=T,p=0.99,main="Histogram MSCI~Portfolio",xlim=c(-0.1,0.1),ylim=c(0,30),xlab="",
                methods=c("add.normal"),colorset=c(rgb(0,0.6,0.6,0.6),"cyan",rgb(0,0,1,0.7)),border.col=rgb(0,0,1,0.5),xaxis=T,yaxis=T)
chart.Histogram(Portfolio_returns[,2],breaks="FD",add=T,methods=c("add.normal"),
                colorset=c(rgb(1,0.3,0,0.4),"orange",rgb(1,0,0,0.7)),border.col=rgb(1,0,0,0.5),
                probability=T,p=0.99,note.lines=0,note.color="black")
legend(-0.1,25 , bty="n",legend=c("~MSCI", "~Portfolio"),
       col=c(rgb(0,0.6,0.6),rgb(1,0.3,0)), pch=15,cex=0.9)
par(bty="n")
boxplot(coredata(merge.xts(Portfolio_returns[,2],msciworld[,5])),
        names=c("Portfolio","MSCI"),ylim=c(-0.1,0.1),las=1,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horizontal=T,xaxt="n")
par(.pardefault)
dev.print(png,width=2700,height=1350,units="px",filename="HistBox Portfolio.png",res=400)


qqnorm(as.numeric(Portfolio_returns[,2]),plot.it=T,main="Normal Q-Q Plot Portfolio",ylab="Returns GM",cex.lab=1.3,mgp=c(2.5,0.6,0))
panel.first=grid(ny = NULL,nx = NA, col = "gray79", lty = "dotted",lwd = par("lwd"), equilogs = F)
qqline(Portfolio_returns[,2],distribution = qnorm,qtype=8,col=2,lwd=1)
legend(-2.3,0.11, bty="n",legend=c(paste("Skewness:",basicStats(Portfolio_returns[,2],ci=0.95)[15,]),
                                 paste("Kurtosis:    ",basicStats(Portfolio_returns[,2],ci=0.95)[16,])),
       col=1, pch=14,cex=1)
dev.print(png,width=2700,height=2700,units="px",filename="Normal Q-Q Portfolio.png",res=500)

# Fama French

FamaFrench <- read.xlsx("timeSeriesHistoric.xlsx",sheet = "FamaFrench",colNames=T,rowNames=T)
FamaFrench <- as.xts(FamaFrench)

regr.ff.amazon <- summary(lm(amazon[,6]~FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]))
regr.ff.baidu <- summary(lm(baidu[,6]~FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]))
regr.ff.disney <- summary(lm(disney[,6]~FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]))
regr.ff.google <- summary(lm(google[,6]~FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]))
regr.ff.merck <- summary(lm(merck[,6]~FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]))
regr.ff.portfolio <- summary(lm((Portfolio_returns[,1]-FVXriskfree[,2])~FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]))

# Amazon FF Regression plot

plot(as.timeSeries.xts(FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]),as.timeSeries.xts(amazon[,6]),main="Fama French Regression slope (Amazon ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="green",bty="l",xlim=c(-0.06,0.06),ylim=c(-0.15,0.15),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(amazon[,6]~msciworld[,4]),col="red")
text(0.042,-0.12,pos=3,offset=3.5,label=bquote(beta==.(round(regr.ff.amazon$coefficients[2,1],4))),col="red",cex=1)
text(0.042,-0.12,pos=3,offset=2.5,label=bquote(beta[SMB]==.(round(regr.ff.amazon$coefficients[3,1],4))),col="red",cex=1)
text(0.042,-0.12,pos=3,offset=1.5,label=bquote(beta[HML]==.(round(regr.ff.amazon$coefficients[4,1],4))),col="red",cex=1)
text(0.042,-0.12,pos=3,label=bquote(R^2==.(round(regr.ff.amazon[["r.squared"]],4))),col="red",cex=1)
par(new=T)
plot(as.timeSeries.xts(msciworld[,4]),as.timeSeries.xts(amazon[,6]),col="darkblue",type="p",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,bty="l",xlim=c(-0.06,0.06),ylim=c(-0.15,0.15),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
legend(-0.055,0.14 , bty="n",legend=c("~MSCI", "~Fama-French"),
       col=c("darkblue", "green"), pch=16,cex=0.9)
dev.print(png,width=2700,height=1350,units="px",filename="FF Regression Amazon.png",res=350)


# Baidu FF Regression plot 

plot(as.timeSeries.xts(FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]),as.timeSeries.xts(baidu[,6]),main="Fama French Regression slope (Baidu ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="green",bty="l",xlim=c(-0.06,0.06),ylim=c(-0.15,0.15),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(baidu[,6]~msciworld[,4]),col="red")
text(0.042,-0.12,pos=3,offset=3.5,label=bquote(beta==.(round(regr.ff.baidu$coefficients[2,1],4))),col="red",cex=1)
text(0.042,-0.12,pos=3,offset=2.5,label=bquote(beta[SMB]==.(round(regr.ff.baidu$coefficients[3,1],4))),col="red",cex=1)
text(0.042,-0.12,pos=3,offset=1.5,label=bquote(beta[HML]==.(round(regr.ff.baidu$coefficients[4,1],4))),col="red",cex=1)
text(0.042,-0.12,pos=3,label=bquote(R^2==.(round(regr.ff.baidu[["r.squared"]],4))),col="red",cex=1)
par(new=T)
plot(as.timeSeries.xts(msciworld[,4]),as.timeSeries.xts(baidu[,6]),col="darkblue",type="p",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,bty="l",xlim=c(-0.06,0.06),ylim=c(-0.15,0.15),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
legend(-0.055,0.14 , bty="n",legend=c("~MSCI", "~Fama-French"),
       col=c("darkblue", "green"), pch=16,cex=0.9)
dev.print(png,width=2700,height=1350,units="px",filename="FF Regression Baidu.png",res=350)


# Disney FF Regression Plot

plot(as.timeSeries.xts(FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]),as.timeSeries.xts(disney[,6]),main="Fama French Regression slope (Disney ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="green",bty="l",xlim=c(-0.06,0.06),ylim=c(-0.1,0.1),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(disney[,6]~msciworld[,4]),col="red")
text(0.042,-0.08,pos=3,offset=3.5,label=bquote(beta==.(round(regr.ff.disney$coefficients[2,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,offset=2.5,label=bquote(beta[SMB]==.(round(regr.ff.disney$coefficients[3,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,offset=1.5,label=bquote(beta[HML]==.(round(regr.ff.disney$coefficients[4,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,label=bquote(R^2==.(round(regr.ff.disney[["r.squared"]],4))),col="red",cex=1)
par(new=T)
plot(as.timeSeries.xts(msciworld[,4]),as.timeSeries.xts(disney[,6]),col="darkblue",type="p",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,bty="l",xlim=c(-0.06,0.06),ylim=c(-0.1,0.1),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
legend(-0.055,0.09 , bty="n",legend=c("~MSCI", "~Fama-French"),
       col=c("darkblue", "green"), pch=16,cex=0.9)
dev.print(png,width=2700,height=1350,units="px",filename="FF Regression Disney.png",res=350)

# Google FF Regression plot

plot(as.timeSeries.xts(FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]),as.timeSeries.xts(google[,6]),main="Fama French Regression slope (Google ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="green",bty="l",xlim=c(-0.06,0.06),ylim=c(-0.1,0.1),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(google[,6]~msciworld[,4]),col="red")
text(0.042,-0.08,pos=3,offset=3.5,label=bquote(beta==.(round(regr.ff.google$coefficients[2,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,offset=2.5,label=bquote(beta[SMB]==.(round(regr.ff.google$coefficients[3,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,offset=1.5,label=bquote(beta[HML]==.(round(regr.ff.google$coefficients[4,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,label=bquote(R^2==.(round(regr.ff.google[["r.squared"]],4))),col="red",cex=1)
par(new=T)
plot(as.timeSeries.xts(msciworld[,4]),as.timeSeries.xts(google[,6]),col="darkblue",type="p",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,bty="l",xlim=c(-0.06,0.06),ylim=c(-0.1,0.1),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
legend(-0.055,0.09 , bty="n",legend=c("~MSCI", "~Fama-French"),
       col=c("darkblue", "green"), pch=16,cex=0.9)
dev.print(png,width=2700,height=1350,units="px",filename="FF Regression Google.png",res=350)


# Merck FF Regression Plot

plot(as.timeSeries.xts(FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]),as.timeSeries.xts(merck[,6]),main="Fama French Regression slope (Merck ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="green",bty="l",xlim=c(-0.06,0.06),ylim=c(-0.06,0.06),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm(merck[,6]~msciworld[,4]),col="red")
text(0.042,-0.05,pos=3,offset=3.5,label=bquote(beta==.(round(regr.ff.merck$coefficients[2,1],4))),col="red",cex=1)
text(0.042,-0.05,pos=3,offset=2.5,label=bquote(beta[SMB]==.(round(regr.ff.merck$coefficients[3,1],4))),col="red",cex=1)
text(0.042,-0.05,pos=3,offset=1.5,label=bquote(beta[HML]==.(round(regr.ff.merck$coefficients[4,1],4))),col="red",cex=1)
text(0.042,-0.05,pos=3,label=bquote(R^2==.(round(regr.ff.merck[["r.squared"]],4))),col="red",cex=1)
par(new=T)
plot(as.timeSeries.xts(msciworld[,4]),as.timeSeries.xts(merck[,6]),col="darkblue",type="p",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,bty="l",xlim=c(-0.06,0.06),ylim=c(-0.06,0.06),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
legend(-0.055,0.05 , bty="n",legend=c("~MSCI", "~Fama-French"),
       col=c("darkblue", "green"), pch=16,cex=0.9)
dev.print(png,width=2700,height=1350,units="px",filename="FF Regression Merck.png",res=350)

# Portfolio FF Regression plot

plot(as.timeSeries.xts(FamaFrench[,1]+FamaFrench[,2]+FamaFrench[,3]),as.timeSeries.xts(Portfolio_returns[,1]-FVXriskfree[,2]),main="Fama French Regression slope (Portfolio ~ MSCI World)",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,type="p",col="green",bty="l",xlim=c(-0.06,0.06),ylim=c(-0.1,0.1),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
abline(lm((Portfolio_returns[,1]-FVXriskfree[,2])~msciworld[,4]),col="red")
text(0.042,-0.08,pos=3,offset=3.5,label=bquote(beta==.(round(regr.ff.portfolio$coefficients[2,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,offset=2.5,label=bquote(beta[SMB]==.(round(regr.ff.portfolio$coefficients[3,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,offset=1.5,label=bquote(beta[HML]==.(round(regr.ff.portfolio$coefficients[4,1],4))),col="red",cex=1)
text(0.042,-0.08,pos=3,label=bquote(R^2==.(round(regr.ff.portfolio[["r.squared"]],4))),col="red",cex=1)
par(new=T)
plot(as.timeSeries.xts(msciworld[,4]),as.timeSeries.xts(Portfolio_returns[,1]-FVXriskfree[,2]),col="darkblue",type="p",
     ylab=expression(paste(Delta," Equity premium")),xlab=expression(paste(Delta," Market premium")),
     pch=20,bty="l",xlim=c(-0.06,0.06),ylim=c(-0.1,0.1),xaxs="i",yaxs="i",
     mgp=c(2.5,0.8,0),las=1,cex.lab=1.3)
legend(-0.055,0.09 , bty="n",legend=c("~MSCI", "~Fama-French"),
       col=c("darkblue", "green"), pch=16,cex=0.9)
dev.print(png,width=2700,height=1350,units="px",filename="FF Regression Portfolio.png",res=350)

FFcol <- colMeans(FamaFrench[,1:3],na.rm=T)
FFfunction <- function(var1,var2)
{
  riskfree[1]+sum(var1*var2)
}

FFexpected <- matrix(c(FFfunction(regr.ff.amazon$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.baidu$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.disney$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.google$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.merck$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.portfolio$coefficients[2,1],FFcol[1]),
                       c(stocks[1,c(1:5)],Portfolio[1,1])-cbind(FFfunction(regr.ff.amazon$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.baidu$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.disney$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.google$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.merck$coefficients[2,1],FFcol[1]),FFfunction(regr.ff.portfolio$coefficients[2,1],FFcol[1])),
                       regr.amazon[["r.squared"]],regr.baidu[["r.squared"]],regr.disney[["r.squared"]],regr.google[["r.squared"]],regr.merck[["r.squared"]],regr.portfolio[["r.squared"]],
                       FFfunction(regr.ff.amazon$coefficients[2:4,1],FFcol),FFfunction(regr.ff.baidu$coefficients[2:4,1],FFcol),FFfunction(regr.ff.disney$coefficients[2:4,1],FFcol),FFfunction(regr.ff.google$coefficients[2:4,1],FFcol),FFfunction(regr.ff.merck$coefficients[2:4,1],FFcol),FFfunction(regr.ff.portfolio$coefficients[2:4,1],FFcol),
                       c(stocks[1,c(1:5)],Portfolio[1,1])-cbind(FFfunction(regr.ff.amazon$coefficients[2:4,1],FFcol),FFfunction(regr.ff.baidu$coefficients[2:4,1],FFcol),FFfunction(regr.ff.disney$coefficients[2:4,1],FFcol),FFfunction(regr.ff.google$coefficients[2:4,1],FFcol),FFfunction(regr.ff.merck$coefficients[2:4,1],FFcol),FFfunction(regr.ff.portfolio$coefficients[2:4,1],FFcol)),
                       regr.ff.amazon[["r.squared"]],regr.ff.baidu[["r.squared"]],regr.ff.disney[["r.squared"]],regr.ff.google[["r.squared"]],regr.ff.merck[["r.squared"]],regr.ff.portfolio[["r.squared"]]),
                     nrow=6,ncol=6,byrow=T,dimnames = list(c("E[r] CAPM","Alpha CAPM","CAPM R^2", "E[r] FF","Alpha FF","FF R^2"),c("Amazon","Baidu","Disney","Google","Merck","Portfolio")))

# Comparison with recent timeline (2018-05-18/2018-08-03)
# Daily geometric returns transformed to weekly returns

FVX.new <- read.xlsx("timeSeriesRecent.xlsx",sheet = "Riskfree",colNames=T,rowNames=T)
FVX.new <- as.xts(FVX.new)

msci.new <- read.xlsx("timeSeriesRecent.xlsx",sheet = "MSCI World",colNames=T,rowNames=T)
msci.new <- as.xts(msci.new)

amzn.new <- read.xlsx("timeSeriesRecent.xlsx",sheet = "Amazon",colNames=T,rowNames=T)
amzn.new <- as.xts(amzn.new)

bidu.new <- read.xlsx("timeSeriesRecent.xlsx",sheet = "Baidu",colNames=T,rowNames=T)
bidu.new <- as.xts(bidu.new)

mrk.new <- read.xlsx("timeSeriesRecent.xlsx",sheet = "Merck",colNames=T,rowNames=T)
mrk.new <- as.xts(mrk.new)

dis.new <- read.xlsx("timeSeriesRecent.xlsx",sheet = "Disney",colNames=T,rowNames=T)
dis.new <- as.xts(dis.new)

goog.new <- read.xlsx("timeSeriesRecent.xlsx",sheet = "Google",colNames=T,rowNames=T)
goog.new <- as.xts(goog.new)

Portfolio.new <- Return.portfolio(merge.xts(amzn.new[,5],bidu.new[,5],dis.new[,5],goog.new[,5],mrk.new[,5],msci.new[,3]),weights=getWeights(maxSR),geometric=T,method="simple",wealth.index = F,contribution=T,na.rm=T)
colnames(Portfolio.new) <- c("Portfolio_GR","Amazon","Baidu","Disney","Google","Merck","MSCI World")

# SML Plot with Jensen Alpha

plot(c(stocks[5,c(1:5)],Portfolio[5,1]),
     c((mean.geometric(amzn.new[,5])+1)^5-1,(mean.geometric(bidu.new[,5])+1)^5-1,(mean.geometric(dis.new[,5])+1)^5-1,(mean.geometric(goog.new[,5])+1)^5-1,(mean.geometric(mrk.new[,5])+1)^5-1,
       (mean.geometric(Portfolio.new[,1])+1)^5-1),
     panel.first=c(abline(a=(mean(FVX.new[,2])*5),b=((mean.geometric(msci.new[,3])+1)^5-1-(mean(FVX.new[,2])*5))),
                   grid(nx = 18, ny = 9, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = F)),
     type="p",main="Security Market Line",xlab=bquote(beta),ylab="Return",cex.lab=1.3,mgp=c(2.5,1,0),
     xlim=c(0,1.8),ylim=c(0,0.018),xaxs="i",yaxs="i",las=1,pch=10,col=color,bty="l",cex=1.2,mgp=c(2.9,0.6,0))
     # width=4,height=3)
minor.tick(nx=5, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
text(c(stocks[5,c(1,3,4,5)]),
     c((mean.geometric(amzn.new[,5])+1)^5-1,(mean.geometric(dis.new[,5])+1)^5-1,(mean.geometric(goog.new[,5])+1)^5-1,(mean.geometric(mrk.new[,5])+1)^5-1)+0.0005,
     label=c(colnames(Portfolio_returns[,c(-1,-2,-4,-8)])),pos=4,cex=0.8)
text(Portfolio[5,1],(mean.geometric(Portfolio.new[,1])+1)^5-1+0.0005,pos=2,label="Portfolio",cex=0.8)
text(stocks[5,2],0.002,label="Baidu",pos=4,cex=0.8)
text(stocks[5,6],
     (mean.geometric(msci.new[,3])+1)^5-1,label="MSCI",pos=3,cex=0.8)
arrows(c(stocks[5,c(1:5)],Portfolio[5,1]),
       mean(FVX.new[,2])*5+c(stocks[5,c(1:5)],Portfolio[5,1])%*%((mean.geometric(msci.new[,3])+1)^5-1-(mean(FVX.new[,2])*5)),
       x1=c(stocks[5,c(1:5)],Portfolio[5,1]),
       y1=c((mean.geometric(amzn.new[,5])+1)^5-1,-0.003,(mean.geometric(dis.new[,5])+1)^5-1,(mean.geometric(goog.new[,5])+1)^5-1,(mean.geometric(mrk.new[,5])+1)^5-1,
            (mean.geometric(Portfolio.new[,1])+1)^5-1-0.0005),
       length=0.1,angle=30,code=3,col="orange",lty="longdash",xpd=T)
text(0.6,0.005,label=bquote(alpha[italic(J)]),col="orange",cex=3)
dev.print(png,width=2700,height=1350,units="px",filename="Expected vs Realized Alpha.png",res=350)

ExpvsRealized <- matrix(c((mean(FVX.new[,2])*5+c(stocks[5,c(1:5)],Portfolio[5,1])%*%((mean.geometric(msci.new[,3])+1)^5-1-(mean(FVX.new[,2])*5))),
                           mean.geometric(cbind(amzn.new[,5],bidu.new[,5],dis.new[,5],goog.new[,5],mrk.new[,5],Portfolio.new[,1])+1)^5-1,
                          t(mean.geometric(cbind(amzn.new[,5],bidu.new[,5],dis.new[,5],goog.new[,5],mrk.new[,5],Portfolio.new[,1])+1)^5-1-mean(FVX.new[,2]))-c(stocks[5,c(1:5)],Portfolio[5,1])%*%((mean.geometric(msci.new[,3])+1)^5-1-(mean(FVX.new[,2]))*5)),
                        nrow=3,ncol=6,byrow=T,dimnames = list(c("Expected Return","Realized Return","Geometric Alpha"),c("Amazon","Baidu","Disney","Google","Merck","Portfolio")))

# write.xlsx(list("Stocks"=stocks,"Portfolio"=Portfolio,"ExpvsRealized"=ExpvsRealized,"FamaFrenchExpected"=FFexpected,"Covariance"=Cov_returns,"Correlations"=Cor_returns),file = "Calculations.xlsx",colNames=T,rowNames=T)
# write.xlsx(list("Riskfree"=as.data.frame(FVXriskfree),"Amazon"=as.data.frame(amazon),"Baidu"=as.data.frame(baidu),"Disney"=as.data.frame(disney),"Google"=as.data.frame(google),"Merck"=as.data.frame(merck),"MSCI World"=as.data.frame(msciworld),"Portfolio"=as.data.frame(Portfolio_returns),"FamaFrench"=as.data.frame(FamaFrench)),file="timeSeriesHistoric.xlsx",rowNames=T,colNames=T)
# write.xlsx(list("Riskfree"=as.data.frame(FVX.new),"Amazon"=as.data.frame(amzn.new),"Baidu"=as.data.frame(bidu.new),"Disney"=as.data.frame(dis.new),"Google"=as.data.frame(goog.new),"Merck"=as.data.frame(mrk.new),"MSCI World"=as.data.frame(msci.new),"Portfolio"=as.data.frame(Portfolio.new)),file="timeSeriesRecent.xlsx",rowNames=T,colNames=T)

