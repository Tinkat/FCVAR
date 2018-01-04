
# clear console
rm(list=ls())
graphics.off()

#import urca library
install.packages("urca")
install.packages("tseries")
install.packages("fracdiff")
install.packages("arfima")
install.packages("forecast")
install.packages("jrvFinance")
#Load libraries
library("urca")
library("tseries")
library("fracdiff")
library("arfima")
library("forecast")
library("fracdiff") # for fracdiff, fdGPH
library("tseries")  # for garch
library("jrvFinance")

#============Data import
#asset_return = read.csv("Total data set_log return.csv", header = TRUE, sep = ";", dec = ".")

inputdata_all <- read.xls("CDS_Bond_5y_sync.xlsx",sheet = 1,header = TRUE,as.is = TRUE)
bond_spread <- read.csv("bond_spreadBP_usriskfree_06122017.csv",sep=",",dec=".",header=TRUE)

#============Date definition
date <- inputdata_all[,1]
#dates <-as.Date(format(date),"%d.%m.%Y")
all_name = colnames(inputdata_all[,2:length(inputdata_all[1,])])

#============Make plot
xdate <- as.Date(inputdata_all[,1],format = "%Y-%m-%d")
xdate_format <- format(xdate, "%d.%m.%Y")
starttime <- xdate[1]
endtime <-  xdate[length(xdate)]          #strptime("20111230", "%Y%m%d")
xrange <- c(starttime,endtime )
atx_sub <- seq( starttime,endtime, by=20)
#==================
#==================

make.plots <- function(x,y, main="", mainx="",mainy="", lag.max)
{
  ylab <- deparse(substitute(x))
  
  fit1 <- fdGPH(x)
  fit2 <- fdGPH(y)
  
  par(mfrow=c(1,3), mai = c(0.5, 0.6, 0.75, 0.2))
  plot(xdate, x, type="l", xlab="Date", ylab="Spread in basis point", col=2,lwd=3, ylim=c(-50,300) , main=main,cex.lab=1.5,cex.axis=1.5,cex.main=1.5,cex.sub=1.5)
  #mtext(paste0("d_CDS = ", round(fit1$d, 2), ", SE_CDS = ", round(fit1$sd.reg, 2)," d_bond = ", round(fit2$d, 2), ", SE_bond = ", round(fit2$sd.reg, 2)), cex =0.8)
  #mtext(paste0("d = ", round(fit2$d, 2), ", SE = ", round(fit2$sd.reg, 2)))
  lines(xdate, y, type="l", xlab="Date", col="blue", ylim=c(0,300) ,lwd=3, main=main, cex=1.5)
  Acf(x, lag.max=lag.max, main=paste0("Series: ", mainx), cex.lab=1.5,cex.axis=1.5,cex.main=2,cex.sub=1.5)
  Acf(y, lag.max=lag.max, main=paste0("Series: ", mainy), cex.lab=1.5,cex.axis=1.5,cex.main=2,cex.sub=1.5)
  axis(1, at=atx_sub, labels=format(atx_sub,  "%d.%m.%Y"), padj=0.5,cex.axis = 2)
}

fi_names =cbind("Citi", "JPM", "BoA", "BNP", "DB", "BARC", "GS" ,"WF", "CA" ,"MS", "RBS" ,"SG" ,"Unicredit")

for (n in 1:13){
  i=2*n+1
  data_all_cds <- as.numeric(inputdata_all[,i])
  data_all_credit <- as.numeric(bond_spread[,n])
  # file name
  file_name = paste(fi_names[n],"_V2.pdf",sep ="")
  file = paste("/Users/hienphamthu/Documents/PhD DISSERTATION/FCVAR/ACF_plot/ACF200",file_name, sep = "_")
  pdf(file,width=15,height=3.8)
  #colnames(data_all) <- all_name[(i-1)]
  make.plots(data_all_cds,data_all_credit, fi_names[n], "CDS spread","Bond spread",200)
  dev.off()
}





