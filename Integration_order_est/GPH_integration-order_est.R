library(arfima)
library(forecast)
library(fracdiff) # for fracdiff, fdGPH
library(tseries)  # for garch

#===estimate and plot d
data <- read.csv("bond_spreadBP_usriskfree_06122017.csv",sep=",",dec=".",header=TRUE)
fi_names =cbind("Citi", "JPM", "BoA", "BNP", "DB", "BARC", "GS" ,"WF", "CA" ,"MS", "RBS" ,"SG" ,"Unicredit")

for (i in 1:13){
y = data[,i]
y.spec = spectrum(y, plot=FALSE)
lhs = log(y.spec$spec)
rhs = log(4*(sin(y.spec$freq/2))^2)

M = 100 
d = vector()
d.up = vector()
d.lo = vector()
for (m in 1:M){
  gph.reg = lm(lhs[1:m] ~ rhs[1:m])
  gph.sum = summary(gph.reg)
  d[m] = - gph.reg$coefficients[2]
  me = sqrt(pi^2/24)*qnorm(.975)/sqrt(m)
  d.up[m] = d[m] + me
  d.lo[m] = d[m] - me
}

#generate sequence for m
m = seq(1,M)  
file = paste("/Users/hienphamthu/Documents/PhD DISSERTATION/FCVAR/d_estimate_GPH/credit_dhat_",fi_names[i],".pdf",sep="")
pdf(file,width=7,height=5)

#plot estimated d dependent on m
par(mfrow=c(1,1))
plot(m,d,type="l",ylim=c(-.5, 3.5),ylab="Estimated d",cex.lab=1,cex.axis=1,cex.main=1.25,cex.sub=1)
lines(m,d.up,lty=2)
lines(m,d.lo,lty=2)
dev.off()
}
