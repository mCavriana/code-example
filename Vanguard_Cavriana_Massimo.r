#Cavriana Massimo Last Edit: 01/16/2015
### LEGEND and useful VARIABLES #########################################################################
# Please use it after the execution of the whole script
# all_sharpe  	 | FoF's Sharpe
# VaR.all     	 | FoF's VaR Values plus VaR for portfolios calculated for point 3.2
# print.2.3   	 | Weight of global min portfolio calculated for point 2.3. You can obtain further
#							   	information in the variables 'print.2.3_1', 'print.2.3_2', 'print.2.3.VaR';
#									 use 'print.2.3d_all' for all information about tangency portfolio
# print.p.10  	 | All information about point 2.4 (portfolio with Return = goal and with Short Sales)
# gmin.p.'v'		 | v can be 'noShort' or 'wShort', those are the global min portfolio calculated for
#									 the point 3.1; usage example: summary(gmin.p.noShort, rf) [rf used for sharpe value]
# ef.tangency.wS | Information about tangency portfolio computed for point 3.5
#									 usage: summary(ef.tangency.wS, rf) [rf used for Sharpe ratio]
# sharpe.conf 	 | Sharpe Value of Tangency portfolio (With Short Sales) and FoF's Sharpe Values
# conf.all    	 | All information (Sharpe, EstReturn and SD) about portfolios and FoF's
# conf.w.all		 | Comparison between efficient portfolios
# print.nS10  	 | All information about point 4.2 (portfolio with return = goal and without Short Sales)
#########################################################################################################
#loading packages and source files
library("zoo")
library("tseries")
library("PerformanceAnalytics")
source(file="http://faculty.washington.edu/ezivot/econ424/portfolio_noshorts.r")

## define some useful global variable
# risk free return
rf = 0.005/12
#VaR function
VaR = function (data, p, investment=1000000) {
	data = as.matrix(data)
	q = apply (data, 2, mean) + apply(data, 2, sd)*qnorm(p)
	VaRValue = investment * (exp(q) -1)
	VaRValue #print VarValue for every found
}
# funds' names array
fund.names = c("VINIX", "VEURX", "VEIEX", "VBLTX", "VBISX", "VPACX")

#Function to convert zooObj to DataFrameObj
zoo.to.data.frame <- function(x, index.name="Date") {
    stopifnot(is.zoo(x))
    xn <- if(is.null(dim(x))) deparse(substitute(x)) else colnames(x)
    setNames(data.frame(index(x), x, row.names=NULL), c(index.name,xn))
}

#download monthly adjusted closing price data of Vanguard fund from Dec 2005 to Sep 2014
VINIX.z = get.hist.quote(instrument="VINIX", start="2005-12-01", end="2014-09-30", quote="AdjClose", 
						provider="yahoo", compression="m", retclass="zoo")

VEURX.z = get.hist.quote(instrument="VEURX", start="2005-12-01", end="2014-09-30", quote="AdjClose", 
						provider="yahoo", compression="m", retclass="zoo")

VEIEX.z = get.hist.quote(instrument="VEIEX", start="2005-12-01", end="2014-09-30", quote="AdjClose", 
						provider="yahoo", compression="m", retclass="zoo")
						
VBLTX.z = get.hist.quote(instrument="VBLTX", start="2005-12-01", end="2014-09-30", quote="AdjClose", 
						provider="yahoo", compression="m", retclass="zoo")

VBISX.z = get.hist.quote(instrument="VBISX", start="2005-12-01", end="2014-09-30", quote="AdjClose", 
						provider="yahoo", compression="m", retclass="zoo")

VPACX.z = get.hist.quote(instrument="VPACX", start="2005-12-01", end="2014-09-30", quote="AdjClose", 
						provider="yahoo", compression="m", retclass="zoo")
						
index(VINIX.z) = as.yearmon(index(VINIX.z))
index(VEURX.z) = as.yearmon(index(VEURX.z))
index(VEIEX.z) = as.yearmon(index(VEIEX.z))
index(VBLTX.z) = as.yearmon(index(VBLTX.z))
index(VBISX.z) = as.yearmon(index(VBISX.z))
index(VPACX.z) = as.yearmon(index(VPACX.z))

# merging all data in a zoo object
all_funds.z = merge(VINIX.z, VEURX.z, VEIEX.z, VBLTX.z, VBISX.z, VPACX.z)
colnames(all_funds.z) = fund.names

# compute cc returns on the merged zoo object
all_funds.ret = diff(log(all_funds.z))
colnames(all_funds.ret) =  fund.names

# convert zoo data to matrix data 
all_funds.ret.mat = coredata(all_funds.ret)

# compute Sharpe Ratio
muhat = apply(all_funds.ret, 2, mean)
all_sigmahat = apply(all_funds.ret, 2, sd)
all_sharpe = (muhat - rf) / all_sigmahat
all_sharpe

### from here I'll use VINIX and VBISX data only, for more information
# about this choice, please read the attached document
#2.3

VINIX.ret = diff(log(VINIX.z))
VBISX.ret = diff(log(VBISX.z))
# calculate mu, sigma and sharpe for the two FoFs
rho = cor(VINIX.ret, VBISX.ret)[1]
vinix.mu = mean(VINIX.ret)
vinix.sigma = sd(VINIX.ret)
vinix.var = vinix.sigma^2
vinix.sharpe = (vinix.mu - rf)/ vinix.sigma

vbisx.mu = mean(VBISX.ret)
vbisx.sigma = sd(VBISX.ret)
vbisx.var = vbisx.sigma^2
vbisx.sharpe = (vbisx.mu -rf) / vbisx.sigma

cov12 = rho*vinix.sigma*vbisx.sigma

# portfolio frontier (c)
w.1=seq(-3 ,3,0.1)
w.2=1-w.1
mu.p = w.1*vinix.mu + w.2*vbisx.mu
sigma2.p = w.1^2 * vinix.var + w.2^2 * vbisx.var + 2*w.1*w.2*cov12
sigma.p = sqrt(sigma2.p)

plot(sigma.p, mu.p, type="b", pch=19, ylim=c(-0.002, 0.006), xlim=c(0, 0.065),
			xlab=expression(sigma[p]),ylab=expression(mu[p]))
points(x=vinix.sigma, y=vinix.mu, pch=19, col="blue")
text(x=vinix.sigma, y=vinix.mu, "VINIX", pos=1, col="blue")
points(x=vbisx.sigma, y=vbisx.mu, pch=19, col="blue")
text(x=vbisx.sigma, y=vbisx.mu, "VBISX", pos=1, col="blue")

# minimum variance portfolio a)
w.1.min = (vbisx.var - cov12)/(vinix.var + vbisx.var - 2*cov12)
w.2.min = 1 - w.1.min
mu.p.min = w.1.min*vinix.mu + w.2.min*vbisx.mu
sigma2.p.min = w.1.min^2 * vinix.var + w.2.min^2 * vbisx.var + 2*w.1.min*w.2.min*cov12
sigma.p.min = sqrt(sigma2.p.min)
text(x=sigma.p.min, y=mu.p.min, labels="global min", pos=4, col="green")
points(x=sigma.p.min, y=mu.p.min,pch=19, col="green")
print.2.3 = cbind(w.1.min, w.2.min)
print.2.3_1 = cbind(vinix.mu, vbisx.mu, mu.p.min)
print.2.3_2 = cbind(vinix.sigma, vbisx.sigma, sigma.p.min)

# compute VaR of minimum variance portfolio
inv = 1000000
VaR.p = (exp(mu.p.min + sigma.p.min * qnorm(0.05)) -1)* inv
VaR.p
# print VaR.p and weighted VaR
#cbind(VaR.p, w.1.min*((exp(vinix.mu + vinix.sigma*qnorm(0.05)) -1)*inv) + w.2.min*((exp(vbisx.mu + vbisx.sigma*qnorm(0.05)) -1)*inv))
vinix.VatR = (exp(vinix.mu + vinix.sigma * qnorm(0.05)) -1) * inv
vbisx.VatR = (exp(vbisx.mu + vbisx.sigma * qnorm(0.05)) -1) * inv
print2.3.VaR = cbind(VaR.p, vinix.VatR, vbisx.VatR)

#compute tangency portfolio
A = (vinix.mu - rf) * vbisx.var - (vbisx.mu - rf) * cov12
B = (vinix.mu - rf) * vbisx.var + (vbisx.mu - rf)* vinix.var - (vinix.mu - rf + vbisx.mu - rf) * cov12
w.1.t = A/B
w.2.t = 1 - w.1.t
mu.p.t = w.1.t*vinix.mu + w.2.t*vbisx.mu
sigma2.p.t = w.1.t^2 * vinix.var + w.2.t^2 * vbisx.var + 2*w.1.t*w.2.t*cov12
sigma.p.t = sqrt(sigma2.p.t)
#add tangency point to the old plot
points(x=sigma.p.t, y=mu.p.t,pch=19, col="yellow")
text(x=sigma.p.t, y=mu.p.t, labels="tangency", col="yellow", pos=2)
#plot tangency portfolio
#VINIX
w.1 = seq(from=0, to=2, by=0.1)
mu.p.1 = rf + w.1*(vinix.mu- rf)
sigma.p.1 = w.1*vinix.sigma
sharpe.1 = (vinix.mu - rf)/vinix.sigma
#VBISX
w.2 = seq(from=0, to=2, by=0.1)
mu.p.2 = rf + w.2*(vbisx.mu- rf)
sigma.p.2 = w.2*vbisx.sigma
sharpe.2 = (vbisx.mu - rf)/vbisx.sigma
#Efficient Portfolio of a risk free asset (T-bill)
w.p.t = seq(from=0, to=2.4, by=0.1)
mu.p.e = rf + w.p.t*(mu.p.t - rf)
sigma.p.e = w.p.t*sigma.p.t

plot(sigma.p, mu.p, type="b", pch=19,ylim=c(0.0049/12, 0.007), xlim=c(0, 0.065), 
			xlab=expression(sigma[p]),ylab=expression(mu[p]), col="red" )
text(x=sigma.p.min, y=mu.p.min, labels="global min", col="green", pos=4)
points(x=sigma.p.min, y=mu.p.min,pch=19, col="green")
points(sigma.p.1, mu.p.1, type="b",  pch=19)
points(sigma.p.2, mu.p.2, type="b",  pch=1)
text(x=0, y=rf, labels=expression(r[f]), pos=2)
points(sigma.p.e, mu.p.e, type="b", col="blue", pch=2)
points(x=sigma.p.t, y=mu.p.t,pch=19, col="blue")
text(x=sigma.p.t, y=mu.p.t, labels="tangency", col="blue", pos=2)
points(x=vinix.sigma, y=vinix.mu,pch=19, col="black")
text(x=vinix.sigma, y=vinix.mu, "VINIX", col="black", pos=1)
points(x=vbisx.sigma, y=vbisx.mu,pch=19, col="black")
text(x=vbisx.sigma, y=vbisx.mu, "VBISX", col="black", pos=2)
#print value of FoFs' weight, the first one stand for the tangency portfolio
print2.3dw = cbind(1.000000, w.1.t, w.2.t)
#print values for mu and sigma of tangency portfolio and the two FoFs
print.2.3d = cbind(mu.p.t, vinix.mu, vbisx.mu)
print.2.3d_2 = cbind(sigma.p.t, vinix.sigma, vbisx.sigma)
# calculate and print Sharpe values
sharpe.p.t = (mu.p.t - rf)/sigma.p.t
print.2.3d_3 = cbind(sharpe.p.t, sharpe.1, sharpe.2)
print.2.3d_all = matrix(nrow=4, ncol=3)
print.2.3d_all[1,] = print2.3dw
print.2.3d_all[2,] = print.2.3d
print.2.3d_all[3,] = print.2.3d_2
print.2.3d_all[4,] = print.2.3d_3
rownames(print.2.3d_all) = c("weight", "mu", "sd", "sharpe")
colnames(print.2.3d_all) = c("portfolio", "VINIX", "VBISX")

#2.4
goal = 0.1/12
#Calculate tangency portfolio's weight in order to reach a mu of 0.1/12 (goal)
w.p.t.10 = ((goal)/(mu.p.t -rf))-(rf/(mu.p.t-rf))
#Calculate VINIX and VBISX weights for this portfolio
w.p.t.vinix = w.p.t.10 * w.1.t
w.p.t.vbisx = w.p.t.10 * w.2.t
#Calculate risk free weight
w.p.rf.10 = 1 - w.p.t.10
#Calculate SD and VaR of this portfolio
sigma.p.10 = w.p.t.10 * sigma.p.t
VaR.p.10 = (exp(goal + sigma.p.10 * qnorm(0.05)) -1)* inv
#Print results
print.p.10 = cbind(w.p.t.10, w.p.t.vinix, w.p.t.vbisx, w.p.rf.10, sigma.p.10, VaR.p.10)
colnames(print.p.10) <- c("|tang. port. weight", "  |vinix weight", "  |vbisx weight", "  |risk free weight", "  |portfolio's SD", "  |portfolio's VaR|")
print.p.10

#3
cov.mat = cov(all_funds.ret)
#3.1 global min var portfolio, short sales not allowed
gmin.p.noShort <- globalMin.portfolio(muhat, cov.mat, shorts=FALSE)
summary(gmin.p.noShort, risk.free=rf)
#short sales allowed
gmin.p.wShort <- globalMin.portfolio(muhat, cov.mat, shorts=TRUE)
summary(gmin.p.wShort, risk.free=rf)

#3.2
#Compute and print VaR
VaR.gmin.p.noShort = (exp(gmin.p.noShort$er + gmin.p.noShort$sd * qnorm(0.05)) -1)* inv
VaR.gmin.p.wShort = (exp(gmin.p.wShort$er + gmin.p.wShort$sd * qnorm(0.05)) -1)* inv
VaR.gmin.p.noShort
VaR.gmin.p.wShort
VaR.all = VaR(all_funds.ret, 0.05)
VaR.all = cbind(VaR.all[1], VaR.all[2], VaR.all[3], VaR.all[4],
				VaR.all[5], VaR.all[6], VaR.gmin.p.noShort, VaR.gmin.p.wShort )
rownames(VaR.all) = c("VaR0.05 ")
colnames(VaR.all) = c("VINIX  ", "VEURX  ", "VEIEX  ", "VBLTX  ", "VBISX  ",
									"VPACX  ", "NoShortSales", "ShortSales")
VaR.all

#3.3
#Compute efficient portfolio frontier, short sales are allowed
ef.wShort <- efficient.frontier(muhat, cov.mat, nport=50)
ef.globMin.wShort <- globalMin.portfolio(muhat, cov.mat)
#e.p.wShort <- efficient.portfolio(muhat, cov.mat, target.return=max(muhat), shorts=TRUE)
#globMin.p.wShort <- globalMin.portfolio(muhat, cov.mat, shorts=TRUE)
#w.ep.wShort = seq(-0.5, 1.5, 0.1)
#w.globMin.p.wShort = 1 - w.ep.wShort
#w.ef = w.ep.wShort %o% e.p.wShort$weights + w.globMin.p.wShort %o% globMin.p.wShort$weights
#cov.ef = w.ef %*% cov.mat %*% t(w.ef)
#mu.p.ef.wShort = w.ep.wShort * e.p.wShort$er + w.globMin.p.wShort * globMin.p.wShort$er
#sigma2.ef.p.wShort = w.ep.wShort^2 * e.p.wShort$sd^2 + w.globMin.p.wShort^2 * globMin.p.wShort$sd^2 + 2*w.ep.wShort*w.globMin.p.wShort*cov.ef
#sigma.ef.p.wShort = sqrt(sigma2.ef.p.wShort)

#3.4
plot(ef.wShort$sd, ef.wShort$er, type="b", pch=19,ylim=c(0.0049/12, 0.008), xlim=c(0, 0.077), 
			xlab=expression(sigma[p]),ylab=expression(mu[p]), col="black" )
points(x=ef.globMin.wShort$sd, y=ef.globMin.wShort$er, pch=19, col="green")
text(x=ef.globMin.wShort$sd, y=ef.globMin.wShort$er,
		labels="global min", col="green", pos=2)
points(x=apply(all_funds.ret, 2, sd), y=apply(all_funds.ret, 2, mean), pch=19, col="black")
text(x=apply(all_funds.ret, 2, sd), y=apply(all_funds.ret, 2, mean),
		labels= fund.names, col="black", pos=4)

#3.5
ef.tangency.wS <- tangency.portfolio(muhat, cov.mat, rf)
#ef.tangency.wS
#3.6
ef.tangency.wS.sharpe = (ef.tangency.wS$er -rf) / ef.tangency.wS$sd
sharpe.conf = cbind(ef.tangency.wS.sharpe, all_sharpe[1], all_sharpe[2],
 all_sharpe[3], all_sharpe[4], all_sharpe[5], all_sharpe[6])
rownames(sharpe.conf) = "SharpeValue "
colnames(sharpe.conf) = (cbind(c("Tang.Port", eval(fund.names))))
sharpe.conf

#3.7
points(x=ef.tangency.wS$sd, y=ef.tangency.wS$er, pch=19, col="blue")
text(x=ef.tangency.wS$sd, y=ef.tangency.wS$er, col="blue", labels="tangency", pos=2)
points(x=0, y=rf, pch=19, col="black")
text(x=0, y=rf, col="black", label="RF", pos=2)
w.tan.wS = seq(from=0, to=3, by=0.1)
mu.tan.wS = rf + w.tan.wS*(ef.tangency.wS$er - rf)
sd.tan.wS = w.tan.wS *ef.tangency.wS$sd
points(sd.tan.wS, mu.tan.wS , type="b", col="blue", pch=16)

#4
ef.tangency.nS <- tangency.portfolio(muhat, cov.mat, rf, shorts=FALSE)
ef.tangency.nS
ef.tangency.nS.sharpe = (ef.tangency.nS$er -rf) / ef.tangency.nS$sd
sharpe.conf.nS = cbind(ef.tangency.nS.sharpe, all_sharpe[1], all_sharpe[2],
 all_sharpe[3], all_sharpe[4], all_sharpe[5], all_sharpe[6])
rownames(sharpe.conf.nS) = "SharpeValue "
colnames(sharpe.conf.nS) = (cbind(c("NoShort", eval(fund.names))))
sharpe.withShort = sharpe.conf[1]
#4.1
conf.all = matrix(nrow=3, ncol=8)
conf.all[1,] = c(sharpe.withShort, sharpe.conf.nS)
conf.all[2,] = c(ef.tangency.wS$er, ef.tangency.nS$er, muhat)
conf.all[3,] = c(ef.tangency.wS$sd, ef.tangency.nS$sd, all_sigmahat)
colnames(conf.all) = c("WithShort", "NoShort", eval(fund.names))
rownames(conf.all) = c("SharpeValue", "EstReturn", "StdDev")
conf.w.all=matrix(nrow=2, ncol=9)
conf.w.all[1,] = c(sharpe.withShort, ef.tangency.wS$er, ef.tangency.wS$sd, ef.tangency.wS$weights)
conf.w.all[2,] = c(sharpe.conf.nS[1] ,ef.tangency.nS$er, ef.tangency.nS$sd, ef.tangency.nS$weights)
conf.all
rownames(conf.w.all) = c("WithShort", "NoShort")
colnames(conf.w.all) = c("SharpeValue", "Port.EstRet", "Port.Est.SD", "VINIX Weight",
	"VEURX Weight", "VEIEX Weight", "VBLTX Weight", "VBISX Weight", "VPACX Weight")

#4.2
mult.w.tan.nS10 = (goal - rf) / (ef.tangency.nS$er - rf)
mu.tan.nS10 = rf + mult.w.tan.nS10*(ef.tangency.nS$er - rf) #Should be equal to 'goal'
sd.tan.nS10 = mult.w.tan.nS10 * (ef.tangency.nS$sd)
w.tan.nS10 = mult.w.tan.nS10*ef.tangency.nS$weights
w.rf.nS10 = 1-mult.w.tan.nS10

#VaR
VaR.nS10 = (exp(goal + sd.tan.nS10 * qnorm(0.05)) -1)* inv
print.nS10 = matrix(nrow=1 , ncol=9)
print.nS10[1,] = c(VaR.nS10, sd.tan.nS10, w.rf.nS10, w.tan.nS10)
colnames(print.nS10) = c("VaR|", "Portfolio SD|", "RF Weight|", "VINIX Weight|",
	"VEURX Weight|", "VEIEX Weight|", "VBLTX Weight|", "VBISX Weight|", "VPACX Weight|")
print.nS10
