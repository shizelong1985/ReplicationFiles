
### ANTONAKAKIS, N., CHATZIANTONIOU, I., AND GABAUER, D. (2019)
### CRYPTOCURRENCY MARKET CONTAGION: MARKET UNCERTAINTY, MARKET COMPLEXITY, AND DYNAMIC PORTFOLIOS (I)
### Journal of International Financial Markets, Institutions and Money
### by David Gabauer (https://sites.google.com/view/davidgabauer/contact-details)

# Information: As my laptop had a defect in 2019 I have lost the original code of this paper.
# Hence, this is an approximation of the original paper. Instead of the TVP-FAVAR method of Koop and Korobilis (2014)
# I am using the standard Principal Component Analysis and plug the factor scores into the TVP-VAR model of Koop and Korobilis (2014).
# Furthermore, I tried to downloaded all cryptocurrencies from CoinMarketCap, however, I could only retrieve a subsample of the original dataset.
# Thus, instead of 45 series that have been used for the PCA I am currently using only 9. Results, however, are qualitatively similar.
# Nevertheless, I wanted to make this code public as I received a lot of requests concerning the TVP-FAVAR model.
# Even though this is not the same it leads to similar results and I sincerely hope that it will be useful for future research.
# Kind regards, David

library("zoo")
library("psych")
library("openxlsx")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

RAW = na.omit(read.xlsx('./data.xlsx', detectDates=TRUE))
DATE = as.Date(RAW[,1])
DATA = RAW[,-1]
DATA = zoo(DATA, order.by=DATE)
k = ncol(DATA)
NAMES = colnames(DATA)

# DATA TRANSFORMATION
X = DATA[-1,]
for (i in 1:k) {
  x = embed(as.numeric(DATA[,i]),2)
  X[,i] = 100*(x[,1]-x[,2])/x[,1]
}
Y = X[,1:9]
Z = X[,-c(1:9)]
PC = principal(Z, nfactors=1)$scores
Y = cbind(Y, PC)
k = ncol(Y)
colnames(Y)[k] = "PC"

# DYNAMIC RETURN CONNECTEDNESS
dca = ConnectednessApproach(Y, 
                            model="TVP-VAR",
                            connectedness="Time",
                            nfore=20,
                            window.size=200,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")))
ind = which(index(Y)=="2017-07-20")
ConnectednessTable(dca$FEVD[,,c(1:ind)])$TABLE
ConnectednessTable(dca$FEVD[,,-c(1:ind)])$TABLE

# CONNECTEDNESS PLOTS
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
plot_npso(dca, save=TRUE)
plot_pci(dca, save=TRUE)
