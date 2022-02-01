
### Cryptocurrency market contagion: Market uncertainty, market complexity, and dynamic portfolios
### Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2019)
### Journal of International Financial Markets, Institutions and Money
### by David Gabauer

# Information: As my laptop had a defect in 2019 I have lost the original code of this paper.
# Hence, this is an approximation of the original paper. Instead of the TVP-FAVAR method of Koop and Korobilis (2014)
# I am using the standard Principal Component Analysis and plug the factor scores into the TVP-VAR model of Koop and Korobilis (2014).
# Furthermore, I tried to downloaded all cryptocurrencies from CoinMarketCap, however, I could only retrieve a subsample of the original dataset.
# Thus, instead of 45 series that have been used for the PCA I am currently using only 9. Results, however, are qualitatively similar.
# Nevertheless, I wanted to make this code public as I received a lot of requests concerning the TVP-FAVAR model.
# Even though this is not the same it leads to similar results and I sincerely hope that it will be useful for future research.
# Kind regards, David

rm(list=c())
library("zoo")
library("psych")
library("openxlsx")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

RAW = na.omit(read.xlsx('./data.xlsx', detectDates=TRUE))
DATE = as.Date(RAW[,1], "%Y-%m-%d")
DATA = RAW[,-1]
DATA = zoo(DATA, order.by=DATE)
k = ncol(DATA)
NAMES = colnames(DATA)

Y = DATA[,1:9]
Z = DATA[,-c(1:9)]
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
ConnectednessTable(dca$CT[,,c(1:ind)])$TABLE
ConnectednessTable(dca$CT[,,-c(1:ind)])$TABLE

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)
plot_pci(dca)
