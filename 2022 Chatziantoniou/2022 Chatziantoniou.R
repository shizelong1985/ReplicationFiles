
### Oil price shocks and exchange rate dynamics:
### New evidence from internal, external and partial connectedness measures for oil importing and exporting countries
### Chatziantoniou, I., Elsayed, AH., Gabauer, D., & Gozgor, G.
### replicated by David Gabauer

rm(list=ls())
library("zoo")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("cegg2022")

Y = cegg2022
NAMES = colnames(Y)
k = ncol(Y)
print(SummaryStatistics(Y))

# DYNAMIC RETURN CONNECTEDNESS
dca = ConnectednessApproach(Y,
                            model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=20,
                            window.size=200,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
ind = which(index(Y)=="2020-01-03")
ConnectednessTable(dca$CT[,,c(1:ind)])$TABLE
ConnectednessTable(dca$CT[,,-c(1:ind)])$TABLE

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)

# CONNECTEDNESS DECOMPOSITION
cd = ConnectednessDecompose(dca, groups=list(c(1,2,3),c(4,5,6,7),c(8,9,10,11)))
cd$TABLE
plot_tci(cd, ylim=c(0,70))
plot_net(cd)
plot_to(cd)
plot_from(cd)

# PARTIAL CONNECTEDNESS
cp = PartialConnectedness(dca, group=c(1,2,3))
cp$TABLE
plot_tci(cp, ylim=c(0,70))
plot_net(cp)
plot_to(cp)
plot_from(cp)
