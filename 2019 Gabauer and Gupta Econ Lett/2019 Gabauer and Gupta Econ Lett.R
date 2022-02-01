
### On The Transmission Mechanism Of Country-Specific And International Economic Uncertainty Spillovers:
### Evidence From A TVP-VAR Connectedness Decomposition Approach
### Gabauer, D., & Gupta, R. (2019)
### Economics Letters
### replicated by David Gabauer

rm(list=c())
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

# DYNAMIC CONNECTEDNESS
data("gg2018")
dca = ConnectednessApproach(gg2018, 
                            model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=10,
                            window.size=200,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)

# CONNECTEDNESS DECOMPOSITION
cd = ConnectednessDecompose(dca, groups=list("US"=c(1,2,3,4), "JP"=c(5,6,7,8)))
print(cd$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(cd)
plot_net(cd)
plot_to(cd)
plot_from(cd)
plot_npso(cd)

# AGGREGATED DECOMPOSITION
ca = ConnectednessAggregate(dca, groups=list("US"=c(1,2,3,4), "JP"=c(5,6,7,8)))
print(ca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(ca)
plot_net(ca)
plot_to(ca)
plot_from(ca)
plot_npso(ca)
