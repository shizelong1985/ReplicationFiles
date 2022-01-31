
### On The Transmission Mechanism Of Country-Specific And International Economic Uncertainty Spillovers:
### Evidence From A TVP-VAR Connectedness Decomposition Approach
### GABAUER, D., and GUPTA, R. (2019)
### ECONOMICS LETTERS
### replicated by David Gabauer

library("ConnectednessApproach")
options(mc.cores=detectCores())

# DYNAMIC CONNECTEDNESS
data("gg2019")
dca = ConnectednessApproach(gg2019, 
                            model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=10,
                            window.size=200,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")))
print(dca$TABLE)
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)

# CONNECTEDNESS DECOMPOSITION
cd = ConnectednessDecomposition(dca, groups=list("US"=c(1,2,3,4), "JP"=c(5,6,7,8)), corrected=FALSE)
print(cd$TABLE)

plot_net(cd)
plot_to(cd)
plot_from(cd)
plot_npso(cd)
