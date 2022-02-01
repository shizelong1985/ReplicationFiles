
### EMU risk-synchronisation and financial fragility through the prism of dynamic connectedness.
### Chatziantoniou, I. & Gabauer, D.
### The Quarterly Review of Economics and Finance
### replicated by David Gabauer

rm(list=c())
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("cg2021")

# DYNAMIC RETURN CONNECTEDNESS
dca = ConnectednessApproach(cg2021, 
                            model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=10,
                            window.size=200,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca, corrected=TRUE)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)
plot_pci(dca)
