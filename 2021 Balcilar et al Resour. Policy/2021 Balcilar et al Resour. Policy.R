
### Crude Oil futures contracts and commodity markets: 
### New evidence from a TVP-VAR extended joint connectedness approach
### Balcilar, M., Gabauer, D., & Umar, Z. (2021)
### Resources Policy
### replicated by David Gabauer

rm(list=c())
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("bgu2021")

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(bgu2021, 
                            model="TVP-VAR",
                            connectedness="Extended Joint",
                            nlag=1,
                            nfore=20,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
