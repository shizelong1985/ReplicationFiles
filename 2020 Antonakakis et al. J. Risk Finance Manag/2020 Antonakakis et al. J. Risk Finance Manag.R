
### Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions
### Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020)
### Journal of Risk and Financial Management
### by David Gabauer

rm(list=c())
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("acg2020")

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(acg2020, 
                            model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=12,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)
