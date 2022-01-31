
### ANTONAKAKIS, N., CHATZIANTONIOU, I., AND GABAUER, D. (2020)
### REFINED MEASURES OF DYNAMIC CONNECTEDNESS BASED ON TIME-VARYING PARAMETERS VECTOR AUTREGRESSIONS
### Journal of Risk and Financial Management
### by David Gabauer (https://sites.google.com/view/davidgabauer/contact-details)

options(mc.cores=detectCores())
library("ConnectednessApproach")
data("acg2020")

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(acg2020, model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=12,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
plot_npso(dca, save=TRUE)
