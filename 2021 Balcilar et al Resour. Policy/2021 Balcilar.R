
### CRUDE OIL FUTURES CONTRACTS AND COMMODITY MARKETS: 
### NEW EVIDENCE FROM A TVP-VAR EXTENDED JOINT CONNECTEDNESS APPROACH
### BALCILAR, M., GABAUER, D., & ZAGHUM, U. (2021)
### RESOURCES POLICY
### replicated by David Gabauer

library("ConnectednessApproach")
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
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
plot_npso(dca, save=TRUE)
