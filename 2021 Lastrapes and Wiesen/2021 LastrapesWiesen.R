
### The joint spillover index
### LASTRAPES, W.D., & WIESEN, T.F.
### Economic Modelling
### replicated by David Gabauer

library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

data("lw2021")
fit = VAR(lw2021, configuration=list(nlag=2))
LW2021 = JointConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=30)
LW2021$TABLE

dca = ConnectednessApproach(lw2021, 
                            model="VAR",
                            connectedness="Joint",
                            nlag=2,
                            nfore=30,
                            window.size=200)
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
