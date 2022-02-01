
### Measuring the Frequency Dynamics of Financial Connectedness and Systemic Risk 
### Barunik, J. & Krehlik, T. (2018)
### Journal of Financial Econometrics
### replicated by David Gabauer

rm(list=ls())
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("dy2012")

# INITIALISATION
nlag = 2
nfore = 100
partition = c(pi+0.00001, pi/4, 0)

# STATIC FREQUENCY CONNECTEDNESS
fit = VAR(dy2012, configuration=list(nlag=nlag))
FrequencyConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=nfore,partition=partition, generalized=TRUE)$TABLE

# DYNAMIC FREQUENCY CONNECTEDNESS
dca = ConnectednessApproach(dy2012, model="VAR",
                            connectedness="Frequency",
                            nlag=nlag, 
                            nfore=nfore,
                            window.size=200,
                            Connectedness_config=list(FrequencyConnectedness=list(partition=partition, generalized=TRUE, scenario="ABS"))
)
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npdc(dca)
plot_pci(dca)
plot_influence(dca)
