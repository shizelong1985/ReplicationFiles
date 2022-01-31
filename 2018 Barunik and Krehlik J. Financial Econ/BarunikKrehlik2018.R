
### BARUNIK AND KREHLIK (2018)
### Measuring the Frequency Dynamics of Financial Connectedness and Systemic Risk 
### Journal of Financial Econometrics
### replicated by David Gabauer

options(mc.cores=detectCores())
library("ConnectednessApproach")
data("dy2012")

# STATIC CONNECTEDNESS
nlag = 2
nfore = 100
partition = c(pi+0.00001, pi/4, 0)
fit = VAR(dy2012, configuration=list(nlag=nlag))
FrequencyConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=nfore,partition=partition, generalized=TRUE)$TABLE

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(dy2012, model="VAR",
                            connectedness="Frequency",
                            nlag=nlag, 
                            nfore=nfore,
                            Connectedness_config=list(FrequencyConnectedness=list(partition=partition, generalized=TRUE))
)
dca$TABLE
