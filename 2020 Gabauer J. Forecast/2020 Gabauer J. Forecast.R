
### Volatility impulse response analysis for DCC‐GARCH models: The role of volatility transmission mechanisms
### Gabauer, D. (2020)
### Journal of Forecasting
### replicated by David Gabauer

rm(list=c())
library("rmgarch")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("g2020")

# INITIALISATION
nfore = 100
standardize = FALSE
k = ncol(g2020)

# DCC-GARCH CONNECTEDNESS APPROACH
garch11.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                          variance.model=list(garchOrder=c(1,1), model="sGARCH"), 
                          distribution.model="norm")
dcc.garch11.spec = dccspec(uspec=multispec(replicate(k, garch11.spec)),
                           dccOrder=c(1,1), distribution="mvnorm")
fit = dccfit(dcc.garch11.spec, data=g2020)
dca = VFEVD(fit, nfore, standardize=standardize) # mistake: VIRF in paper are TRUE
virf = dca$GVIRF
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_from(dca)
plot_to(dca)
plot_npso(dca)
