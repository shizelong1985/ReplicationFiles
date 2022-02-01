
### Independent Policy, Dependent Outcomes: A Game of Cross-Country Dominoes across European Yield Curves
### Chatziantoniou, I., Gabauer, D., & Stenfor, A. (2021).
### replicated by David Gabauer

rm(list=ls())
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("cgs2022")
SummaryStatistics(cgs2022)

# DYNAMIC RETURN CONNECTEDNESS
dca = ConnectednessApproach(cgs2022, 
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

# CONDITIONAL CONNECTEDNESS
cc_chf = ConditionalConnectedness(dca, group=c(1,2,3))
cc_dkk = ConditionalConnectedness(dca, group=c(4,5,6))
cc_eur = ConditionalConnectedness(dca, group=c(7,8,9))
cc_gbp = ConditionalConnectedness(dca, group=c(10,11,12))
cc_nok = ConditionalConnectedness(dca, group=c(13,14,15))
cc_sek = ConditionalConnectedness(dca, group=c(16,17,18))
plot_tci(cc_nok, corrected=TRUE)
plot_net(cc_nok)

cc_2y = ConditionalConnectedness(dca, group=seq(1,18,3))
cc_5y = ConditionalConnectedness(dca, group=seq(2,18,3))
cc_10y = ConditionalConnectedness(dca, group=seq(3,18,3))
cc_2y$TABLE
cc_5y$TABLE
cc_10y$TABLE
plot_tci(cc_2y, corrected=TRUE)
plot_net(cc_2y)

# AGGREGATED DECOMPOSITION
ca_currency = ConnectednessAggregate(dca, groups=list("CHF"=c(1,2,3), "DKK"=c(4,5,6), "EUR"=c(7,8,9), "GBP"=c(10,11,12), "NOK"=c(13,14,15), "SEK"=c(16,17,18)))
print(ca_currency$TABLE)
plot_tci(ca_currency, corrected=TRUE)
plot_net(ca_currency)

ca_maturity = ConnectednessAggregate(dca, groups=list("2Y"=seq(1,18,3), "5Y"=seq(2,18,3), "10Y"=seq(3,18,3)))
print(ca_maturity$TABLE)
plot_tci(ca_maturity, corrected=TRUE)
plot_net(ca_maturity)
