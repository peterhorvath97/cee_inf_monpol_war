Tech Summary and Preliminary Estimates
================

# Decomposing the headline inflation rate

I run a Fixed Effects OLS regression across our country panel to
determine the contribution of core, food, and energy inflation rates to
the headline harmonized CPI inflation (all data are from Eurostat).

$CPI_{i,t} = \beta_{1}CORE_{i,t} + \beta_{2}FOOD_{i,t} + \beta_{3}ENERGY_{i,t}+ \delta_{i} + \gamma_{t} + \epsilon_{i,t}$

where $\delta_{i}$ are country fixed effects, $\gamma_{t}$ are annual
fixed effects and $\beta_{j}$ reference the weight of each component in
the headline inflation rate.

Below are the results for the last four years of the sample (2019-2023):

![Contribution of Core inflation, Food and Energy prices to headline
inflation rates.](README_files/figure-gfm/unnamed-chunk-2-1.png)

Same plot with fixed “Y” axis:

![Contribution of Core inflation, Food and Energy prices to headline
inflation rates, fixed Y
axis.](README_files/figure-gfm/unnamed-chunk-3-1.png)

Potential follow-up:

Mediation analysis (/ an instrumental variable approach) could show if
there is any inter-play among the rates. It is not too unreasonable to
believe that government measures, such as price caps were instead
realized in growth of non-targeted goods. For example, price caps on
food or fuel items - resulting in revenue losses - were recouped by
firms via increasing the prices of items found in the core CPI basket.
This could be done by regressing each component on the other components
and accounting for the indirect effects.

# Real rates, equilibrium and policy stance

Here I first calculate real interest rates as
$r_{t} = i_{t} - \pi_{t+1}$, where $r_{t}$ is the real rate, $i_{t}$ is
the nominal rate and $\pi_{t}$ is the annualized CPI inflation rate. The
graph below shows the real rates across the countries in the sample:

![Real rates across
countries.](README_files/figure-gfm/unnamed-chunk-5-1.png)

To separate the natural rate from cyclical fluctuations, I use two
variations of the HP filter, a One-sided, and a Two-sided version. The
two approaches yield similar trends, with the one sided (naturally)
showing more fluctuations. The trend component estimated with the
filters can be considered as the equilibrium real interes rate, these
can be seen in the graph below: ![Natural real interest
rates.](README_files/figure-gfm/unnamed-chunk-6-1.png) The graphs here
show a historically declining equilibrium rate up to 2019/2020, where we
can observe a reversion in this trend.

The deviations from the trend, i.e. the cyclical components thus can be
used to asses policy stance:
![](README_files/figure-gfm/Policy%20stance%20-%20deviation%20from%20the%20equilibrium.-1.png)<!-- -->
All graphs here agree that policy stance was following a very loose
monetary policy around 2-8 percent below the equilibrium, at the time
the war shock hit the economies. The graphs also show the fast response
of the regional central banks, as the real rate gaps rose sharply in the
following months.
