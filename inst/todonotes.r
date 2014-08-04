P=  Help files for ?Boland.mask1, ?Boland.CH1, rather than just ?Boland.leopards1?
       and same for ?Boland.mask2, ?Boland.CH2.

# amend readme to give installation instructions and test

## plotting
P=  Rethink plotting function names and change accordingly, e.g. rename "plotDgam" to "plotsmooths"? - make plot.secrgam, if type = "density" and multisession return "plotting of density for multi-session data not yet implemented".
# add overbird session example to secrgam.fit examples 
P=  Make image.plot.mask deal with sessions properly.
P=  PlotDgam: 
P=    a. Make plot.secrgam and plotDgam deal with session properly
P=    b. Add option not to plot CI?
P=    d. LATER: Bivariates smooths: plot marginals and 3D plot?
P=    e. Adapt plotDgam to deal sensibly with 1D smooths when there are some multivariate smooth terms
         and/or specify which to plot.
P=  Add option to plot relative SE to plot.secrgam?

P=  fitted.secrgam vs region.ND? - is there a big difference? if so, disable ours (for getting CIs) compare simple models
P=  Remove trap.covar.r (Superceded by secr function addCovariates?)
P=  Change AIC.secrgam documentation to say need secrgam object first.
    LATER: Fix AIC.secrgam( ) so that it works even if 1st object is class secr rather than secrgam
P= LATER: Vignettes?
   * mask constuction
   * example analysis with spatial and temporal variables
P= Put on CRAN?
P= LATER?: Set up unit testing or something similar.

DONE:
  Removed D.bspline.r
