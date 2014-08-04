secrgam
=======

GAMs for Spatially Explicit Capture-Recapture density surface estimation

## Installation

```{r}
install.packages(c("devtools","secr","mgcv","fields"), dependencies = TRUE)
require(devtools)
install_github("secrgam", "david-borchers", args = list(devtools.install.args = "--no-multiarch"))
```