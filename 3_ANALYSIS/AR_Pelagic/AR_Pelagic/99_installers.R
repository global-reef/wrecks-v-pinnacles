# Install remotes if you don't already have it
install.packages("remotes")

# Then install cmdstanr from GitHub
remotes::install_github("stan-dev/cmdstanr")

library(cmdstanr)
install_cmdstan()
cmdstanr::cmdstan_version()
