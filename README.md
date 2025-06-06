# Pelagic Pinnacle Project
========================

This repository contains data and analysis for the Pelagic Pinnacle Project, which investigates fish assemblages on pelagic pinnacles in the Gulf of Thailand, compared to fringing reefs and artificial shipwrecks.

Overview
--------

The project explores whether wrecks support distinct fish communities or functionally resemble other reef types. Diver-based visual surveys were conducted across multiple sites, with species recorded by estimated size class and trophic group.

Bayesian multivariate models (zero-inflated negative binomial) are used to quantify patterns in total abundance, functional group composition, and species-specific responses across habitat types. Models include random effects for diver and site, and results are interpreted using posterior means and credible intervals.

Main Features
-------------

- Timed swim fish surveys across pinnacles, reefs, and wrecks
- Functional group classification of observed species
- Bayesian multivariate negative bionimal and ZINB models via `brms` in R
- Site-level random effects and habitat-type comparisons
- Posterior visualization and credible interval summaries

Notes
-----

- Survey data are collected by Global Reef researchers based in Koh Tao, Thailand.
- Fieldwork and data processing are ongoing; results may be updated as more surveys are completed.
- This project contributes to understanding the ecological role of mid-shelf reef features in regional conservation planning.

License
-------

This project is private and not licensed for redistribution. For collaboration inquiries, please contact scarlett@global-reef.com.
