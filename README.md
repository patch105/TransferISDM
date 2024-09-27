# AntarcticISDM

Code to in support of "Does integrating data in species distribution models improve spatial model transfer?". _In Preparation._ Charlotte R. Patterson, Xiaotian Zheng, Scott D. Foster, Justine D. Shaw & Kate J. Helmstedt.

## Project overview
Integrated species distribution models (ISDMs) have the potential to provide better predictions than models fitted to single data types, but this has never been tested in scenarios of spatial transfer. Here, we test the performance of integrated, presence-only, and presence-absence models in simulated model transfer scenarios that emulate common causes of spatial model transfer failure: environmental dissimilarity, spatial autocorrelation, and bias in the presence-only dataset.

In Antarctica, spatially constrained survey design and an urgent need for improved prediction of biodiversity patterns motivates the use of ISDMs to predict species distributions at understudied sites. We therefore also perform a case study, testing whether models fitted with and without model integration of presence-only and presence-absence data can provide more accurate predictions of moss and lichen distributions in East Antarctica. We validate model predictions fitted at one location with a separate systematic survey at a distinct location in East Antarctica. 

This work was supported by the Australian Research Council (ARC) SRIEAS Grant SR200100005 Securing Antarctica's Environmental Future.

## Simulation Study Overview

We simulate a virtual landscape with two covariates (Fig. 1a) that drive the distribution of a single species (Fig. 1b). A pair of sites are situated within the landscape, one the Reference site where model fitting occurred, and one the Target site, our remote location with no data to which we aim to transfer our model (Fig. 1b). Presence-only and presence-absence data are simulated at the Reference site (Fig. 1c). Integrated, PO, and PA models are fit to the survey data at the Reference site before models are transferred to the Target site for prediction. By altering conditions at the Target site, different model transfer scenarios of environmental extrapolation and spatial autocorrelation can be explored. Predictive performance of models at the Target and Reference sites are assessed relative to the true species distribution.

## Run a scenario

[0a. Run ALL Replicates.R](https://github.com/patch105/AntarcticISDM/blob/main/0a.Run_ALL_Replicates.R): Run the scenario.

[0.Summarise_Plot_ALL_Jobs.R](https://github.com/patch105/AntarcticISDM/blob/main/0.Summarise_Plot_ALL_Jobs.R): Plot the outputs from the scenario.

## Separate components of the code


## Biological data acquisition and cleaning


## Environmental data acquisition and cleaning

## Scenario 1.

## Scenario 2.

Contact Charlotte Patterson at crpattrsn@gmail.com for queries.
