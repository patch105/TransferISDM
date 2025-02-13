# AntarcticISDM

Code in support of "Transferring species distribution models to understudied systems: Does data integration help?". _In Preparation._ Charlotte R. Patterson, Xiaotian Zheng, Scott D. Foster, Justine D. Shaw & Kate J. Helmstedt.

## Project overview
Integrated species distribution models (ISDMs) have the potential to provide better predictions than models fitted to single data types, but this has never been tested in scenarios of spatial transfer. Here, we test the performance of integrated, presence-only, and presence-absence models in simulated model transfer scenarios that emulate common causes of spatial model transfer failure: environmental dissimilarity and spatial autocorrelation. We also examine how two aspects of data quality - bias and presence-only detection probability - influence the outcomes of these model transfer scenarios. 

This work was supported by the Australian Research Council (ARC) SRIEAS Grant SR200100005 Securing Antarctica's Environmental Future.

## Simulation Study Overview

We simulate a virtual landscape with two covariates (Fig. 1a) that drive the distribution of a single species (Fig. 1b). A pair of sites are situated within the landscape, one the training site where model fitting occurred, and one the projection site, our remote location with no data to which we aim to transfer our model (Fig. 1b). Presence-only and presence-absence data are simulated at the training site (Fig. 1c). Integrated, PO, and PA models are fit to the survey data at the training site before models are transferred to the projection site for prediction. By altering conditions at the training site, different model transfer scenarios of environmental extrapolation and spatial autocorrelation can be explored. Predictive performance of models at the projection and training sites are assessed relative to the true simulated species distribution.

See Methods of Patterson et al. for further details.

## Simulation Code Overview

At its simplest, you can run a scenario from the simulation code via running sequentially these two scripts:

Run a scenario with the [0a. Run ALL Replicates.R](https://github.com/patch105/AntarcticISDM/blob/main/0a.Run_ALL_Replicates.R) function. Here you name your scenario and set the scenario specifications and parameters. 

Plot the outputs from the scenario with the [0.Summarise_Plot_ALL_Jobs.R](https://github.com/patch105/AntarcticISDM/blob/main/0.Summarise_Plot_ALL_Jobs.R) function. Your input scenario name in this script must match your scenario name in the previous script.

Embedded in these two scripts are calls to several other functions and scripts which are outlined below.

## Run a scenario

[0a. Run ALL Replicates.R](https://github.com/patch105/AntarcticISDM/blob/main/0a.Run_ALL_Replicates.R) calls the following scripts:

[0b.Run_Replicate.R](https://github.com/patch105/AntarcticISDM/blob/main/0b.Run_Replicate.R): This script sequentially calls and saves all the separate functions of the simulation (steps 1-12 below). In here is where you can adjust the priors and the mesh parameters.

[0b.Run_Replicate_SA.R](https://github.com/patch105/AntarcticISDM/blob/main/0b.Run_Replicate_SA.R) (The Spatial Autocorrelation version of the above, called when you set scenario.type = "Spatial.Auto")

[1.Simulate_Covariates.R](https://github.com/patch105/AntarcticISDM/blob/main/1.Simulate_Covariates.R): Simulate two spatially-varying covariates as Gaussian random fields.

[2.Simulate_Latent_Distribution.R](https://github.com/patch105/AntarcticISDM/blob/main/2.Simulate_Latent_Distribution.R): Simulate the latent species distribution, either with or without spatial autocorrelation (determined by parameter 'latent.type').

[3.Simulate_Enviro_Extrapolation.R](https://github.com/patch105/AntarcticISDM/blob/main/3.Simulate_Enviro_Extrapolation.R): Randomly select two sites, one your training site and one your projection site. Calculate the environmental distance between the sites with the Shape metric.

[4.PO_Sampling.R](https://github.com/patch105/AntarcticISDM/blob/main/4.PO_Sampling.R): Sample PO data at the training site. Data are either biased or not, depending on the parameter 'bias'.

[5.PA_Sampling.R](https://github.com/patch105/AntarcticISDM/blob/main/5.PA_Sampling.R): Sample the PA data at the training site via quadrat surveys.

[6.Run_Model.R](https://github.com/patch105/AntarcticISDM/blob/main/6.Run_Model.R): Set up and run the integrated, presence-only and presence-absence models. Model types are designated via the object 'mod.type'.

[7.Extract_Model_Results.R](https://github.com/patch105/AntarcticISDM/blob/main/7.Extract_Model_Results.R): Pull out model parameter estimates. 

[8.Make_Truth.R](https://github.com/patch105/AntarcticISDM/blob/main/8.Make_Truth.R): Save the true log species intensity for comparison with predictions.

[9.Predict_from_fitted.R](https://github.com/patch105/AntarcticISDM/blob/main/9.Predict_from_fitted.R): Take fitted models and predict from the training to the projection site. 

[10.Validation_True_Intensity.R](https://github.com/patch105/AntarcticISDM/blob/main/10.Validation_True_Intensity.R): Compare the predicted and true log species intensity at the projection site.

## Plot the outputs of a scenario

[0.Summarise_Plot_ALL_Jobs.R](https://github.com/patch105/AntarcticISDM/blob/main/0.Summarise_Plot_ALL_Jobs.R) calls the following scripts:

[11.Plot_Validation_True_Intensity.R](https://github.com/patch105/AntarcticISDM/blob/main/11.Plot_Validation_True_Intensity.R): This creates and saves plots looking at the predictive performance of the integrated, PO, and PA models.

[12.Plot_Model_Outputs.R](https://github.com/patch105/AntarcticISDM/blob/main/12.Plot_Model_Outputs.R): This creates and saves plots looking at the parameter estimates from each model.

[12B.Plot_Data.R](https://github.com/patch105/AntarcticISDM/blob/main/12B.Plot_Data.R): This plots, for each replicate, the PO and PA data at the training site.

[12C.Plot_Predictions.R](https://github.com/patch105/AntarcticISDM/blob/main/12C.Plot_Predictions.R): This creates and saves plots of model predictions at both the projection and training sites.

## Simulation scenarios

The following scenarios were run for the manuscript:

Scenario name | Environmental dissimilarity | Spatial autocorrelation | Sampling bias? | Number of PO records | Run scenario 
--- | --- | --- | --- |--- |---
(1) Environmental dissimilarity with bias in PO data | Low to High |  | Yes | Low, High | [Scenario_1](https://github.com/patch105/AntarcticISDM/tree/main/Scenarios/Scenario_1) 
(2) Spatial autocorrelation |  | Range = 20, 100, 200 |  | Low, High | (link)  
(3) Environmental dissimilarity & spatial autocorrelation | Low to High | Range = 20 |  | Low, High | (link)  
(4) Spatial autocorrelation with Bias in PO data | Low | Range = 20, 100, 200 | Yes | Low, High | (link)  
(5) Environmental extrapolation, Spatial autocorrelation & Bias in PO data | Low to High | Range = 20, 100, 200 | Yes | Low, High | (link) 

## Final summary figures from manuscript




## Antarctic Case Study


## Biological data acquisition and cleaning


## Environmental data acquisition and cleaning

## Scenario 1.

## Scenario 2.

## Session details
R version 4.4.1
terra 1.7.78
spatstat 3.0-7
ggplot2 3.5.1
sf 1.0-19
viridis 0.6.5
readr 2.1.5
purrr 1.0.2


Contact Charlotte Patterson at crpattrsn@gmail.com for queries.
