# AntarcticISDM

Code in support of "Transferring species distribution models to understudied systems: Does data integration help?". _In Preparation._ Charlotte R. Patterson, Xiaotian Zheng, Scott D. Foster, Justine D. Shaw & Kate J. Helmstedt.

## Project overview
Integrated species distribution models (ISDMs) have the potential to provide better predictions than models fitted to single data types, but this has never been tested in scenarios of spatial transfer. Here, we test the performance of integrated, presence-only, and presence-absence models in simulated model transfer scenarios that emulate common causes of spatial model transfer failure: environmental dissimilarity and spatial autocorrelation. We also examine how two aspects of data quality - bias and presence-only detection probability - influence the outcomes of these model transfer scenarios. 

This work was supported by the Australian Research Council (ARC) SRIEAS Grant SR200100005 Securing Antarctica's Environmental Future.

## Simulation Study Overview

We simulate a virtual landscape with two covariates (Fig. 1a) that drive the distribution of a single species (Fig. 1b). A pair of sites are situated within the landscape, one the training site where model fitting occurred, and one the projection site, our remote location with no data to which we aim to transfer our model (Fig. 1b). Presence-only and presence-absence data are simulated at the training site (Fig. 1c). Integrated, PO, and PA models are fit to the survey data at the training site before models are transferred to the projection site for prediction. By altering conditions at the training site, different model transfer scenarios of environmental extrapolation and spatial autocorrelation can be explored. Predictive performance of models at the projection and training sites are assessed relative to the true simulated species distribution.

See Methods of Patterson et al. for further details.

![Figure 1. A workflow plot.](https://github.com/patch105/AntarcticISDM/blob/main/Figures/FIGURE_1.png)

Figure 1. (a) A visual representation of spatial model transfer, where a model is fitted to occurrence records at a training site and predicted to a projection site where there are few or no records. (b-c) Representations of the two key challenges addressed in this simulation design that are often associated with model transfer: environmental dissimilarity of the training and projection sites and unaccounted-for spatial autocorrelation of the species distribution that can operate across different autocorrelation ranges and levels of variance. From Patterson et al. 



![Figure 2. A workflow plot.](https://github.com/patch105/AntarcticISDM/blob/main/Figures/FIGURE_2.png)

Figure 2. (a) A visual representation of the simulation study design. Steps 1-3 are the basic structure of the simulation workflow which generates a landscape with a single species distribution, places training and projection sites on the landscape and samples presence-only and presence-absence records of the species at the training site. Boxes 1a and 2a are spatial transfer scenarios. Boxes 3a-b are data scenarios. (b) The modelling framework, where integrated or single-dataset models are fitted to the training site data. Boxes 4a-b are model-fitting options. (c) Fitted models are transferred to the projection site for evaluation. From Patterson et al. 


## Simulation Code Overview

At its simplest, you can run a scenario all the way through from the simulation code via running this script:

Run a scenario with the [0a. Run ALL Replicates.R](https://github.com/patch105/AntarcticISDM/blob/main/0a.Run_ALL_Replicates.R) script. Here, you name your scenario and set the scenario specifications and parameters. All the scripts in the [Scenarios folder](https://github.com/patch105/AntarcticISDM/blob/main/Scenarios) are just variations of this master script.

Embedded in this script are calls to several other scripts which are outlined below. Some scripts were inspired by code from [Simmonds et al. 2020](https://github.com/NERC-CEH/IOFFsimwork).

## Set up simulation of a scenario

[0a. Run ALL Replicates.R](https://github.com/patch105/AntarcticISDM/blob/main/0a.Run_ALL_Replicates.R) calls the following scripts:

[0b.Run_Replicate.R](https://github.com/patch105/AntarcticISDM/blob/main/0b.Run_Replicate.R): This script sequentially calls and saves all the separate functions of the simulation (steps 1-12 below). In here is where you can adjust the priors and the mesh parameters.

[0b.Run_Replicate_SA.R](https://github.com/patch105/AntarcticISDM/blob/main/0b.Run_Replicate_SA.R) (The Spatial Autocorrelation version of the above, called when you set scenario.type = "Spatial.Auto" in the 0a.Run_ALL_Replicates.R script)

[1.Simulate_Covariates.R](https://github.com/patch105/AntarcticISDM/blob/main/1.Simulate_Covariates.R): Simulate two spatially-varying covariates as Gaussian random fields.

[2.Simulate_Latent_Distribution.R](https://github.com/patch105/AntarcticISDM/blob/main/2.Simulate_Latent_Distribution.R): Simulate the latent species distribution, either with or without spatial autocorrelation (determined by parameter 'latent.type' in the 0a.Run_ALL_Replicates.R script).

[3.Simulate_Enviro_Extrapolation.R](https://github.com/patch105/AntarcticISDM/blob/main/3.Simulate_Enviro_Extrapolation.R): Randomly select two sites, one your training site and one your projection site. Calculate the environmental distance between the sites with the Shape metric.

[4.PO_Sampling.R](https://github.com/patch105/AntarcticISDM/blob/main/4.PO_Sampling.R): Sample PO data at the training site. Data are either biased or not, depending on the parameter 'bias'from the 0a.Run_ALL_Replicates.R script.

[5.PA_Sampling.R](https://github.com/patch105/AntarcticISDM/blob/main/5.PA_Sampling.R): Sample the PA data at the training site via quadrat surveys.

## Run models in a scenario and extract results

[6.Run_Model.R](https://github.com/patch105/AntarcticISDM/blob/main/6.Run_Model.R): Set up and run the integrated, presence-only and presence-absence models. Model types are designated via the object 'mod.type' in the 0a.Run_ALL_Replicates.R script and can include or not include a bias covariate and a Gaussian random field. 

[7.Extract_Model_Results.R](https://github.com/patch105/AntarcticISDM/blob/main/7.Extract_Model_Results.R): Pull out model parameter estimates. 

## Evaluate models compared to the true species distribution

[8.Make_Truth.R](https://github.com/patch105/AntarcticISDM/blob/main/8.Make_Truth.R): Save the true log species intensity for comparison with predictions.

[9.Predict_from_fitted.R](https://github.com/patch105/AntarcticISDM/blob/main/9.Predict_from_fitted.R): Take fitted models and predict from the training to the projection site (called Site B). You can also predict back to the training site (Site A).

[10.Validation_True_Intensity.R](https://github.com/patch105/AntarcticISDM/blob/main/10.Validation_True_Intensity.R): Compare the predicted and true log species intensity at the projection site. You can also compare the predicted and true log species intensity at the training site.

## Plot the data and predictions from a scenario (for reference when reviewing results)

[11a.Plot_Data.R](https://github.com/patch105/AntarcticISDM/blob/main/12B.Plot_Data.R): This plots, for each replicate, the PO and PA data at the training site overlaid on the true species log intensity.

[11b.Plot_Predictions.R](https://github.com/patch105/AntarcticISDM/blob/main/12C.Plot_Predictions.R): This creates and saves plots of model predictions at both the projection and training sites per model per replicate.

## Simulation scenarios

The following scenarios were run for the manuscript:

Scenario name | Environmental dissimilarity | Spatial autocorrelation | Sampling bias? | Number of PO records | Run scenario 
--- | --- | --- | --- |--- |---
(1) Environmental dissimilarity with bias in PO data | Low to High |  | Yes | High | [Scenario_1](https://github.com/patch105/AntarcticISDM/blob/main/Scenarios/0a.Run_ALL_Replicates_Scenario_1.R) 
(2) Environmental dissimilarity with bias in PO data | Low to High |  | Yes | Low | [Scenario_2](https://github.com/patch105/AntarcticISDM/blob/main/Scenarios/0a.Run_ALL_Replicates_Scenario_2.R) 
(3) Spatial autocorrelation with bias in PO data | Low | Range = 20, 100, 200; RE variance* = 0.2,1,5 | Yes | High | [Scenario 3](https://github.com/patch105/AntarcticISDM/tree/main/Scenarios/Scenario_3)  
(4) Spatial autocorrelation with bias in PO data | Low | Range = 20, 100, 200; RE variance = 0.2,1,5 | Yes | Low | [Scenario 4](https://github.com/patch105/AntarcticISDM/tree/main/Scenarios/Scenario_4)  

\*The relative proportion of variance in the latent species distribution assigned to the random effect vs. the fixed effect. 

## Code for plotting figures from manuscript

[Figure 3. and supplementary figures Scenarios 1 & 2](https://github.com/patch105/AntarcticISDM/blob/main/Plotting/PLOTTING_Scenario_1_2.R)

[Figure 4. and supplementary figures Scenarios 3 & 4](https://github.com/patch105/AntarcticISDM/blob/main/Plotting/PLOTTING_Scenario_3_4.R)

[Figure 5. and supplementary figures Scenarios 3 & 4](https://github.com/patch105/AntarcticISDM/blob/main/Plotting/PLOTTING_Scenario_3_4.R)

[Count the number of PO records and presence/absences in PA data across scenarios](https://github.com/patch105/AntarcticISDM/blob/main/Number_PO_PA_ALL_Scenarios.R)

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
