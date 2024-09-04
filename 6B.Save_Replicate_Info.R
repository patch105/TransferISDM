

# 6B. Save Scenario Information -------------------------------------------

save_scenario_info_func <- function(reps.setup.list,
                                    scenario_name,
                                    nreps,
                                    range_cov1,
                                    range_cov2,
                                    beta0,
                                    beta1,
                                    beta2,
                                    scal,
                                    response,
                                    rast_cellsA,
                                    rast_cellsB,
                                    prior.mean,
                                    int.sd,
                                    other.sd,
                                    prior.range,
                                    prior.space.sigma,
                                    max.n,
                                    dep.range,
                                    expans.mult,
                                    max.edge,
                                    cutoff,
                                    offset,
                                    distributionFormula) {
  
  # Create an empty list to store results
  results_list <- list()
  
  imap(reps.setup.list, function(extrap.type, extrap.name) {
    
    imap(extrap.type, function(rep, rep_index) {
      
      scenario_info.df <- data.frame(
        scenario_name = scenario_name,
        nreps = nreps,
        range_cov1 = range_cov1,
        range_cov2 = range_cov2,
        beta0 = beta0,
        beta1 = beta1,
        beta2 = beta2,
        scal = scal,
        response = response,
        rast_cellsA = rast_cellsA,
        rast_cellsB = rast_cellsB,
        prior.mean = prior.mean,
        int.sd = int.sd,
        other.sd = other.sd,
        prior.range = prior.range,
        prior.space.sigma = prior.space.sigma,
        max.n = max.n,
        dep.range = dep.range,
        expans.mult = expans.mult,
        max.edge = max.edge,
        cutoff = cutoff,
        offset = offset,
        distributionFormula = distributionFormula,
        n_po_gridA = reps.setup.list$Low[[1]]$n_po_gridA 
        
      )
        
        
        
      
      
    })
    
  })
  

  
  
}




reps.setup.list$Low[[1]]$n_presence_gridA
reps.setup.list$Low[[1]]$n_absence_gridA
