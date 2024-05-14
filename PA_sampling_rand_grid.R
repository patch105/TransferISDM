
spp_process <- cbind(x = lg.s$x, y = lg.s$y)

po <- thinpp[, c("x", "y")]

# find species coordinates from underlying LGCP that are in region a
inbox_idx_a <- which(spp_process[, "x"] >= dom_a_bbox["east_min"] &
                       spp_process[, "x"] <= dom_a_bbox["east_max"] & 
                       spp_process[, "y"] >= dom_a_bbox["north_min"] &
                       spp_process[, "y"] <= dom_a_bbox["north_max"])