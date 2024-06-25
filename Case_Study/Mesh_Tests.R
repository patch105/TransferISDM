
library(inlabru)
library(RISDM)

mesh.default <- makeMesh(predictors.icefree.NorthEastAnt,
                         max.n = c(500,200), # Default c(500,200)
                         dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
                         expans.mult = 1.5, # Default, 1.5 x dep.range
                         max.edge = NULL, # Default c(0.2, 0.5)*dep.range
                         cutoff = NULL, # Default 0.2*max.edge1
                         offset = NULL, # Default is dep.range
                         doPlot = TRUE)

boundary <- fmesher::fm_as_segm(st_as_sf(ice_freeSPVE.NorthEastAnt.buffered))
boundary <- fmesher::fm_as_segm(st_as_sf(ACBRS.NorthEastAnt))

max.edge = diff(range(st_coordinates(bio_north_east_ant_sf)[,1]))/(3*5)

dep.range <- 10000 # 10km Set the range based on biology


mesh <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(bio_north_east_ant_sf),
                                 boundary = boundary,
                                 max.edge = c(0.2, 0.5)*dep.range,
                                 crs=3031)

mesh <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(bio_north_east_ant_sf),
                                 boundary = boundary,
                                 max.edge = c(0.2, 0.5)*dep.range,
                                 cutoff = 0.2*max.edge,
                                 crs=3031)

mesh <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(bio_north_east_ant_sf),
                                 boundary = boundary,
                                 max.edge = c(0.2, 0.5)*dep.range,
                                 cutoff = 50,
                                 crs=3031)

dep.range <- 2000
mesh <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(bio_north_east_ant_sf),
                                 boundary = boundary,
                                 max.edge = c(0.2, 0.5)*dep.range,
                                 cutoff = 50,
                                 crs=3031)

RISDM::checkMesh(mesh)


bound.outer = diff(range(st_coordinates(bio_east_ant_sf)[,1]))/3

mesh3 <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(bio_east_ant_sf),
                                  boundary = boundary,
                                  max.edge = c(1,2)*max.edge,
                                  offset = c(max.edge, bound.outer),
                                  cutoff = 0.2*max.edge)

# Extract and save mesh
mesh.vrt <- fmesher::fm_vertices(mesh)

ggplot() +
  inlabru::gg(mesh) +
  # gg(mesh.vrt, color = "red") +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat)$xmin, st_bbox(Vestfold.landsat)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat)$ymin, st_bbox(Vestfold.landsat)$ymax))
  

  
