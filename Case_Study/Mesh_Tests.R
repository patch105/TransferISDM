
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
  

  
############ 27/06/2024 ###################

# Trying a couple of different buffer widths
ACBRS.NorthEastAnt.buffer1k <- terra::buffer(ACBRS.NorthEastAnt, 1000)
ACBRS.NorthEastAnt.buffer1k <- terra::aggregate(ACBRS.NorthEastAnt.buffer1k, by = NULL, dissolve = T, fun = "mean")

ACBRS.NorthEastAnt.buffer200 <- terra::buffer(ACBRS.NorthEastAnt, 200)
ACBRS.NorthEastAnt.buffer200 <- terra::aggregate(ACBRS.NorthEastAnt.buffer200, by = NULL, dissolve = T, fun = "mean")

plot(ACBRS.NorthEastAnt.buffer1k)
plot(ACBRS.NorthEastAnt.buffer200)

boundary <- fmesher::fm_as_segm(st_as_sf(ACBRS.NorthEastAnt))
boundary1k <- fmesher::fm_as_segm(st_as_sf(ACBRS.NorthEastAnt.buffer1k))
boundary200 <- fmesher::fm_as_segm(st_as_sf(ACBRS.NorthEastAnt.buffer200))

dep.range <- 10000 # 10km Set the range based on biology

mesh.boundary <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(PO.sf),
                                          boundary = boundary,
                                          max.edge = c(0.2, 0.5)*dep.range,
                                          cutoff = 50,
                                          crs=3031)

mesh.200.boundary <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(PO.sf),
                                              boundary = boundary200,
                                              max.edge = c(0.2, 0.5)*dep.range,
                                              cutoff = 50,
                                              crs=3031)

mesh.1k.boundary <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(PO.sf),
                                                      boundary = boundary1k,
                                                      max.edge = c(0.2, 0.5)*dep.range,
                                                      cutoff = 50,
                                                      crs=3031)

ggplot() +
  inlabru::gg(mesh.boundary)

ggplot() +
  inlabru::gg(mesh.boundary) +
  # gg(mesh.vrt, color = "red") +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax))


ggplot() +
  inlabru::gg(mesh.200.boundary)

ggplot() +
  inlabru::gg(mesh.200.boundary) +
  # gg(mesh.vrt, color = "red") +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax))


ggplot() +
  inlabru::gg(mesh.1k.boundary)

ggplot() +
  inlabru::gg(mesh.1k.boundary) +
  # gg(mesh.vrt, color = "red") +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax))



