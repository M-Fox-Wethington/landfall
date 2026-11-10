# Path settings for the BRI workstation, where the ADD layers, the REMA tiles
# and the 1 km mosaic already sit in the drone-processing repository. Source
# this before drawing anything:
#
#   source(system.file("scripts/bri_workstation.R", package = "landfall"))
#
# The REMA folder is the one the per-site neighborhood maps already use, so
# tiles fetched by pack_sites() land beside them. terra's temporary files go to
# the same drive, because C: is nearly full on this machine. Nothing here is
# under OneDrive, which turns a layer of this size back into a placeholder
# that exists, reports its full size and throws when anything reads it.

repo <- "G:/BRI-Antarctic-Drone-Processing"
landfall::landfall_paths(
  cache         = file.path(repo, "figures/cache/landfall"),
  rema_dir      = file.path(repo, "figures/cache/rema"),
  rema_1km      = file.path(repo, "figures/cache/rema/rema_mosaic_1km_v2.0_filled_cop30_dem.tif"),
  spatial       = file.path(repo, "peninsula_priority_bundle/spatial_data"),
  # The bundle carries only the 500 m contours, which put six lines in a
  # 25 km window. The 100 m set puts 266 there, so it is set on its own.
  contours      = file.path(repo, "add_contours/add_contours_high_res_v7.3.gpkg"),
  output        = file.path(repo, "figures/output/landfall"),
  terra_tmp     = file.path(repo, "figures/cache/terra_tmp")
)
