infra_wkt_column_names <- c(
  "wkt",
  "wkt_geom",
  "geom_wkt",
  "geometry",
  "geom",
  "shape",
  "shape_wkt",
  "well_known_text",
  "geo",
  "spatial_geom",
  "geometrie",   # fr / de
  "geometria",   # es / it / pt
  "geometrija"   # sl / hr / sr
)

usethis::use_data(infra_wkt_column_names, overwrite = TRUE)
