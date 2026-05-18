infra_y_column_names <- c(
  "y",
  "y_coord",
  "coord_y",
  "northing",
  "north",
  "latitude",
  "lat",
  "latitud",
  "latitudine", # es, it
  "breite", # de
  "geografische_breite",
  "geographische_breite",
  "norte",
  "nord",
  "norden", # es, fr/it, de
  "sur",
  "sud",
  "sued",
  "sul", # es, fr/it, de, pt
  "hochwert" # de Gauss-Krüger
)

usethis::use_data(infra_y_column_names)

infra_x_column_names <- c(
  "x",
  "x_coord",
  "coord_x",
  "easting",
  "east",
  "longitude",
  "long",
  "lon",
  "lng",
  "longitud",
  "longitudine", # es, it
  "laenge",
  "lange", # de
  "geografische_laenge",
  "geographische_laenge",
  "este",
  "est",
  "ost",
  "leste", # es, fr/it, de, pt
  "oeste",
  "ouest", # es, fr
  "rechtswert" # de Gauss-Krüger
)

usethis::use_data(infra_x_column_names)
