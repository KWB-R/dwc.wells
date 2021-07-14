# well_features -----------------------------------------------------------------

well_features <- c(
  "well_id",
  "well_name",
  "well_id_replaced",
  "construction_date",
  "construction_year",
  "operational_start.date",
  "operational_start.year",
  "waterworks",
  "well_gallery",
  "admissible_discharge",
  "operational_start.Qs",
  "aquifer_coverage",
  "screen_material",
  "diameter",
  "n_screens",
  "filter_length",
  "well_depth",
  "drilling_method",
  "surface_water",
  "surface_water.distance",
  "inliner",
  "inliner.date",
  "volume_m3_d.mean",
  "volume_m3_d.sd",
  "quality.Cu",
  "quality.DR",
  "quality.EC",
  "quality.DO",
  "quality.Fe_tot",
  "quality.Mn",
  "quality.NO3",
  "quality.P_tot",
  "quality.pH",
  "quality.PO4",
  "quality.Redox",
  "quality.SO4",
  "quality.Temp",
  "quality.TSS"
)


# pump_test_vars ---------------------------------------------------------------

pump_test_vars <- c(
  "well_id",
  "date",
  "key",
  #"key2",
  "Qs_rel",
  "days_since_operational_start",
  "well_age_years",
  "n_rehab",
  "last_rehab.date",
  "time_since_rehab_days",
  "time_since_rehab_years"
)


# model features ---------------------------------------------------------------
model_features_with_plot_names <- list(

  # well characteristics
  "well_age_years" = "Well age [yrs]",
  "construction_year" = "Construction year",
  "screen_material" = "Filter material",
  "n_screens" = "Number of filter screens",
  "diameter" = "Diameter [mm]",
  "filter_length" = "Filter length [m]",
  "well_depth" = "Well depth [m]",
  "drilling_method" = "Drilling method",
  "admissible_discharge" = "Admissible discharge [m³/h]",
  "operational_start.Qs" = "Initial specific capacity [m³/(h, m)]",

  # site properties
  "waterworks" = "Waterworks",
  "well_gallery" = "Well gallery",
  "aquifer_coverage" = "Aquifer coverage",
  "surface_water" = "Surface water",
  "surface_water.distance" = "Distance to surface water [m]",

  # rehabilitation
  "n_rehab" = "Number of rehabs",
  "time_since_rehab_years" = "Time since rehab [yrs]",
  "inliner" = "Inliner",

  # operational information
  "volume_m3_d.mean" = "Abstraction volume, mean [m³/d]",
  "volume_m3_d.sd" = "Abstraction volume, std-dev [m³/d]",

  # water quality
  "quality.DR" = "Dry residues [mg/L]",
  "quality.EC" = "Electrical conductivity [µS/cm]",
  "quality.DO" = "Dissolved oxygen [mg/L]",
  "quality.Temp" = "Temperature [°C]",
  "quality.pH" = "pH",
  "quality.Redox" = "Redox potential [mV]",
  #"quality.Fe_tot" = expression(paste("Fe"[tot], " concentration in mg/L")),
  "quality.Fe_tot" = "Fe (tot) concentration [mg/L]",
  "quality.Mn" = "Mn concentration [mg/L]",
  "quality.Cu" = "Cu concentration [mg/L]",
  "quality.NO3" =  "NO3 concentration [mg/L]",
  "quality.P_tot" = "P (tot) concentration [mg/L]",
  #"quality.PO4" = expression(paste("PO"[4], " concentration [mg/L]")),
  "quality.PO4" = "PO4 concentration [mg/L]",
  "quality.SO4" = "SO4 concentration [mg/L]",
  "quality.TSS" = "TSS concentration [mg/L]"

  )

model_features <- names(model_features_with_plot_names)


redundant_vars <- c(
  "construction_date",
  "operational_start.date",
  "operational_start.year",
  "waterworks"
)
