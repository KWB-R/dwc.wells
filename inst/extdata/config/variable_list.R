# well_features -----------------------------------------------------------------

well_features <- c(
  "well_id",
  "well_name",
  "well_id_replaced",
  "construction_date",
  "construction_year",
  "operational_start.date",
  "operational_start.year",
  "screen_material",
  "n_screens",
  "diameter",
  "filter_length",
  "well_depth",
  "drilling_method",
  "admissible_discharge",
  "operational_start.Qs",
  "waterworks",
  "well_gallery",
  "aquifer_coverage",
  "surface_water",
  "surface_water.distance",
  "inliner",
  "inliner.date",
  "volume_m3_d.mean",
  "volume_m3_d.sd",
  "volume_m3_d.cv",
  "W_static.sd",
  "quality.DR",
  "quality.EC",
  "quality.DO",
  "quality.Temp",
  "quality.pH",
  "quality.Redox",
  "quality.Fe_tot",
  "quality.Mn",
  "quality.Cu",
  "quality.NO3",
  "quality.P_tot",
  "quality.PO4",
  "quality.SO4",
  "quality.TSS",
  "well_function",
  "operational_state"
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
  "W_static.sd" = "Static water level, std-dev [m]",
  "surface_water" = "Surface water",
  "surface_water.distance" = "Distance to surface water [m]",

  # rehabilitation
  "n_rehab" = "Number of rehabs",
  "time_since_rehab_years" = "Time since rehab [yrs]",
  "inliner" = "Inliner",

  # operational information
  "volume_m3_d.mean" = "Abstraction volume, mean [m³/d]",
  "volume_m3_d.sd" = "Abstraction volume, std-dev [m³/d]",
  "volume_m3_d.cv" = "Abstraction volume, cv [-]",

  # water quality
  "quality.DR" = "Dry residues [mg/L]",
  "quality.EC" = "Electrical conductivity [µS/cm]",
  "quality.DO" = "Dissolved oxygen [mg/L]",
  "quality.Temp" = "Temperature [°C]",
  "quality.pH" = "pH [-]",
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

  #"well_function" = "Brunnenfunktion"

  )

model_features <- names(model_features_with_plot_names)

top5_model_features <- c("well_age_years",
                         "time_since_rehab_years",
                         "well_gallery",
                         "n_rehab",
                         "volume_m3_d.cv")
