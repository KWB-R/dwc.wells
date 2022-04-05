# path_list --------------------------------------------------------------------

path_list <- list(

  # project folder
  project = file.path(kwb.utils::desktop(), "tmp/DWC/wells"),

  # data preparation folder
  data_prep = "<project>/01_data_prep",

  # mas access database ---
  db =  "<data_prep>/01_data_in/ms_access/BWB_WV_Brunnenexport_2017.mdb",

  # csv data ---
  csv_data = "<data_prep>/01_data_in/csv",
  data_wells = "<csv_data>/GWBR_02.csv",
  data_drilling = "<csv_data>/Bohrungen_02.csv",
  data_drilling_tech = "<csv_data>/Bohrtechnik.csv",
  data_pump_tests = "<csv_data>/Arbeitsberichte.csv",
  data_W_static = "<csv_data>/RWS.csv",
  data_quantity = "<csv_data>/ERG_02.csv",
  data_operational_hours = "<csv_data>/Betriebsstunden_zw_Regenerierungen.csv",
  #data_quality = "<csv_data>/___.csv",
  #data_quality_para1 = "<csv_data>/LIMS_Para.csv",
  #data_quality_para2 = "<csv_data>/LIMS_PM.csv",
  data_kf_ = "<csv_data>/KF-Werte.csv",
  data_kf = "<csv_data>/kf_mean_filled_HSc.csv",

  # renamings ---
  renamings = dwc.wells::extdata_file("renamings"),
  renamings_main = "<renamings>/main.csv",
  renamings_screen_material = "<renamings>/screen_material.csv",
  renamings_casing_material = "<renamings>/casing_material.csv",
  renamings_well_function = "<renamings>/well_function.csv",
  renamings_waterworks = "<renamings>/waterworks.csv",
  renamings_drilling_method = "<renamings>/drilling_method.csv",
  renamings_surface_water = "<renamings>/surface_water.csv",
  renamings_quality = "<renamings>/quality.csv",
  lookup_actions = "<renamings>/actions.csv",

  # output path ---
  data_prep_out = "<data_prep>/03_data_out",

  # plotting ---
  plotting = "<project>/02_plotting",

  # statistics ---
  statistics = "<project>/03_statistics",

  # modelling ---
  modelling = "<project>/04_modelling"

)
