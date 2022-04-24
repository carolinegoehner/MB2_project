################################################
###    COMBINED DROUGHT INDICATOR (CDI)      ###
### developed by Sepulcre-Canto et al., 2012 ###
################################################

# 2022-04-23
# RStudio 2021.09.0 "Ghost Orchid" - Running on Windows 11

# Caroline Göhner
# University of Würzburg, EAGLE M.Sc
# caroline.goehner@stud-mail.uni-wuerzburg.de
# Github: https://github.com/carolinegoehner

# Project-script - Introduction to Programming and Geostatistics (04-GEO-MB2),
# Department of Remote Sensing, Dr. Martin Wegmann




#_______________________________________________________________________________
# Idea:
# The idea to this program came through an instagram post by ESA earth on 1st of March 2022:
# https://www.instagram.com/p/CakVAiZMu1a/?utm_source=ig_web_copy_link
# where ESA used the NDMI(Normalized Difference Moisture Index) based on NIR and SWIR to droughts in the Iberian Peninsula.

# Aim:
# This program aims to combine further indexes to determine drought for a selectable region of interest in Europe, within
# a selectable time period. Using the Combined Drought Index (CDI), the region is divided into four different categories of
# identified drought to monitor drought trends. This index also includes previous conditions, as in the ESA post, but does
# not include only 3 previous years, but can go back to 2000, or 1950 in the case of precipitation data.

# Requirements:
# No data is needed, all data required for the calculation of the CDI is downloaded in this script: Precipitation data from E-OBS,
# remote sensing data from MODIS, a basemap from OSM and, depending on what region of interest you chose, Natural Earth data.
# No additional scripts or software are needed.
# The output will be a PDF containing a map and a barplot with the area of the respective categories for your selected area.
# The needed packages are installed and loaded automatically further down in the script if they are not already present. They
# include the following packages: basemaps, getPass, ggmap, ggspatial, gridExtra, lubridate, mapview, MODIStsp, ncdf4, Orcs,
# pacman, raster, rgdal, rts, sf, sp, svDialogs, and zoo.

# This program is structured into the following sections to make it easier to read:
# -> What is the CDI?
# -> Needed Packages
# -> Setting of Variables for Your Investigation
# -> (1) SPI - Standardised Precipitation Index
# -> (2) NDMI anomaly and (3) NDVI anomaly
# -> Calculation of CDI




#_______________________________________________________________________________
# WHAT IS THE CDI?

# Three drought indicators are combined:
# 1) Standardized Precipitation Index (SPI)
#  - precipitation anomalies, comparison of observed total prec. amounts for period if interest (1, 3, 12 - 48 months)
#    with long-term historic rainfall for that period
#  - instead of the non-transparent spi function provided by the SPEI package, the calculation is made with a simple formula,
#    as with the two other indicators
#  - calculated using the E-OBS precipitation dataset for Europe, Version 25.0e currently available from 1950-01-01 to 2021-12-31

# 2) Soil Moisture Anomaly (SMA)
#    substituted here by the Normalized Differenced Moisture Index Anomaly (NDMIA)
#  - calculated using monthly MODIS Terra data, available since 2000-02-01

# 3) FAPAR (Fraction of Absorbed Photo-synthetically Active Radiation) Anomaly
#    substitued here by the Normalized Differenced Vegetation Index Anomaly (NDVIA)
#    as also done by Pilar Jiménez-Donaire et al., 2020
#  - calculated using monthly MODIS Terra data, available since 2000-02-01

# Classifies areas according to 4 primary drought categories:
# (1) "Watch" - precipitation deficit,
# (2) "Warning" - soil moisture deficit,
# (3) "Alert type I" - vegetation stress following precipitation deficit, and
# (4) "Alert type II" - vegetation stress following precipitation/soil moisture deficit

# References:
# Copernicus European Drought Observatory (EDO), 2019. EDO Indicator Factsheet. Combined Drought Indicator (CDI). Available online:
#  https://edo.jrc.ec.europa.eu/documents/factsheets/factsheet_combinedDroughtIndicator.pdf.
# Cornes, R. et al., 2018. An Ensemble Version of the E-OBS Temperature and Precipitation Datasets.
#  J. Geophys. Res. Atmos., 123. doi:10.1029/2017JD028200.
# Didan, K., 2021. MODIS/Terra Vegetation Indices Monthly L3 Global 1km SIN Grid V061. NASA EOSDIS Land Processes DAAC.
#  doi:10.5067/MODIS/MOD13A3.061.
# Natural Earth. Free vector and raster map data. Available online: https://www.naturalearthdata.com.
# Pilar Jiménez-Donaire, M. et al., 2020. Evaluation of a combined drought indicator and its potential for agricultural drought
#  prediction in southern Spain. Nat. Hazards Earth Syst. Sci., 20, 21-33, 2020, doi.org/10.5194/nhess-20-21-2020.
# Sepulcre-Canto, G. et al., 2012. Development of a Combined Drought Indicator to detect agricultural drought in Europe.
#  Nat. Hazards Earth Syst. Sci., 12, 3519-3531, 2012, doi:10.5194/nhess-12-3519-2012.




# ______________________________________________________________________________
# NEEDED PACKAGES:

# pacman will install and load all needed packages if they are not already present
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dependencies = TRUE)
}

# needed packages for this script:
p_load(basemaps, getPass, ggmap, ggspatial, gridExtra, lubridate, mapview, MODIStsp, ncdf4, Orcs, raster, rgdal,
       rts, sf, sp, svDialogs, zoo, install = TRUE)

# needed packages for MODIStsp:
p_load(leaflet, shiny, shinydashboard, shinyFiles, shinyalert, rappdirs, shinyjs, leafem, mapedit, magrittr)




# ______________________________________________________________________________
# SETTING OF VARIABLES FOR YOUR INVESTIGATION:

# select output directory, (change) name of the folder that will be created to store the data in
setwd(dlg_dir(gui = .GUI)$res)
out_dir <- "data/"

if(dir.exists(out_dir)){
  print(paste0(out_dir, " already exists."))
} else {
  dir.create(out_dir)
  print("The output directory is created.")
}

# Please define here your region of interest in Europe: To reduce the runtime of the script (download of MODIS data!),
# select a small roi, e.g. using the provided polygon for the region around Seville, Spain (c.a. 7500 km²).
# You can also draw your own roi or upload your study area as a shapefile or geoPackage.
# Furthermore, you can calculate the CDI for a specific country entering its English name here. For this
# purpose, the country boundaries from Natural Earth are downloaded and used further down in the script.
# Naming "Europe" is the largest region you can choose, as data is available only for Europe.

my_roi <- ext2spy(extent(c(-6.388723, -5.40123, 36.8174, 37.59289)), as_sf = TRUE)
# my_roi <- draw_ext()
# my_roi <- "Europe"
# my_roi <- "Luxembourg"
# my_roi <- readOGR("D:/Documents/path/to/your_shape_or_geopkg.shp/gpkg")
mapview(my_roi)

# Period you want to investigate, e.g. 2021
# note: at the moment, data is only available until 2021-12-31
my_year <- as.numeric(dlg_input(message = "Please enter the year you want to calculate the CDI for.", default = "2021",
                                           gui = .GUI)$res)

# the 3-months period within the growing season (April-October) you want to investigate, e.g. April to June
my_months <- match(dlg_list(month.name, multiple = TRUE, preselect = c("April", "May", "June"),
                            title = "Please select a 3 months-period within the growing season.", gui = .GUI)$res, month.name)

# Long-term historical period you want to compare your chosen period to, e.g 1991-2020 (only for SPI calculation)
my_hist <- as.numeric(dlg_list(choices = 1950:2021, multiple = TRUE, preselect = c(1991:2020),
                               title = "Please select the long-term period.", gui = .GUI)$res)




# ______________________________________________________________________________
# (1) SPI - STANDARDISED PRECIPITATION INDEX

# ------------------------------------------------------------------------------
# 1. Download and import of precipitation data from E-OBS:

http <- "http://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/Grid_0.1deg_reg_ensemble/rr_ens_mean_0.1deg_reg_v25.0e.nc"
file_name <- "prec.nc"

if(file.exists(paste0(out_dir, file_name))){
  print(paste0(file_name, " already exists."))
} else {
  print("The file will be downloaded now. This will take about 5 minutes.")
  download.file(http, destfile = paste0(out_dir, file_name), mode = "wb") # 1,5 GB, takes about 5 mins.
}

prec <- brick(paste0(out_dir, file_name))



# ------------------------------------------------------------------------------
# 2. Keep only data of chosen months for actual and long-term historical period

# creating subsets using bandnames is much faster than using indexing with expressions
# for first bandnr and last bandnumber of period to be investigated
start_bnd <- bandnr(prec[[paste0("X", format(as_date(as.yearmon(paste(my_year, my_months[1], sep = "-")), frac = 0), "%Y.%m.%d"))]])
end_bnd <- bandnr(prec[[paste0("X", format(as_date(as.yearmon(paste(my_year, tail(my_months, 1), sep = "-")), frac = 1), "%Y.%m.%d"))]])
my_prec <- prec[[start_bnd:end_bnd]]

# subsetting historical period for comparison
start_bnd <- bandnr(prec[[paste0("X", format(as_date(as.yearmon(my_hist[1]), frac = 0), "%Y.%m.%d"))]])
end_bnd <- bandnr(prec[[paste0("X", format(ceiling_date(as_date(as.yearmon(tail(my_hist, 1)), frac = 1), unit = "years") - 1, "%Y.%m.%d"))]])
my_hist_prec <- prec[[start_bnd:end_bnd]]
# to keep only chosen months of historical period
my_hist_prec <- subset(my_hist_prec, which(month(as_date(names(my_hist_prec), format = 'X%Y.%m.%d', tz = NULL)) %in% my_months))

# removing objects that will no longer be used
rm(prec)



# ------------------------------------------------------------------------------
# 3. Crop data to chosen roi

# function to crop and mask a raster (layer/stack/brick) depending on what roi is chosen
funCROP <- function(x){

  if(class(my_roi) == "character"){

    # if a country is named Natural Earth´s shapefile containing country borders worldwide is downloaded and
    # used to crop and mask the raster data
    if(my_roi != "Europa"){
      print(paste0("Your chosen roi is: ", my_roi, ". The data will be cropped to its extent. This may take a few minutes."))
      http <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip"
      file_name <- "ne_10m_admin_0_countries.zip"

      if(file.exists(paste0(out_dir, file_name))){
        print(paste0(file_name, " already exists."))
      } else {
        print("The file will be downloaded now. This will take only a few seconds.")
        download.file(http, destfile = paste0(out_dir, file_name), mode = "wb")
      }

      unzip(zipfile = paste0(out_dir, file_name), exdir = substr(out_dir, 1, nchar(out_dir)-1))
      countries <- readOGR(paste0(out_dir, substr(file_name, 1, nchar(file_name)-3), "shp"))
      mask(crop(x, countries[countries$NAME == my_roi,]), countries[countries$NAME == my_roi,])

    # if Europe is chosen nothing changes
    } else if (my_roi == "Europa") {
      print("Your roi is Europa, the whole data extent will be used.")
    }

  # if an own polygon is chosen it will be applied as roi
  } else if (class(my_roi) == "SpatialPolygonsDataFrame") {
    print("Your roi is your polygon. The data will be cropped to its extent. This may take a few minutes.")
    my_prec <- mask(crop(x, my_roi), my_roi)

  # if a polygon is drawed it will be applied as roi
  } else if (class(my_roi) == "sf") {
    print("Your roi is your drawed polygon. The data will be cropped to its extent.")
    my_prec <- mask(crop(x, my_roi), my_roi)

   # in case something is wrong with the roi
  } else {
    print("Please check your defined roi.")
  }
}

my_prec <- funCROP(my_prec)
my_hist_prec <- funCROP(my_hist_prec)



# ------------------------------------------------------------------------------
# 4. SPI calculation

# click here to see the non-transparent and unused formula for SPI calculation provided in the SPEI package:
# ----
# to calculate the SPI for gridded data (RasterBrick) it has to be converted to a matrix
#my_prec <- stack(my_prec, my_hist_prec)
#spi_matrix <- as.matrix(my_prec)

# a function is defined to calculate the spi:
# the time-series object with the values of the SPI is converted to numeric and passed again to matrix in applying
# start and end in time-series object include the time span of the historical and current data
#funSPI <- function(x, sc, start, end, na.rm = TRUE, ...){
#   dat <- ts(x, start = c(my_hist[1], my_months[1]), end = c(my_year, tail(my_months, 1)), frequency = 2)
#   as.numeric((spi(dat, sc, ref.start = start, ref.end = end, na.rm = na.rm, ...))$fitted)
#}

# scale represents the current months to calculate SPI for, e.g. scale = 3 for 3 months,
# start and end here stand for the historical reference period
#spi_matrix <- t(apply(spi_matrix, 1, funSPI, sc = length(my_months), start = c(my_hist[1], my_months[1]),
#                      end = c(tail(my_hist, 1), tail(my_months, 1))))

# a raster can now be populated with the gained SPI values and averaged over the current months
#SPI <- mean(subset(setValues(my_prec, spi_matrix),
#                   which(year(as_date(names(setValues(my_prec, spi_matrix)), format = 'X%Y.%m.%d', tz = NULL)) %in% my_year)),
#                   na.rm = TRUE)

# removing objects that will no longer be used
#rm(my_prec, spi_matrix)
# ----

# instead using the following transparent and simpler formula:
# sum precipitation yearly, mean for long-term historical period
my_prec <- sum(my_prec)
my_hist_prec <- mean(zApply(setZ(my_hist_prec, as.Date(names(my_hist_prec), format = 'X%Y.%m.%d', tz = NULL)), by = year, fun = sum))

SPI <- (my_prec - my_hist_prec) / sd(my_hist_prec[], na.rm = TRUE)

# removing objects that will no longer be used
rm(my_prec, my_hist_prec)




#_______________________________________________________________________________
# (2) NDMI ANOMALY AND (3) NDVI ANOMALY

# ------------------------------------------------------------------------------
# 1. Download of MODIS data:

# function for downloading MODIS MOD13A3 for chosen date and roi
funMODIS <- function(my_year){
  MODIStsp(
    gui = FALSE,
    out_folder = substr(out_dir, 1, nchar(out_dir)-1),
    selprod =  "Vegetation_Indexes_Monthly_1Km (M*D13A3)",
    bandsel = c("NDVI", "b2_NIR", "b7_SWIR"),
    sensor = "Terra",
    downloader = "http",
    user = user,
    password = password,
    start_date = format(as_date(as.yearmon(paste(my_year, my_months[1], sep = "-")), frac = 0), "%Y.%m.%d"),
    end_date = format(as_date(as.yearmon(paste(my_year, tail(my_months, 1), sep = "-")), frac = 1), "%Y.%m.%d"),
    spatmeth = "bbox",
    bbox = matrix(as.numeric(st_bbox(SPI)), ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))),
    out_projsel = "User Defined",
    output_proj = as.character(crs(SPI)),
    delete_hdf = TRUE,
    parallel = TRUE)
}

# looping over the years to avoid downloading the data for every month of a year
# 99 files (9 per year), takes about 10 mins for region around Sevilla:

# if you get this "Error: http server seems to be down! Please try again later. Aborting!"
# please go to your EOSDIS Earthdata profile page and manually add "LP DAAC Data Pool" under Applications > Authorized Apps

for(i in 1:length(c((my_year - 10):my_year))){
  if (i == 1){
    my_year <- my_year - 10
    print(paste0("Downloading MODIS data for: ", my_year))
    user <- dlg_input(message = "Please enter your username for EOSDIS Earthdata by NASA to download MODIS data: ", gui = .GUI)$res
    password <-  getPass("Please enter your password: ")
    sapply(my_year, FUN = funMODIS)
  } else {
    my_year <- my_year + 1
    print(paste0("Downloading MODIS data for: ", my_year))
    sapply(my_year, FUN = funMODIS)
  }
}



# ------------------------------------------------------------------------------
# 2. Calculation of NDMIA (NDMI anomaly)

# note: as only the MODIS band 7 SWIR is available within the monthly MOD13A3 dataset,
# NDMI will be calculated using that one instead of the normally used band 6

# NDMI = (NIR - SWIR) / (NIR + SWIR)
# NDMIA = (NDMIi - NDMIµ) / sd(NDMIµ) - with NDMIi being the NDMI for actual period and NDMIµ the long-term NDMI mean

# function to import the MODIS data as raster stacks as means
funSTACK_MEANi <- function(path) {
  mean(stack(list.files(path = path, pattern = paste0(my_year, '.*\\.tif$'),
                        all.files = TRUE, full.names = TRUE)))
}
# similar function, inverting the selection to process long-term historical period other than chosen actual year
funSTACK_MEANµ <- function(path) {
  mean(stack(grep(list.files(path = path, full.names = TRUE), pattern = paste0(my_year, '.*\\.tif$'),
                              invert = TRUE, value = TRUE)))
}

# calculate NDMI for the chosen period using downloaded NIR and SWIR bands
NDMIi <- overlay(NIRi <- funCROP(funSTACK_MEANi("data/VI_Monthly_1Km_v6/b2_NIR")),
                 SWIRi <- funCROP(funSTACK_MEANi("data/VI_Monthly_1Km_v6/b7_SWIR")),
                 fun = function(NIRi, SWIRi){(NIRi - SWIRi) / (NIRi + SWIRi)},
                 rm(NIRi, SWIRi))

# long-term historical NDMI mean; this may take a few seconds
NDMIµ <- overlay(NIRµ <- funCROP(funSTACK_MEANµ("data/VI_Monthly_1Km_v6/b2_NIR")),
                 SWIRµ <- funCROP(funSTACK_MEANµ("data/VI_Monthly_1Km_v6/b7_SWIR")),
                 fun = function(NIRµ, SWIRµ){(NIRµ - SWIRµ) / (NIRµ + SWIRµ)},
                 rm(NIRµ, SWIRµ))

# calculation of NDMI anomaly, dividing by the standard deviation
NDMIA <- (NDMIi - NDMIµ) / sd(NDMIµ[], na.rm = TRUE)

# removing objects that will no longer be used
rm(NDMIi, NDMIµ)



# ------------------------------------------------------------------------------
# 3. Calculation of NDVIA (NDVI anomaly)

# NDVI = (NIR — Red) / (NIR + Red)
# NDVIA = (NDVIi - NDVIµ) / sd(NDVIµ) - with NDVIi being the NDMI for actual period and NDVIµ the long-term NDVI mean

# using downloaded monthly NDVI
NDVIA <- overlay(NDVIi <- funCROP(funSTACK_MEANi("data/VI_Monthly_1Km_v6/NDVI")),
                 NDVIµ <- funCROP(funSTACK_MEANµ("data/VI_Monthly_1Km_v6/NDVI")),
                 fun = function(NDVIi, NDVIµ){(NDVIi - NDVIµ) / sd(NDVIµ[], na.rm = TRUE)},
                 rm(NDVIi, NDVIµ))




#_______________________________________________________________________________
# CALCULATION OF CDI

# ------------------------------------------------------------------------------
# 1. CDI "classification"

# resample SPI
SPI <- resample(SPI, NDMIA, method = "bilinear")

# to generate a CDI raster containing the four different categories/classes, the single components are added up,

SPI[SPI >= (-1)] <- 0
SPI[SPI < (-1)] <- 1

NDMIA[NDMIA >= (-1)] <- 0
NDMIA[NDMIA < (-1)] <- 2

NDVIA[NDVIA >= (-1)] <- 0
NDVIA[NDVIA < (-1)] <- 4

CDI <- overlay(SPI, NDMIA, NDVIA, fun = function(x) sum(x))
# "Watch": SPI < -1 --> 1
# "Warning": SPI < −1, NDMIA < −1 --> 1 + 2 = 3
# "Alert type I": SPI < −1, NDVIA < −1  --> 1 + 4 = 5
# "Alert type II": SPI < −1, NDMIA < −1, NDVIA < −1  --> 1 + 2 + 4 = 7

# all even numbers do not include SPI < -1, but is needed for every category
CDI[(CDI %% 2) == 0] <- NA



# ------------------------------------------------------------------------------
# 2. Mapping CDI

# transform CDI raster to a data.frame to plot discrete values
CDI_df <- cbind(xyFromCell(CDI, seq_len(ncell(CDI))), as.data.frame(getValues(CDI)))
names(CDI_df) <- c("x", "y", "value")

# defining aesthetics for the map to display raster in discrete values (CDI categories)
CDI_values <- c(1, 3, 5, 7)
CDI_names <- c("Watch", "Warning", "Alert type I", "Alert type II")
CDI_colors <- c("goldenrod1", "orange2", "orangered3", "red4")
CDI_colors <- c("lightgoldenrod1", "goldenrod2", "red4", "purple")
names(CDI_colors) <- CDI_values

# map also containing OSM basemap
map <- ggmap(get_map(bbox(CDI), source = "osm")) +
  geom_raster(data = CDI_df, aes(x = x, y = y, fill = as.character(value)), alpha = 0.35) +
  coord_sf(crs = 4326) +
  scale_fill_manual(name = "CDI Category",
                    values = CDI_colors,
                    labels = CDI_names,
                    na.translate = TRUE) +
  labs(title = "Combined Drought Indicator",
       subtitle = "Developed by Sepulcre-Canto et al., 2012 \nApplication by Caroline Göhner, University of Würzburg, EAGLE M.Sc",
       caption = "Contains: E-OBS; modified MODIS MOD13A3; OpenStreetMap contributors.") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8)) +
  theme(legend.position = c(1.1, 0.8)) +
  annotation_north_arrow(style = north_arrow_minimal) +
  annotation_scale(pad_x = unit(2, "cm"))
map



# ------------------------------------------------------------------------------
# 3. Plotting CDI area

# preparing a data.frame for the plot
area <- data.frame(tapply(area(CDI), CDI[], sum))
names(area) <- "area"
area$names[is.na(row.names(area))] <- NA
area$names[row.names(area) == 1] <- CDI_names[1]
area$names[row.names(area) == 3] <- CDI_names[2]
area$names[row.names(area) == 5] <- CDI_names[3]
area$names[row.names(area) == 7] <- CDI_names[4]

# barplot containing area of CDI classes (in km² as pixel size is 1x1km)
barplot <- ggplot(data = area, aes(x = reorder(names, c(1, 2, 3, 4)), y = area)) +
  geom_col(fill = CDI_colors, alpha = 0.8, width = 0.5) +
  labs(title = "Area of the respective CDI categories",
       x = "", y = "Area in km²") +
  geom_text(aes(label = round(area, 0)), y = 0, vjust = -2)
barplot



# ------------------------------------------------------------------------------
# 4. Saving CDI map and plot in one pdf

# the plot is saved in your selected workspace
pdf(paste0("CDI_", my_year, ".pdf"), width = 15, height = 10)
gridExtra::grid.arrange(map, barplot, layout_matrix = cbind(c(1, 1, 1, 1), c(1, 1, 1, 1), c(NA, NA, 2, 2)))
dev.off()
