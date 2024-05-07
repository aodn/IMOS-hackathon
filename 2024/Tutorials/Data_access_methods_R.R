#### 1. Interacting with Geoserver - Use case: discover a layer of interest in the portal, then harvest data programmatically
## Online resources: 
##    https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/
##    https://eblondel.github.io/ows4R/articles/wcs.html

  ## Install packages
  install.packages(c("ows4R", "httr", "tidyverse", "sf"))
  
  ## Load relevant libraries
  rm(list = ls())
  library(ows4R); library(httr); library(tidyverse); library(sf);

  ## Create connection to Geoserver instance
  geoserver_URL <- 'http://geoserver-123.aodn.org.au/geoserver/ows';
  url <- parse_url(geoserver_URL)
  url$query <- list(service = "wfs", request = "GetCapabilities")
  request <- build_url(url); ## The request URL allows to view all metadata stored on a Geoserver instance in a web browser, in XML format

  ## Get metadata on Geoserver layers programmatically
  geo_client <- WFSClient$new(geoserver_URL, serviceVersion = "2.0.0"); 
  
  geo_client$getFeatureTypes(pretty = TRUE); ## List all available layer names and titles, as listed here https://geoserver-123.aodn.org.au/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?2&filter=false
  geo_client$getFeatureTypes() %>% map_chr(function(x){x$getName()}) ## Get only layer names. Requires tidyverse. Can also be obtained by adding $name to the previous command.
  geo_client$getCapabilities()$getOperationsMetadata()$getOperations() %>% map(function(x){x$getParameters()}) %>% pluck(3, "outputFormat") ## Extract available output formats
  geo_client$getCapabilities()$getFeatureTypes() %>% map(function(x){x$getBoundingBox()}) ## Extract bounding boxes for all layers
  geo_client$getCapabilities()$getFeatureTypes() %>% map_chr(function(x){x$getAbstract()}) ## Get abstract about the contents of the layers.
  
  ## List all available fields for layer of interest
  geo_client$describeFeatureType(typeName = "imos:atf_acoustic_qc_detections_map") %>% map_chr(function(x){x$getName()}) 
    
  
  ## WFS download
    ## Entire dataset
    url$query <- list(service = "wfs", request = "GetFeature", typeName = 'imos:atf_acoustic_qc_detections_map', srsName = "EPSG:4326")
    request <- build_url(url);
    dat <- read_sf(request); dim(dat); head(dat)
    
    ## Filter by attribute - numerical column, time and bounding box
    url$query <- list(service = "wfs", request = "GetFeature", typeName = 'imos:atf_acoustic_qc_detections_map', srsName = "EPSG:4326",
                      cql_filter = "Detection_QC<2")
    request <- build_url(url);
    dat_f <- read_sf(request); dim(dat_f); head(dat_f)

    url$query <- list(service = "wfs", request = "GetFeature", typeName = 'imos:aatams_sattag_qc_dm_profile_map', srsName = "EPSG:4326",
                      cql_filter = "timestamp>'2020-01-01'")
    request <- build_url(url);
    dat_f <- read_sf(request); dim(dat_f); head(dat_f)
    
     ## Bounding box filtering - only works on _data layers!
    url$query <- list(service = "wfs", request = "GetFeature", typeName = 'imos:aatams_biologging_snowpetrel_data', srsName = "EPSG:4326",
                      bbox = "-100, -80, 80, -66")
    request <- build_url(url);
    dat_fbb <- read_sf(request); dim(dat_fbb); head(dat_fbb)    
    
    
## -------------------------------------------------------------------------------------------------------- ## 
## 2. Interacting with THREDDS server - Access to single NetCDF file, plus list files for scripting
## Online resources:
    ## https://help.aodn.org.au/downloading-data-from-servers/opendap/
    ## https://rdrr.io/github/bocinsky/thredds/f/README.md - thredds library to be installed from here
    
    ## Install packages
    install.packages("ncdf4")
    
    ## Load relevant libraries
    # remotes::install_github("bocinsky/thredds")
    library(ncdf4); library(thredds); 
  
  ## Locate and use the OPeNDAP URL of a NetCDF file
      nc_url <- 'https://thredds.aodn.org.au/thredds/dodsC/IMOS/Argo/dac/csiro/7900933/profiles/R7900933_015.nc'
      dat <- nc_open(nc_url)
      summary(dat)
      ncatt_get(dat, 0) ## List all global attributes
      summary(dat$var)
      ncatt_get(dat, 'TEMP') ## List all attributes for TEMP variable
      temp <- ncvar_get(dat, 'TEMP'); dim(temp)
      psal <- ncvar_get(dat, 'PSAL');
      pres <- ncvar_get(dat, 'PRES');
      lat <- ncvar_get(dat, 'LATITUDE'); lon <- ncvar_get(dat, 'LONGITUDE');
      date <- ncvar_get(dat, 'JULD_LOCATION'); ncatt_get(dat, 'JULD_LOCATION'); as.POSIXct(date * 24 * 60 * 60, origin = '1950-01-01', tz = 'UTC')
      plot(temp[,1], -pres[,1], type = 'l'); lines(temp[,2], -pres[,2], col = 'red')
  
  ## Explore content of THREDDS server
      ## Base URL
      loca_url <- 'https://thredds.aodn.org.au/thredds/'
      datasets <- tds_list_datasets(thredds_url = loca_url)
      datasets
      ## Drilling down - 1
      loca_url <- gsub('//thredds/', '/', datasets[datasets$dataset == "IMOS/",]$path)
      loca_url
      datasets <- tds_list_datasets(thredds_url = loca_url)
      datasets    
      ## Drilling down - 2
      loca_url <- gsub('//thredds/', '/', datasets[datasets$dataset == "AATAMS/",]$path)
      loca_url
      loca_url
      datasets <- tds_list_datasets(thredds_url = loca_url)
      datasets
      ## And on, and on. OR you could go straight to base URL in your browser and paste it as `final_url` below
      final_url = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/AATAMS/satellite_tagging/MEOP_QC_CTD/ct36/catalog.html'
      datasets <- tds_list_datasets(thredds_url = final_url)
      datasets
      loca_url <- datasets[2,]$path ## Select second dataset in list. There could be done programmatically, i.e. for (i in 2:nrow(datasets)){}
      loca_url
      loca_services <- tds_list_services(loca_url)
      loca_services
      dat <- nc_open(gsub('.html', '', loca_services$path[1])); ## Read NetCDF file
        ncatt_get(dat, 0); summary(dat$var) ## Start processing
    
    
    
    
## -------------------------------------------------------------------------------------------------------- ## 
## 3. Interacting with AWS S3 bucket
## Online resources:
    ## https://help.aodn.org.au/downloading-data-from-servers/amazon-s3-servers/
    ## https://github.com/cloudyr/aws.s3 
    ## To use the above package, you will need an to generate access keys through the creation of an AWS account
    ## https://github.com/apache/arrow/blob/main/r/cheatsheet/arrow-cheatsheet.pdf ## for reading Parquet files
    ## https://bioconductor.org/packages/release/bioc/vignettes/Rarr/inst/doc/Rarr.html ## for reading Zarr files. Installed via https://www.bioconductor.org/packages/release/bioc/html/Rarr.html. More Zarr-related resources https://www.r-bloggers.com/2022/09/reading-zarr-files-with-r-package-stars/
    
    ## Install packages
    install.packages(c("aws.s3", "arrow"))
    
    if (!require("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    
    ## Install Rarr
    BiocManager::install("Rarr")
      
    library(aws.s3); library(dplyr); library(arrow); library(Rarr);
    bucket_exists(bucket = "s3://imos-data/", region = "ap-southeast-2") ## Most of AODN data is stored on Amazon Web Services S3 object storage (AWS S3). http://data.aodn.org.au/
    
    ## List bucket content
    get_bucket(bucket = 's3://imos-data/', region = "ap-southeast-2") ## List all objects in a public bucket, rendered as list
    get_bucket_df(bucket = "s3://imos-data/", region = "ap-southeast-2", max = 200) %>% as_tibble() ## List first 200 objects in a public bucket, rendered as data frame. Use max = Inf to list all files
    get_bucket_df(bucket = "s3://imos-data/", region = "ap-southeast-2", prefix="IMOS/AATAMS/satellite_tagging/ATF_Location_QC_DM") %>% as_tibble() ## List all files in subdirectory
    
    ## Downloads
      ## Individual file
      save_object(object = "IMOS/AATAMS/satellite_tagging/ATF_Location_QC_DM/wd15_dm.zip", bucket = "s3://imos-data/", 
                  region = "ap-southeast-2", file = "~/Downloads/wd15_dm.zip")
      ## All in subdirectory
      keys <- get_bucket_df(bucket = "s3://imos-data/", region = "ap-southeast-2", prefix="IMOS/AATAMS/satellite_tagging/MEOP_QC_CTD") %>% as_tibble() 
      keys <- keys$Key ## List all files in subdirectory using the `prefix` argument. Potential for filtering by LastModified field
      for (i in 1:length(keys)){
          key <- keys[i]; 
          save_object(object = key, bucket = "s3://imos-data/", region = "ap-southeast-2", file = paste0("~/Downloads/", basename(key)))
      }




  
## -------------------------------------------------------------------------------------------------------- ## 
## 4. Interacting with Parquet datasets - not available yet through IMOS
  get_bucket_df(bucket = "s3://gbr-dms-data-public/", region = "ap-southeast-2", max = 200) %>% as_tibble()
  dat <- save_object(object = "abs-lgas-2021/data.parquet/part.0.parquet", bucket = "s3://gbr-dms-data-public/", region = "ap-southeast-2") ## Not working
  save_object(object = "abs-lgas-2021/data.parquet/part.0.parquet", bucket = "s3://gbr-dms-data-public/", region = "ap-southeast-2", file = '~/Downloads/abs.parquet') ## Download Parquet file on local machine
  dat <- arrow::read_parquet("s3://gbr-dms-data-public/abs-lgas-2021/data.parquet/part.0.parquet") ## Read S3 Parquet file directly
  
  


  
## -------------------------------------------------------------------------------------------------------- ## 
## 5. Interacting with Zarr datasets - not available yet through IMOS
  
  # Install packages
  install.packages("paws.storage")
  
  # Working example
  s3_address <- "https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.4/idr0076A/10501752.zarr/0"
  s3_client <- paws.storage::s3(
    config = list(
      credentials = list(anonymous = TRUE), 
      region = "auto",
      endpoint = "https://uk1s3.embassy.ebi.ac.uk")
  )
  zarr_overview(s3_address, s3_client = s3_client)
  z2 <- read_zarr_array(s3_address, s3_client = s3_client, index = list(c(1, 10), NULL, NULL))
  
  ## plot the first slice in blue
  image(log2(z2[1, , ]),
        col = hsv(h = 0.6, v = 1, s = 1, alpha = 0:100 / 100),
        asp = dim(z2)[2] / dim(z2)[3], axes = FALSE
  )
  ## overlay the tenth slice in green
  image(log2(z2[2, , ]),
        col = hsv(h = 0.3, v = 1, s = 1, alpha = 0:100 / 100),
        asp = dim(z2)[2] / dim(z2)[3], axes = FALSE, add = TRUE
  )
  
  ## Another working example - based on group of arrays
  s3_address <- "https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr"
  s3_client <- paws.storage::s3(
    config = list(
      credentials = list(anonymous = TRUE), 
      region = "auto",
      endpoint = "https://ncsa.osn.xsede.org")
  )
  zarr_overview(s3_address, s3_client = s3_client) ## precip shape indicates it follows time x latitude x longitude
  
    ## Refine - precipitation dataset
    s3_address <- "https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr/precip"
    s3_client <- paws.storage::s3(
      config = list(
        credentials = list(anonymous = TRUE), 
        region = "auto",
        endpoint = "https://ncsa.osn.xsede.org")
    )
    zarr_overview(s3_address, s3_client = s3_client)
    z2 <- read_zarr_array(s3_address, s3_client = s3_client, index = list(c(1, 9225), NULL, NULL))
    ## Refine - longitude
    s3_address <- "https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr/longitude"
    s3_client <- paws.storage::s3(
      config = list(
        credentials = list(anonymous = TRUE), 
        region = "auto",
        endpoint = "https://ncsa.osn.xsede.org")
    )
    zarr_overview(s3_address, s3_client = s3_client)
    lon <- read_zarr_array(s3_address, s3_client = s3_client)
    ## Refine - latitude
    s3_address <- "https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr/latitude"
    s3_client <- paws.storage::s3(
      config = list(
        credentials = list(anonymous = TRUE), 
        region = "auto",
        endpoint = "https://ncsa.osn.xsede.org")
    )
    zarr_overview(s3_address, s3_client = s3_client)
    lat <- read_zarr_array(s3_address, s3_client = s3_client)                
    image(z2[1, , ], axes = F) 
    axis(1, at = seq(0, 359, 20)/360, labels = seq(lon[1], lon[359], 20), axis(2, at = seq(0, 1, 30/180)/180, labels = seq(lat[1], 90, 30)))
    
    
    
    
    
