#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

make_netcdf <- function(df_output, df_info, nc_path, sitelat, sitelon) {
  df_info_act <- df_info[df_info$name.bethy != '', ]
  year <- df_output[1, 'year']

  if (any(df_output[,'year'] != year)) {
    PEcAn.logger::logger.severe('Only one year per file allowed')
  }

  timedim <- ncdf4::ncdim_def('time',
                              units = sprintf('days since %i-01-01', year),
                              vals = df_output[,'doy']-1,
                              calendar = 'standard',
                              unlim = TRUE)
  latdim <- ncdf4::ncdim_def('lat', 'degrees_north', vals = as.numeric(sitelat), longname = 'station_latitude')
  londim <- ncdf4::ncdim_def('lon', 'degrees_east', vals = as.numeric(sitelon), longname = 'station_longitude')

  var_lst <- list()
  dims = list(lon = londim, lat = latdim, time = timedim)
  # declare variables
  for (ind_var in seq_len(nrow(df_info_act))) {
    var_lst[[ind_var]] <- ncdf4::ncvar_def(df_info_act[ind_var, 'name.bethy'],
                                           units = df_info_act[ind_var, 'unit.bethy'],
                                           dim = dims,
                                           missval = -999,
                                           longname = df_info_act[ind_var, 'long_name'])
  }

  nc <- ncdf4::nc_create(nc_path, var_lst)
  varfile <- file(sprintf('%s.var', nc_path), 'w')
  # write variables to the netcdf & the .var file
  for (ind_var in seq_along(var_lst)) {
    ncdf4::ncvar_put(nc, var_lst[[ind_var]], df_output[, var_lst[[ind_var]]$name])
    cat(paste(var_lst[[ind_var]]$name, var_lst[[ind_var]]$longname), file = varfile, sep = "\n")
  }
  close(varfile)
  ncdf4::nc_close(nc)
}

split_by_year <- function(df) {
  years <- seq(df[1, 'year'], df[nrow(df), 'year'])
  lapply(years, function(year) df[df$year == year,])
}

process_output <- function(df.raw, df.info) {
  varnames <- df.info[df.info$name.basgra != '', 'name.basgra']
  if (ncol(df.raw) != length(varnames) + 3) {
    PEcAn.logger::logger.severe(sprintf('Wrong number of columns in output file (%i expected, %i present',
                                        length(varnames) + 3, ncol(df.raw)))
  }
  colnames(df.raw) <- c('decimal_year', 'year', 'doy', varnames)

  vars2extract <- df.info[df.info$name.bethy != '',]
  convert_ind <- function(ind_row) {
    if (vars2extract[ind_row, 'name.bethy'] == 'NEE') {
      # below, convert g m-2 day-1 to kg m-2 s-1
      result <- (df.raw[,'ECORES'] - df.raw[,'GPP']) * 1e-3 / (24*3600)
    } else if (vars2extract[ind_row, 'name.basgra'] != '') {
      data.raw <- df.raw[,vars2extract[ind_row, 'name.basgra']]
      conv <- vars2extract[ind_row, 'conv2bethy']
      if (is.na(conv)) PEcAn.logger::logger.severe(sprintf('Cannot unit-convert bethy variable: %s',
                                                           vars2extract[ind_row, 'name.bethy']))
      result <- data.raw*conv
    } else {
      PEcAn.logger::logger.severe(sprintf('Cannot convert bethy variable: %s',
                                          vars2extract[ind_row, 'name.bethy']))
    }
    return(result)
  }
  bethy_lst <- lapply(seq_len(nrow(vars2extract)), convert_ind)
  names(bethy_lst) <- vars2extract$name.bethy
  bethy_lst[['year']] <- df.raw[,'year']
  bethy_lst[['doy']] <- df.raw[,'doy']
  df.processed <- as.data.frame(bethy_lst)
  split_by_year(df.processed)
}


date_from_year_doy <- function(year, doy) {
  lubridate::ymd(sprintf('%i-01-01', year)) + lubridate::days(doy-1)
}

# Compose the model2netcdf function from the output writing and output processing
# functions.
get_model2netcdf <- function(make_netcdf_func, get_output_func, get_out_info_func) {
  func <- function(outdir, sitelat, sitelon, start_date, end_date) {
    df_info <- get_out_info_func()
    output_path <- file.path(outdir, "basgrabgc.out") # we assume that run.id is included in outdir
    output.raw <- get_output_func(output_path)
    output <- process_output(output.raw, df_info) # list(df_year1, df_year2, ...)
    num_years <- length(output)
    
    # no subsetting but check that the start, end dates match
    first_date_read <- date_from_year_doy(output[[1]][1, 'year'], output[[1]][1, 'doy'])
    num_days_last_yr <- nrow(output[[num_years]])
    last_date_read <- date_from_year_doy(output[[num_years]][num_days_last_yr, 'year'],
                                         output[[num_years]][num_days_last_yr, 'doy'])

    if (first_date_read != start_date) {
      PEcAn.logger::logger.severe(sprintf('start_date mismatch: %s <> %s',
                                          as.character(first_date_read),
                                          as.character(start_date)))
    } else if (last_date_read != end_date) {
      PEcAn.logger::logger.severe(sprintf('end_date mismatch: %s <> %s',
                                          as.character(last_date_read),
                                          as.character(end_date)))
    }

    for (ind_yr in seq_len(num_years)) {
      yr <- lubridate::year(start_date) + ind_yr - 1
      nc_path <- file.path(outdir, sprintf('%i.nc', yr))
      make_netcdf_func(output[[ind_yr]], df_info, nc_path, sitelat, sitelon)
    }
  }
}


##-------------------------------------------------------------------------------------------------#
##' Convert MODEL output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.BASGRABGC
##' @title Code to convert MODELS's output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Julius Vira
model2netcdf.BASGRABGC <- get_model2netcdf(make_netcdf,
                                           get_output_func = function(filepath) {
                                             read.table(filepath)},
                                           get_out_info_func = function() {
                                             read.csv(system.file('basgra-bgc.output.map.csv',
                                                                  package='PEcAn.BASGRABGC'))}
                                           )
