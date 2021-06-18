#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


make.times <- function(times.numeric, units) {
  timespec <- strsplit(units, ' ')[[1]]
  if (length(timespec) < 3) {
    logger.severe('Something wrong with time:units')
  }
  timeunit <- timespec[1]
  basetime <- lubridate::ymd_hms(timespec[3])
  lapply(times.numeric, function(x) basetime + lubridate::duration(x, units=timeunit))
}


read.met <- function(file.path) {
  nc <- ncdf4::nc_open(file.path)
  times.numeric <- c(ncdf4::ncvar_get(nc, 'time'))
  times <- make.times(times.numeric, ncatt_get(nc, 'time', 'units')$value)

  variables <- c('air_temperature', 'relative_humidity', 'eastward_wind', 'windspeed', 'precipitation_flux',
                 'northward_wind', 'northward_wind', 'surface_downwelling_shortwave_flux_in_air')

  values <- list()
  units <- list()

  for (var in variables) {
    if (var %in% names(nc$var)) {
      values[[var]] <- c(ncdf4::ncvar_get(nc, var))
      units[[var]] <- ncdf4::ncatt_get(nc, var, 'units')$value
    }
  }

  return(list(values=values, units=units, times=times))
}

process.met <- function(values, units, times) {
  if (!('windspeed' %in% names(values))) {
    # evaluate wind speed before averaging
    values[['windspeed']] <- sqrt(values$northward_wind^2 + values$eastward_wind^2)
    units[['windspeed']] <- units[['northward_wind']]
  }

  # Check that all required variables are present
  num_times <- length(times)
  variables.req <- c('air_temperature', 'relative_humidity', 'windspeed', 'precipitation_flux',
                     'surface_downwelling_shortwave_flux_in_air')
  for (var in variables.req) {
    if (!(var %in% names(values))) {
      logger.severe(sprintf('%s not in names(values)', var))
    }
    if (!(var %in% names(units))) {
      logger.severe(sprintf('%s not in names(units)', var))
    }
    if (length(values[[var]]) != num_times) {
      logger.severe(sprintf('length(times) != length(values[[%s]])', var))
    }
  }

  
  # aggregate to daily
  values[['year']] <- vapply(times, lubridate::year, 1)
  values[['doy']] <- vapply(times, lubridate::yday, 1)
  values.daily <- aggregate(values, list(values$year, values$doy), mean)
  values.daily[['tdmin']] <- aggregate(values$air_temperature, list(values$year, values$doy), min)$x
  values.daily[['tdmax']] <- aggregate(values$air_temperature, list(values$year, values$doy), max)$x
  units[['tdmin']] <- units[['tdmax']] <- units[['air_temperature']]

  # collect into a data frame with the basgra format and units

  uc <- function(var, unit) {
    udunits2::ud.convert(values.daily[[var]], units[[var]], unit)
  }
  num.days <- nrow(values.daily)
  weather <- data.frame(ST = rep(-99, num.days),
                        YR = values.daily$year,
                        DOY = values.daily$doy,
                        T = uc('air_temperature', 'degC'),
                        TMMXI = uc('tdmax', 'degC'),
                        TMMNI = uc('tdmin', 'degC'),
                        RH = values.daily$relative_humidity,
                        RAINI = uc('precipitation_flux', 'kg/m2/day'),
                        WNI = uc('windspeed', 'm/s'),
                        GR = uc('surface_downwelling_shortwave_flux_in_air', 'MJ/m2/day'))
                        
  
  
}
                     


##-------------------------------------------------------------------------------------------------#
##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.MODEL
##' @title Write MODEL met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param overwrite logical: replace output files if they already exist?
##' @return OK if everything was succesful.
##' @export
##' @author Rob Kooper
##-------------------------------------------------------------------------------------------------#
met2model.BASGRABGC <- function(in.path, in.prefix, outfolder, start_date, end_date,  overwrite = FALSE) {
  file.in <- file.path(in.path, in.prefix)
  PEcAn.logger::info(sprintf('processing met file: %s', file.in))
  from.file <- read.met(file.in)
  processed <- process.met(as.data.frame(from.file$values), from.file$units, from.file$times)
  # follow SIPNET
  out.file <- paste(in.prefix, strptime(start_date, "%Y-%m-%d"),
                    strptime(end_date, "%Y-%m-%d"),
                    "clim",
                    sep = ".")
  out.file.full <- file.path(outfolder, out.file)
  results <- data.frame(file = out.file.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/plain",
                        formatname = "basgrabgc.clim",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = out.file,
                        stringsAsFactors = FALSE)
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }
  write.table(processed, out.file.full, quote = FALSE, sep = '\t', row.names = FALSE)
  return(results)
  
  # Please follow the PEcAn style guide:
  # https://pecanproject.github.io/pecan-documentation/master/coding-style.html
  
  # Note that `library()` calls should _never_ appear here; instead, put
  # packages dependencies in the DESCRIPTION file, under "Imports:".
  # Calls to dependent packages should use a double colon, e.g.
  #    `packageName::functionName()`.
  # Also, `require()` should be used only when a package dependency is truly
  # optional. In this case, put the package name under "Suggests:" in DESCRIPTION. 
  
} # met2model.MODEL
