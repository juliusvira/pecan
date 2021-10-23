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
    PEcAn.logger::logger.severe('Something wrong with time:units')
  }
  timeunit <- timespec[1]
  formats <- c('%Y-%m-%d %H:%M:%s','%Y-%m-%d %H:%M', '%Y-%m-%d')
  basetime <- lubridate::parse_date_time(timespec[3], formats)
  lapply(times.numeric, function(x) basetime + lubridate::duration(x, units=timeunit))
}


read.met <- function(file.path) {
  nc <- ncdf4::nc_open(file.path)
  times.numeric <- c(ncdf4::ncvar_get(nc, 'time'))
  times <- make.times(times.numeric, ncdf4::ncatt_get(nc, 'time', 'units')$value)

  variables <- c('air_temperature', 'relative_humidity', 'eastward_wind', 'wind_speed', 'precipitation_flux',
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
  #browser()
  if (!('wind_speed' %in% names(values))) {
    # evaluate wind speed before averaging
    values[['wind_speed']] <- sqrt(values$northward_wind^2 + values$eastward_wind^2)
    units[['wind_speed']] <- units[['northward_wind']]
  }

  # Check that all required variables are present
  num_times <- length(times)
  variables.req <- c('air_temperature', 'relative_humidity', 'wind_speed', 'precipitation_flux',
                     'surface_downwelling_shortwave_flux_in_air')
  for (var in variables.req) {
    if (!(var %in% names(values))) {
      PEcAn.logger::logger.severe(sprintf('%s not in names(values)', var))
    }
    if (!(var %in% names(units))) {
      PEcAn.logger::logger.severe(sprintf('%s not in names(units)', var))
    }
    if (length(values[[var]]) != num_times) {
      PEcAn.logger::logger.severe(sprintf('length(times) != length(values[[%s]])', var))
    }
  }

  # aggregate to daily
  values[['year']] <- vapply(times, lubridate::year, 1)
  values[['doy']] <- vapply(times, lubridate::yday, 1)
  # !!note that aggregate returns the rows ordered by the aggregation variables!!
  values.daily <- aggregate(values, list(values$doy, values$year), mean)
  values.daily[['tdmin']] <- aggregate(values$air_temperature, list(values$doy, values$year), min)$x
  values.daily[['tdmax']] <- aggregate(values$air_temperature, list(values$doy, values$year), max)$x
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
                        WNI = uc('wind_speed', 'm/s'),
                        GR = uc('surface_downwelling_shortwave_flux_in_air', 'MJ/m2/day'))
                        
  
  
}
                     

compose.met2model <- function(read.met.fcn, process.met.fcn) {
  # we parametrize the sub-functions to make testing easier
  met2model.fcn <- function(in.path, in.prefix, outfolder, start_date, end_date,  overwrite = FALSE, full.path = FALSE, ...) {
    # full.path => just read that file
    if (full.path) {
      files.in <- file.path(in.path, in.prefix)
    } else {
      # yearly files
      files.in <- vapply(seq(lubridate::year(start_date), lubridate::year(end_date)),
                         function(y) file.path(in.path, sprintf('%s.%i.nc', in.prefix, y)),
                         character(1))
    }
    # read and join all data. Do this before aggregation, because the input files might
    # not be exactly yearly.
    inputs.read <- lapply(files.in,
                          function(name) {
                            PEcAn.logger::logger.info(sprintf('processing met file: %s', name))
                            read.met.fcn(name)})
    # check consistency
    for (input in inputs.read) {
      if (any(unlist(input$units) != unlist(inputs.read[[1]]$units))) {
        PEcAn.logger::logger.severe('Units differ in files')
      }
    }
    all.values <- do.call(rbind, lapply(inputs.read, function(ip) as.data.frame(ip$values)))
    all.times <- do.call(c, lapply(inputs.read, function(ip) ip$times))
    processed <- process.met.fcn(all.values, inputs.read[[1]]$units, all.times)
    
    daydiff <- diff(processed$doy)
    if (any(daydiff > 1) || any(daydiff < 0 & daydiff > -365)) {
      PEcAn.logger::logger.severe('Inconsistent met times')
    }
    
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
  } 
  return(met2model.fcn)
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
##' @author Julius Vira
##-------------------------------------------------------------------------------------------------#
met2model.BASGRABGC <- compose.met2model(read.met, process.met)
