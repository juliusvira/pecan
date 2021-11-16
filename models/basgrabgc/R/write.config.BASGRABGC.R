#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

process.traits <- function(trait.values, settings) {
  #if (length(trait.values) > 2 || (length(trait.values) == 2 && names(trait.values)[2] != 'env')) {
  #  # allow env samples 
  #  PEcAn.logger::logger.severe("More than 1 pft not supported")
  #}

  if (!is.null(settings$run$inputs$basgrabgc.param.map.file)) {
    # mainly for injecting a specific parameter map in the unit test
    PEcAn.logger::logger.info("Using", settings$run$inputs$basgrabgc.param.map.file, "as a parameter map")
    param.map <- read.csv(file = settings$run$inputs$basgrabgc.param.map.file)
  } else {
    param.map <- read.csv(file = system.file("basgra-bgc.param.map.csv", package = "PEcAn.BASGRABGC"))
  }

  pft.traits <- unlist(trait.values)
  pft.names <- names(pft.traits)
  if (any(duplicated(pft.names))) {
    PEcAn.logger::logger.severe(paste0(c('Duplicated traits in:', pft.names)))
  }
  # The parameter mapping logic:
  # - the param.map table contains all parameters needed by Basgra-BGC
  # - only parameters that have a defined name.bethy and that name is in pft.names are mapped
  # - conv2basgra unit conversion will be applied. If both unit.basgra and unit.bethy are blank, assume 1
  # - if a parameter is not mapped, the default value is taken
    
  map.row <- function(ind_row) {
    name.bethy <- param.map[ind_row, 'name.bethy']
    if (!is.na(name.bethy) && name.bethy %in% pft.names) {
      conv <- param.map[ind_row, 'conv2basgra']
      if (is.na(conv) && param.map[ind_row, 'unit.basgra'] == '' && param.map[ind_row, 'unit.bethy'] == '') {
        # only assume no conversion if both bethy and basgra quantities are marked unitless
        conv <- 1.0
      }
      value <- pft.traits[which(pft.names == name.bethy)] * conv
    } else {
      value <- param.map[ind_row, 'default']
    }
    if (is.na(value)) {
      PEcAn.logger::logger.severe(sprintf("Failed to map parameter: %s -> %s", param.map[ind_row, 'name.basgra'], name.bethy))
    }
    value
  }

  params <- vapply(seq_len(nrow(param.map)), map.row, 1.0)
  df.params <- data.frame(name=param.map$name.basgra, value=params)
}

##-------------------------------------------------------------------------------------------------#
##' Writes a MODEL config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.BASGRABGC
##' @title Write configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for basgra-bgc for given run
##' @export
##' @author Julius Vira
##-------------------------------------------------------------------------------------------------#
write.config.BASGRABGC <- function(defaults, trait.values, settings, run.id) {
  # Please follow the PEcAn style guide:
  # https://pecanproject.github.io/pecan-documentation/develop/coding-style.html
  # Note that `library()` calls should _never_ appear here; instead, put
  # packages dependencies in the DESCRIPTION file, under "Imports:".
  # Calls to dependent packages should use a double colon, e.g.
  #    `packageName::functionName()`.
  # Also, `require()` should be used only when a package dependency is truly
  # optional. In this case, put the package name under "Suggests:" in DESCRIPTION. 

  
  
  
  # write params into a file

  df.params <- process.traits(trait.values, settings)
  param.file <- file.path(settings$rundir, run.id, "basgrabgc.param")
  conn <- file(param.file, 'w')
  cat(nrow(df.params), file = conn, sep = '\n') # first line in the number of params
  write.table(df.params, file = conn, col.names = FALSE, row.names = FALSE, quote = FALSE)
  close(conn)

  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.BASGRABGC"), n = -1)
  }

  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }
  
  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }
  
  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)

  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  config.file.path <- file.path(outdir, paste0("CONFIG.", run.id, ".txt"))
  jobsh <- gsub("@CONFIG@", config.file.path, jobsh)

  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@",settings$run$end.date , jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  PEcAn.logger::logger.info(sprintf('job script written to %s', file.path(settings$rundir, run.id, "job.sh")))
  
  #-----------------------------------------------------------------------
  ### Edit a templated config file for runs
  if (!is.null(settings$model$config) && file.exists(settings$model$config)) {
    # full path
    PEcAn.logger::logger.info("Using", settings$model$config, "as template")
    config.text <- readLines(con = settings$model$config, n = -1)
  } else {
    filename <- system.file('config.template', package = "PEcAn.BASGRABGC")
    PEcAn.logger::logger.info("Using", filename, "as config template")
    config.text <- readLines(con = filename, n = -1)
  }

  config.text <- gsub("@SITE_MET@", settings$run$inputs$met$path, config.text)
  start.date <- lubridate::parse_date_time(settings$run$start.date, c('%Y-%m-%d %H:%M:%s', '%Y-%m-%d %H:%M', '%Y-%m-%d'))
  
  config.text <- gsub("@START_DOY@", format(start.date, "%j"), config.text)
  config.text <- gsub("@START_YEAR@", format(start.date, "%Y"), config.text)
  end.date <-  lubridate::parse_date_time(settings$run$end.date, c('%Y-%m-%d %H:%M:%s', '%Y-%m-%d %H:%M', '%Y-%m-%d'))
  # basgra-bgc outputs at the start of time step, the last time step does not go to output.
  num_days <- as.integer(difftime(end.date, start.date), units='days') + 1 
  config.text <- gsub("@NUM_DAYS@", num_days, config.text)

  #config.text <- gsub("@OUTDIR@", settings$host$outdir, config.text)
  #config.text <- gsub("@ENSNAME@", run.id, config.text)
  
  filename <- 'basgrabgc.out'
  config.text <- gsub("@PATH_PARAM@", param.file, config.text)
  config.text <- gsub("@OUT_PATH@", file.path(settings$host$outdir, run.id, filename), config.text)
  config.text <- gsub("@PATH_MANAGEMENT@", settings$run$inputs$management$path, config.text)
  if (!is.null(settings$run$inputs$poolinitcond$path)) {
    # Allow overriding soil path using the poolinitcond 
    soil.path <- settings$run$inputs$poolinitcond$path
  } else {
    soil.path <- settings$run$inputs$soil$path
  }
  config.text <- gsub("@PATH_SOIL@", soil.path, config.text)
  
  #-----------------------------------------------------------------------
  
  writeLines(config.text, con = config.file.path)
  PEcAn.logger::logger.info(sprintf('config file written to %s', config.file.path))
} # write.config.MODEL

