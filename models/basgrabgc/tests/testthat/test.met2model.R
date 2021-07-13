context("met2model")

outfolder <- tempfile()
setup(dir.create(outfolder, showWarnings = FALSE))
teardown(unlink(outfolder, recursive = TRUE))

ymdf <- lubridate::ymd_hm

test_that('make.times works with seconds', {
  seconds = c(3600, 36*3600, 0)
  datetimes <- make.times(seconds, 'seconds since 2001-02-01T00:00:00')
  expect_equal(datetimes[[1]], ymdf('2001-02-01T01:00'))
  expect_equal(datetimes[[2]], ymdf('2001-02-02T12:00'))
  expect_equal(datetimes[[3]], ymdf('2001-02-01T00:00'))
})

test_that('make.times works with hours', {
  hours = c(1, 36, 0)
  datetimes <- make.times(hours, 'hours since 2001-02-01T00:00:00')
  expect_equal(datetimes[[1]], ymdf('2001-02-01T01:00'))
  expect_equal(datetimes[[2]], ymdf('2001-02-02T12:00'))
  expect_equal(datetimes[[3]], ymdf('2001-02-01T00:00'))
})

test_that('make.times works with days', {
  days = c(10, 366, 0)
  datetimes <- make.times(days, 'days since 2001-02-01T00:00:00')
  expect_equal(datetimes[[1]], ymdf('2001-02-11T00:00'))
  expect_equal(datetimes[[2]], ymdf('2002-02-02T00:00'))
  expect_equal(datetimes[[3]], ymdf('2001-02-01T00:00'))
})

test_that('make.times works with date-only reftime', {
  days = c(10, 366, 0)
  datetimes <- make.times(days, 'days since 2001-02-01')
  expect_equal(datetimes[[1]], ymdf('2001-02-11T00:00'))
  expect_equal(datetimes[[2]], ymdf('2002-02-02T00:00'))
  expect_equal(datetimes[[3]], ymdf('2001-02-01T00:00'))
})

test_that('make.times fails with bad inputs', {
  values <- c(1,2,3)
  expect_error(make.times(values, 'nonsense since 2001-02-01T00:00:00'))
  # unfortunately the code below doesn't fail but returns bogus dates:
  # expect_error(make.times(values, 'hours since 2001-invalid-01T00:00'))
})

test_that('read.met reads some data and units', {
  file.path <- 'test.met.1d.nc'
  result <- read.met(file.path)
  val <- result$values
  # ncdump -p 9 test.met.1d.nc
  expect_equal(val$air_temperature, c(269.50885, 270.82162))
  expect_equal(val$eastward_wind, c(1.21748793, -0.791418254))

  expect_equal(result$units$air_temperature, 'Kelvin')
  expect_equal(result$units$precipitation_flux, 'kg/m2/s')

  my.times <- ymdf(c('2000-01-01 12:00', '2000-01-02 12:00'))
  expect_equal(result$times[[1]], my.times[1])
  expect_equal(result$times[[2]], my.times[2])
})

test_that('process.met works', {
  values <- list('air_temperature' = c(300, 301, 302, 303),
                 'relative_humidity' = c(0.4, 0.5, 0.6, 0.7),
                 'wind_speed' = c(10, 15, 20, 25),
                 'precipitation_flux' = c(0.1, 0.2, 0.3, 0.4),
                 'surface_downwelling_shortwave_flux_in_air' = c(100, 50, 20, 10))
  units <- list('air_temperature' = 'Kelvin',
                'relative_humidity' = '',
                'wind_speed' = 'm/s',
                'precipitation_flux' = 'kg/m2/s',
                'surface_downwelling_shortwave_flux_in_air' = 'W/m2')
  times <- list(ymdf('2000-01-01 06:00'), ymdf('2000-01-01 18:00'),
                ymdf('2000-01-02 06:00'), ymdf('2000-01-02 18:00'))
  processed <- process.met(as.data.frame(values), units, times)

  expect_equal(nrow(processed), 2)
  expect_equal(names(processed), c('ST', 'YR', 'DOY', 'T', 'TMMXI', 'TMMNI', 'RH', 'RAINI', 'WNI', 'GR'))
  K2C <- udunits2::ud.convert(0, 'K', 'degC')
  make.daily <- function(x) c(sum(x[1:2])*0.5, sum(x[3:4])*0.5)
  expect_equal(processed$T, make.daily(values$air_temperature+K2C))
  expect_equal(processed$TMMXI, c(301, 303)+K2C)
  expect_equal(processed$TMMNI, c(300, 302)+K2C)
  expect_equal(processed$RH, make.daily(values$relative_humidity))
  expect_equal(processed$RAINI, make.daily(values$precipitation_flux*24*3600))
  expect_equal(processed$GR, make.daily(values$surface_downwelling_shortwave_flux_in_air) * 24*3600*1e-6)
  
  expect_equal(processed$YR, rep(2000, 2))
  expect_equal(processed$DOY, c(1,2))
  
  # check computation of wind_speed
  values.nows <- values
  values.nows[['wind_speed']] <- NULL
  values.nows[['eastward_wind']] <- c(1,1, 2,2)
  values.nows[['northward_wind']] <- c(3,3, 4,4)
  units.nows <- units
  units.nows[['northward_wind']] <- 'm/s'
  ws <- sqrt(c(1, 2)^2 + c(3, 4)^2)
  processed <- process.met(as.data.frame(values.nows), units.nows, times)
  expect_equal(processed$WNI, ws)

  # check that a missing variable results in error
  values.broken <- values
  values.broken[['air_temperature']] <- NULL
  expect_error(process.met(as.data.frame(values.broken), units, times))

  # check that a missing unit results in error
  units.broken <- units
  units.broken[['air_temperature']] <- NULL
  expect_error(process.met(as.data.frame(values), units.broken, times))

  # check that a mismatched time list results in error
  expect_error(process.met(as.data.frame(values), units, times[1]))
})

test_that('met2model.BASGRABGC succeeds with a test dataset', {
  dirpath <- '.'
  filename.6h <- 'fakemet.6h.nc'
  filename.1d <- 'fakemet.1d.nc'
  start_date <- ymdf('2000-01-01 00:00')
  end_date <-  ymdf('2001-01-01 00:00')
  result.6h <- met2model.BASGRABGC(dirpath, filename.6h, outfolder, start_date, end_date, full.path = TRUE)
  table.6h <- read.table(result.6h[['file']], sep='\t', header = TRUE)
  result.1d <- met2model.BASGRABGC(dirpath, filename.1d, outfolder, start_date, end_date, full.path = TRUE)
  table.1d <- read.table(result.1d[['file']], sep='\t', header = TRUE)
  expect_equal(table.6h$T, table.1d$T, tolerance=1e-3)
  expect_equal(table.6h$RAINI, table.1d$RAINI, tolerance=1e-3)
  expect_equal(table.6h$T[1:6],
               udunits2::ud.convert(c(269.50885,270.821625,266.196716,265.576843,261.543457,268.926422), 'K', 'degC'),
               tolerance = 1e-3)
  expect_setequal(names(result.6h), c('file', 'host', 'mimetype', 'formatname', 'startdate', 'enddate', 'dbfile.name'))
  # min/max temperatures and wind speed are expected to differ
  
})

test_that('multi-year processing selects the right files',{
  # use stub functions for read.met and process.met to just check that the expected files
  # functions are called on the expected files.
  read.met.stub <- function(file.path) {
    list(values=list(path=file.path), units='', times=file.path)
  }
  process.met.stub <- function(values, units, times) {
    data.frame(name=values[1], doy=c(1,2))
  }
  fcn <- compose.met2model(read.met.stub, process.met.stub)
  start_date <- ymdf('2000-01-01 00:00')
  end_date <-  ymdf('2001-12-31 23:00')
  result <- fcn('dirname', 'filename', outfolder, start_date, end_date)
  table <- read.table(result[['file']], sep='\t', header = TRUE)
  expected <- c('dirname/filename.2000.nc', 'dirname/filename.2001.nc')
  expect_equal(table$path, expected)
})

## test_that("Met conversion runs without error", {
##   skip("This is a template test that will not run. To run it, remove this `skip` call.")
##   nc_path <- system.file("test-data", "CRUNCEP.2000.nc",
##                          package = "PEcAn.utils")
##   in.path <- dirname(nc_path)
##   in.prefix <- "CRUNCEP"
##   start_date <- "2000-01-01"
##   end_date <- "2000-12-31"
##   result <- met2model.MODEL(in.path, in.prefix, outfolder, start_date, end_date)
##   expect_s3_class(result, "data.frame")
##   expect_true(file.exists(result[["file"]][[1]]))
## })
