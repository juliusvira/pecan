context("model2netcdf")

tmpdir <- tempfile()
setup(dir.create(tmpdir, showWarnings = FALSE))
teardown(unlink(tmpdir, recursive = TRUE))

df.test <- data.frame(dyear = seq(2021, 2023, length=3*365),
                      year = c(rep(2021, 365), rep(2022, 365), rep(2023, 365)) ,
                      doy = rep(seq(365), 3),
                      ECORES = c(rep(1, 365), rep(2, 365), rep(3, 365)),
                      GPP = rep(99, 3*365))
df.info <- data.frame(name.basgra = c('ECORES', 'GPP', ''),
                      long_name = c('Eco Res', 'Gross Peanut Production', 'Net Ecosystem Exchange'),
                      name.bethy = c('', 'GPP', 'NEE'),
                      unit.basgra = c('g m-2 d-1', 'peanuts s-1', ''),
                      unit.bethy = c('', 'p s-1', 'kg m-2 s-1'),
                      conv2bethy = c(NA, 10, NA))

test_that('process_output works', {
  years <- c(2021, 2022, 2023)
  output <- process_output(df.test, df.info)
  expect_equal(length(output), 3) # three years
  for (ind_yr in seq_len(3)) {
    expect_setequal(output[[ind_yr]]$year, years[ind_yr])
    expect_setequal(output[[ind_yr]]$GPP, 99*10) # conv2bethy = 10
    expect_equal(output[[ind_yr]]$NEE, # see above how the values are defined
                 udunits2::ud.convert(ind_yr - rep(99, 365), 'g m-2 d-1', 'kg m-2 s-1'))
    expect_equal(nrow(output[[ind_yr]]), 365)
    expect_false(any(is.na(output[[ind_yr]])))
  }
  expect_error(process_output(df.test, df.info[,1:4]))
})

test_that('model2netcdf date checking logic works', {
  stub_func <- get_model2netcdf(make_netcdf_func = function(...){},
                                get_output_func = function(...) { df.test[1:365,] },
                                get_out_info_func = function() { df.info })
  expect_silent(stub_func('', NA, NA, lubridate::ymd('2021-01-01'), lubridate::ymd('2021-12-31')))
  expect_error(stub_func('', NA, NA, lubridate::ymd('2020-01-01'), lubridate::ymd('2021-12-31')))
  expect_error(stub_func('', NA, NA, lubridate::ymd('2021-01-01'), lubridate::ymd('2024-01-01')))
})

test_that('netcdf files are requested to be created as expected', {
  my.sitelat <- 1.0
  my.sitelon <- 9.124
  my.info <- df.info
  my.output <- process_output(df.test, df.info)
  files_created <- list()
  check_netcdf <- function(df_output, df_info, nc_path, sitelat, sitelon) {
    # this function checks that make_netcdf is called with right arguments and records
    # the files that would be created
    year <- df_output[1,'year']
    name_expected <- file.path(tmpdir, sprintf('%i.nc', year))
    expect_equal(nc_path, name_expected)
    expect_equal(sitelat, my.sitelat)
    expect_equal(sitelon, my.sitelon)
    expect_equal(df_info, my.info)
    files_created[[length(files_created)+1]] <<- nc_path
  }
  stub_func <- get_model2netcdf(make_netcdf_func = check_netcdf,
                                get_output_func = function(...) df.test,
                                get_out_info_func = function() df.info)
  start_date <- lubridate::ymd('2021-01-01')
  end_date <- lubridate::ymd('2023-12-31')
  stub_func(tmpdir, my.sitelat, my.sitelon, start_date, end_date)
  expect_equal(files_created, list(file.path(tmpdir, '2021.nc'), file.path(tmpdir, '2022.nc'), file.path(tmpdir, '2023.nc')))
})

test_that('make_netcdf writes what is required', {
  df <- process_output(head(df.test), df.info)[[1]]
  nc_path <- file.path(tmpdir, 'test.nc')
  sitelat <- 90.0
  sitelon <- 180.0
  make_netcdf(df, df.info, nc_path, sitelat = sitelat, sitelon = sitelon)
  expect_true(file.exists(nc_path))

  nc <- ncdf4::nc_open(nc_path)
  time_units <- ncdf4::ncatt_get(nc, 'time', 'units')$value
  expect_equal(time_units, 'days since 2021-01-01')
  # in the following I want the attributes dropped thus c(ncdf4::ncvar_get(...))
  times <- c(ncdf4::ncvar_get(nc, 'time'))
  expect_equal(times, df$doy-1)
  expect_equal(c(ncdf4::ncvar_get(nc, 'lat')), sitelat)
  expect_equal(c(ncdf4::ncvar_get(nc, 'lon')), sitelon)
  expect_equal(c(ncdf4::ncvar_get(nc, 'GPP')), df[, 'GPP'])
  expect_equal(c(ncdf4::ncvar_get(nc, 'NEE')), df[, 'NEE'])
  expect_equal(ncdf4::ncatt_get(nc, 'GPP', 'long_name')$value, df.info[df.info$name.bethy == 'GPP', 'long_name'])
  expect_equal(ncdf4::ncatt_get(nc, 'NEE', 'long_name')$value, df.info[df.info$name.bethy == 'NEE', 'long_name'])
  expect_equal(ncdf4::ncatt_get(nc, 'GPP', 'units')$value, df.info[df.info$name.bethy == 'GPP', 'unit.bethy'])
  expect_equal(ncdf4::ncatt_get(nc, 'NEE', 'units')$value, df.info[df.info$name.bethy == 'NEE', 'unit.bethy'])
  ncdf4::nc_close(nc)
  # check the varfile
  var_table <- read.table(sprintf('%s.var', nc_path))
  expect_equal(var_table[,1], df.info$name.bethy[df.info$name.bethy != ''])
  # the following should hold but doesn't because there are (legitimately) spaces in the long_names.
  # expect_equal(var_table[,2], df.info$long_name[df.info$name.bethy != ''])
})

test_that('model2netcdf passes with real input', {
  # the test.output should be created with the scr/make_test_data.sh script in the BASGRA-BGC directory
  func2file <- get_model2netcdf(make_netcdf,
                                get_output_func = function(ignored) {
                                  read.table('test.output')},
                                get_out_info_func = function() {
                                  read.csv(system.file('basgra-bgc.output.map.csv', package='PEcAn.BASGRABGC'))}
                                )
  sitelat <- 0.0
  sitelon <- 0.1
  start_date <- lubridate::ymd('1999-09-01')
  end_date <- lubridate::ymd('1999-09-20')

  expect_silent(func2file(tmpdir, sitelat, sitelon, start_date, end_date))
  # capture the output sent to netcdf
  outputs <- list()
  capture_output <- function(df_output, df_info, nc_path, sitelat, sitelon) {
    outputs[[length(outputs)+1]] <<- df_output
  }
  func2lst <- get_model2netcdf(capture_output,
                               get_output_func = function(ignored) {
                                 read.table('test.output')},
                               get_out_info_func = function() {
                                  read.csv(system.file('basgra-bgc.output.map.csv', package='PEcAn.BASGRABGC'))}
                               )
  func2lst(tmpdir, sitelat, sitelon, start_date, end_date)
  expect_length(outputs, 1)
  expect_false(any(is.na(outputs[[1]])))
  expect_equal(nrow(outputs[[1]]), 20)
  expect_equal(outputs[[1]][20, 'leaf_carbon_content'], 91.02158*1e-3)
})
