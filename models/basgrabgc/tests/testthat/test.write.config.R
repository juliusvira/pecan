run.id <- 0

outfolder <- tempfile()
setup(dir.create(file.path(outfolder, run.id), recursive=TRUE, showWarnings = FALSE))
teardown(unlink(outfolder, recursive = TRUE))

settings = list(host = list(rundir = outfolder, outdir = outfolder),
                model = list(binary = '__binary__'),
                run = list(start.date = '2001-01-02',
                           end.date = '2002-01-01 01:00',
                           inputs = list(met = list(path = '__met__'))),
                rundir = outfolder)

param.map.file.test <- system.file('test.param.map.csv', package = 'PEcAn.BASGRABGC')
param.map <- read.csv(file = param.map.file.test)
trait.values.test <- list(list(second = 1, first = 2, unused = -99)) # change the order
defaults <- NULL

test_that('Parameters are mapped and converted according to the table', {
  my.settings <- settings
  my.settings$run$inputs$basgrabgc.param.map.file <- param.map.file.test
  result <- write.config.BASGRABGC(defaults, trait.values.test, my.settings, run.id)
  # read back the params and check
  param.file <- file.path(settings$rundir, run.id, "basgrabgc.param")
  params <- read.table(file = param.file)[,1]
  expect_equal(length(params), nrow(param.map))
  # first two converted from trait.values
  expect_equal(params[1], trait.values.test[[1]][['first']] * param.map$conv2basgra[1])
  expect_equal(params[2], trait.values.test[[1]][['second']] * param.map$conv2basgra[2])
  # third not in trait.values
  expect_equal(params[3], param.map$default[3])
})

test_that('Mapping the parameters with the full table passes without errors', {
  param.map <- read.csv(file = system.file("basgra-bgc.param.map.csv", package = "PEcAn.BASGRABGC"))
  # create a trait.values with a value for all variables with a name.bethy in the param.map
  values <- param.map[['default']][param.map$name.bethy != '']
  trait.names <- param.map[['name.bethy']][param.map$name.bethy != '']
  names(values) <- trait.names
  result <- write.config.BASGRABGC(defaults, list(values), settings, run.id)
  expect_true(TRUE)
})

test_that('The job script exists and is not empty',  {
  my.settings <- settings
  my.settings$run$inputs$basgrabgc.param.map.file <- param.map.file.test    
  write.config.BASGRABGC(defaults, trait.values.test, my.settings, run.id)
  jobfile <- file.path(settings$rundir, run.id, "job.sh")
  expect_true(file.exists(jobfile))
  text <- readLines(con = jobfile)
  expect_true(length(text) > 1)
})

test_that('The config file has the correct content', {
  write.config.BASGRABGC(defaults, trait.values.test, settings, run.id)
  config.file.path <- file.path(outfolder, run.id, paste0("CONFIG.", run.id, ".txt"))
  text <- readLines(con = config.file.path)
  lines <- strsplit(text, '\n')
  # the file is in fixed format
  expect_equal(as.integer(lines[[14]]), lubridate::year(settings$run$start.date))
  expect_equal(as.integer(lines[[15]]), 2) # start_doy
  expect_equal(as.integer(lines[[16]]), 365-1)
  expect_equal(lines[[20]], settings$run$inputs$met$path)
  expect_equal(lines[[22]], file.path(settings$rundir, run.id, "basgrabgc.param"))
  # [[24]] management: not implemented
  path.out <- file.path(settings$host$outdir, paste0("out", run.id))
  expect_equal(lines[[26]], path.out)
  # [[28]] soil: not implemented
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
