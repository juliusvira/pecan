context("write.config")

run.id <- 0

outfolder <- tempfile()
setup(dir.create(file.path(outfolder, run.id), recursive=TRUE, showWarnings = FALSE))
teardown(unlink(outfolder, recursive = TRUE))

settings = list(host = list(rundir = outfolder, outdir = outfolder),
                model = list(binary = '__binary__'),
                run = list(start.date = '2001-02-02',
                           end.date = '2002-01-01 01:00',
                           inputs = list(met = list(path = '__met__'),
                                         management = list(path = '__management__'),
                                         soil = list(path = '__soil__')),
                           site = list(lat = 10.0, lon = 40.0)),
                rundir = outfolder)

param.map.file.test <- 'test.param.map.csv'
param.map <- read.csv(file = param.map.file.test)
trait.values.test <- list(list(second = 1, first = 2, unused = -99)) # change the order
defaults <- NULL

test_that('Parameters are mapped and converted according to the table', {
  my.settings <- settings
  my.settings$run$inputs$basgrabgc.param.map.file <- param.map.file.test
  result <- write.config.BASGRABGC(defaults, trait.values.test, my.settings, run.id)
  # read back the params and check
  param.file <- file.path(settings$rundir, run.id, "basgrabgc.param")
  conn <- file(param.file, 'r')
  num.params <- as.integer(readLines(conn, 1))
  table <- read.table(file = conn)
  close(conn)
  param.names <- table[,1]
  params <- table[, 2]

  expect_equal(num.params, length(params))
  expect_equal(length(params), nrow(param.map))
  # first two converted from trait.values
  expect_equal(params[1], trait.values.test[[1]][['first']] * param.map$conv2basgra[1])
  expect_equal(params[2], trait.values.test[[1]][['second']] * param.map$conv2basgra[2])
  # third not in trait.values
  expect_equal(params[3], param.map$default[3])
  expect_equal(param.names, param.map$name.basgra)
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
  expect_equal(as.integer(lines[[15]]), 33) # start_doy
  expect_equal(as.integer(lines[[16]]), 365-(33-1) + 1) # include the +1 day needed to get the last day to output
  expect_equal(lines[[20]], settings$run$inputs$met$path)
  expect_equal(lines[[22]], file.path(settings$rundir, run.id, "basgrabgc.param"))
  expect_equal(lines[[24]], settings$run$inputs$management$path)
  path.out <- file.path(settings$host$outdir, run.id, 'basgrabgc.out')
  expect_equal(lines[[26]], path.out)
  expect_equal(lines[[28]], settings$run$inputs$soil$path)
})
