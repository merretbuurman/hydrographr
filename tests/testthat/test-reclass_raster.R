
###########################################
### Testing the reclass_raster function ###
###########################################


tests_quiet=TRUE

if (!(tests_quiet)) print("_______________________________")
if (!(tests_quiet)) print("Testing: reclass_raster")


#########################
### Some preparations ###
#########################

# Where to store and download files:
if (! exists("tmpdir")){
  tmpdir <- tempdir()
}


#############
### Tests ###
#############

## where to find/store input rasters:
input_raster_dir <- system.file("tests", "test_rasters", package = "hydrographr")
output_raster_dir <- tmpdir

## Two mini input rasters:
input_raster_no_NAs   <- "supermini_testraster_7subcatchments.tif"             # Contains these 7 subcatchments: 506530641, 506531753, 506530642, 506531030, 506531031, 506534272, 506535314
input_raster_with_NAs <- "supermini_testraster_7subcatchments.contains_NA.tif" # Contains these 6 subcatchments: 506530641, NA,        506530642, 506531030, 506531031, 506534272, 506535314

## Settings to be used in every test
bigtiff <- FALSE
quiet <- TRUE


## Generate the test input raster with NA pixels from the one without NAs:
if (not(file.exists(file.path(input_raster_dir, input_raster_with_NAs)))) {

	if (!tests_quiet) message("Creating test raster with NA...")
    # Input table:
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    new_subcIDs <-   c(506531031, NA,        506531030, 506530642, 506531753, 506530641, 506534272)
    prep_input_table <- as.data.frame(list(
        subcID_old=seven_subcIDs,
        subcID=new_subcIDs))
    prep_input_table$subcID_old <- as.integer(prep_input_table$subcID_old)
    prep_input_table$subcID     <- as.integer(prep_input_table$subcID)

    # This warns: "NAs introduced in output raster: Some raster values are NA
    # in the input table and will result in NA pixels in the output raster."
    suppressWarnings(reclass_raster(
        data = prep_input_table,
        rast_val = "subcID_old",
        new_val = "subcID",
        raster_layer = file.path(input_raster_dir, input_raster_no_NAs),
        recl_layer = file.path(input_raster_dir, input_raster_with_NAs),
        bigtiff = bigtiff,
        quiet = quiet,
        read = FALSE))
}




# test 1
testname = "1 Simplest case: Each subcID gets one value"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    seven_values <- 1:7
    input_table <- as.data.frame(list(
        subcID=seven_subcIDs,
        bio99=seven_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_no_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test01.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    output_raster <- reclass_raster(
        data = input_table,
        rast_val = "subcID",
        new_val = "bio99",
        raster_layer = input_raster_path,
        recl_layer = output_raster_path,
        bigtiff = bigtiff,
        quiet = quiet,
        read = TRUE)

    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is equal to the overall number of pixels
    # (because we don't expect NAs):
    expect_equal(sum(received$count), 200)

    # Check there are no NAs:
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 0)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal(received$count[received$value == 1], 2)
    expect_equal(received$count[received$value == 2], 6)
    expect_equal(received$count[received$value == 3], 10)
    expect_equal(received$count[received$value == 4], 18)
    expect_equal(received$count[received$value == 5], 18)
    expect_equal(received$count[received$value == 6], 29)
    expect_equal(received$count[received$value == 7], 117)

    # Reclass rules:
    #506530641 = 2
    #506531753 = 3
    #506530642 = 4
    #506531030 = 5
    #506531031 = 6
    #506534272 = 7
    #506535314 = 8
})


# test 2
testname = "2 Simple case: Table contains too many values and gets reduced (like env90m)."
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    ten_subcIDs   <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272,
                       519999991, 529999992, 539999993)
    ten_values  <- 1:10
    input_table <- as.data.frame(list(
        subcID=ten_subcIDs,
        bio99=ten_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_no_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test02.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    output_raster <- reclass_raster(
        data = input_table,
        rast_val = "subcID",
        new_val = "bio99",
        raster_layer = input_raster_path,
        recl_layer = output_raster_path,
        bigtiff = bigtiff,
        quiet = quiet,
        read = TRUE)

    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is equal to the overall number of pixels (because we don't expect NAs):
    expect_equal(sum(received$count), 200)

    # Check there are no NAs:
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 0)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal(received$count[received$value == 1], 2)
    expect_equal(received$count[received$value == 2], 6)
    expect_equal(received$count[received$value == 3], 10)
    expect_equal(received$count[received$value == 4], 18)
    expect_equal(received$count[received$value == 5], 18)
    expect_equal(received$count[received$value == 6], 29)
    expect_equal(received$count[received$value == 7], 117)

    ## Check that the three fake subcIDs (519999991, 529999992, 539999993)
    ## or rather their values (8-10) do not turn up.
    expect_false(8  %in% received$value)
    expect_false(9  %in% received$value)
    expect_false(10 %in% received$value)

    # Reclass rules:
    #506530641 = 1
    #506531753 = 2
    #506530642 = 3
    #506531030 = 4
    #506531031 = 5
    #506534272 = 6
    #506535314 = 7
})


# test 3
testname = "3 Missing rows in table, NAs introduced"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

	## Why does this introduce NAs? Documntation says:
	## > "Category values which are not explicitly reclassified to a new value
	## >  by the user will be reclassified to 0."
    ## https://grass.osgeo.org/grass43/manuals/html_grass4/html/r.reclass.html

    ## Prepare:
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    four_subcIDs <-  c(506531031, 506535314, 506531030, 506530642)
    four_values  <- 1:4
    input_table <- as.data.frame(list(
        subcID=four_subcIDs,
        bio99=four_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_no_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test03.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    expected_err_msg <- "NAs introduced in output raster: Some raster values are missing in the input table and will get NAs in the output raster."
    expect_warning(
        output_raster <- reclass_raster(
            data = input_table,
            rast_val = "subcID",
            new_val = "bio99",
            raster_layer = input_raster_path,
            recl_layer = output_raster_path,
            bigtiff = bigtiff,
            quiet = quiet,
            read = TRUE),
        message = expected_err_msg, fixed = TRUE)

    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is smaller than the overall number of pixels (because we expect NAs):
    expect_equal(sum(received$count), 2+6+10+18)

    # Check there are 174 NAs: (18+29+117)
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 18+29+117)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal(received$count[received$value == 1], 2)
    expect_equal(received$count[received$value == 2], 6)
    expect_equal(received$count[received$value == 3], 10)
    expect_equal(received$count[received$value == 4], 18)

    ## Check that the three missing subcIDs (506531753, 506530641, 506534272)
    ## or rather their values (1-3) do not turn up.
    expect_false(5 %in% received$value)
    expect_false(6 %in% received$value)
    expect_false(7 %in% received$value)

    # Reclass rules:
    #506530641 = 1
    #506531753 = 2
    #506530642 = 3
    #506531030 = 4
})


# test 4
testname = "4 Mixed case: Missing rows in table, NAs introduced, but also superfluous rows - both must be handled"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # number of pixels:              2,         6,        10,        18,        18,        29,       117
    seven_subcIDs       <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    eight_mixed_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753,
                             519999991, 529999992, 539999993)
    eight_values <- 1:8
    input_table <- as.data.frame(list(
        subcID=eight_mixed_subcIDs,
        bio99=eight_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_no_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test04.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    expected_err_msg <- "NAs introduced in output raster: Some raster values are missing in the input table and will result in NA pixels in the output raster."
    expect_warning(
        output_raster <- reclass_raster(
            data = input_table,
            rast_val = "subcID",
            new_val = "bio99",
            raster_layer = input_raster_path,
            recl_layer = output_raster_path,
            bigtiff = bigtiff,
            quiet = quiet,
            read = TRUE),
        expected_err_msg, fixed = TRUE)

    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is smaller than the overall number of pixels (because we expect NAs):
    expect_equal(sum(received$count), 2+6+10+18+18)

    # Check there are 146 NAs:
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 29+117)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal(received$count[received$value == 1], 2)
    expect_equal(received$count[received$value == 2], 6)
    expect_equal(received$count[received$value == 3], 10)
    expect_equal(received$count[received$value == 4], 18)
    expect_equal(received$count[received$value == 5], 18)

    ## Check that the three fake subcIDs (519999991, 529999992, 539999993)
    ## or rather their values (6-8) do not turn up.
    expect_false(6 %in% received$value)
    expect_false(7 %in% received$value)
    expect_false(8 %in% received$value)

    # Reclass rules:
    #506530642 = 1
    #506531030 = 2
    #506531031 = 3
    #506534272 = 4
    #506535314 = 5
})


# test 5
testname = "5 Table contains some NAs, must be NULL in reclass rules (or removed)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    seven_values  <- c(        1,         2,        NA,         4,        NA,         6,         7)
    input_table <- as.data.frame(list(
        subcID=seven_subcIDs,
        bio99=seven_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_table$bio99 <- as.integer(input_table$bio99)
    input_raster_name <- input_raster_no_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test05.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    expected_err_msg <- "NAs introduced in output raster: Some raster values are NA in the input table and will result in NA pixels in the output raster."
    expect_warning(
        output_raster <- reclass_raster(
            data = input_table,
            rast_val = "subcID",
            new_val = "bio99",
            raster_layer = input_raster_path,
            recl_layer = output_raster_path,
            bigtiff = bigtiff,
            quiet = quiet,
            read = TRUE),
        expected_err_msg, fixed = TRUE)

    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is smaller than the overall number of pixels (because we expect NAs):
    expect_equal(sum(received$count), 2+6+18+29+117)

    # Check there are no NAs:
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 10+18)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal (received$count[received$value == 1], 2)
    expect_equal (received$count[received$value == 2], 6)
    #expect_equal(received$count[received$value == 3], 10) # NA assigned in input table
    expect_equal (received$count[received$value == 4], 18)
    #expect_equal(received$count[received$value == 5], 18) # NA assigned in input table
    expect_equal (received$count[received$value == 6], 29)
    expect_equal (received$count[received$value == 7], 117)

    ## Check that the three fake subcIDs (519999991, 529999992, 539999993)
    ## or rather their values (1-3) do not turn up.
    expect_false(3  %in% received$value)
    expect_false(5  %in% received$value)

    # Reclass rules:
    #506530641 = 1
    #506531753 = 2
    #506530642 = NULL
    #506531030 = 4
    #506531031 = NULL
    #506534272 = 6
    #506535314 = 7

    # or:
    #506530641 = 1
    #506531753 = 2
    #506531030 = 4
    #506534272 = 6
    #506535314 = 7
})



# test 6
testname = "6 Table has non-unique subcIDs"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # number of pixels:            2,         6,        10,        18,        18,        29,       117
    seven_subcIDs     <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    nonunique_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506530641)
    seven_values <- 1:7
    input_table <- as.data.frame(list(
        subcID=nonunique_subcIDs,
        bio99=seven_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_no_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test06.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    expected_err_msg <- "rast_val: Column 'subcID' contains non-unique values."
    expect_error(
        reclass_raster(
            data = input_table,
            rast_val = "subcID",
            new_val = "bio99",
            raster_layer = input_raster_path,
            recl_layer = output_raster_path,
            bigtiff = bigtiff,
            quiet = quiet,
            read = TRUE),
        expected_err_msg, fixed=TRUE
    )
})



# test 7
testname = "7 Raster already contains NAs - they are not reclassified (even if table has a value for NA)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # Note: "506535314" does not exist in the raster!
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    new_subcIDs   <- c(506531031, 506535314,        NA, 506530642, 506531753, 506530641, 506534272)
    seven_values <- 1:7
    input_table <- as.data.frame(list(
        subcID=new_subcIDs,
        bio99=seven_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_with_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test07.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    expected_err_msg <- "NAs introduced in output raster: Some raster values are missing in the input table and will result in NA pixels in the output raster."
    expect_warning(
        output_raster <- reclass_raster(
            data = input_table,
            rast_val = "subcID",
            new_val = "bio99",
            raster_layer = input_raster_path,
            recl_layer = output_raster_path,
            bigtiff = bigtiff,
            quiet = quiet,
            read = TRUE),
        expected_err_msg, fixed = TRUE)

    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is smaller than the overall number of pixels (because we expect NAs):
    expect_true(sum(received$count) < 200)
    expect_equal(sum(received$count), 2+18+18+29+117)

    # Check there are no NAs:
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 6+10)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal (received$count[received$value == 1], 2)
    #expect_equal(received$count[received$value == 2], 6)  # not in input raster
    #expect_equal(received$count[received$value == 3], 10) # NA in input table
    expect_equal (received$count[received$value == 4], 18)
    expect_equal (received$count[received$value == 5], 18)
    expect_equal (received$count[received$value == 6], 29)
    expect_equal (received$count[received$value == 7], 117)

    ## Check that the three fake subcIDs (519999991, 529999992, 539999993)
    ## or rather their values (do not turn up.
    expect_false(2  %in% received$value)
    expect_false(3  %in% received$value)
})


# test 8
testname = "8 reclass_value: All specified ones (in this case, all non-NA!) get the same value (masking)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # Note: "506535314" does not exist in the raster!
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    seven_values <- 1:7 # will be ignored, as we pass "reclass_value"!
    input_table <- as.data.frame(list(
        subcID=seven_subcIDs,
        bio99=seven_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_with_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test08.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    output_raster <- reclass_raster(
        data = input_table,
        rast_val = "subcID",
        new_val = "bio99",
        reclass_value = 888,
        raster_layer = input_raster_path,
        recl_layer = output_raster_path,
        bigtiff = bigtiff,
        quiet = quiet,
        read = TRUE)

    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is smaller than the overall number of pixels (because we expect NAs):
    expect_true(sum(received$count) < 200)
    expect_equal(sum(received$count), 2+10+18+18+29+117)

    # Check there are no NAs:
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 6)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal (received$count[received$value == 888], 2+10+18+18+29+117)

    ## Check that the three fake subcIDs (519999991, 529999992, 539999993)
    ## or rather their values (1-3) do not turn up.
    expect_false(1  %in% received$value)
    expect_false(2  %in% received$value)
    expect_false(3  %in% received$value)
    expect_false(4  %in% received$value)
    expect_false(5  %in% received$value)
    expect_false(6  %in% received$value)
    expect_false(7  %in% received$value)

    # Reclass rules:
    #506530641 = 888
    #506530642 = 888
    #506531030 = 888
    #506531031 = 888
    #506534272 = 888
    #506535314 = 888
})


# test 9
testname = "9 reclass_value: All specified ones get the same value (masking)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    four_subcIDs <-  c(506531031, 506535314, 506531030, 506530642)
    four_values  <- 1:4 # will be ignored
    input_table <- as.data.frame(list(
        subcID=four_subcIDs,
        bio99=four_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_no_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test09.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    expected_err_msg <-"NAs introduced in output raster: Some raster values are missing in the input table and will result in NA pixels in the output raster."
    expect_warning(
        output_raster <- reclass_raster(
            data = input_table,
            rast_val = "subcID",
            new_val = "bio99",
            reclass_value = 888,
            raster_layer = input_raster_path,
            recl_layer = output_raster_path,
            bigtiff = bigtiff,
            quiet = quiet,
            read = TRUE),
        expected_err_msg, fixed = TRUE)


    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is smaller than the overall number of pixels (because we expect NAs):
    expect_true(sum(received$count) < 200)
    expect_equal(sum(received$count), 2+6+10+18)

    # Check there are no NAs:
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 18+29+117)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal (received$count[received$value == 888], 2+6+10+18)

    ## Check that the three fake subcIDs (519999991, 529999992, 539999993)
    ## or rather their values (1-3) do not turn up.
    expect_false(1  %in% received$value)
    expect_false(2  %in% received$value)
    expect_false(3  %in% received$value)
    expect_false(4  %in% received$value)
    expect_false(5  %in% received$value)
    expect_false(6  %in% received$value)
    expect_false(7  %in% received$value)

    # Reclass rules:
    #506530641 = 888
    #506530642 = 888
    #506531030 = 888
})


# test 10
testname = "10 Missing rows in table, set all others to ..."
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    ## Prepare:
    # number of pixels:        2,         6,        10,        18,        18,        29,       117
    seven_subcIDs <- c(506531031, 506535314, 506531030, 506530642, 506531753, 506530641, 506534272)
    four_subcIDs <-  c(506531031, 506535314, 506531030, 506530642)
    four_values  <- 1:4
    input_table <- as.data.frame(list(
        subcID=four_subcIDs,
        bio99=four_values))
    input_table$subcID <- as.integer(input_table$subcID)
    input_raster_name <- input_raster_with_NAs
    input_raster_path <- file.path(input_raster_dir, input_raster_name)
    output_raster_path <- file.path(output_raster_dir, paste0(input_raster_name, ".test10.tif"))

    ## Run:
    skip_if(not(file.exists(input_raster_path)), paste0("Input raster not found: ", input_raster_path))
    expected_err_msg <- "NAs introduced in output raster: Some raster values are missing in the input table and will get NAs in the output raster."
    expect_warning(
        output_raster <- reclass_raster(
            data = input_table,
            rast_val = "subcID",
            new_val = "bio99",
            all_others = 88,
            raster_layer = input_raster_path,
            recl_layer = output_raster_path,
            bigtiff = bigtiff,
            quiet = quiet,
            read = TRUE),
        message = expected_err_msg, fixed = TRUE)

    ## Prepare results:
    received <- terra::freq(output_raster)

    ## Check that the number of classified pixels
    ## is smaller than the overall number of pixels (because we expect NAs):
    expect_true(sum(received$count) < 200)
    expect_equal(sum(received$count), 2+10+18+18+29+117)

    ## Check that we have 6 pixels with NA (which were already NA in the input raster)
    expect_equal(sum(terra::freq(output_raster, value=NA)$count), 6)

    ## Check that the number of pixels of each subcID
    ## match the number of pixels of each reclass-value:
    expect_equal (received$count[received$value == 1], 2)
    #expect_equal(received$count[received$value == 2], 6) # not in input raster
    expect_equal (received$count[received$value == 3], 10)
    expect_equal (received$count[received$value == 4], 18)
    expect_equal (received$count[received$value == 88], 18+29+117)

    # Reclass rules:
    #506530641 = 1
    #506531753 = 2
    #506530642 = 3
    #506531030 = 4
    #* = 88
})
