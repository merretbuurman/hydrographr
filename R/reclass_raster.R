#' @title Reclassify a raster layer
#'
#' @description Reclassifies a raster .tif layer based on a look-up table, such
#' that the output raster contains the new values. The function uses the r.reclass
#' function of GRASS GIS.
#'
#' Note that the input raster needs to be of type integer. If the input raster
#' layer has floating point values, you can multiply it by some factor
#' (e.g. 1000) to achieve integer values, otherwise the GRASS GIS r.reclass will
#' round the raster values down to the next integer which is not always desired.
#'
#' @param data data.frame or data.table. Input table with the original and new
#'  values used to reclassify the raster.
#' @param rast_val character. The name of the column in the input table
#'  containing the original raster values.
#' @param new_val character. The name of the column in the input table
#'  containing the new raster values, which need to be integer values.
#'  In case of floating point values, consider multiplying the values e.g.
#'  by 1000 to keep three decimals.
#' @param reclass_value integer. Value to be assigned to all pixels of the
#'  raster whose values are listed in the input table (column rast_val). (Pixels
#'  whose value is not listed in the input table will be assigned NA).
#'  If reclass_value is given, the column new_val of the input table is ignored.
#'  Default is FALSE, i.e. the column new_val of the input table will be used.
#' @param raster_layer Full path to the input raster layer (.tif file).
#' @param recl_layer character. Full path of the output raster layer
#'  (.tif file), i.e., the reclassified raster file.
#' @param no_data numeric. The no_data value of the new .tif layer.
#' Default is -9999.
#' @param type character. Data type (needed by GRASS r.out.gdal, so check the
#'  GRASS documentation). Options are Byte, Int16, UInt16, Int32, UInt32,
#'  CInt16, CInt32. Default is Int32. Note that only integer types are allowed.
#' @param compression character. Compression of the written output file (needed
#'  by GRASS r.out.gdal, so check the GRASS documentation. Compression levels
#'  can be defined as "none", "low", or "high". Default is "low", referring to
#'  compression type "DEFLATE" and compression level 2. "high" refers to
#'  compression level 9.
#' @param bigtiff logical. Define whether the output file is expected to be a
#'  BIGTIFF (file size larger than 4 GB). If FALSE and size > 4GB no file will
#'  be written. (Needed by GRASS r.out.gdal, so check the GRASS documentation.
#'  Default is TRUE.
#' @param read logical. If TRUE, then the reclassified raster .tif layer
#'  gets read into R as a SpatRaster (terra object).
#'  If FALSE, the layer is only stored on disk. Default is FALSE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#'  Default is TRUE.
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table data.table fwrite
#' @importFrom processx run
#' @importFrom terra rast
#' @export
#'
#' @author Marlene Schürz, Thomas Tomiczek
#'
#' @references
#' https://grass.osgeo.org/grass82/manuals/r.reclass.html
#'
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Read the stream order for each sub-catchment as a data.table
#' # my_dt <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#' type = "net", as_dt = T)
#'
#'
#' # Select the stream segment ID and and the Strahler stream order
#' str_ord <- my_dt[,c("stream", "strahler")]
#'
#' # Define input and output raster layer
#' stream_raster <- paste0(my_directory,
#'                         "/hydrography90m_test_data/stream_1264942.tif")
#'
#' recl_raster <- paste0(my_directory,
#'                       "/hydrography90m_test_data/reclassified_raster.tif")
#'
#' # Reclassify the stream network to obtain the Strahler stream order raster
#' str_ord_rast <- reclass_raster(data = str_ord,
#'                                rast_val = "stream",
#'                                new_val = "strahler",
#'                                raster_layer = stream_raster,
#'                                recl_layer = recl_raster)
#'
# Reclassify the stream network with the value 1 across the network
#' str_ord_rast <- reclass_raster(data = str_ord$stream,
#'                                reclass_value = 1,
#'                                rast_val = "stream",
#'                                raster_layer = stream_raster,
#'                                recl_layer = recl_raster)

reclass_raster <- function(data, rast_val, new_val = FALSE, raster_layer,
                           recl_layer, reclass_value = FALSE,
                           no_data = -9999, type = "Int32",
                           compression = "low", bigtiff = TRUE,
                           read = FALSE, quiet = TRUE) {

  # Check operating system
  sys_os <- get_os()

  ### Check the input table:

  # Check if data.frame is defined
  if (missing(data))
    stop("data: Input data.frame is missing.")

  # Check if input data is of type data.frame, data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  ### Checks about the column rast_val:

  # Check if rast_val is defined
  if (missing(rast_val))
    stop("rast_val: Column name of current raster value is missing.")

  # Check if rast_val column name exists
  if (is.null(data[[rast_val]]))
    stop(paste0("rast_val: Column name '", rast_val,
    "' does not exist."))

  # Check if values of the rast_val column are integer
  if (!is.integer(data[[rast_val]]))
    stop(paste0("rast_val: Values of column ", rast_val,
      " have to be integers."))

  # Check if rast_val column contents are unique, as we cannot assign two
  # different values to the same pixels. (In that case GRASS ignores the
  # first mapping and only uses the second mapping, but to avoid this, we
  # disallow non-unique values.
  if ( length(data[[rast_val]]) != length(unique(data[[rast_val]])) )
    stop(paste0("rast_val: Column '", rast_val,
    "' contains non-unique values."))

  ### Check if the new values are given by one value (reclass_value),
  ### or by a column in the table:

  # Check if new_val column name exists (when no reclass_value is given)
  if (isFALSE(new_val) && isFALSE(reclass_value))
    stop(paste0("new_val: Column name '", new_val,
                "' does not exist."))

  # Check if values of the new_val column are numeric (when no reclass_value is given)
  if (isFALSE(reclass_value)) {
    if (!is.integer(data[[new_val]])) {
      stop(paste0("new_val:", new_val, ": Column must contain integers."))
    }
  }

  # Check if values of the new_val column are NA (when no reclass_value is given)
  # This is not fatal, but inform user...
  if (isFALSE(reclass_value)) {
    if (any(is.na(data[[new_val]]))) {
      num <- sum(is.na(data[[new_val]]))
      if (!quiet) message(paste0("Info: NA values are present among the new",
                                 " values (column '", new_val, "', ", num,
                                 " NA values). Not a problem, but these may",
                                 " result in NA pixels in the output raster.")
    }
  }

  # Check if reclass_value is an numeric or integer value
  if (!isFALSE(reclass_value)) {
    if (!(is.numeric(reclass_value) || is.integer(reclass_value))) {
      stop(paste0("reclass_value:", reclass_value, " must be integers."))
    }
  }

  ### Check the raster layers:

  # Check if raster_layer is defined
  if (missing(raster_layer))
    stop("raster_layer: Path of the input raster layer is missing.")

  # Check if raster_layer exists
  if (!file.exists(raster_layer))
    stop(paste0("raster_layer: ", raster_layer, " does not exist."))

  # Check if raster_layer and recl_layer ends with .tif
  if (!endsWith(raster_layer, ".tif"))
    stop("raster_layer: Input raster is not a .tif file.")
  if (!endsWith(recl_layer, ".tif"))
    stop("recl_layer: Output raster file path needs to end with .tif.")

  # Check if recl_layer is defined
  if (missing(recl_layer))
    stop("recl_layer: Path for the output raster layer is missing.")

  ### Other checks:

  # Check if type is one of the listed types
  if (!(type == "Int16" || type == "UInt16" || type == "CInt16" ||
        type == "Int32" || type == "UInt32" || type == "CInt32" ||
        type == "Byte"))
    stop("type: Has to be 'Byte', 'Int16', 'UInt16', 'Int32', 'UInt32',
    'CInt16', or 'CInt32' ")

  # Check and translate compression into the compression type and the
  # compression level which is applied to the tiff file when writing it.
  if(compression == "none") {
    compression_type  <- "NONE"
    compression_level <- 0
  } else if (compression == "low") {
    compression_type  <- "DEFLATE"
    compression_level <- 2
  } else if (compression == "high") {
    compression_type  <- "DEFLATE"
    compression_level <- 9
  } else {
    stop("'compression' must be one of 'none', 'low', or 'high'.")
  }

  # Define whether BIGTIFF is used or not. BIGTIFF is required for
  # tiff output files > 4 GB.
  if(bigtiff) {
    bigtiff <- "YES"
  } else {
    bigtiff <- "NO"
  }

  if (!is.logical(read))
   stop("read: Has to be TRUE or FALSE.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  ## Finished the input checks!

  # Make bash scripts executable
  make_sh_exec()

  # To check the raster values and data values
  # load raster and convert to data.frame
  raster_oldvalue <- rast(raster_layer)
  raster_oldvalue <- as.data.frame(raster_oldvalue)
  raster_oldvalue <- as.data.frame(unique(raster_oldvalue[[1]]))
  colnames(raster_oldvalue) <- "rast_val"

  # Check if rast_val is missing raster values
  if (length(data[[rast_val]]) < length(raster_oldvalue$rast_val)) {
    print("Reclassification is missing raster values: Warning NA's are introduced!")
  }

  # Check and handle if raster values are provided in rast_val that are not in the raster tif file.
  if (length(data[[rast_val]]) != length(raster_oldvalue$rast_val)) {
    # Index all raster values which are not in the input data table
    indx_notmiss_raster <- which(raster_oldvalue$rast_val %in% data[[rast_val]])
    # Get missing raster values
    # These are all the values in the input raster, which have NO corresponding value in the table!
    # (but we only get into this section if the table contains values that are not in the raster, so the opposite...)
    miss_raster <- raster_oldvalue[-c(indx_notmiss_raster),]
    print(paste0("These values of the raster were not found in the data table:", 
                 paste(miss_raster, collapse = ", ")))
    # Write all values found in raster tif file and input data table as data frame
    same_val1 <- as.data.frame(raster_oldvalue[c(indx_miss_raster),])
    # Set name for raster values
    colnames(same_val1) <- "val"
    # Index all input data table values which are not in the raster tif file
    indx_miss_rast_val <- which(data[[rast_val]] %in% raster_oldvalue$rast_val)
    # Get missing input data values
    miss_rast_val <- data[-c(indx_miss_rast_val),]
    # Get only missing raster input data values to throw out message
    missing_rast_values <- data[-c(indx_miss_rast_val),1]
    print(paste0("These raster values of the data table were not found in the raster:", 
                 paste(missing_rast_values, collapse = ", ")))
    # Write all values found in input data table and raster file as data frame
    same_val2 <- data[c(indx_miss_raster),]
    # Combine all values found in raster and input data table
    same_val <- as.data.frame(cbind(same_val1, same_val2))
    # Write missing raster values as data frame
    miss_raster <- as.data.frame(miss_raster)
    # Set name for raster values
    colnames(miss_raster) <- "val"
    # Give missing values NA
    miss_raster$rast_val <- NA
    miss_raster$new_val <- NA
    # Set to the same names to combine with all values table
    colnames(miss_raster) <- c("val", rast_val, new_val)
    # Combine all values table with missing values table and use as input data
    data <- rbind(same_val, miss_raster)

    # List both rast_val and new_val columns to check if they are of equal length
    dat <- list(data[[rast_val]], list(data[[new_val]]))

    # In case new_val is bigger than rast_val length of rast_val will be used to reclassify
    data <- setNames(do.call(cbind.data.frame,
                            lapply(lapply(dat, unlist),
                                   `length<-`, max(lengths(dat)))), paste0(c(rast_val, new_val)))
  }



  # Create the rules file:
  #
  # The r.reclass function of GRASS GIS requires a text file
  # including the old and the new value with an = between
  # (e.g. 1 = 20)
  #
  # For testing influence of NAs in rules table:
  #rules <- rbind(rules, list(NA,"=","99"))
  #rules <- rbind(rules, list("*","=","88"))
  #
  # If we had a row in the table where old value is NA and new value is,
  # say, 99, this would lead to a reclass rule line
  #  = 99
  # This would NOT reclassify any old NAs (NA in old raster), or any
  # non-classified values (which have a value in old raster that is not
  # present in the reclass rules) to 99. The line would be ignored.
  #
  # If we add a line
  # * = 88
  # then this will classify all leftover pixels that had a values in the
  # old raster (not NA!), but are not in the reclass rules, to 88.
  if (isFALSE(reclass_value)) {
    rules <- data.table::data.table(old = data[[rast_val]],
                                    equal = "=",
                                    new = data[[new_val]])

  } else {
    data$reclass <- reclass_value
    data$reclass <- as.integer(data$reclass)
    rules <- data.table::data.table(old = data[[rast_val]],
                                    equal = "=",
                                    new = data[["reclass"]])
  }

  # Write reclass rules to temp txt file in temp dir, for GRASS to use.
  # (Will be removed further below)
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  rules_path <- paste0(tempdir(), "/reclass_rules_", rand_string, ".txt")
  fwrite(rules, rules_path, sep = " ", col.names = FALSE)


  # Run GRASS GIS using a bash script, which calls GRASS command r.reclass
  if (sys_os == "linux" || sys_os == "osx") {

    # Open GRASS GIS session under Linux or Max OS
    processx::run(
      system.file("sh", "reclass_raster.sh", package = "hydrographr"),
      args = c(raster_layer, rules_path, recl_layer, no_data, type,
               compression_type, compression_level, bigtiff),
      echo = !quiet)

  } else {

    # Check if WSL and Ubuntu are installed
    check_wsl()

    # Change paths for WSL
    wsl_raster_layer <- fix_path(raster_layer)
    wsl_recl_layer <- fix_path(recl_layer)
    wsl_rules_path <- fix_path(rules_path)
    wsl_sh_file <- fix_path(
      system.file("sh", "reclass_raster.sh", package = "hydrographr"))

    # Open GRASS GIS session on WSL
    processx::run(
      system.file("bat", "reclass_raster.bat", package = "hydrographr"),
      args = c(wsl_raster_layer, wsl_rules_path, wsl_recl_layer, no_data,
        type, compression_type, compression_level, bigtiff, wsl_sh_file),
      echo = !quiet)
  }

  # Inform user about output
  if (file.exists(recl_layer)) {
    if (!quiet) message("Reclassified raster saved under: ", recl_layer)
  } else {
    stop("Output file was not written. File size may have been larger than 4GB",
         "\nSet bigtiff = TRUE, for writing large output files.")
  }

  # Delete temporary reclass_rules txt file
  file.remove(rules_path)

  # Load output into R session
  if (read == TRUE) {
    recl_rast <- rast(recl_layer)
    return(recl_rast)
  }
}
