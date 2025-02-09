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
#' @param all_others integer. Value to be assigned all pixels of the raster
#'  that are not explicitly listed in the input table (and that are not NA in
#'  the input raster). Defaults to NULL (then all those pixels with get NA
#'  values). Can be combined with 'new_val' and with 'reclass_value'.
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
#' @author Marlene Schürz, Thomas Tomiczek, Merret Buurman
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
                           recl_layer, reclass_value = FALSE, all_others = NULL,
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

  # Four cases can happen:
  # (1) Raster values and table values are the same set - easy!
  # (2) Raster contains values that are missing in the table:
  #     The output raster will contain NA values in those pixels.
  #     Example: Big raster, we only reclassify few pixels.
  #     TODO: Will the others really get NA, or will they keep their initial values???
  # (3) Table contains values that are missing in the raster:
  #     Not a problem for the reclassification, because all pixels
  #     will have values, but the reclass rules may become too big,
  #     so we need to remove those rows from the table.
  #     Example: Env90m, the tables are huge, and you'll use a subset of a
  #     raster tile to reclassify.
  # (4) Mixed: Some raster-values are missing from the table, and some
  #     table values are missing from the raster.
  #     Example: Env90m, huge table, but your raster covers part of the
  #     neighbouring area and contains some subcatchment that the table doesn't.

  # If both are the same:
  if (length(data[[rast_val]]) == length(raster_oldvalue$rast_val)) {
    if (!quiet) message(paste0("Info: Input raster and column '", rast_val,
                               "' of the input table contain the same set of",
                               " (old) values. No (additional) NA values ",
                               " introduced."))
    #message('TRUE: ', all(data[[rast_val]] %in% raster_oldvalue$rast_val))
    #message('TRUE: ', all(raster_oldvalue$rast_val %in% data[[rast_val]]))
  }

  # If not all raster values are in table, we probably don't need to do anything!
  if (! all( raster_oldvalue$rast_val %in% data[[rast_val]] )) {

    # Warn if column rast_val is missing values which are present in input raster,
    # i.e. raster contains values that are not in the table, and which will
    # be classified as NA in the output raster:
    rows_raster_not_in_table <- which(! raster_oldvalue$rast_val %in% data[[rast_val]])
    not_in_table <- raster_oldvalue$rast_val[rows_raster_not_in_table]
    msg <- paste0("NAs introduced in output raster: Some raster values are",
                  " missing in the input table and will result in NA pixels",
                  " in the output raster.")
    warning(msg)

    # Inform more detail:
    if (!quiet) {
      num <- length(not_in_table)
      details <- paste0(msg, " The following ", num, " raster values are",
                        " missing in the input table (column '", rast_val,
                        "'): ", paste0(not_in_table, collapse=", "))
      message(details)
    }
  }


  # If not all table values are in the raster, we just have a lot of superfluous
  # rows in the table that we need to take away (e.g. big Env90m tables!)
  if (! all( data[[rast_val]] %in% raster_oldvalue$rast_val )) {

    if (!quiet) message(paste("Info: Input table contains many values that",
                              " are not present in the input raster. Removing",
                              " those rows from the input table."))

    #message("Num rows table: ", nrow(data))
    rows_table_in_raster <- which(data[[rast_val]] %in% raster_oldvalue$rast_val)
    data <- data[rows_table_in_raster,]
    #message("Num rows table: ", nrow(data))
    # Note: This also removes rows from the table where the old value is NA
    # If we left a line where old value is NA and new value is, say, 99, this
    # would lead to a reclass rule line " = 99", which would NOT reclassify any
    # NAs...
  }


  # If any of the new values are NA, we haved to replace them
  # with the string "NULL", which is what GRASS expects:
  if (any(is.na(data[[new_val]]))) {

    # Warn user:
    msg <- paste0("NAs introduced in output raster: Some raster values are NA",
                  " in the input table and will result in NA pixels in the",
                  " output raster.")
    warning(msg)

    # Inform in more detail:
    if (!quiet) {
      rows_raster_NA_in_table <- which(is.na(data[[new_val]]))
      NA_in_table <- data[[new_val]][rows_raster_NA_in_table]
      #num <- sum(is.na(data[[new_val]]))
      num <- sum(rows_raster_NA_in_table)
      details <- paste0(msg, " The following ", num, " raster values are",
                        " NA in the input table (column '", rast_val, "'): ",
                        paste0(NA_in_table, collapse=", "))
      message(details)
    }

    # Remove those lines OR set to "NULL" to make them nodata:
    # (Or set to no_data? - No, that is done by GRASS!)
    which_rows_have_NAs <- is.na(data[[new_val]])
    data <- data[not(which_rows_have_NAs),]
    #data[[new_val]] [is.na(data[[new_val]])] <- "NULL"
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

  ## Reclassifying all remaining pixels (which are not NA):
  # If we add a line
  # * = 88
  # then this will classify all leftover pixels that had a values in the
  # old raster (not NA!), but are not in the reclass rules, to 88.
  if (!is.null(all_others)) {
    rules <- rbind(rules, list("*", "=", all_others))
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
