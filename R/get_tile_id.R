#' @title Get the Hydrography90m 20°x20° tile ID
#'
#' @description Identifies the 20°x20° tile IDs of the Hydrography90m
#' data in which the input points are located. The IDs can then be used to
#' download the data using \code{\link{download_tiles()}}. The input is a data
#' frame with point coordinates. For orientation, please also see the tiles at
#' the \url{https://hydrography.org/hydrography90m/hydrography90m_layers}
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the longitude / latitude coordinates in WGS84.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param lookup_dir character. Optional. The path to the temp dir to use for
#' downloading and storing ancillary files. If not set, tempdir() is used.
#' In some cases, windows users have issues with using tempdir(), so passing
#' your own path can help in these cases.
#' @importFrom data.table fread
#' @export
#'
#' @author Afroditi Grigoropoulou
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load species occurrence data
#' species_occurrence <- read.table(paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/spdata_1264942.txt"),
#'                                  header = TRUE)
#'
#' # Get the tile ID
#' get_tile_id(data = species_occurrence,
#'             lon = "longitude", lat = "latitude")


get_tile_id <- function(data, lon, lat, lookup_dir = tempdir()) {

  # TODO may have to remove superfluous slash at the end of lookup_dir??
  reg_un <- get_regional_unit_id(data, lon, lat, lookup_dir)

  # TODO may have to remove superfluous slash at the end of lookup_dir??
  lookup_file <- paste0(lookup_dir, "/lookup_tile_regunit.txt")

  if (!file.exists(lookup_file)) {
    download.file("https://drive.google.com/uc?export=download&id=1deKhOEjGgvUXPwivYyH99hgHlJV7OgUv&confirm=t",
                  destfile = lookup_file,
                  quiet = FALSE)

  }

  lookup <- fread(lookup_file)

  # Find the tile ID in the lookup table based on the regional unit id
  tile_id <- sort(unique(lookup[lookup$reg_unit %in% reg_un, tile]))
  tile_id


}
