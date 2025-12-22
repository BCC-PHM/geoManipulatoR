#' Fetch ONS Postcode Directory data for one or more Local Authorities
#'
#' Given a vector of local authority names or codes, fetches records from the
#' ONS Postcode Directory REST API and returns requested geography fields.
#'
#' @param la_names Character vector of local authority names or codes (e.g. "Birmingham" or "E08000025").
#' @param fields Comma-separated string of fields to return.
#'   Default: "PCD,OA21,LSOA21,MSOA21,OSWARD".
#' @param aggregation Character scalar indicating desired aggregation level (passed to `process_data()`).
#' @param IMD Logical; include IMD field if TRUE.
#' @param population Logical; passed to `process_data()`.
#'
#' @return A data.frame (tibble) of fetched data after processing.
#' @export
fetch_data <- function(
    la_names,
    fields = "PCD,OA21,LSOA21,MSOA21,OSWARD",
    aggregation = "LSOA21",
    IMD = TRUE,
    population = FALSE
) {
  if (missing(la_names) || length(la_names) == 0) {
    stop("`la_names` must be a non-empty character vector.", call. = FALSE)
  }
  
  message("Finding LA codes...")
  la_codes <- fetch_la_codes(la_names)
  
  message("Creating REST API url...")
  url <- create_url(la_codes, fields, IMD)
  
  message("Fetching Data...")
  df <- fetch_request(url)
  
  message("Processing Data...")
  # NOTE: you said you'll update column names later; leaving as-is for now.
  # process_data() must exist elsewhere in your package.
  df <- process_data(df, aggregation, population)
  
  df
}
