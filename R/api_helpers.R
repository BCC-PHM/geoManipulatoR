#' Build an ArcGIS REST API query URL
#'
#' @keywords internal
create_url <- function(la_codes, out_fields, IMD) {
  base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Online_ONS_Postcode_Directory_Live/FeatureServer/0/query"
  
  where_clause <- build_where_clause(la_codes)
  
  # Accept user input as a comma-separated string, like your original code
  out_fields_vec <- trimws(unlist(strsplit(out_fields, ",")))
  
  # Ensure PCD is included
  out_fields_vec <- unique(c("PCD", out_fields_vec))
  
  # Optionally add IMD
  if (isTRUE(IMD) && !"IMD" %in% out_fields_vec) {
    out_fields_vec <- c(out_fields_vec, "IMD")
  }
  
  fields_str <- paste(out_fields_vec, collapse = ",")
  
  url <- paste0(
    base_url,
    "?where=", where_clause,
    "&outFields=", utils::URLencode(fields_str),
    "&outSR=4326&f=json"
  )
  
  url
}

#' Build URL-encoded WHERE clause for OSLAUA codes
#'
#' @keywords internal
build_where_clause <- function(la_codes) {
  la_codes <- toupper(as.character(la_codes))
  la_codes <- la_codes[nzchar(la_codes)]
  
  if (length(la_codes) == 0) {
    stop("No valid local authority codes were provided.", call. = FALSE)
  }
  
  conditions <- paste0("OSLAUA = '", la_codes, "'")
  
  clause <- if (length(conditions) == 1) {
    conditions
  } else {
    paste(conditions, collapse = " OR ")
  }
  
  utils::URLencode(clause, reserved = FALSE)
}

#' Fetch paginated ArcGIS REST API results into a single data frame
#'
#' @param base_url The base API endpoint URL produced by `create_url()`.
#' @param verbose Logical; print progress.
#' @return A data.frame (tibble).
#' @keywords internal
fetch_request <- function(base_url, verbose = TRUE) {
  batch_size <- 2000
  offset <- 0
  all_data <- list()
  
  repeat {
    resp <- httr::GET(
      base_url,
      query = list(
        resultOffset = offset,
        resultRecordCount = batch_size
      )
    )
    
    if (httr::status_code(resp) != 200) {
      stop("Request failed with status code: ", httr::status_code(resp), call. = FALSE)
    }
    
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(txt, flatten = TRUE)
    
    batch_df <- parsed$features
    if (is.null(batch_df) || length(batch_df) == 0) break
    
    names(batch_df) <- sub("^attributes\\.|^geometry\\.", "", names(batch_df))
    all_data[[length(all_data) + 1]] <- batch_df
    
    offset <- offset + batch_size
    if (isTRUE(verbose)) message("Fetched records: ", offset)
  }
  
  dplyr::bind_rows(all_data)
}
