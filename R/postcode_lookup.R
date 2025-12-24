#' Download and enrich postcode lookup for selected Local Authorities
#'
#' Downloads postcodes for the requested LA(s) from the ONSPD Live ArcGIS API and
#' returns postcodes with LSOA codes. Optionally adds higher geographies from the API
#' (MSOA/ward/PCON/LA etc) and/or IMD attributes by joining to the packaged IMD lookup.
#'
#' @param area Character vector of LA names or LA codes.
#' @param postcode_field Postcode field to return from API. Default "PCDS".
#' @param add_geogs Logical. If TRUE, adds MSOA/ward/PCON/LA codes (and any others you request) from API.
#' @param geogs Character vector of API geography fields to include when `add_geogs=TRUE`.
#' @param add_imd Logical. If TRUE, joins IMD info from the packaged lookup by LSOA21CD.
#' @param imd_cols Character vector of columns to bring across from the IMD lookup when `add_imd=TRUE`.
#' @param verbose Logical; print progress.
#' @param batch_size Integer; API page size.
#' @param warn_threshold Numeric; fuzzy-match warning threshold for LA names.
#'
#' @return A tibble with postcode + LSOA, optionally with geographies and IMD columns.
#' @export
get_postcode_lookup <- function(
    area,
    postcode_field = "PCDS",
    add_geogs = TRUE,
    geogs = c("LSOA21CD", "MSOA21CD", "WD25CD", "PCON24CD", "LAD25CD"),
    add_imd = FALSE,
    imd_cols = c("IMDSCORE", "2024POP", "LA24CD", "LA24NM"),
    verbose = TRUE,
    batch_size = 2000,
    warn_threshold = 0.01
) {
  if (missing(area) || length(area) == 0 || !all(nzchar(as.character(area)))) {
    stop("`area` must be a non-empty character vector of LA names or codes.", call. = FALSE)
  }
  
  # Use packaged lookup for name->code resolving and optional IMD join
  imd_lookup <- load_imd_lookup()
  
  # Resolve LA names/codes (using LA24 fields in your lookup)
  la_codes <- resolve_la_codes(area, lookup = imd_lookup, warn_threshold = warn_threshold)
  
  # Fields to request from API:
  # Always include LSOA21CD (needed for joining) and postcode_field
  api_fields <- "LSOA21CD"
  if (isTRUE(add_geogs)) {
    api_fields <- unique(c(api_fields, geogs))
  }
  api_fields <- paste(api_fields, collapse = ",")
  
  url <- create_url(
    la_codes = la_codes,
    out_fields = api_fields,
    postcode_field = postcode_field,
    return_geometry = FALSE
  )
  
  pc <- fetch_request(url, verbose = verbose, batch_size = batch_size)
  
  # Standardise postcode formatting
  if (!postcode_field %in% names(pc)) {
    stop("API response did not contain the requested postcode field: ", postcode_field, call. = FALSE)
  }
  pc <- pc |>
    dplyr::mutate(
      !!postcode_field := toupper(trimws(as.character(.data[[postcode_field]]))),
      LSOA21CD = toupper(trimws(as.character(.data$LSOA21CD)))
    ) |>
    dplyr::filter(
      !is.na(.data[[postcode_field]]), nzchar(.data[[postcode_field]]),
      !is.na(.data$LSOA21CD), nzchar(.data$LSOA21CD)
    ) |>
    dplyr::distinct(.data[[postcode_field]], .keep_all = TRUE)
  
  # Optional: attach IMD columns (from packaged lookup) via LSOA21CD
  if (isTRUE(add_imd)) {
    missing_imd_cols <- setdiff(c("LSOA21CD", imd_cols), names(imd_lookup))
    if (length(missing_imd_cols) > 0) {
      stop("IMD lookup missing column(s): ", paste(missing_imd_cols, collapse = ", "), call. = FALSE)
    }
    
    imd_small <- imd_lookup |>
      dplyr::mutate(LSOA21CD = toupper(trimws(as.character(.data$LSOA21CD)))) |>
      dplyr::select(dplyr::all_of(c("LSOA21CD", imd_cols))) |>
      dplyr::distinct()
    
    pc <- pc |>
      dplyr::left_join(imd_small, by = "LSOA21CD")
  }
  
  pc
}


#' Build an ArcGIS REST API query URL (ONSPD Live)
#'
#' @param la_codes Character vector of LA codes used in the API field LAD25CD
#' @param out_fields Comma-separated string of fields to return (in addition to PCDS)
#' @param postcode_field Which postcode field to return ("PCDS" recommended; alternatively "PCD7"/"PCD8")
#' @param return_geometry Logical; FALSE is faster (attributes only)
#' @keywords internal
create_url <- function(
    la_codes,
    out_fields = "LSOA21CD",
    postcode_field = "PCDS",
    return_geometry = FALSE
) {
  base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Online_ONS_Postcode_Directory_Live/FeatureServer/0/query"
  
  where_clause <- build_where_clause(la_codes)
  
  out_fields_vec <- trimws(unlist(strsplit(out_fields, ",")))
  out_fields_vec <- out_fields_vec[nzchar(out_fields_vec)]
  
  # Ensure postcode field is included
  out_fields_vec <- unique(c(postcode_field, out_fields_vec))
  
  fields_str <- paste(out_fields_vec, collapse = ",")
  
  paste0(
    base_url,
    "?where=", where_clause,
    "&outFields=", utils::URLencode(fields_str),
    "&outSR=4326",
    "&returnGeometry=", tolower(as.character(isTRUE(return_geometry))),
    "&f=json"
  )
}

#' Build URL-encoded WHERE clause for LAD25CD codes
#'
#' @keywords internal
build_where_clause <- function(la_codes) {
  la_codes <- toupper(as.character(la_codes))
  la_codes <- la_codes[nzchar(la_codes)]
  
  if (length(la_codes) == 0) {
    stop("No valid local authority codes were provided.", call. = FALSE)
  }
  
  conditions <- paste0("LAD25CD = '", la_codes, "'")
  
  clause <- if (length(conditions) == 1) {
    conditions
  } else {
    paste(conditions, collapse = " OR ")
  }
  
  utils::URLencode(clause, reserved = FALSE)
}

#' Fetch paginated ArcGIS REST API results into a single data frame
#'
#' @param base_url The API endpoint URL produced by `create_url()`.
#' @param verbose Logical; print progress.
#' @param batch_size Integer; records per page.
#' @return A tibble of attributes.
#' @keywords internal
fetch_request <- function(base_url, verbose = TRUE, batch_size = 2000) {
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
