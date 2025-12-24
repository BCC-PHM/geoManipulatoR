#' Get population-weighted IMD (and national rank/decile) for an area at a chosen geography level
#'
#' Loads the packaged IMD lookup (LSOA IMD score + population + higher geographies),
#' aggregates IMD to the requested level using population weights (except LSOA),
#' calculates national rank and decile at that level, then filters to the requested area.
#'
#' @param area Optional. Character vector of LA names or LA codes (matches `LA24CD` / `LA24NM`),
#'   e.g. "Birmingham" or "E08000025". If NULL/empty, returns all areas covered by the lookup.
#' @param level Character scalar. One of: "LSOA21CD", "MSOA21CD", "WD25CD", "PCON24CD", "LA24CD".
#' @param lookup Optional data.frame for testing. If NULL, uses packaged lookup file.
#' @param rank_desc Logical. If TRUE (default), higher IMD score = more deprived = rank 1 (and decile 1 = most deprived).
#' @param warn_threshold Numeric. Jaro-Winkler distance threshold for warning on fuzzy LA matches.
#'
#' @return A tibble with one row per unit at `level`, including:
#'   `pop_total`, `imd_weighted`, `rank_national`, `decile_national`.
#' @export
get_imd <- function(
    area = NULL,
    level = "LSOA21CD",
    lookup = NULL,
    rank_desc = TRUE,
    warn_threshold = 0.01
) {
  filter_area <- !(is.null(area) || length(area) == 0 || all(!nzchar(as.character(area))))
  
  allowed_levels <- c("LSOA21CD", "MSOA21CD", "WD25CD", "PCON24CD", "LA24CD")
  if (!level %in% allowed_levels) {
    stop("`level` must be one of: ", paste(allowed_levels, collapse = ", "), call. = FALSE)
  }
  
  df <- if (is.null(lookup)) load_imd_lookup() else as.data.frame(lookup)
  
  # required columns for any run
  base_required <- c("LSOA21CD", "IMDSCORE", "2024POP", "LA24CD", "LA24NM")
  missing_base <- setdiff(base_required, names(df))
  if (length(missing_base) > 0) {
    stop("Lookup is missing required column(s): ", paste(missing_base, collapse = ", "), call. = FALSE)
  }
  
  # normalise types
  df <- df |>
    dplyr::mutate(
      LA24CD = toupper(trimws(as.character(.data$LA24CD))),
      LA24NM = toupper(trimws(as.character(.data$LA24NM))),
      IMDSCORE = as.numeric(.data$IMDSCORE),
      `2024POP` = as.numeric(.data$`2024POP`)
    )
  
  # resolve LA names/codes to LA codes (if filtering)
  if (filter_area) {
    area_codes <- resolve_la_codes(area, lookup = df, warn_threshold = warn_threshold)
  }
  
  # ---- LSOA special case: no aggregation, just rank/decile on IMDSCORE ----
  if (level == "LSOA21CD") {
    out <- df |>
      dplyr::filter(!is.na(.data$IMDSCORE)) |>
      dplyr::transmute(
        LSOA21CD = .data$LSOA21CD,
        LA24CD = .data$LA24CD,
        LA24NM = .data$LA24NM,
        pop_total = .data$`2024POP`,
        imd_weighted = .data$IMDSCORE
      ) |>
      add_rank_and_decile(score_col = "imd_weighted", rank_desc = rank_desc)
    
    if (filter_area) {
      out <- dplyr::filter(out, .data$LA24CD %in% area_codes)
    }
    return(out)
  }
  
  # ---- Other levels: ensure the requested level column exists ----
  if (!level %in% names(df)) {
    stop("Lookup is missing the requested level column: ", level, call. = FALSE)
  }
  
  out <- df |>
    aggregate_imd(level = level) |>
    add_rank_and_decile(score_col = "imd_weighted", rank_desc = rank_desc)
  
  if (filter_area) {
    out <- dplyr::filter(out, .data$LA24CD %in% area_codes)
  }
  
  out
}

#' Load packaged IMD lookup (CSV stored in inst/extdata/)
#' @keywords internal
load_imd_lookup <- function() {
  path <- system.file("extdata", "imd_lookups.csv", package = "geoManipulatoR")
  if (path == "") {
    stop(
      "Could not find 'imd_lookups.csv'. Put it in inst/extdata/imd_lookups.csv and reinstall/load the package.",
      call. = FALSE
    )
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Aggregate IMD to a chosen geography level using population weights
#' @param df Lookup data.frame
#' @param level Column name to aggregate to (e.g. 'MSOA21CD')
#' @keywords internal
aggregate_imd <- function(df, level) {
  lvl <- rlang::sym(level)
  
  df |>
    dplyr::filter(
      !is.na(.data$`2024POP`), .data$`2024POP` > 0,
      !is.na(.data$IMDSCORE),
      !is.na(!!lvl), nzchar(trimws(as.character(!!lvl))),
      !is.na(.data$LA24CD)
    ) |>
    dplyr::group_by(!!lvl) |>
    dplyr::summarise(
      pop_total = sum(.data$`2024POP`, na.rm = TRUE),
      imd_weighted = sum(.data$IMDSCORE * .data$`2024POP`, na.rm = TRUE) / pop_total,
      # NOTE: some geographies (e.g. PCON) can span multiple LAs; see intersects/within enhancement later.
      LA24CD = dplyr::first(.data$LA24CD),
      LA24NM = dplyr::first(.data$LA24NM),
      .groups = "drop"
    ) |>
    dplyr::rename(!!level := !!lvl)
}

#' Add national rank and decile based on a score column
#' @param df Data frame
#' @param score_col Name of numeric score column
#' @param rank_desc If TRUE, higher score ranks as more deprived (rank 1, decile 1 = most deprived)
#' @keywords internal
add_rank_and_decile <- function(df, score_col, rank_desc = TRUE) {
  sc <- rlang::sym(score_col)
  
  df |>
    dplyr::mutate(
      rank_national = if (isTRUE(rank_desc)) {
        dplyr::min_rank(dplyr::desc(!!sc))
      } else {
        dplyr::min_rank(!!sc)
      },
      decile_national = if (isTRUE(rank_desc)) {
        dplyr::ntile(dplyr::desc(!!sc), 10)
      } else {
        dplyr::ntile(!!sc, 10)
      }
    )
}

#' Resolve LA names or codes to LA codes using imd_lookups.csv (via the loaded lookup)
#'
#' @param area Character vector of LA names or LA codes
#' @param lookup IMD lookup data.frame containing LA24CD and LA24NM
#' @param warn_threshold Jaro-Winkler distance above which a warning is issued
#'
#' @return Character vector of LA24CD codes
#' @keywords internal
resolve_la_codes <- function(area, lookup, warn_threshold = 0.01) {
  area <- as.character(area)
  area_clean <- toupper(trimws(area))
  area_clean <- area_clean[nzchar(area_clean)]
  
  la_lookup <- lookup |>
    dplyr::distinct(LA24CD, LA24NM) |>
    dplyr::mutate(
      LA24CD = toupper(trimws(as.character(.data$LA24CD))),
      LA24NM = toupper(trimws(as.character(.data$LA24NM)))
    )
  
  vapply(area_clean, function(x) {
    # already a valid LA code
    if (x %in% la_lookup$LA24CD) return(x)
    
    # fuzzy match on LA name
    d <- stringdist::stringdist(x, la_lookup$LA24NM, method = "jw")
    i <- which.min(d)
    
    if (length(i) == 0 || is.na(d[i])) {
      stop("Could not resolve local authority: ", x, call. = FALSE)
    }
    
    if (d[i] > warn_threshold) {
      warning(
        sprintf(
          "No exact match for '%s' (distance = %.03f). Using '%s'.",
          x, d[i], la_lookup$LA24NM[i]
        ),
        call. = FALSE
      )
    }
    
    la_lookup$LA24CD[i]
  }, character(1)) |>
    unname()
}
