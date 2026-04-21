#' Retrieve Timeseries Metadata by Station/Site and Parameter from DBHydro Insights
#'
#' @description
#' Queries the SFWMD DBHydro Insights API (`/v1/insights-data/cont/ts`) to
#' retrieve timeseries metadata — including Timeseries IDs (DBKEYs), frequency,
#' statistic, datum, and status — for a given station or site and parameter
#' combination. This function mirrors the filter logic used by the Insights web
#' UI and is the recommended approach for resolving DBKEYs programmatically
#' in place of the legacy DBHYDRO Browser `show_dbkeys_matched` workflow.
#'
#' @param station `character(1)`. The station or site name to query
#'   (e.g., `"S333_H"`, `"S333"`). For `location_type = "STATION"`,
#'   the SFWMD Insights API typically expects the `_H` suffix for
#'   hydrologic stations (e.g., `"S333_H"`).
#' @param parameter `character(1)`. The parameter code to filter by
#'   (e.g., `"STG"` for stage, `"FLOW"` for flow,
#'   `"RAIN"` for rainfall, `"PHOSPHORUS"` for total phosphorus).
#' @param startdate `character(1)` or `NULL`. Start date of the query
#'   window in `"YYYY-MM-DD"` format. Defaults to 30 days before the
#'   current system date if `NULL`.
#' @param enddate `character(1)` or `NULL`. End date of the query
#'   window in `"YYYY-MM-DD"` format. Defaults to the current system date
#'   if `NULL`.
#' @param frequency `character(1)`. Frequency filter for the timeseries.
#'   Common values include `"DA"` (daily), `"BP"` (breakpoint/
#'   instantaneous), and `"ALL"` (no filter). Defaults to `"ALL"`.
#' @param statistic `character(1)`. Statistic filter for the timeseries.
#'   Common values include `"MEAN"`, `"INST"` (instantaneous), and
#'   `"ALL"` (no filter). Defaults to `"ALL"`.
#' @param status `character(1)`. Status filter for the timeseries.
#'   Typically `"ALL"`, `"Active"`, or `"Inactive"`.
#'   Defaults to `"ALL"`.
#' @param location_type `character(1)`. The type of location identifier
#'   supplied in `station`. Must be one of `"STATION"` (default,
#'   for hydrologic structures and gages) or `"SITE"` (for water quality
#'   monitoring sites). Validated via [base::match.arg()].
#' @param timeout_sec `numeric(1)`. Maximum number of seconds to wait for
#'   an API response before timing out. Defaults to `60`.
#' @param pause_sec `numeric(1)`. Number of seconds to pause before
#'   issuing the API request. Useful for rate-limiting when calling this
#'   function in a loop. Defaults to `0` (no pause).
#'
#' @return A `data.frame` with one row per matching timeseries, containing
#'   columns returned by the Insights API (which may include `timeseriesId`,
#'   `dbkey`, `station`, `parameter`, `frequency`,
#'   `statistic`, `datum`, `status`, among others), plus three
#'   appended columns:
#'   \describe{
#'     \item{`station_requested`}{The value passed to `station`.}
#'     \item{`parameter_requested`}{The value passed to `parameter`.}
#'     \item{`location_type_used`}{The resolved value of `location_type`.}
#'   }
#'   If no timeseries are found or the response cannot be coerced to a
#'   `data.frame`, the raw parsed response is returned invisibly and a
#'   diagnostic message is printed listing the top-level names of the API
#'   response.
#'
#' @details
#' This function POSTs a JSON query to the SFWMD DBHydro Insights continuous
#' timeseries endpoint (`/v1/insights-data/cont/ts`), replicating the
#' exact payload structure used by the Insights web UI. The `locations`
#' field in the payload is constructed as:
#' \preformatted{
#'   [{"name": "<station>", "type": "<location_type>"}]
#' }
#'
#' All filter fields (`category`, `dbkeys`, `recorder`,
#' `stationIDs`, `usgsIDs`) that are not directly exposed as
#' arguments default to `"ALL"` (no filtering), consistent with the
#' Insights UI default behavior.
#'
#' The `reporttype` is fixed to `"timeseries"` and
#' `reportformat` to `"table"`, which returns metadata for each
#' matching timeseries rather than the underlying data values. To retrieve
#' actual data, pass the returned `timeseriesId` / `dbkey` values
#' to [insight_dbkey_meta()] or the relevant fetch functions.
#'
#' @note
#' \itemize{
#'   \item The SFWMD Insights API is not publicly documented. The endpoint and
#'     payload structure were derived by inspecting browser network traffic
#'     against <https://insightsdata.sfwmd.gov> and are subject to change
#'     without notice.
#'   \item For hydrologic stations, the API typically expects the `_H`
#'     suffix (e.g., `"S333_H"` rather than `"S333"`). For water
#'     quality sites, use `location_type = "SITE"` and the bare station
#'     name.
#'   \item This function requires the \pkg{httr} and \pkg{jsonlite} packages.
#' }
#'
#' @examples
#' \dontrun{
#' # All stage timeseries at S333 (hydrologic station, all frequencies)
#' meta <- insight_station_datatype_meta(
#'   station   = "S333_H",
#'   parameter = "STG"
#' )
#'
#' # Daily mean stage only, specific date window
#' meta_da <- insight_station_datatype_meta(
#'   station    = "S333_H",
#'   parameter  = "STG",
#'   startdate  = "2020-01-01",
#'   enddate    = "2022-12-31",
#'   frequency  = "DA",
#'   statistic  = "MEAN"
#' )
#'
#' # Water quality site — total phosphorus
#' meta_wq <- insight_station_datatype_meta(
#'   station       = "S333",
#'   parameter     = "PHOSPHORUS",
#'   location_type = "SITE"
#' )
#' }
#'
#' @importFrom httr POST add_headers timeout stop_for_status content
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export

insight_station_datatype_meta <- function(station,
                                          parameter,
                                          startdate     = NULL,
                                          enddate       = NULL,
                                          frequency     = "ALL",
                                          statistic     = "ALL",
                                          status        = "ALL",
                                          location_type = c("STATION", "SITE"),
                                          timeout_sec   = 60,
                                          pause_sec     = 0) {

  #  input validation
  station       <- as.character(station)
  parameter     <- as.character(parameter)
  location_type <- match.arg(location_type)   # defaults to "STATION" if not supplied

  #  date handling
  # If both NULL -> omit dates field entirely (API may return all records)
  # If only one supplied -> default the other to a sensible bound
  if (is.null(startdate) && is.null(enddate)) {
    dates_field <- NULL                              # omit from payload
  } else {
    if (is.null(startdate)) startdate <- "1900-01-01"          # open left bound
    if (is.null(enddate))   enddate   <- format(Sys.Date(), "%Y-%m-%d")  # open right bound
    dates_field <- list(startdate = startdate, enddate = enddate)
  }

  #  URL (confirmed from browser capture)
  url <- "https://insightsdata.api.sfwmd.gov/v1/insights-data/cont/ts"

  #  POST body
  body <- list(
    query = list(
      locations    = list(list(name = station, type = location_type)),
      parameters   = list(parameter),
      category     = list("ALL"),
      dates        = list(startdate = startdate, enddate = enddate),
      dbkeys       = list("ALL"),
      frequency    = list(frequency),
      recorder     = list("ALL"),
      reportformat = "table",
      reporttype   = "timeseries",
      stationIDs   = list("ALL"),
      statistic    = list(statistic),
      status       = list(status),
      usgsIDs      = list("ALL")
    )
  )

  if (pause_sec > 0) Sys.sleep(pause_sec)

  resp <- POST(
    url,
    body   = toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    add_headers(
      "Content-Type" = "application/json",
      "Accept"       = "application/json, text/plain, */*",
      "Origin"       = "https://insightsdata.sfwmd.gov",
      "Referer"      = "https://insightsdata.sfwmd.gov/"
    ),
    timeout(timeout_sec)
  )

  stop_for_status(resp)

  txt <- content(resp, as = "text", encoding = "UTF-8")
  x   <- fromJSON(txt, flatten = TRUE)

  # extract timeseries metadata
  if (!is.null(x$timeseries)) {
    ts <- x$timeseries
  } else if (!is.null(x$data)) {
    ts <- x$data
  } else if (!is.null(x$results)) {
    ts <- x$results
  } else {
    ts <- x
  }

  # coerce to data.frame
  if (is.data.frame(ts)) {
    df <- ts
  } else if (is.list(ts) && length(ts)) {
    df <- as.data.frame(ts, stringsAsFactors = FALSE)
  } else {
    message("No timeseries found. Raw response names: ", paste(names(x), collapse = ", "))
    return(invisible(x))
  }

  # Correct conversion from Unix milliseconds -> Date
  if ("startDate" %in% names(df)) df$startDate <- as.Date(as.POSIXct(df$startDate / 1000, origin = "1970-01-01", tz = "UTC"))
  if ("endDate"   %in% names(df)) df$endDate   <- as.Date(as.POSIXct(df$endDate   / 1000, origin = "1970-01-01", tz = "UTC"))

  df$station_requested   <- station
  df$parameter_requested <- parameter
  df$location_type_used  <- location_type

  # reorder columns so startDate and endDate are adjacent
  col_order <- names(df)
  date_cols  <- c("startDate", "endDate")
  date_cols  <- date_cols[date_cols %in% col_order]   # only keep if present

  if (length(date_cols) == 2) {
    # insert both date cols after the position of startDate
    anchor    <- which(col_order == "startDate")
    remaining <- col_order[!col_order %in% date_cols]
    col_order <- append(remaining, date_cols, after = anchor - 1)
    df        <- df[, col_order, drop = FALSE]
  }


  rownames(df) <- NULL
  df
}



#' Retrieve Station Metadata from the DBHydro Insights API
#'
#' @description
#' Queries the SFWMD DBHydro Insights API to retrieve station or site metadata
#' by iterating over one or more station identifiers. For each station, the
#' function attempts a sequence of candidate GET endpoints (e.g.,
#' `stationInfo`, `siteInfo`) until a successful response is returned.
#' Optionally, nested timeseries or DBKEY list-columns in the response are
#' "exploded" into one row per timeseries/DBKEY for downstream use with
#' [insight_dbkey_meta()] or [insight_station_datatype_meta()].
#'
#' @param stations `character` vector. One or more station or site
#'   identifiers to query (e.g., `c("S333_H", "S11A_H")`). Values are
#'   coerced to character, unlisted, and stripped of `NA` or empty strings
#'   before querying.
#' @param timeout_sec `numeric(1)`. Maximum number of seconds to wait for
#'   an API response before timing out. Defaults to `60`.
#' @param pause_sec `numeric(1)`. Number of seconds to pause before each
#'   individual API request. Useful for rate-limiting when querying many
#'   stations in a loop. Defaults to `0` (no pause).
#' @param base_url `character(1)`. Base URL for the SFWMD DBHydro Insights
#'   API. Defaults to
#'   `"https://insightsdata.api.sfwmd.gov/v1/insights-data"`. Change only
#'   if the API host or version path changes.
#' @param station_path `character` vector. One or more candidate endpoint
#'   path segments to attempt for each station, tried in order until one
#'   succeeds. The full request URL is constructed as
#'   `<base_url>/<station_path>/<station>`. Defaults to
#'   `c("stationInfo", "siteInfo", "stationDbkeys", "siteDbkeys")`.
#'   Update this argument if the correct Insights endpoint becomes known.
#' @param explode_timeseries `logical(1)`. If `TRUE` (default), any
#'   nested list-column in the API response whose name matches a known
#'   timeseries/DBKEY candidate (`"timeseries"`, `"timeSeries"`,
#'   `"timeseriesList"`, `"dbkeys"`, `"dbkeyList"`,
#'   `"data"`) is expanded into one row per element, with station and
#'   endpoint metadata carried through to each row. If `FALSE`, the raw
#'   (possibly nested) response is returned as-is.
#'
#' @return A `data.frame` combining results across all requested stations,
#'   with one row per station (or one row per timeseries/DBKEY if
#'   `explode_timeseries = TRUE` and a nested list-column is found).
#'   Columns vary depending on the API response but always include:
#'   \describe{
#'     \item{`station_requested`}{The station identifier passed by the
#'       caller.}
#'     \item{`endpoint_used`}{The `station_path` segment that
#'       returned a successful response.}
#'     \item{`url_used`}{The full URL used for the successful request.}
#'     \item{`request_ok`}{`logical`. `TRUE` if the request
#'       succeeded; `FALSE` if all endpoint attempts failed.}
#'     \item{`error_message`}{`character`. `NA` on success;
#'       a diagnostic message if all attempts failed.}
#'   }
#'   When results across stations have differing column sets, missing columns
#'   are filled with `NA` before binding rows. Row names are reset to
#'   `NULL`.
#'
#' @details
#' For each station in `stations`, `insight_station_meta()` issues
#' sequential GET requests across the `station_path` candidates, stopping
#' at the first successful (non-error, non-404) response. This "try until
#' success" strategy accommodates the fact that the SFWMD Insights API is not
#' publicly documented at the endpoint level, and the correct path may vary
#' by station type (hydrologic vs. water quality).
#'
#' Two internal helpers support robust parsing:
#' \itemize{
#'   \item `null_to_na()` — converts `NULL` elements within a parsed
#'     JSON list to `NA`, preventing `as.data.frame()` failures.
#'   \item `as_df_safe()` — wraps `as.data.frame()` in
#'     [base::tryCatch()], falling back to a single list-column
#'     `data.frame` if coercion fails.
#' }
#'
#' If `explode_timeseries = TRUE` and a recognized nested list-column is
#' found, only the **first** matching candidate column is expanded. The
#' expanded rows inherit `station_requested`, `endpoint_used`,
#' `url_used`, `request_ok`, and `error_message` from the
#' parent response.
#'
#' If **all** `station_path` attempts fail for a given station (due to
#' 404, timeout, or parse errors), a single diagnostic row is returned for
#' that station with `request_ok = FALSE` and a descriptive
#' `error_message`, rather than stopping the entire batch.
#'
#' @note
#' \itemize{
#'   \item The SFWMD Insights API is not publicly documented. Endpoint paths
#'     and response schemas were derived from browser network inspection of
#'     <https://insightsdata.sfwmd.gov> and are subject to change without
#'     notice. If all default `station_path` attempts return 404, update
#'     `station_path` to the correct segment observed in browser
#'     DevTools.
#'   \item This function uses GET requests and is suited for metadata retrieval.
#'     For timeseries data retrieval by station and parameter, see
#'     [insight_station_datatype_meta()], which uses a POST-based
#'     query engine.
#'   \item This function requires the \pkg{httr} and \pkg{jsonlite} packages.
#' }
#'
#' @examples
#' \dontrun{
#' # Single station — try all default endpoint paths
#' meta <- insight_station_meta("S333_H")
#'
#' # Multiple stations with a polite pause between requests
#' meta <- insight_station_meta(
#'   stations  = c("S333_H", "S11A_H", "S12A_H"),
#'   pause_sec = 0.2
#' )
#'
#' # Pin to a known working endpoint (skip trial-and-error)
#' meta <- insight_station_meta(
#'   stations     = "S333_H",
#'   station_path = "stationInfo"
#' )
#'
#' # Return raw nested response without exploding timeseries list-columns
#' meta_raw <- insight_station_meta(
#'   stations           = "S333_H",
#'   explode_timeseries = FALSE
#' )
#'
#' # Check which requests succeeded
#' meta[, c("station_requested", "endpoint_used", "request_ok", "error_message")]
#' }
#'
#' @importFrom httr GET add_headers timeout stop_for_status content
#' @importFrom jsonlite fromJSON
#'
#' @export
insight_station_meta <- function(stations,
                                 timeout_sec = 60,
                                 pause_sec = 0,
                                 # Base host you already use:
                                 base_url = "https://insightsdata.api.sfwmd.gov/v1/insights-data",
                                 # Most likely endpoint patterns (you can change this in one place):
                                 station_path = c("stationInfo", "siteInfo", "stationDbkeys", "siteDbkeys"),
                                 # If TRUE, try to expand nested timeseries/dbkey lists to one row per key
                                 explode_timeseries = TRUE) {

  #  input validation
  stations <- unlist(stations, use.names = FALSE)
  stations <- as.character(stations)
  stations <- stations[!is.na(stations) & nzchar(stations)]
  if (!length(stations)) stop("`stations` must contain at least one station/site identifier.")

  #  helpers
  null_to_na <- function(x) {
    # convert NULL elements inside lists to NA
    if (is.list(x)) {
      x <- lapply(x, function(v) if (is.null(v)) NA else v)
    }
    x
  }

  as_df_safe <- function(x) {
    x <- null_to_na(x)
    df <- tryCatch(
      as.data.frame(x, stringsAsFactors = FALSE),
      error = function(e) data.frame(value = I(list(x)), stringsAsFactors = FALSE)
    )
    df
  }

  # Try multiple endpoint paths until one works for a given station
  fetch_one_station <- function(station) {

    attempt_paths <- station_path

    for (pth in attempt_paths) {

      url <- paste0(base_url, "/", pth, "/", station)

      if (pause_sec > 0) Sys.sleep(pause_sec)

      out <- tryCatch({
        resp <- GET(url, timeout(timeout_sec))
        stop_for_status(resp)

        txt <- content(resp, as = "text", encoding = "UTF-8")
        x <- fromJSON(txt, flatten = TRUE)

        # Convert NULLs to NA where possible
        x <- null_to_na(x)

        df <- as_df_safe(x)
        df$request_ok <- TRUE
        df$error_message <- NA_character_
        df$station_requested <- station
        df$endpoint_used <- pth
        df$url_used <- url

        # Optionally “explode” nested time series lists into one row per dbkey/timeseriesId
        if (explode_timeseries) {
          # common candidate names we might see
          candidates <- c("timeseries", "timeSeries", "timeseriesList", "dbkeys", "dbkeyList", "data")

          found <- intersect(candidates, names(df))
          if (length(found)) {
            col <- found[1]

            # If the column contains a list-column, expand it
            if (is.list(df[[col]])) {
              # Convert list-column elements into data.frames and rbind
              expanded <- lapply(seq_len(nrow(df)), function(i) {
                xi <- df[[col]][[i]]
                if (is.null(xi) || (length(xi) == 1 && is.na(xi))) return(NULL)
                di <- as_df_safe(xi)
                di
              })
              expanded <- Filter(Negate(is.null), expanded)

              if (length(expanded)) {
                exp_df <- do.call(rbind, expanded)
                # carry station + endpoint info down
                exp_df$station_requested <- station
                exp_df$endpoint_used <- pth
                exp_df$url_used <- url
                exp_df$request_ok <- TRUE
                exp_df$error_message <- NA_character_
                return(exp_df)
              }
            }
          }
        }

        df
      }, error = function(e) {
        NULL
      })

      # If success, return immediately
      if (!is.null(out)) return(out)
    }

    # If all paths failed, return diagnostic row
    data.frame(
      request_ok = FALSE,
      error_message = "All station_path endpoint attempts failed (404/timeout/parse). Update `station_path` to the correct Insights endpoint.",
      station_requested = station,
      endpoint_used = NA_character_,
      url_used = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  dfs <- lapply(stations, fetch_one_station)

  #  Combine while allowing different columns across responses
  all_names <- unique(unlist(lapply(dfs, names)))
  dfs_aligned <- lapply(dfs, function(d) {
    missing <- setdiff(all_names, names(d))
    if (length(missing)) d[missing] <- NA
    d[all_names]
  })

  out <- do.call(rbind, dfs_aligned)
  rownames(out) <- NULL
  out
}

