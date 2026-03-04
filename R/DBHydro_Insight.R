#' Build a DBHydro Insights water-quality (chem) query payload
#'
#' @description
#' Constructs a nested list formatted for use as the `query` object in
#' DBHydro Insights chemistry/water-quality report requests (e.g.,
#' `/v1/insights-data/chem/report/data`). The output is intended to be
#' JSON-encoded and sent as a request body such as `list(query = <result>)`.
#'
#' @details
#' The function standardizes common filter fields (`parameters`, `methods`,
#' `projects`, `matrices`, `paramGroups`) into lists. Locations are
#' formatted as a list of `list(name = <...>, type = <...>)` objects.
#'
#' If `locations` is exactly `"ALL"`, then the location type is forced to
#' `"ALL"` (regardless of `location_type`).
#'
#' Additional filters supported by the API (e.g., `testNumbers`, `sampleTypes`,
#' `paramGroups`, etc.) can be provided via `...` and will be included in the
#' returned list as-is (after the function adds its standard fields).
#'
#' @param locations Character vector of location identifiers (e.g., station codes).
#'   Use `"ALL"` to request all locations.
#' @param location_type Character scalar giving the location identifier type used when
#'   `locations` is not `"ALL"`. Common values include `"STATION"` (default),
#'   and other API-supported types.
#' @param parameters Character vector of parameter identifiers, or `"ALL"`.
#' @param methods Character vector of method identifiers, or `"ALL"`.
#' @param projects Character vector of project identifiers, or `"ALL"`.
#' @param matrices Character vector of matrix identifiers, or `"ALL"`.
#' @param paramGroups Character vector of parameter-group identifiers, or `"ALL"`.
#' @param testNumbers Optional numeric vector (or list) of test numbers to filter by.
#'   If `NULL` (default), the field is omitted from the query payload.
#' @param ... Additional named query elements to include in the payload.
#'
#' @return
#' A nested `list` representing the query payload. This object is suitable for JSON
#' encoding and inclusion in a request body such as `list(query = insight_wq_query(...))`.
#'
#' @examples
#' # Single station, include an additional filter (testNumbers)
#' # single test number
#' q1 <- insight_wq_query(locations = "S333", testNumbers = 25)
#'
#' # Multiple stations
#' q2 <- insight_wq_query(
#'   locations = c("S333", "G722"),
#'   testNumbers = c(25)
#' )
#'
#' # All locations (location type forced to ALL)
#' q3 <- insight_wq_query(locations = "ALL")
#'
#' @export
insight_wq_query <- function (locations = "ALL",
                              location_type = "STATION",
                              parameters = "ALL",
                              methods = "ALL",
                              projects = "ALL",
                              matrices = "ALL",
                              paramGroups = "ALL",
                              testNumbers = NULL,
                              ...) {

  # helper: normalize scalar/vector/list to list of numerics
  as_numeric_list <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    as.list(x)
  }

  x <- list(
    parameters = as.list(parameters),
    methods = as.list(methods),
    projects = as.list(projects),
    matrices = as.list(matrices),
    paramGroups = as.list(paramGroups),
    ...
  )

  # only include testNumbers if provided
  tn <- as_numeric_list(testNumbers)
  if (!is.null(tn) && length(tn) > 0) {
    x[["testNumbers"]] <- tn
  }

  # location formatting
  if (length(locations) == 1 && locations == "ALL") {
    x[["locations"]] <- list(list(name = locations, type = "ALL"))
  } else {
    x[["locations"]] <- lapply(locations, function(loc) {
      list(name = loc, type = location_type)
    })
  }

  x
}

#' Fetch DBHydro Insights water-quality (chemistry) time series data
#'
#' @description
#' Queries the SFWMD DBHydro Insights Chemistry reporting endpoint and returns a
#' parsed `data.frame` of water-quality time series results for a station and
#' (optionally) one or more test numbers. The function sends a POST request with a
#' JSON body containing the query payload produced by [insight_wq_query()].
#'
#' @details
#' The function:
#' \itemize{
#'   \item Validates that `startDate <= endDate`.
#'   \item Formats `startDate` and `endDate` as `YYYYMMDD`.
#'   \item Builds a query payload using [insight_wq_query()] with
#'         `locations = station_id` and `testNumbers = test_number`.
#'   \item Calls `https://insightsdata.api.sfwmd.gov/v1/insights-data/chem/report/data`
#'         using [httr::POST()] with query parameters
#'         `reportType`, `format="json"`, `startDate`, and `endDate`.
#'   \item Parses the JSON response using [jsonlite::fromJSON()]
#'         and extracts `$timeseries`.
#'   \item Coerces `sigfigValue` to numeric and derives convenience fields:
#'         `colDATETIME`, `firstTriggerDATETIME`, `censored`, and `HalfMDL`.
#' }
#'
#' The derived fields are computed as:
#' \itemize{
#'   \item `censored`: `TRUE` if `sigfigValue < 0`, else `FALSE`.
#'   \item `HalfMDL`: if censored, `abs(sigfigValue) / 2`, else `sigfigValue`.
#' }
#'
#' This function uses a helper `date.fun()` to parse date-time strings to POSIXct
#' in the `"America/New_York"` timezone. Ensure `date.fun()` is available
#' (e.g., exported by your package or attached in the calling environment).
#'
#' @param startDate Start date. Can be a `Date` object or a character string
#'   coercible by `as.Date()` (e.g., `"2001-10-05"`).
#' @param endDate End date. Can be a `Date` object or a character string
#'   coercible by `as.Date()` (e.g., `"2002-03-04"`).
#' @param station_id Character scalar station identifier (e.g., `"S333"`).
#'   Passed to [insight_wq_query()] as `locations`.
#' @param test_number Numeric, integer, character, or list. Test number(s) used to
#'   filter results; passed to [insight_wq_query()] as `testNumbers`.
#'   If `NA` values are present, a warning is emitted.
#' @param reportType Character scalar report type for the API. Defaults to
#'   `"timeseries"`. Other values may be supported by the API.
#' @param ... Additional named query elements passed to [insight_wq_query()].
#'   Use this to include other filters supported by the endpoint (e.g., `parameters`,
#'   `methods`, `projects`, `matrices`, `paramGroups`, etc.).
#'
#' @return
#' A `data.frame` containing the `timeseries` object returned by the API,
#' with additional derived columns:
#' \itemize{
#'   \item `colDATETIME` (POSIXct): parsed from `collectDate`.
#'   \item `firstTriggerDATETIME` (POSIXct): parsed from `firstTriggerDate`.
#'   \item `censored` (logical): indicates `sigfigValue < 0`.
#'   \item `HalfMDL` (numeric): half the absolute value for censored results, else the value.
#' }
#'
#' @seealso
#' [insight_wq_query()] for building the query payload.
#'
#' @examples
#' \dontrun{
#' # Basic request for a station and a single test number
#' df <- insight_fetch_wq(
#'   startDate   = "2001-10-05",
#'   endDate     = "2002-03-04",
#'   station_id  = "S333",
#'   test_number = 25
#' )
#'
#' # Pass additional filters via ...
#' df2 <- insight_fetch_wq(
#'   startDate   = as.Date("2001-10-05"),
#'   endDate     = as.Date("2002-03-04"),
#'   station_id  = "S333",
#'   test_number = c(25, 26),
#'   parameters  = "ALL",
#'   matrices    = "ALL"
#' )
#' }
#'
#' @export
insight_fetch_wq <- function(
    startDate,
    endDate,
    station_id,
    test_number,
    reportType = "timeseries", ...
){

  if(startDate>endDate){stop("Check dates date_min can't be after date_max")}
  if(anyNA(test_number)) {warning("`test_number` is missing or contains NA values.")}

  startDate_fmt <- format(as.Date(startDate), "%Y%m%d")
  endDate_fmt <- format(as.Date(endDate), "%Y%m%d")

  query <- insight_wq_query(locations = station_id, testNumbers = test_number, ...)

  url <- "https://insightsdata.api.sfwmd.gov/v1/insights-data/chem/report/data"

  res <- POST(
    url,
    query = list(
      reportType = reportType,
      format = "json",
      startDate = startDate_fmt,
      endDate = endDate_fmt
    ),
    body = list(query = query),
    encode = "json"
  )

  # error handling
  stop_for_status(res)

  if (status_code(res) != 200) {
    stop("Request failed with status: ", httr::status_code(res))
  }

  # extract and parse
  txt <- content(res, as = "text", encoding = "UTF-8")
  obj <- fromJSON(txt, flatten = TRUE)

  # pull timeseries
  ts <- obj$timeseries

  # if timeseries is a list, ts may already be a data.frame when flattened.
  # Otherwise, you can coerce:
  if (!is.data.frame(ts)) {
    ts <- as.data.frame(ts, stringsAsFactors = FALSE)
  }
  data <- ts
  data$sigfigValue <- as.numeric(data$sigfigValue)

  data$colDATETIME <- date.fun(data$collectDate,form="%Y-%m-%d %R",tz="America/New_York")
  data$firstTriggerDATETIME <- date.fun(data$firstTriggerDate,form="%Y-%m-%d %R",tz="America/New_York")
  data$censored <- ifelse(data$sigfigValue<0,TRUE,FALSE)
  data$HalfMDL <- ifelse(data$censored == TRUE,abs(data$sigfigValue)/2,data$sigfigValue)

  meta <- unlist(obj[!(names(obj)%in%"timeseries")])
  attr(data, "metadata") <- meta
  return(data)

}



#' @title Hydrologic and continuous data retrieval from South Florida Water Management District online database (DBHYDRO)
#'
#' @param startDate Required as a Date `as.Date` or other format (i.e. `as.POSIXct`)
#' @param endDate Required as a Date `as.Date` or other format (i.e. `as.POSIXct`)
#' @param dbkey Required as a character, defines which data is to be accessed. Also called `Timeseries ID`
#' @param datum vertical datum of presented data (`NGVD29` or `NAVD88`)
#' @param period Timespan of data requested (1day, 3days, 1week, 2 weeks, 30days, 365days, por)
#' @param reportType default is `timeseries`, Defines the structure of the data presentation see user manual for more
#' @param format set to json for ease of data retrieval
#' @param ... not used, to pass argument along to other functions, in the future.
#' @description using the new data management platform DBHydro Insights,
#'  this function using the Districts API access to retrieve data from the database
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' # Daily Discharge Data
#' sdate <- as.Date("2001-05-01");
#' edate <- as.Date("2002-05-01");
#' dat <- insight_fetch_daily(sdate,edate,"FE771")
#'
#' # retrieve metadata
#' attr(dat,"metadata")
#' }
insight_fetch_daily <- function(
    startDate,
    endDate,
    dbkey,
    datum = NULL,
    period = NULL,
    reportType = "timeseries",
    format = "json",
    ...
){
  if(startDate>endDate){stop("Check dates date_min can't be after date_max")}
  startDate_fmt <- format(as.Date(startDate), "%Y%m%d")
  endDate_fmt <- format(as.Date(endDate), "%Y%m%d")

  base_url <- "https://insightsdata.api.sfwmd.gov/v1/insights-data/cont/data"

  # Collapse timeseries IDs into comma-separated string
  ids <- paste(dbkey, collapse = ",")

  # Helper to include argument only if not NULL or empty
  safe_list <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    as.list(as.character(x))
  }

  query <- list(
    timeseriesIds = ids,
    startDate = (startDate_fmt),
    endDate =(endDate_fmt),
    period = (period),
    datum = (datum),
    reportType = reportType,
    format = format,
    ...
  )

  # Remove NULL entries (arguments not passed)
  query <- query[!sapply(query, is.null)]

  res <- GET(base_url, query = query)
  # Parse JSON content
  json_data <- content(res, as = "text",encoding = "UTF-8", type = "application/json")

  df <- fromJSON(json_data,flatten=T)

  data <- df$timeseries
  colnames_vals <- c("DBKEY","Station","source","parameter","DATETIME","Data.Value","units","code","revisionDate","qualityCode")
  colnames(data) <- colnames_vals
  data$Date <- as.POSIXct(data$DATETIME,format="%m/%d/%Y",tz="EST")
  data$DATETIME <- as.POSIXct(data$DATETIME,format="%m/%d/%Y %H:%M:%S",tz="EST")
  tail(data)

  meta <- unlist(df[!(names(df)%in%"timeseries")])

  # store metadata as attribute
  attr(data, "metadata") <- meta
  return(data)
}

#' @title DBHydro Insights reference tables
#'
#' @param table_name reference table name from <https://insightsdata.sfwmd.gov/#/reference-tables>
#'
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' insight_fetch_ref("collectionMethod")
#' }
insight_fetch_ref <- function(table_name){

  url <- paste0("https://insightsdata.api.sfwmd.gov/v1/insights-data/referenceTables/lookup?referenceTable=",table_name)

  # Get data from API
  res <- GET(url)

  # Parse JSON content
  json_data <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(json_data, flatten = TRUE)

  # Extract the 'results' field and convert to data.frame
  project_df <- as.data.frame(parsed$results)

  return(project_df)
}


#' @title A list of reference table names for use with `insight_fetch_ref`
#'
#' @description A character vector of available reference table names as of August 2025 from <https://insightsdata.sfwmd.gov/#/reference-tables>.
#'
#' @format A character vector of length 28.
#' @keywords data
#' @export
#' @examples
#' \dontrun{
#' ref_table_name
#' insight_fetch_ref(ref_table_name[1])  # e.g., "agency"; list of agencies
#' }

ref_table_name <- c("agency","basin","qualityCode","dataType","frequency","group","project","offsets",
                    "recorderType","sensor","stationType","statisticType","structureType","refElv",
                    "collectionMethod","dataInvestigation","qualifier","discharge","genderCode",
                    "matrix","programType","sampleType","samplingPurpose","species","tissueType",
                    "upDwnStream","validationLevel","weatherCode")


#' Retrieve DBHYDRO Insights DBKEY metadata for multiple DBKEYs (safe)
#'
#' Queries the SFWMD DBHYDRO Insights API `dbkeyInfo` endpoint for one or
#' more DBKEYs and returns a combined data frame with one row per requested DBKEY.
#'
#' Unlike a "fail-fast" implementation, this function is *safe* for batch pulls:
#' it will continue processing even if one or more DBKEY requests fail. For each
#' DBKEY, the returned row includes `request_ok` and `error_message`
#' indicating whether that specific request succeeded.
#'
#' For successful requests, the JSON response is flattened and any `NULL`
#' elements are converted to `NA` prior to coercion into a 1-row data frame.
#' The output always includes `dbkey_requested` to preserve the original
#' input DBKEY, even if the API response is missing or malformed.
#'
#' @param dbkeys A character or numeric vector of DBKEY identifiers. Each DBKEY
#'   is requested individually (one HTTP request per DBKEY).
#' @param timeout_sec Numeric scalar. Request timeout in seconds passed to
#'   `httr::timeout()`. Default is 60.
#' @param pause_sec Numeric scalar. Optional delay (in seconds) between requests
#'   to reduce the chance of rate-limiting. Default is 0 (no pause).
#'
#' @return A `data.frame` with one row per requested DBKEY. For successful
#'   requests, columns include metadata fields returned by the API (e.g.,
#'   `station`, `dataType`, `frequency`, `startDate`,
#'   `endDate`, coordinates, etc.). The output always contains:
#'   \describe{
#'     \item{`dbkey_requested`}{DBKEY value submitted in the request.}
#'     \item{`request_ok`}{Logical flag indicating if that request succeeded.}
#'     \item{`error_message`}{Character error message when `request_ok = FALSE`; otherwise `NA`.}
#'   }
#'
#' @details
#' This function performs one request per DBKEY and then row-binds the results.
#' Because API responses can vary slightly across DBKEYs, it aligns columns across
#' all returned rows, filling missing columns with `NA` prior to binding.
#'
#' @seealso [httr::GET()], [httr::timeout()],
#'   [httr::stop_for_status()], [jsonlite::fromJSON()]
#'
#' @importFrom httr GET timeout stop_for_status content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' # Successful and failing DBKEYs can be mixed; failures are captured in output
#' meta <- insight_dbkey_meta(c("91510", "BADKEY", "91512"), pause_sec = 0.1)
#'
#' # Inspect results
#' head(meta)
#'
#' # See which DBKEYs failed
#' subset(meta, !request_ok)
#' }
#'
#' @export
insight_dbkey_meta <- function(dbkeys, timeout_sec = 60, pause_sec = 0) {
  # input validation
  dbkeys <- unlist(dbkeys, use.names = FALSE)
  dbkeys <- as.character(dbkeys)
  if (!length(dbkeys)) stop("`dbkeys` must contain at least one DBKEY.")

  fetch_one_safe <- function(dbkey) {
    url <- paste0("https://insightsdata.api.sfwmd.gov/v1/insights-data/dbkeyInfo/", dbkey)

    # optional pause to be polite / avoid rate limits
    if (pause_sec > 0) Sys.sleep(pause_sec)

    out <- tryCatch({
      resp <- GET(url, timeout(timeout_sec))
      stop_for_status(resp)

      txt <- content(resp, as = "text", encoding = "UTF-8")
      x <- fromJSON(txt, flatten = TRUE)

      # Convert NULL to NA
      x[x %in% list(NULL)] <- NA

      df <- as.data.frame(x, stringsAsFactors = FALSE)

      df$request_ok <- TRUE
      df$error_message <- NA_character_
      df$dbkey_requested <- as.character(dbkey)
      df
    }, error = function(e) {
      data.frame(
        dbkey = NA_character_,
        request_ok = FALSE,
        error_message = conditionMessage(e),
        dbkey_requested = as.character(dbkey),
        stringsAsFactors = FALSE
      )
    })

    out
  }

  dfs <- lapply(dbkeys, fetch_one_safe)

  # Combine while allowing different columns across responses
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
