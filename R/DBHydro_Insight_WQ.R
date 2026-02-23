#' Fetch SFWMD Insights water quality data (CSV legacy or JSON ts)
#'
#' @param startDate,endDate Date or coercible to Date
#' @param station_id character vector of station IDs (e.g., "S333")
#' @param test_number character/integer vector of parameter/test numbers, or "ALL"
#' @param methods,projects,matrices,paramGroups,sampleTypes optional filters (vector or "ALL")
#' @param api one of "auto", "json", "csv"
#' @param reportType for legacy CSV endpoint (default "timeseries")
#' @param format for legacy endpoint (default "csv")
#' @param offset,limit optional pagination controls for JSON endpoint
#' @param ... additional fields to include in the query payload
#'
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils read.csv
#'
#' @return data.frame with metadata in attr(x, "metadata")
#' @examples
#' \dontrun{
#' sdate <- as.Date("2001-05-01");
#' edate <- as.Date("2002-05-01");
#' dat <- insight_fetch_wq(sdate,edate,c("S12A","S12B"),"25")
#'
#' # retrieve metadata
#' attr(dat,"metadata")
#' }
#'
insight_fetch_wq2 <- function(
    startDate,
    endDate,
    station_id,
    test_number,
    methods = NULL,
    projects = NULL,
    matrices = NULL,
    paramGroups = NULL,
    sampleTypes = NULL,
    api = c("auto","json", "csv"),
    reportType = "timeseries",
    format = "csv",
    offset = NULL,
    limit = NULL,
    ...
) {
  api <- match.arg(api)

  startDate <- as.Date(startDate)
  endDate   <- as.Date(endDate)
  if (startDate > endDate) stop("Check dates: startDate can't be after endDate")
  if (anyNA(test_number)) warning("`test_number` contains NA values")

  startDate_fmt <- format(startDate, "%Y%m%d")
  endDate_fmt   <- format(endDate, "%Y%m%d")

  safe_list <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    as.list(as.character(x))
  }

  loc_list <- lapply(as.character(station_id),
                     function(loc) list(name = loc, type = "STATION"))

  # build JSON query
  parameters <- safe_list(test_number)
  if (is.null(parameters)) parameters <- list("ALL")

  testNumbers <- safe_list(test_number)
  if (is.null(testNumbers)) testNumbers <- list("ALL")

  matrices_l <- safe_list(matrices)
  if (is.null(matrices_l)) matrices_l <- list("ALL")

  methods_l <- safe_list(methods)
  if (is.null(methods_l)) methods_l <- list("ALL")

  paramGroups_l <- safe_list(paramGroups)
  if (is.null(paramGroups_l)) paramGroups_l <- list("ALL")

  projects_l <- safe_list(projects)
  if (is.null(projects_l)) projects_l <- list("ALL")

  sampleTypes_l <- safe_list(sampleTypes)
  if (is.null(sampleTypes_l)) sampleTypes_l <- list("ALL")

  json_query <- list(
    locations   = loc_list,
    parameters  = parameters,
    testNumbers = testNumbers,
    matrices    = matrices_l,
    methods     = methods_l,
    paramGroups = paramGroups_l,
    projects    = projects_l,
    sampleTypes = sampleTypes_l,
    ...
  )

  if (api == "auto") {
    out <- try(
      .insight_fetch_wq_json(startDate_fmt, endDate_fmt, json_query, offset, limit),
      silent = TRUE
    )
    if (!inherits(out, "try-error")) return(.postprocess_wq(out))

    out <- .insight_fetch_wq_csv(
      startDate_fmt, endDate_fmt,
      loc_list, test_number,
      methods, projects, matrices, paramGroups, sampleTypes,
      reportType, format, ...
    )
    return(.postprocess_wq(out))
  }

  if (api == "json") {
    out <- .insight_fetch_wq_json(startDate_fmt, endDate_fmt, json_query, offset, limit)
    return(.postprocess_wq(out))
  }

  out <- .insight_fetch_wq_csv(
    startDate_fmt, endDate_fmt,
    loc_list, test_number,
    methods, projects, matrices, paramGroups, sampleTypes,
    reportType, format, ...
  )

  .postprocess_wq(out)
}


# ---- internal: JSON fetcher ----
.insight_fetch_wq_json <- function(startDate_fmt, endDate_fmt, query_list,
                                   offset = NULL, limit = NULL) {

  base_url <- "https://insightsdata.api.sfwmd.gov/v1/insights-data/chem/ts?"

  url <- paste0(
    base_url,
    "&startDate=", startDate_fmt,
    "&endDate=", endDate_fmt
  )

  if (!is.null(offset)) url <- paste0(url, "&offset=", offset)
  if (!is.null(limit))  url <- paste0(url, "&limit=", limit)

  body <- list(query = query_list)

  res <- httr::POST(
    url,
    body = body,
    encode = "json",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json",
      "Origin" = "https://insightsdata.sfwmd.gov"
    )
  )

  if (httr::status_code(res) != 200) {
    stop("JSON request failed: ", httr::status_code(res))
  }

  txt <- httr::content(res, "text", encoding = "UTF-8")
  dat <- jsonlite::fromJSON(txt, flatten = TRUE)

  results <- dat$results
  if (is.null(results)) stop("No `results` field in JSON response")

  # metadata handling (no %||%)
  if (!is.null(dat$filters)) {
    attr(results, "metadata") <- dat$filters
  } else if (!is.null(dat$metadata)) {
    attr(results, "metadata") <- dat$metadata
  } else {
    attr(results, "metadata") <- list()
  }

  results
}

# internal: CSV (legacy) fetcher
.insight_fetch_wq_csv <- function(
    startDate_fmt, endDate_fmt,
    loc_list, test_number,
    methods, projects, matrices, paramGroups, sampleTypes,
    reportType, format, ...
) {

  url <- paste0(
    "https://insightsdata.api.sfwmd.gov/v1/insights-data/chem/report/data?",
    "reportType=", reportType,
    "&format=", format,
    "&startDate=", startDate_fmt,
    "&endDate=", endDate_fmt
  )

  safe_list <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    as.list(as.character(x))
  }

  body_list <- list(
    locations   = loc_list,
    parameters  = safe_list(test_number),
    methods     = safe_list(methods),
    projects    = safe_list(projects),
    matrices    = safe_list(matrices),
    paramGroups = safe_list(paramGroups),
    sampleTypes = safe_list(sampleTypes),
    ...
  )

  body_list <- body_list[!vapply(body_list, is.null, logical(1))]
  body <- list(query = body_list)

  res <- httr::POST(
    url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    httr::add_headers("Content-Type" = "application/json")
  )

  if (httr::status_code(res) != 200) {
    stop("CSV request failed: ", httr::status_code(res))
  }

  txt <- httr::content(res, "text", encoding = "UTF-8")
  lines <- readLines(textConnection(txt))

  head_idx <- which(!startsWith(lines, "#"))
  head_idx_diff <- c(0, diff(head_idx))
  data_start <- head_idx[head_idx_diff > 1][1]
  if (is.na(data_start)) data_start <- head_idx[1]

  metadata <- lines[1:(data_start - 1)]
  data <- read.csv(text = paste(lines[data_start:length(lines)], collapse = "\n"))

  attr(data, "metadata") <- metadata
  data
}


# internal: common post-processing
.postprocess_wq <- function(data) {
  # Apply your common transformations when columns exist (need to add JSON columns)

    if ("collectDate" %in% names(data)) {
      data$colDATETIME <- date.fun(
        data$collectDate,
        form = "%Y-%m-%d %R",
        tz = "America/New_York"
      )
    }

    if ("firstTriggerDate" %in% names(data)) {
      data$firstTriggerDATETIME <- date.fun(
        data$firstTriggerDate,
        form = "%Y-%m-%d %R",
        tz = "America/New_York"
      )
    }

    if ("sigFigValue" %in% names(data)) {
      data$censored <- data$sigFigValue < 0
      data$HalfMDL  <- ifelse(
        data$censored,
        abs(data$sigFigValue) / 2,
        data$sigFigValue
      )
    }

    data
}


sdate <- as.Date("2001-05-01");
edate <- as.Date("2002-05-01");
dat <- insight_fetch_wq2(sdate,edate,c("S12A","S12B","S333"),"25",methods = "ALL",api = "json")

unique(dat$method)
