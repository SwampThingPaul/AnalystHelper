
#' @title Water Quality Data retrieval from South Florida Water Management District online database (DBHYDRO)

#' @param startDate Required as a Date `as.Date` or other format (i.e. `as.POSIXct`)
#' @param endDate Required as a Date `as.Date` or other format (i.e. `as.POSIXct`)
#' @param station_id SFWMD Water Quality Monitoring location <https://insights.sfwmd.gov/#/mappage/site>
#' @param test_number The DBHydro 1 to 4 digit test number, see details
#' @param methods A 1 to 3 letter code for the method of sample collection, see details
#' @param projects DBHydro water quality project codes
#' @param matrices A 1 to 3 letter code for the sampling material
#' @param paramGroups Letter code for the parameter group, see details
#' @param sampleTypes A letter code defining sample and other type of samples (i.e. QC/QC)
#' @param reportType default is `timeseries`
#' @param format default is `csv`
#' @param ... not used, to pass argument along to other functions, in the future.
#'
#' @description using the new data management platform DBHydro Insights,
#'  this function using the Districts API access to retrieve data from the database
#' @details
#' For `test_number`, `methods`,`projects`,`matrices`,`sampleTypes` and others
#' see the `insight_ref()` function for the corresponding tables.
#'
#' The function returns a data.frame from DBHydro insights with extract columns, all original data is unaltered.
#' The following columns have been added to the data.frame
#' \itemize{
#'  \item `colDATETIME` = Collection datetime as a POSIXct object in the America/New_York timezone (EST/EDT)
#'  \item `firstTriggerDATETIME` = for autosamplers, First Trigger Date as a POSIXct object in the America/New_York timezone (EST/EDT)
#'  \item `censored` = logical `TRUE`/`FALSE` if the values was reported below the MDL
#'  \item `HalfMDL` = if the value was reported less than the MDL than the value was replaced by half the MDL.
#'  }
#'
#'
#' For `paramGroups`
#' \itemize{
#'  \item O: organics
#'  \item N: nutrients
#'  \item M: metals
#'  \item P: physical parameters
#'  \item B: biological
#'  \item F: field
#'  \item MI: major ions
#'  \item MIS: miscellaneous
#'  }
#'
#' @importFrom httr POST add_headers status_code
#' @importFrom jsonlite toJSON
#' @export
#' @examples
#' \dontrun{
#' sdate <- as.Date("2001-05-01");
#' edate <- as.Date("2002-05-01");
#' dat <- insight_fetch_wq(sdate,edate,c("S12A","S12B"),"25")
#'
#' # retrieve metadata
#' attr(dat,"metadata")
#' }
insight_fetch_wq <- function(
    startDate,
    endDate,
    station_id,
    test_number,
    methods = NULL,projects = NULL,
    matrices = NULL,paramGroups = NULL,
    sampleTypes = NULL,
    reportType = "timeseries",format = "csv", ...
){
  if(startDate>endDate){stop("Check dates date_min can't be after date_max")}
  if(anyNA(test_number)) {warning("`test_number` is missing or contains NA values.")}

  startDate_fmt <- format(as.Date(startDate), "%Y%m%d")
  endDate_fmt <- format(as.Date(endDate), "%Y%m%d")

  url <- paste0(
    "https://insightsdata.api.sfwmd.gov/v1/insights-data/chem/report/data?",
    "reportType=", reportType,
    "&format=", format,
    "&startDate=", startDate_fmt,
    "&endDate=", endDate_fmt
  )

  # Build query lists
  loc_list <- lapply(station_id, function(loc) list(name = loc, type = "STATION"))

  # Helper to include argument only if not NULL or empty
  safe_list <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    as.list(as.character(x))
  }

  body_list <- list(
    locations = loc_list,
    parameters = safe_list(test_number),
    methods = safe_list(methods),
    projects = safe_list(projects),
    matrices = safe_list(matrices),
    paramGroups = safe_list(paramGroups),
    sampleTypes = safe_list(sampleTypes),
    ...
  )

  # Remove NULL entries (arguments not passed)
  body_list <- body_list[!sapply(body_list, is.null)]

  body <- list(query = body_list)

  res <- POST(
    url,
    add_headers(`Content-Type` = "application/json"),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  if (status_code(res) != 200) {
    stop("Request failed with status: ", httr::status_code(res))
  }

  csv <- content(res, as = "text", encoding = "UTF-8")
  lines <- readLines(textConnection(csv))

  # Identify where the actual CSV header begins (usually first non-`#` line)
  head_idx <- which(!startsWith(lines, "#"))
  head_idx_diff <- c(0,diff(head_idx))

  data_start <- head_idx[head_idx_diff>1]#which(!startsWith(lines, "#"))[1]

  # Extract metadata/header separately
  metadata <- lines[1:(data_start - 1)]
  metadata
  # read the actual data
  data <- read.csv(text = paste(lines[data_start:length(lines)],collapse="\n" ))

  data$colDATETIME <- date.fun(data$collectDate,form="%Y-%m-%d %R",tz="America/New_York")
  data$firstTriggerDATETIME <- date.fun(data$firstTriggerDate,form="%Y-%m-%d %R",tz="America/New_York")
  data$censored <- ifelse(data$sigFigValue<0,TRUE,FALSE)
  data$HalfMDL <- ifelse(data$censored == TRUE,abs(data$sigFigValue)/2,data$sigFigValue)

  # store metadata as attribute
  attr(data, "metadata") <- metadata
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
