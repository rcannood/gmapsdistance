#' Define package environment
#'
#' \code{pkg.env} is a package environment that contains the variable
#' \code{api_key} with the user's Google Maps API key
pkg_env <- new.env()
assign("api_key", NULL, envir = pkg_env)

#' Get the Google Maps API key
#'
#' This function returns the user's Google Maps API key that was defined with
#' \code{set_api_key}.
#'
#' @return the user's api key
#'
#' @export
#'
#' @examples
#' get_api_key()
get_api_key <- function() {
  get("api_key", envir = pkg_env)
}

#' Set the Google Maps API key
#'
#' This function stores a user's Google Maps API key as the package's
#' environmental variable
#'
#' @param key is the user's Google Maps API key
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_api_key("MY-GOOGLE-MAPS-API-KEY")
#' }
set_api_key <- function(key) {
  assign("api_key", key, envir = pkg_env)
}

#' Compute Distance with Google Maps
#'
#' The function gmapsdistance uses the Google Maps Distance Matrix API in order
#' to compute the distance between two points. In order to be able to use the
#' function you will need an API key and enable the Distance Matrix API in the
#' Google Developers Console For more information about how to get a key, go to
#' https://developers.google.com/maps/documentation/distance-matrix/get-api-key#key
#' For more information about the Google Maps Distance Matrix API go to
#' https://developers.google.com/maps/documentation/distance-matrix/intro?hl=en
#'
#' @param origin  A string containing the description of the starting point.
#'   Should be inside of quoutes (""). If more than one word is used, they
#'   should be separated by a plus sign e.g. "Bogota+Colombia". Coordinates in
#'   LAT-LONG format are also a valid input as long as they can be identified by
#'   Google Maps.
#' @param destination A string containing the description of the end point.
#'   Should be the same format as the variable "origin".
#' @param combinations Which combinations of origin and destination should be made.
#'   Can be either "all", or "pairwise".
#' @param time_type Must be either "now", "depart_at", or "arrive_by".
#' @param time Used for specifying departure or arrival times.
#' @param mode A string containing the mode of transportation desired. Should be
#'   inside of double quotes (",") and one of the following: "bicycling",
#'   "walking", "transit" or "driving".
#' @param avoid Whether to avoid "tolls", "highways", "ferries" or "indoor".
#' @param traffic_model Whether to use a "best_guess", "pessimistic", or "optimistic" traffic model.
#' @param key In order to use the Google Maps Distance Matrix API it is
#'   necessary to have an API key. The key should be inside of quotes. Example:
#'   "THISISMYKEY". This key an also be set using \code{set_api_key("THISISMYKEY")}.
#'
#' @import rvest
#' @import xml2
#' @importFrom pbapply pblapply
#'
#' @export
#'
#' @examples
#' gmapsdistance("Washington+DC", "New+York+City+NY")
gmapsdistance <- function(
  origin,
  destination,
  combinations = c("all", "pairwise"),
  time = NULL,
  time_type = c("now", "depart_at", "arrive_by"),
  mode = c("driving",  "walking",  "bicycling",  "transit"),
  avoid = c(),
  traffic_model = c("best_guess",  "pessimistic", "optimistic"),
  key = get_api_key()
) {

  # match arguments
  combinations <- match.arg(combinations)
  time_type <- match.arg(time_type)
  mode <- match.arg(mode)
  traffic_model <- match.arg(traffic_model)

  # If 'avoid' parameter is not recognized:
  avoid_opts <- c("tolls",  "highways",  "ferries",  "indoor")
  if (!all(avoid %in% avoid_opts)) {
    stop(
      "Avoid parameters not recognized. Avoid should be a subset of",
      paste(sQuote(avoid_opts), collapse = ", ")
    )
  }

  # process time
  time_seconds <-
    if (is.null(time)) {
      NA
    } else if (is.character(time)) {
      as.integer(as.POSIXct(strptime(time, "%Y-%m-%d %H:%M:%OS", tz="GMT")))
    } else if (is.finite(time)) {
      time
    } else {
      NA
    }

  if (!is.na(time_seconds) && time_seconds < as.integer(Sys.time())) {
    time_seconds <- NA
  }

  if (time_type != "now" && is.na(time_seconds)) {
    stop("Time must be somewhere in the future, and specified as NULL, 'YYYY-MM-DD HH:MM:SS', or the number of seconds passed since 1970-01-01.")
  }

  time_string <- case_when(
    time_type == "now" ~ "departure_time=now",
    time_type == "depart_at" ~ paste0("departure_time=", time_seconds),
    time_type == "arrive_by" ~ paste0("arrival_time=", time_seconds)
  )

  if (combinations == "pairwise" && length(origin) != length(destination)){
    stop("Size of origin and destination vectors must be the same when using the option: combinations == 'pairwise'")
  }

  # process origin and destination
  if (combinations == "all") {
    data <- crossing(origin, destination)
  } else if(combinations == "pairwise") {
    data <- data_frame(origin, destination)
  }

  # iterate over rows
  bind_rows(pbapply::pblapply(seq_len(nrow(data)), function(i) {
    origin <- data$origin[[i]]
    destination <- data$destination[[i]]

    # Set up URL
    url <- paste0(
      ifelse(is.null(key), "http://", "https://"),
      "maps.googleapis.com/maps/api/distancematrix/xml?origins=", origin,
      "&destinations=", destination,
      "&mode=", mode,
      "&units=metric",
      "&", time_string,
      "&traffic_model=", traffic_model,
      ifelse(!is.null(avoid), paste0("&avoid=", avoid, collapse = ""), ""),
      ifelse(!is.null(key), paste0("&key=", gsub(" ", "", key)), "")
    )

    # read the xml
    xml <- xml2::read_xml(url)

    # return any error messages
    error_message <- html_node(xml, "error_message") %>% html_text
    if (!is.na(error_message)) {
      stop("Google API returned an error: ", error_message)
    }

    # parse the output
    out <- data_frame(
      destination,
      origin,
      status = html_node(xml, "status") %>% html_text,
      duration_value = html_node(xml, "duration value") %>% html_text %>% as.integer,
      duration_text = html_node(xml, "duration text") %>% html_text,
      distance_value = html_node(xml, "distance value") %>% html_text %>% as.integer,
      distance_text = html_node(xml, "distance text") %>% html_text
    )

    if (out$status == "OVER_QUERY_LIMIT") {
      stop("You have exceeded your allocation of API requests for today.")
    }

    # add duration_in_traffic if present
    if (is.null(key) == FALSE && mode == "driving"){
      out <- out %>% mutate(
        duration_in_traffic_value = html_node(xml, "duration_in_traffic value") %>% html_text %>% as.integer,
        duration_in_traffic_text = html_node(xml, "duration_in_traffic text") %>% html_text
      )
    }

    # return output
    out
  }))

}
