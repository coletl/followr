#' Get contribution data from Follow The Money
#'
#' This package utilizes the Follow The Money API and returns a tidy data frame
#' of political contributions.
#'
#' @param state
#' @param year
#' @param entity
#' @param api_key
#'
#' @export
#' @examples

flw_get_contributions <-
  function (state = NULL, year = NULL, entity = NULL,
            api_key = NULL, tidy = FALSE){
    if (is.null(api_key)) {
      api_key <- Sys.getenv("follow_the_money_key")
      if (nchar(api_key) == 0) {
        stop("No Follow the Money API key found in your environment. Either\n           specify one directly or use flw_set_api_key() to set the\n           environment variable.")
      }
    }

    url <- paste0("https://api.followthemoney.org/?mode=json",
                  "&APIKey=", api_key)
    url <- paste0(url, "&gro=c-t-eid,y,d-id")

    if (!is.null(state)) {
      url <- paste0(url, "&s=", paste0(state, collapse = ","))
    }
    if (!is.null(year)) {
      url <- paste0(url, "&y=", paste0(year, collapse = ","))
    }
    if (!is.null(entity)) {
      url <- paste0(url, "&c-t-eid=", paste0(entity, collapse = ","))
    }

    request <- httr::GET(url, httr::accept("application/json"))
    content <- jsonlite::fromJSON(httr::content(request, "text",
                                                encoding = "UTF-8"))
    records <- content$records
    contributions <- tibble::data_frame(candidate = records$Career_Summary$Career_Summary,
                                        election_year = records$Election_Year$Election_Year,
                                        state = records$State$State, contributors = records$Contributor$Contributor,
                                        general_industry = records$General_Industry$General_Industry,
                                        broad_sector = records$Broad_Sector$Broad_Sector, contribution_date = records$Date$Date,
                                        contributor_city = records$City$City,
                                        contributor_state = records$State$State, contributor_zip = records$Zip$Zip,
                                        amount = records$Amount$Amount)

    # Tidy up
    if(tidy) {
      require(dplyr)

      contributions <-
        contributions %>%
        mutate(contribution_date = as.Date(contribution_date),
               amount = as.double(amount)) %>%
        mutate_if(is.character, function(val) ifelse(val == "", NA_character_, val))
    }

    return(contributions)
  }

