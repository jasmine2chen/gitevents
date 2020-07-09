# library(httr)
# library(jsonlite)
# library(dplyr)

#' Get a list of events from a GitHub respository
#'
#' Returns the 30 most recent events from a public repository (within the last 90 days), including all metadata, as a list of lists.
#'
#' @param owner string
#' @param repo string
#'
#' @return list
#' @export
#'
#' @examples
#' get_repo_events("nganlyle", "534_project")
get_repo_events <- function(owner, repo) {
  path <- sprintf("repos/%s/%s/events", owner, repo)
  url <- httr::modify_url("https://api.github.com/", path = path)
  resp <- httr::GET(url)

# raise error if the reponse is not in json
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

# parse response with jsonlite
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

# raise error if the request fails
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )

}

#' Create a dataframe of events from a GitHub repository
#'
#' Returns a dataframe with selected information about the 30 most recent events from a public repository (within the last 90 days). Information includes event id, event type, date, time, author and push message if the event was a push.
#'
#' @param owner string
#' @param repo string
#'
#' @return dataframe (2D list)
#' @export
#'
#' @examples create_repo_df("nganlyle", "534_project")
create_repo_df <- function(owner, repo) {
  repo_events <- get_repo_events(owner, repo)
  repo_events_no <- length(repo_events$content)

  type <- rep(NA, repo_events_no) # extract event type
  for (i in 1:repo_events_no) {
    type[i] <- repo_events$content[[i]]$type
  }

  datetime <- rep(NA, repo_events_no) # extract event date/time
  for (i in 1:repo_events_no) {
    datetime[i] <- repo_events$content[[i]]$created_at
  }

  # extract actor info
  actor_id <- rep(NA, repo_events_no)
  for (i in 1:repo_events_no) {
    actor_id[i] <- repo_events$content[[i]]$actor$id
  }

  actor_dlogin <- rep(NA, repo_events_no)
  for (i in 1:repo_events_no) {
    actor_dlogin[i] <- repo_events$content[[i]]$actor$display_login
  }

  event_id <- rep(NA, repo_events_no) # extract event id
  for (i in 1:repo_events_no) {
    event_id[i] <- repo_events$content[[i]]$id
  }

  # extract msg if pushevent
  push_msg <- rep(NULL, repo_events_no)
  for (i in 1:repo_events_no) {
    if (repo_events$content[[i]]$type == "PushEvent") {
      push_msg[i] <- repo_events$content[[i]]$payload$commits[[1]]$message
    }
    else {
      push_msg[i] <- NA
    }
  }

  # create a df with events info
  df <- as.data.frame(cbind(event_id, type, datetime, actor_id, actor_dlogin, push_msg))
  df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%dT%H:%M:%OS")
  return(df)
}

#' Create events stamp for a repository
#'
#' @param owner string
#' @param repo string
#' @param log_date string yyyy-mm-dd
#'
#' @return dataframe (2D list)
#' @export
#'
#' @examples get_repo_log("nganlyle", "534_project", "2020-01-26")
get_repo_log <- function(owner, repo, log_date) {
  df <- create_repo_df(owner, repo)
  df[ , 'datecol'] <- as.Date(as.character(df$datetime))
  df[ , 'timecol'] <- format(df$datetime, format = "%H:%M:%OS")
  df <- subset(df, datecol == log_date)
  select <- c("event_id","type","actor_dlogin","push_msg",
    "timecol")
  stamp <- df[select]
  colnames(stamp) <- c("Event ID", "Event Type", "Actor", "Message", "Time")
  return(stamp)
  # kableExtra::kable(stamp, format = "html", col.names = c("Event ID", "Event Type",
  #   "Actor", "Message", "Time"), caption = log_date)
  # print(paste("Events on ", log_date))
  # print(stamp)
}


# ## Testing
# events_534 <- get_repo_events("nganlyle", "534_project")
# events_httr <- get_repo_events("hadley", "httr")
#
# df_534 <- create_repo_df("nganlyle", "534_project")
# df_httr <- create_repo_df("hadley", "httr")
# df_pandas <- create_repo_df("pandas-dev", "pandas")
#
# log <- get_repo_log("nganlyle", "534_project", "2020-01-26")


  # repo_name = rep(NA,repo_events_no)
  # for (i in 1:repo_events_no) {
  #   repo_name[i]=repo_events$content[[i]]$repo$name
  # }
  #
  # repo_id = rep(NA,repo_events_no)
  # for (i in 1:repo_events_no) {
  #   repo_id[i]=repo_events$content[[i]]$repo$id
  # }
  # repo_url = rep(NA,repo_events_no)
  # for (i in 1:repo_events_no) {
  #   repo_url[i]=repo_events$content[[i]]$repo$url
  # }
