# library(jsonlite)
# library(httr)
# library(ggplot2)
# #library(RColorBrewer)
# library(gridExtra)
# # library(lemon)
# library(knitr)
# #library(kableExtra)
# library(formattable)

#' Get a list of public events from GitHub
#'
#' Returns the 30 most recent public events from Github, including all metadata, as a list of lists.
#'
#'
#' @return list
#' @export
#'
#' @examples get_git_events()
get_git_events <- function() {      # function to retrieve events data from github events api
  url <- "https://api.github.com/events"
  httr::GET(url)
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
        "GitHub API request failed.",
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
      response = resp
    ),
    class = "github_api"
  )
}



#' Create a dataframe of public events from GitHub
#'
#' Returns a dataframe with selected information about the 30 most recent events from Github. Information includes event type, actor, repo and payload if the event was a push.
#'
#'
#' @return dataframe (2D list)
#' @export
#'
#' @examples create_events_df()

create_events_df <- function(){    # function to retrieve dataframe for github events
  git_events <- get_git_events()
  git_events_len <- length(git_events$content)

  # extract event type
  type=rep(NA,git_events_len)
  for (i in 1:git_events_len) {
    type[i]=git_events$content[[i]]$type
  }

  # extract actor name
  actor=rep(NA,git_events_len)
  for (i in 1:git_events_len) {
    actor[i]=git_events$content[[i]]$actor$login
  }

  # extact repo name
  repo=rep(NA,git_events_len)
  for (i in 1:git_events_len) {
    repo[i]=git_events$content[[i]]$repo$name
  }

  # extract payload size
  payload=rep(NULL,git_events_len)
  for (i in 1:git_events_len) {
    payload[i]=ifelse(length(git_events$content[[i]]$payload$size)==0, NA,git_events$content[[i]]$payload$size)
  }

  # convert type, payload, actor, org,repo lists into dataframe
  df=as.data.frame(cbind(type, payload, actor, repo))
  return(df)
}

#' Create plots for event type and payload size
#'
#'
#' @return plot
#' @export
#'
#' @examples get_events_plots()

get_events_plots <- function(){     # function to generate plot for github event type and payload
  df=create_events_df()
  ls <- list()

  g1 <- ggplot2::ggplot(df, ggplot2::aes(type)) +
    ggplot2::geom_bar() +
    ggplot2::labs(title="Event Type",
         y = "Freqency")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,hjust=1))

  ls[[1]] =g1 + ggplot2::scale_fill_brewer(palette="Set2")


  g2 <- ggplot2::ggplot(na.omit(df), ggplot2::aes(payload)) +
    ggplot2::geom_bar()+
    ggplot2::labs(title="Payload Size",
         y = "Freqency")
  ls[[2]] =g2 + ggplot2::scale_fill_brewer(palette="Set2")


  return(gridExtra::marrangeGrob(ls, ncol = 1, nrow = 2))
}


#' Create a table for actor and repo
#'
#'
#' @return table
#' @export
#'
#' @examples get_events_lists()
get_events_lists <- function(){
  df=create_events_df()
  df1=df[,3:4]
  return(df1)
  }

# ## Testing
# get_events_plots()
# get_events_lists("/events")

