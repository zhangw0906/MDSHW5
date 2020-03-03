#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' get_oldest_player()
get_oldest_player <- function(df , year = 1986) {

  # read the data
  bulk_data = df
  # select the year.
  filter(bulk_data , Year == year)  %>%               # filter by the  indicated year
    select(Player, Age , PTS) %>%                     # select just the three columns
    filter(Age == max(Age)) %>%                     # filter to the oldest age
    arrange(desc(PTS) ) %>%                       # order them from most PTS to least
    head(1) -> oldest_scorer                    # take the top one

  return(oldest_scorer[['Player']])

}
