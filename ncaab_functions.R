# ncaab functions

game_info <- function(nodes) {
  nodes[1] %>% html_nodes("a") %>% html_text() -> t1
  nodes[2] %>% html_text() %>% as.numeric() -> s1
  nodes[4] %>% html_nodes("a") %>% html_text() -> t2
  nodes[5] %>% html_text() %>% as.numeric() -> s2
  tibble(t1 = t1, t2 = t2, s1 = s1, s2 = s2)
}

all_games <- function(scores) {
  map(scores, ~game_info(.)) %>% 
    bind_rows()
}

html_to_scores <- function(html) {
  html %>% 
    html_nodes("table.teams") %>% 
    map(~html_nodes(., "td")) -> scores
  scores
}

date_to_df <- function(date) {
  print(date)
  year <- year(date)
  month <- month(date)
  day <- day(date)
  the_url <- glue("https://www.sports-reference.com/cbb/boxscores/index.cgi?month={month}&day={day}&year={year}")
  html <- read_html(the_url)
  Sys.sleep(1)
  scores <- html_to_scores(html)
  all_games(scores)
}

these_dates <- function(start, end) {
  # start ond end are yyyy-mm-dd dates as strings
  tibble(date = seq(as.Date(start), as.Date(end), "days")) %>% 
    rowwise() %>% 
    mutate(game_scores = list(date_to_df(date))) -> d
  d %>% 
    unnest(game_scores)
  
}