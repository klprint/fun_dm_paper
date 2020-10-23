library(httr)
library(jsonlite)
library(tidyverse)

get_paper_avail <- function(id){

  # country <- toupper(country)
  got <- GET(glue::glue("https://products.dm.de/store-availability/DE/availability?dans=595420,708997,137425,28171,485698,799358,863567,452740,610544,846857,709006,452753,879536,452744,485695,853483,594080,504606,593761,525943,842480,535981,127048,524535&storeNumbers={id}"))

  if(got$status_code == 404){
    return(NULL)
  }

  data <- fromJSON(rawToChar(got$content))

  item_numbers = names(data$storeAvailabilities)
  df <- lapply(1:length(data$storeAvailabilities), function(n){
    tibble(
      stockLevel = data$storeAvailabilities[[n]]$stockLevel,
      storeNumber = data$storeAvailabilities[[n]]$store$storeNumber,
      item_id = item_numbers[n]
    )
  }) %>% do.call(rbind,.)

  df %>%
    group_by(storeNumber) %>%
    summarise(n = sum(stockLevel, na.rm = T)) %>%
    pull(n)
}

get_paper_avail_vec <- function(id){
  k <- 0
  lapply(id, function(i){
    k <<- k+1
    n <- get_paper_avail(i)

    print(k)
    if(k %% 10 == 0) Sys.sleep(1)

    return(n)
  }) %>%
    do.call("c",.)
}

de_stores <- read_csv("data/_ALL_DE_STORES.csv")

de_stores <- de_stores %>%
  mutate(nPaper = get_paper_avail_vec(storeNumber))

de_stores <- de_stores %>%
  select(city, storeNumber, lon, lat, nPaper) %>%
  mutate(
    bin = cut(nPaper, c(-1, 0, 10, 50, 100, 1000))
  )

de_stores <- de_stores %>%
  mutate(date = Sys.Date())

time <- Sys.time()
time <- str_split_fixed(time, " ", 3)[,2]

de_stores <- de_stores %>%
  mutate(time = time)

time <- str_replace_all(time, ":", "_")

write_csv(de_stores,
          glue::glue("data/{Sys.Date()}_{time}_deStoresStatus.csv"))



