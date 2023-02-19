# Examples from the promise package rewritten as async

(promise <- future_promise(operation()) %>%
  then(onRejected = function(err) {
    warning("An error occurred: ", err)
    warning("Using default value of 0 instead")
    0
  }
  )
)

promise <- async({
  tryCatch(
    operation(),
    function(err) {
      warning("An error occurred: ", err)
      warning("Using default value of 0 instead")
      0
    })
})


# support on.exit for cleaning up in generators/asyncawaits
# async shoud by default do what future_promise does?

library(async)
library(promises)
library(futures)
plan(multisession)

function(input, output, session) {
  output$plot <- renderPlot(async({
    result <- await(future_promise({ expensive_operation }))
    plot(head(result, input$n))
  }))
}


# you should be able to read promises from within async blocks

# It seems like I'm going to be writing await(future_promise()) a lot
# so maybe I just collapse that into await_future().  Also it would be
# nice to not have a dependency on promises (or at least not require
# importnig promises) (but nevertheless implement objects that work
# with the promises package.)

function(input, output, session) {
  observeEvent(input$refresh_data, {
    future_promise({
      df <- read.csv(url)
      saveRDS(df, "cached.rds")
      df
    }) %...>%
      data()
  })
}

function(input, output, session) {
  observeEvent(input$refresh_data, async({
    data <- await_future({
      df <- read.csv(url)
      saveRDS(df, "cached.rds")
      df
    })
    data(df)
  })
  }

  # "cranwhales" seems like a decent place to start.


  data <- eventReactive(input$date, {
    date <- input$date
    year <- lubridate::year(date)

    url <- glue("http://cran-logs.rstudio.com/{year}/{date}.csv.gz")
    path <- file.path("data_cache", paste0(date, ".csv.gz"))

    async(
      await_future({
        if (!file.exists(path)) {
          download.file(url, path)
        }
        read_csv(path, col_types = "Dti---c-ci", progress = FALSE)
      })
  })

  whales <- reactive(async({
    await(data) %>%
      count(ip_id) %>%
      arrange(desc(n)) %>%
      head(input$count)
  }))

  whale_downloads <- reactive(async({
    data_df <- await(data())
    whales_df <- await(whales())
    # we should write await_all() anyway...
    inner_join(data_df, whales_df, "ip_id") %>% select(-n)
  }))

  output$total_downloaders <- renderValueBox(async({
    await(data) %>%
      pull(ip_id) %>%
      unique() %>%
      length() %>%
      format(big.mark = ",") %>%
      valueBox("unique downloaders")
  }))

  output$downloaders <- renderPlot(async({
    await(whales()) %>%
      ggplot(aes(ip_name, n)) +
      geom_bar(stat = "identity") +
      ylab("Downloads on this day")
  }))


  data <- eventReactive(input$date, async({
    date <- input$date
    year <- lubridate::year(date)

    url <- glue("http://cran-logs.rstudio.com/{year}/{date}.csv.gz")
    path <- file.path("data_cache", paste0(date, ".csv.gz"))

    p <- Progress$new()
    p$set(value = NULL, message = "Downloading data...")
    await_future(
      if (!file.exists(path)) {
        download.file(url, path)
      }
    )
    p$set(message = "Parsing data...")
    tryCatch(
      await(read_csv(path, col_types = "Dti---c-ci", progress = FALSE)),
      finally=p$close)
  }))


  # now reading the future package proper. This means...

  library(future)
  plan(multisession)

  # Now what is this %<-% thing doing? is it making an R-level promise
  # or something like that? Yup, that's exactly what it is.

plan(multisession)
pid <- Sys.getpid()
a %<-% {
  pid <- Sys.getpid()
  cat("Future 'a' ...\n")
  3.14
}
b %<-% {
  rm(pid)
  cat("Future 'b' ...\n")
  Sys.getpid()
}
c %<-% {  # this prints "future a" on account of capturing the variable "a"!
  cat("Future 'c' ...\n")
  2 * a
}
b
c
