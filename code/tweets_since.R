# function to page through a user timeline
#
# args:
#   usr: twitter username
#   id:  which tweet ID to page back from
#   n:   desired number of tweets retrieved
#
# returns:
#   (data.frame) of tweets, ideally with the number of rows = to n
tweets_since <- function(usr = "jack", id = 8675309, n = 1000) {
  # initial pull based on id
  frame_1st <- userTimeline(usr, n = 200, maxID = id) %>% twListToDF()
  
  # assume most dated tweet is at the bottom
  next_id   <- last(frame_1st$id)
  
  # frame to hold results
  frame_out <- data.frame(stringsAsFactors = FALSE)
  
  # track l to manage loop
  # track i to manage number of queries
  l <- nrow(frame_out)
  i <- 0

  while (l < n) {
    Sys.sleep(2)
    message("Querying...")
    tl        <- userTimeline(usr, n = 200, maxID = next_id) %>% twListToDF()
    next_id   <- last(tl$id)
    frame_out <- bind_rows(frame_out, tl)
    l         <- nrow(frame_out)
    message(paste(l, "tweets gathered..."))
    
    # increment i-- stop the loop if maximum is reached
    i <- i + 1
    if (i == 180) {
      message("Query limit reached.")
      break
    }
  }
  
  frame_out <- bind_rows(frame_1st, frame_out)
  frame_out
}
