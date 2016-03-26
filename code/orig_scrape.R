# initialize --------------------------------------------------------------

library(twitteR)
library(dplyr)

# source api credentials
source("api-credentials.R")

setup_twitter_oauth(
  api_key,
  api_sec,
  api_tok,
  api_tok_sec
)

# gather ------------------------------------------------------------------

first_hc <- userTimeline("hillaryclinton", n = 200)
id       <- last(first_hc)$id
dat_hc   <- data.frame()

while (nrow(dat_hc) < 1500) {
  Sys.sleep(2)
  message("Querying...")
  hc  <- userTimeline("hillaryclinton", n = 200, maxID = id)

  id  <- last(hc)$id
  hc  <- twListToDF(hc)

  message("Binding...")
  dat_hc <- bind_rows(dat_hc, hc)
  message(paste0(nrow(dat_hc), " tweets..."))
}

dat_hc <- twListToDF(first_hc) %>% bind_rows(., dat_hc)

first_bs <- userTimeline("berniesanders", n = 200)
id       <- last(first_hc)$id
dat_bs   <- data.frame()

while(nrow(dat_bs) < 1500) {
  Sys.sleep(2)
  message("Querying...")
  bs  <- userTimeline("berniesanders", n = 200, maxID = id)

  id  <- last(bs)$id
  bs  <- twListToDF(bs)

  message("Binding...")
  dat_bs <- bind_rows(dat_bs, bs)
  message(paste0(nrow(dat_bs), " tweets..."))
}

dat_bs <- twListToDF(first_bs) %>% bind_rows(., dat_bs)

# cleaning ----------------------------------------------------------------

library(quanteda)
library(lubridate)

dat_hc <- dat_hc %>%
  mutate(
    yr   = year(created),
    mo   = month(created)
  )

sum_hc <- dat_hc %>%
  group_by(mo) %>%
  summarise(
    total = n(),
    socialism = sum(grepl("socialism", text), grepl("Socialism", text)),

    # race
    afri  = sum(grepl("Afri", text), grepl("afri", text)),
    race  = sum(grepl("race", text), grepl("Race", text)),
    priv  = sum(grepl("privilege", text), grepl("privilege", text)),
    white = sum(grepl("white", text), grepl("White", text)),
    black = sum(grepl("black", text), grepl("Black", text)),
    all_race   = sum(afri, race, priv, white, black),

    # gender
    gender  = sum(grepl("Gender", text), grepl("gender", text)),
    women   = sum(grepl("Women", text), grepl("women", text)),
    woman   = sum(grepl("Woman", text), grepl("woman", text)),
    moms    = sum(grepl("moms", text), grepl("Moms", text)),
    all_gen = sum(gender, women, woman, moms),

    # college
    college = sum(grepl("college", text), grepl("College", text)),

    # wages
    wage     = sum(grepl("Wage", text), grepl("wage", text)),
    an_hour  = sum(grepl("an hour", text)),
    all_wage = sum(wage, an_hour),

    # health care
    health = sum(grepl("health", text), grepl("Health", text)),

    # guns
    guns = sum(grepl("gun", text), grepl("Gun", text))
  )

sum_hc <- sum_hc %>%
  mutate(
    month = factor(mo, levels = c(7, 8, 9, 10, 11, 12, 1, 2),
                   labels = c("July", "August", "September", "October",
                              "November", "December", "January", "February"),
                   ordered = TRUE)
  )

dat_bs <- dat_bs %>%
  mutate(
    yr   = year(created),
    mo   = month(created)
  )

sum_bs <- dat_bs %>%
  group_by(mo) %>%
  summarise(
    total = n(),
    socialism = sum(grepl("socialism", text), grepl("Socialism", text)),

    # race
    afri       = sum(grepl("Afri", text), grepl("afri", text)),
    race       = sum(grepl("race", text), grepl("Race", text)),
    priv       = sum(grepl("privilege", text), grepl("privilege", text)),
    white      = sum(grepl("white", text), grepl("White", text)),
    black      = sum(grepl("black", text), grepl("Black", text)),
    all_race   = sum(afri, race, priv, white, black),

    # gender
    gender  = sum(grepl("Gender", text), grepl("gender", text)),
    women   = sum(grepl("Women", text), grepl("women", text)),
    woman   = sum(grepl("Woman", text), grepl("woman", text)),
    moms    = sum(grepl("moms", text), grepl("Moms", text)),
    all_gen = sum(gender, women, woman, moms),

    # college
    college = sum(grepl("college", text), grepl("College", text)),

    # wages
    wage     = sum(grepl("Wage", text), grepl("wage", text)),
    an_hour  = sum(grepl("an hour", text)),
    all_wage = sum(wage, an_hour),

    # health care
    health = sum(grepl("health", text), grepl("Health", text)),

    # guns
    guns = sum(grepl("gun", text), grepl("Gun", text))
  )

sum_bs <- sum_bs %>%
  mutate(
    month = factor(mo, levels = c(7, 8, 9, 10, 11, 12, 1, 2),
                   labels = c("July", "August", "September", "October",
                              "November", "December", "January", "February"),
                   ordered = TRUE)
  )

# plot --------------------------------------------------------------------

sum_bs <- mutate(sum_bs, id = "sanders")
sum_hc <- mutate(sum_hc, id = "clinton")
frame  <- bind_rows(sum_bs, sum_hc)
frame  <- filter(frame, !mo %in% c(7, 8, 9))

# normalize tweets as a function of total tweets/month
frame  <- frame %>%
  group_by(id) %>%
  mutate(all_race_norm = all_race / total)

ggplot(frame, aes(x = month, y = all_race_norm, fill = id)) +
  geom_bar(stat = "identity", position = "dodge")

melt_frame <- melt(frame, id.vars = c("id", "month")) %>%
  filter(variable %in% c("all_race", "all_gen", "all_wage", "guns", "socialism",
                         "health")) %>%
  mutate(variable = factor(variable, levels = c("all_race", "all_gen", "all_wage",
                                                "guns", "socialism", "health"),
                           labels = c("Race", "Gender", "Wage/Pay",
                                      "Guns", "Socialism", "Health")),
         id = factor(id, levels = c("sanders", "clinton"),
                     labels = c("Sanders", "Clinton")))

ggplot(melt_frame, aes(x = month, y = value, fill = id)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable) +
  theme_light() +
  theme(
    legend.position   = c(.9, .9),
    legend.key        = element_rect(color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.title      = element_blank(),
    axis.ticks        = element_blank(),
    panel.border      = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  labs(x = "", y = "Topic Mentioned")

# output files ------------------------------------------------------------

write.csv(dat_bs, "sanders-tweets-1015-0216.csv", row.names = FALSE)
write.csv(dat_hc, "clinton-tweets-0715-0216.csv", row.names = FALSE)
