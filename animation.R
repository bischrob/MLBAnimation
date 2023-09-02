library(tidyverse)
library(magrittr)
library(baseballr)
library(mlbplotR)
library(lubridate)
library(gganimate)
library(ggtext)

last10 = function(x) {
  y = rep(NA,length(x))
  for(i in 1:length(x)){
    start = i - 9
    if(i < 10) start = 1
    y[i] = mean(x[start:i],na.rm = T)
  }
  return(y)
}

teams = mlbplotR::load_mlb_teams()

fullschedule = mlb_schedule(season = 2023)

schedule = fullschedule %>%
  filter(series_description == "Regular Season") %>%
  select(date,game_guid,teams_home_team_id,teams_home_team_name,teams_away_team_id,teams_away_team_name,teams_home_score,teams_away_score)
home = schedule %>%
  select(date, game_guid,team_id = teams_home_team_id,team =  teams_home_team_name,runs = teams_home_score,runsAgainst = teams_away_score)
away = schedule %>%
  select(date, game_guid,team_id = teams_away_team_id,team = teams_away_team_name,runs = teams_away_score,runsAgainst = teams_home_score)

scores = bind_rows(home,away)  %>%
  arrange(date) %>%
  filter(date < as.Date(Sys.time())) %>%
  group_by(team_id) %>%
  mutate(rollingO = last10(runs),
         rollingD = last10(runsAgainst)) %>%
  left_join(teams %>% select(team_id = team_id_num,team_abbr)) %>%
  group_by(date,team_id,team,team_abbr) %>%
  summarize_at(vars(rollingO,rollingD),mean) %>%
  ungroup() %>%
  unite("key",c(date,team_id),remove = F)

alldates = expand_grid(date = scores$date,team = scores$team) %>% distinct_all() %>%
  left_join(scores %>% select(team_id,team,team_abbr)) %>%
  distinct_all() %>%
  unite("key",c(date,team_id),remove = F)
scores = bind_rows(scores,alldates %>% filter(!key %in% scores$key))
scores %<>%
  arrange(date) %>%
  group_by(team) %>%
  mutate_at(vars(rollingO,rollingD),list(function(x){
    for(i in 1:length(x)){
      while(is.na(x[i])){
        x[i] = lag(x)[i]
      }
    }
    return(x)
  })) %>%
  ungroup() %>%
  mutate(date = lubridate::as_date(date)) %>%
  filter(date > "2023-04-15")

g = scores %>%
  filter(date == "2023-08-31") %>%
  ggplot(aes(x = rollingO, y = rollingD, team_abbr = team_abbr)) +
  geom_mlb_logos(width = .075) +
  theme_minimal() +
  coord_equal(clip = "off") +
  scale_y_reverse() +
  labs(title = glue::glue("<span><img src = mlb.png width = '60'> **MLB**</span><span style = 'font-size: 26pt'> for *{format(as.Date(Sys.time()),'%b %d')}*</span><br><span style = 'font-size: 18pt'>10 Game Scoring Averages</span>"),
       x = "<img src='arrowO.png' width='200'>",
       y = "<img src='arrowD.png' width='200'>",
       caption = "**runs** scored and allowed") +
  theme(text = element_text(size = 36, color = "#041e42"),
        plot.title = element_markdown(lineheight = .25),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        plot.background = element_rect(fill = 'white', color = NA),
        plot.caption = element_markdown(size = 24, color = "#041e42")
  )
ggsave(glue::glue("figures/mlbRollingScores.png"),dpi = 300, width = 9, height = 6)

options(gganimate.dev_args = list(width = 900, height = 600))

months = scores$date %>% month() %>% unique
for(month in months){

  g = scores %>%
    filter(month(date) == month) %>%
    filter(date == ceiling_date(date,"month")-1) %>%
    ggplot(aes(x = rollingO, y = rollingD, team_abbr = team_abbr)) +
    geom_mlb_logos(width = .075) +
    theme_minimal() +
    coord_equal(clip = "off") +
    scale_y_reverse() +
    labs(title = glue::glue("<span><img src = mlb.png width = '60'> **MLB**</span><span style = 'font-size: 26pt'> for *{month(month, label = T)}*</span><br><span style = 'font-size: 18pt'>10 Game Scoring Averages</span>"),
         x = "<img src='arrowO.png' width='200'>",
         y = "<img src='arrowD.png' width='200'>",
         caption = "**runs** scored and allowed") +
    theme(text = element_text(size = 36, color = "#041e42"),
          plot.title = element_markdown(lineheight = .25),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.background = element_rect(fill = 'white', color = NA),
          plot.caption = element_markdown(size = 24, color = "#041e42")
    )
  ggsave(glue::glue("figures/mlbRollingScores-{month}.png"),dpi = 300, width = 9, height = 6)


  a =
    scores %>%
    ungroup() %>%
    filter(month(date) == month) %>%
    # filter(team == "Arizona Diamondbacks") %>%
    ggplot(aes(x = rollingO, y = rollingD, team_abbr = team_abbr)) +
    geom_mlb_logos(width = .075) +
    theme_minimal() +
    coord_equal(clip = "off") +
    scale_y_reverse() +
    labs(title = "<span><img src = mlb.png width = '60'> **MLB**</span><span style = 'font-size: 26pt'> for *{format(frame_time,'%b %d')}*</span><br><span style = 'font-size: 18pt'>10 Game Scoring Averages</span>",
         x = "<img src='arrowO.png' width='200'>",
         y = "<img src='arrowD.png' width='200'>") +
    theme(text = element_text(size = 36, color = "#041e42"),
          plot.title = element_markdown(lineheight = .25),
          axis.title.x = element_markdown(),
          axis.title.y = element_markdown(),
          plot.background = element_rect(fill = 'white', color = NA)
    ) +
    transition_time(date) #+
    # xlim(c(2,8)) +
    # ylim(c(8,2))

  animate(a, nframes = 330, fps = 10,end_pause = 30)
  anim_save(glue::glue("figures/MLBAnimation-{month}.gif"))
}
