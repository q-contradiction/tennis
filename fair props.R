# Evaluating different methods to remove bookmaker's overround in tennis odds

# A reproduction in R of the methodology applied in Football from: 
# Wisdom of Crowd-Joseph Buchdahl (www.football-data.co.uk/blog/wisdom_of_the_crowd.php)

# Additional methods included in the implied package
# applying same logic to different bookmakers 
# and find the optimal method to arrive at a set of fair probabilities
# using rank probability score, linear regression and returns from fair odds

# all data taken from tennis-data.co.uk

suppressPackageStartupMessages({
  library(broom)
  library(tidyverse)
  library(implied) #https://opisthokonta.net/?p=1797
})

rm(list = ls())

# Data --------------------------------------------------------------------

tennis_odds <- readRDS("./data-raw/tennis-data.rds")

# odds abbreviations for bookies in tennis-data.co.uk
odds_abbs <- list(Bet365 = c("B365W", "B365L"), Pinnacle = c("PSW", "PSL"), 
                  Centrebet = c("CBW", "CBL"), Gamebookers = c("GBW", "GBL"), 
                  Interwetten = c("IWW", "IWL"), Sportingbet = c("SBW", "SBL"),
                  Expekt = c("EXW", "EXL"), Ladbrokes = c("LBW", "LBL"),  
                  StanJames = c("SJW", "SJL"), Unibet = c("UBW", "UBL"), 
                  BWin = c("B.WW", "B.WL"))

tennis_odds <- suppressWarnings(mutate_at(tennis_odds, vars(unlist(odds_abbs)), as.numeric))

# non NA odds of different bookmakers
n_obs <- colMeans(!is.na(tennis_odds[, unlist(map(odds_abbs, 1))])) %>% 
  sort(decreasing = T)

# Fair probabilities ------------------------------------------------------

fair_probabilities <- function(bookmaker, method) {
  
  params <- as.list(environment())
  
  odds <- odds_abbs[[bookmaker]] 
  
  # filter NA or invalid odds (overround > 1, all odds > 1)
  valid_overround <- rowSums(1/tennis_odds[, odds], na.rm = T) > 1
  df <- tennis_odds[valid_overround, odds] 
  
  for (i in seq_along(odds)) {
    df <- filter(df, !!sym(odds[i]) > 1.1)
  }
  #browser()
  fair_props <- implied_probabilities(df[, odds], method) 
  
  indexes <- which(!fair_props[["problematic"]])  
  
  fair_props <- as.data.frame(fair_props[["probabilities"]]) %>%
    setNames(c("PW", "PL"))
  
  return (bind_cols(params, df[indexes, ], fair_props[indexes, ]))
}

methods <- c("basic", "wpo", "bb", "or", "power", "jsd")

props_table <- expand.grid(bookmaker = c("Pinnacle", "Bet365", "Ladbrokes", "Expekt"),
                           method = methods[1:5], 
                           stringsAsFactors = F) %>%
  pmap_dfr(fair_probabilities, .progress = T)


# Evaluation --------------------------------------------------------------

## Brier Score ----

brier_score <- props_table %>%
  mutate(bs = (1 - PW)^2) %>%
  summarise(bs_avg = mean(bs), .by = c(bookmaker, method)) %>%
  arrange(bs_avg)

## Actual & Implied prob (regression) ----

props_long <- props_table %>%
  pivot_longer(cols = c(PW, PL), names_to = "player_prop", values_to = "implied_probability") %>%
  mutate(evaluation = ifelse(player_prop == "PW", 1, 0)) %>%
  group_by(bookmaker, method) %>%
  mutate(implied_prop_int = cut(implied_probability, breaks = seq(0, 1, 0.01))) %>%
  group_by(implied_prop_int, .add = T) %>%
  summarise(obs = n(), 
            implied_prop = mean(implied_probability),
            actual_prop = mean(evaluation), .groups = "drop")

reg <- props_long %>%
  group_nest(bookmaker, method) %>%
  mutate(model = map(data, ~ lm(actual_prop ~ implied_prop - 1, weights = obs, data = .x)), 
         coef = map(model, tidy), 
         adj_r_squared = map_dbl(model, ~ glance(.x) %>% pull(adj.r.squared))) %>%
  select(bookmaker, method, coef, adj_r_squared) %>%
  unnest(cols = c(coef)) %>%
  mutate(dev = abs(estimate - 1)) %>%
  arrange(dev)

ggplot(props_long, aes(x = implied_prop, y = actual_prop, color = method)) +
  geom_point(alpha = 0.2) + geom_smooth(method = "lm", formula = y ~ x-1,  mapping = aes(weight = obs), se = F) + 
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(bookmaker ~.) + theme_bw() +
  labs(x = "Odds implied Probability", y = "Actual Probability")

## Fair Returns ----

# fair_PnL = Profit n Loss if bet on fair odds of Winner & Loser 
fair_bets <- props_table %>%
  group_by(bookmaker, method) %>%
  mutate(fair_PnL = -2 + 1/PW,
         NBets = seq(2, by = 2, length.out = n()), 
         fair_PnL_r = cumsum(fair_PnL), 
         yield = fair_PnL_r/NBets) %>%
  ungroup()

# steady negative profit for basic (underestimating low probability events) 
# and positive yield for bb method (overestimating)
# power overestimates sligtly
# or & wpo most reliable  
fair_bets %>% 
  ggplot(aes(x = NBets, y = fair_PnL_r, color = method)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(bookmaker ~., scales = "free_x") + theme_bw() +
  labs(title = "Profit & Loss with fair odds from different bookies",
       subtitle = "Different ovverround methods")
