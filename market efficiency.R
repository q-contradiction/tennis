# Inefficiencies in tennis market
# Methodology from: football-data.co.uk/blog/wisdom_of_the_crowd.php (Joseph Buchdahl)
# applied in the tennis market

# all data taken from tennis-data.co.uk

suppressPackageStartupMessages({
  library(scales)
  library(implied) #https://opisthokonta.net/?p=1797
  library(viridis)
  library(dqrng)
  library(tidyverse)
})

options(scipen = 999)
rm(list = ls())

# Data --------------------------------------------------------------------

tennis_odds <- readRDS("./data-raw/tennis-data.rds")

# odds abbreviations in tennis-data.co.uk
odds_abbs <- list(Bet365 = c("B365W", "B365L"), Pinnacle = c("PSW", "PSL"), 
                  Centrebet = c("CBW", "CBL"), Gamebookers = c("GBW", "GBL"), 
                  Interwetten = c("IWW", "IWL"), Sportingbet = c("SBW", "SBL"),
                  Expekt = c("EXW", "EXL"), Ladbrokes = c("LBW", "LBL"),  
                  StanJames = c("SJW", "SJL"), Unibet = c("UBW", "UBL"), 
                  BWin = c("B.WW", "B.WL"), Max = c("MaxW", "MaxL"), Avg = c("AvgW", "AvgL"))

tennis_odds <- tennis_odds %>% 
  mutate_at(vars(unlist(odds_abbs)), as.numeric) %>%
  mutate(ID = as.numeric(rownames(.))) 

# max odds {spot any errors}         
max_odds <- tennis_odds %>%
  select(all_of(unlist(odds_abbs))) %>% lapply(min, na.rm = T) 

tennis_odds <- filter(tennis_odds, if_all(all_of(unlist(odds_abbs)), ~ (. <= 101 & . > 1) | is.na(.)))

# count non NA odds
# remove BWin, Gamebookers, Sportingbet - few observations
n_obs <- colMeans(!is.na(tennis_odds[, unlist(map(odds_abbs, 1))])) %>% 
  sort(decreasing = T)

# Build Odds Table --------------------------------------------------------

fair_probabilities <- function(data, bookie) {
  
  odds <- odds_abbs[[bookie]]
  
  # filter NA or invalid odds (overround > 1, all odds > 1)
  valid_overround <- rowSums(1/data[, odds], na.rm = T) > 1
  
  data <- data[valid_overround, ]
  
  fair_props <- implied_probabilities(data[, odds], "or") 
  
  is_ok <- which(!fair_props[["problematic"]])  
  
  fair_props <- as.data.frame(fair_props[["probabilities"]]) %>%
    setNames(c("PW", "PL"))
  
  return (bind_cols(data[is_ok, "ID", drop = F], fair_props[is_ok, ]))
}

bookies_ref <- setdiff(names(odds_abbs), grep("Max", names(odds_abbs), value = T))

props_ref <- lapply(bookies_ref, fair_probabilities, data = tennis_odds)
names(props_ref) <- bookies_ref

build_odds_table <- function(data, bookie_ref, bookie_bet) {
  
  odds_ref <- odds_abbs[[bookie_ref]]; odds_bet <- odds_abbs[[bookie_bet]]
  
  data <- data[, c("ID", odds_ref, odds_bet)] %>% 
    filter(if_all(all_of(odds_bet), ~. > 1))
  
  if (nrow(data) == 0) return (NULL)

  data <- inner_join(data, props_ref[[bookie_ref]], by = "ID")
  
  dfList <- list()
  
  for (i in 1:2) {
    dfList[[i]] <- data %>% 
      select(ID, ends_with(c("W", "L")[i])) %>%
      mutate(player = c("W", "L")[i])
  }
  
  bets <- dfList %>% 
    lapply(setNames, c("ID", "odds_ref", "odds_bet", "prop_ref", "player")) %>%
    bind_rows() %>% 
    mutate(EV = prop_ref*odds_bet - 1,
           PnL = -1 + (player == "W")*odds_bet) %>%
    mutate(bookie_ref = bookie_ref, 
           bookie_bet = bookie_bet) %>%
    arrange(ID)
  
  return (bets)
}

lines <- setdiff(names(odds_abbs), c("BWin", "Gamebookers", "Sportingbet"))

# compare every bookmaker against the other
odds_table <- expand.grid(bookie_ref = lines, 
                          bookie_bet = lines, 
                          stringsAsFactors = FALSE) %>%
  filter(bookie_ref != bookie_bet) %>%
  filter(bookie_ref != "Max") %>%
  
  pmap_dfr(build_odds_table, data = tennis_odds) 

# Expected Value ----------------------------------------------------------

# PnL = f(EV) for all bets with bookie ref to estimate true probability of an event
ev_table <- odds_table %>% 
  group_by(bookie_ref, bookie_bet) %>%
  mutate(EV_int = ntile(EV, 40)) %>%
  group_by(EV_int, .add = T) %>%
  summarize(n = n(),
            EV_mean = mean(EV), 
            PnL_mean = mean(PnL), .groups = "drop")

ev_table %>%
  filter(between(EV_mean, -0.25, 0.25)) %>%
  ggplot(aes(x = EV_mean, y = PnL_mean, color = bookie_bet)) + geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = F) + 
  geom_abline(slope = 1, intercept = 0) + 
  annotate("segment", x = -Inf, xend = 0, y = 0, yend = 0, linetype = "dashed") +
  annotate("segment", x = 0, xend = 0, y = -Inf, yend = 0, linetype = "dashed") +
  facet_wrap(bookie_ref ~.) + 
  theme_bw() + 
  labs(title = "Pnl[bet] ~ f(EV[bet, ref])", 
       x = "Expected Value", y = "Profit & Loss")

# correlation between PnL and EV
cor_table <- ev_table %>% 
  summarize(obs = sum(n),
            cor = cor(PnL_mean, EV_mean), .by = c(bookie_ref, bookie_bet)) 

# average correlation PnL and EV
# Pinnacle's supremacy as estimator of true probability of an event
cor_table %>%
  summarise(cor_mean = mean(cor), .by = bookie_ref) %>%
  arrange(cor_mean) %>%
  mutate(bookie_ref = factor(bookie_ref, levels = bookie_ref)) %>%
  ggplot(aes(x = bookie_ref, y = cor_mean, fill = bookie_ref)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = T, option = "B") +
  coord_flip() + guides(fill="none") + scale_y_continuous(labels = percent_format()) +  theme_bw() +
  labs(x = NULL, y = "mean correlation", title = "Mean Correlation of Bookmakers against pre-closing lines")  

cor_table %>% 
  ggplot(aes(x = bookie_bet, y = bookie_ref, fill = cor)) +
  geom_tile() +
  scale_fill_viridis(option = "F", direction = -1) +
  labs(title = "EV and PnL Correlation between Bookie Ref and Bet",
       x = "Bookie Bet", y = "Bookie Ref", fill = "Corr") +
  theme_bw()

# EV distribution using Pinnacle's price as reference
odds_table %>% 
  filter(bookie_ref == "Pinnacle") %>%
  ggplot(aes(x = EV, y = after_stat(density), color = bookie_bet)) +
  geom_histogram(binwidth = 0.005) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(bookie_bet ~. , ncol = 2) + 
  coord_cartesian(xlim = c(-0.25, 0.25)) + guides(color = "none") + theme_bw() +
  labs(title = "Expected Value distribution using Pinnacle's Price")


# Value Bets --------------------------------------------------------------

# percentage of value bets (EV > 0) using Pinnacle , group_var can be {bookie_bet, Sex, Surface}
plot_vb_pct <- function(bookies, group_var) {
  
  var <- sym(group_var)
  
  pl <- odds_table %>%
    filter(bookie_ref == "Pinnacle", bookie_bet %in% bookies) %>%
    left_join(tennis_odds[, c("ID", "Sex", "Surface")], by = "ID") %>%
    group_by(!!var) %>%
    summarise(EV_pos_pct = sum(EV > 0) / n(), .groups = "drop") %>%
    arrange(EV_pos_pct) %>%
    mutate(var_f = factor(!!var, levels = !!var)) %>%
    ggplot(aes(x = var_f, y = EV_pos_pct, fill = var_f)) +
    geom_bar(stat = "identity") + 
    scale_fill_viridis(discrete = T, option = "B") + guides(fill = "none") + 
    scale_y_continuous(labels = percent_format()) + theme_bw() +
    labs(x = NULL, y = "Percentage of bets with positive EV")
  print(pl)
}

plot_vb_pct(setdiff(lines, "Max"), "bookie_bet")

value_bet_series <- odds_table %>% 
  filter(EV > 0) %>%
  group_by(bookie_ref, bookie_bet) %>%
  arrange(ID) %>%
  mutate(NVBets = seq(1, n()),
         EV_total = cumsum(EV),
         PnL_total = cumsum(PnL)) %>% 
  ungroup() %>%
  mutate(Yield = PnL_total/NVBets)


plot_vb_series <- function(bookies_ref, bookies_bet) {
  
  pl_returns <- value_bet_series %>%
    filter(bookie_ref %in% bookies_ref, bookie_bet %in% bookies_bet) %>%
    ggplot(aes(color = bookie_bet)) + 
    geom_line(aes(x = NVBets, y = PnL_total), alpha = 0.5) + 
    geom_smooth(aes(x = NVBets, y = EV_total, linetype = "dashed"), method = "lm", formula = y ~ 0 + x) +
    facet_wrap(bookie_ref ~., scales = "free") +
    theme_bw() +
    scale_linetype_manual(name = NULL, values = "dashed", labels = "EV_total") +
    labs(title = "Cumulative ProfitnLoss and EV ~ f(Number of Value Bets)", 
         x = "Value Bets", y = "PnL") + theme_bw() 
  print(pl_returns)
}

plot_vb_series("Pinnacle", setdiff(lines, "Max"))

# Identifying which bookmaker can be used to profit against another
value_bets <- value_bet_series %>%
  group_by(bookie_ref, bookie_bet) %>%
  slice_max(NVBets) %>%
  ungroup() %>%
  filter(PnL_total > 0) %>%
  select(bookie_ref, bookie_bet, NVBets:Yield)

# repetitive samples to calculate yield 
bootstrap <- function(bookie, nbets, nsims = 2500) {
  
  params <- as.list(environment())[1:2]
  odds <- odds_abbs[[bookie]]
  
  data <- na.omit(tennis_odds[, odds]) %>%
    filter(if_all(all_of(odds), ~. > 1)) %>%
    setNames(c("W", "L")) %>%
    pivot_longer(cols = c(W, L), names_to = "selection", values_to = "odds") %>%
    mutate(PnL = -1 + (selection == "W")*odds)   
  
  sampling <- function(df) {
    df[dqsample(nrow(df), nbets, T), ] %>% 
      summarise(yield = sum(PnL) / nbets) 
  }
  
  # sampling nsims times
  sim_res <- replicate(nsims, sampling(data), simplify = F) %>% 
    bind_rows() %>%
    summarise(yield_avg = mean(yield),
              yield_sd = sd(yield)) %>%
    bind_cols(params)
  
  return (sim_res)
}

bootstrapping <- value_bets %>% 
  select(bookie_bet, NVBets) %>%
  distinct(.keep_all = T) %>%
  rename(nbets = NVBets, bookie = bookie_bet) %>%
  pmap_dfr(bootstrap, .progress = T)

value_bets <- left_join(value_bets, bootstrapping, by = c("bookie_bet" = "bookie", 
                                                          "NVBets" = "nbets")) %>%
  mutate(p_value = pmap_dbl(list(q = Yield, mean = yield_avg, sd = yield_sd), pnorm, lower.tail = F), 
         logp = log10(p_value)) %>% 
  select(-c(yield_avg, yield_sd)) %>%
  mutate(p_value = format(p_value, digits = 1, scientific = T)) %>% 
  arrange(desc(PnL_total)) 
