# betting functions

# breakeven bet
breakeven_rate <- function(.ml) {
  ifelse(.ml < 0,
         abs(.ml) / (abs(.ml) + 100),
         100 / (.ml + 100))
}

# calc juice
calc_juice <- function(.mls) {
  1 - (1 / sum(breakeven_rate(.mls)))
}


p_to_ml <- function(.ml) {
  round(ifelse(.ml > .5,
               (.ml / (1 - .ml)) * -100,
               ((1 - .ml) / .ml) * 100))
}


# money line to probability
ml_to_p <- function(.ml) {
  ifelse(.ml < 0,
         .ml / (.ml - 100),
         100 / (.ml + 100))
}


# calculate overround
calc_overround <- function(.mls) {
  sum(ml_to_p(.mls))
}

# calculate juice free breakeven win percentage
calc_juicefree_breakeven <- function(.mls) {
  breakeven_rate(.mls) / sum(breakeven_rate(.mls))
}


expected_scores <- function(h_sprd, tot) {
  require(tibble)
  a_pts <- (tot + h_sprd) / 2
  h_pts <- a_pts - h_sprd
  tibble(
    spread = h_sprd,
    total = tot,
    home_scr = h_pts,
    away_scr = a_pts
  )
}


calc_hold <- function(.mls) {
  1 - 1 / calc_overround(.mls)
}


calc_edge <- function(win_p, odds) {
  win_p * us_dec(odds) - 1
}


hfa_calc <- function(p_A, p_lg) {
  x <- log(p_A / (1 - p_A)) + log(p_lg / (1 - p_lg))
  gtools::inv.logit(x)
}


bradley_terry <- function(t_A, t_B) {
  exp(t_A) / (exp(t_A) + exp(t_B))
}

log_five <- function(p_A, p_B) {
  (p_A / (1 - p_A)) /  ((p_A / (1 - p_A)) + (p_B / (1 - p_B)))
}


risking <- function(amt, juice) {
  # amt is the amount risked, juice is the
  # odds in us format i.e. -120
  # output is the amount won
  ifelse(juice < 0, abs(amt / ((juice) / 100)),
         amt * ((juice) / 100))
}
to_win <- function(amt, juice) {
  # amt = the amount win desired
  # juice = odds in us format
  # output is the amount necassary to risk
  ifelse(juice < 0, abs((juice / 100) * amt),
         amt / (juice / 100))
}

us_pct <- function(.x) {
  1 / ifelse(.x < 100, 1 - (100 / .x), 1 + (.x / 100))
}

us_dec <- function(.x) {
  ifelse(.x < 100, 1 - (100 / .x), 1 + (.x / 100))
}

pct_us <- function(.x) {
  ifelse(.x > .5,
         (.x / (1 - .x)) * -100,
         ((1 - .x) / .x) * 100)
}

dec_us <- function(.x) {
  ifelse(.x >= 2, 100 * (.x - 1), -100 / (.x - 1))
}


two_way_ev <- function(.win, .loss, .w_pct) {
  (.win * .w_pct) - (.lss * (1 - .w_pct))
}

get_dog_line <- function(fav, juice) {
  p_to_ml(-1 / (juice - 1) - ml_to_p(fav))
}