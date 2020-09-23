# TAC Merge
# Nick Lotito, v.2019-09
library(dplyr)
library(tidyr)
library(rlang)

# options(crayon.enabled = FALSE) #temp fix to disable colors in dplyr output

setwd("~/GitHub/TAC")
load("source.Rdata")

# attack.list <- c(2:6,9)
# target.list <- c(1,6,8,9,11,13:16,18:21)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

matches <- tribble(
    ~gname_match, ~match,
    0, "a",
    1, "a",
    2, "b",
    3, "b",
    4, "c",
    97,"d",
    96,"d",
    5, "e",
    13,"f"
)

attack.list <- c(2:6,9)
target.list <- c(1,6,8,9,11,13:16,18:21)

attack.more.list <- c(2,3)
subtarg.more.list <- c(2, 7, 8, 11, 42, 44, 49:52, 57, 60, 65:67, 69:81, 86, 87, 95:105)


df <- left_join(link, gtd) %>%
	mutate(crit = ifelse(crit1 == TRUE & crit2 == TRUE & crit3 == TRUE, T, F),
		attack = ifelse(attacktype1 %in% attack.list | attacktype2 %in% attack.list | attacktype3 %in% attack.list, T, F),
		target = ifelse(targtype1 %in% target.list, T, F),
		attack.more = ifelse(attacktype1 %in% attack.more.list | attacktype2 %in% attack.more.list | attacktype3 %in% attack.more.list, T, F),
		target.more = ifelse(targsubtype1 %in% subtarg.more.list, T, F)) %>%
	select(dyadid, year, gname_match, fatal:target.more) %>%
	mutate(t = ifelse(crit == T & attack == T & target == T, T, F),
	       f = ifelse(t == T & fatal == T, T, F),
	       m = ifelse(t == T & mass == T, T, F),
	       tm = ifelse(crit == T & attack.more == T & target.more == T, T, F),
	       fm = ifelse(tm == T & fatal == T, T, F),
	       mm = ifelse(tm == T & mass == T, T, F),
	       gname_match = as.numeric.factor(gname_match)) %>%
    left_join(matches) %>%
    filter(!is.na(match)) %>%
    arrange(dyadid, year, match, t) %>%
    group_by(dyadid, year, match) %>%
    summarize_at(vars(t:mm), sum) %>%
    ungroup() %>% 
    complete(nesting(dyadid,year),match,fill = list(t = 0, f = 0, m = 0, tm = 0, fm = 0, mm = 0)) %>% 
    group_by(dyadid, year) %>% 
    mutate_at(vars(t:mm), cumsum) %>% 
    gather(key, value, -dyadid, -year, -match) %>% 
    mutate(variable = paste0(key, "_", match)) %>% 
    select(-match, -key) %>% 
    spread(variable, value) %>% 
    ungroup() %>% 
    mutate_all(as.integer)

df