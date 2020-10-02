# Generate Custom TAC Count Data
# Nick Lotito, v.2020-09

require(dplyr)
require(tidyr)

load("source.Rdata")

######################################################
# Define UCDP-GTD match levels
######################################################

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

######################################################
# Define TAC generation function
######################################################

tac.generate <- function(unit = "group",
                         crit = c(1:3), 
                         attacktype = c(2:6,9),
                         targtype = c(1,6,8,9,11,13:16,18:21)){
    
    crit <- as.integer(crit)
    my.crit1 <- ifelse(1 %in% crit, TRUE, FALSE)
    my.crit2 <- ifelse(2 %in% crit, TRUE, FALSE)
    my.crit3 <- ifelse(3 %in% crit, TRUE, FALSE)

    attack.list <- as.integer(attacktype)
    target.list <- as.integer(targtype)

    as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

    if (unit == "group") {
        df <- left_join(df.source, gtd, by = "eventid") %>% 
            rename(uid = sidebid)
    } else if (unit == "dyad") {
        df <- left_join(df.source, gtd, by = "eventid") %>%
            rename(uid = dyadid)
    } else {
        stop("'unit' must be 'group' or 'dyad'")
    }

    # generate counts
    df <- df %>%
        mutate(crit = ifelse((crit1 == T | my.crit1 == F) & 
                             (crit2 == T | my.crit2 == F) & 
                             (crit3 == T | my.crit3 == F), T, F),
            attack = ifelse(attacktype1 %in% attack.list | attacktype2 %in% attack.list | attacktype3 %in% attack.list, T, F),
            target = ifelse(targtype1 %in% target.list, T, F)) %>%
        select(uid, year, gname_match, fatal:target) %>%
        mutate(t = ifelse(crit == T & attack == T & target == T, T, F),
               f = ifelse(t == T & fatal == T, T, F),
               m = ifelse(t == T & mass == T, T, F),
               gname_match = as.numeric.factor(gname_match)) %>%
        left_join(matches, by = "gname_match") %>%
        filter(!is.na(match)) %>%
        arrange(uid, year, match, t) %>%
        group_by(uid, year, match) %>%
        summarize_at(vars(t:m), sum) %>%
        ungroup() %>% 
        complete(nesting(uid,year),match,fill = list(t = 0, f = 0, m = 0)) %>%
        group_by(uid, year) %>% 
        mutate_at(vars(t:m), cumsum) %>% 
        gather(key, value, -uid, -year, -match) %>% 
        mutate(variable = paste0(key, "_", match)) %>% 
        select(-match, -key) %>% 
        spread(variable, value) %>% 
        ungroup() %>% 
        mutate_all(as.integer)

    # expand to group/dyad date range
    if (unit == "group") {
        df <- left_join(date.range.group, df, by = c("uid", "year"))
        df <- bind_rows(filter(df, year == 1993),
                 mutate_all(filter(df, year != 1993), ~replace_na(., 0))) %>% 
              arrange(uid, year) %>% 
              rename(sidebid = uid)
    } else if (unit == "dyad") {
        df <- left_join(date.range.dyad, df, by = c("uid", "year"))
        df <- bind_rows(filter(df, year == 1993),
                 mutate_all(filter(df, year != 1993), ~replace_na(., 0))) %>% 
              arrange(uid, year) %>% 
              rename(dyadid = uid)
    }
}