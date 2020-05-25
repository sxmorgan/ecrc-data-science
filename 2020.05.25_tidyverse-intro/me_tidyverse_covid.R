setwd('~/Dropbox/ecrc-data-science/2020.05.25_tidyverse-intro/')

library(tidyverse)  # data science toolkit
library(magrittr)   # tidy operators
library(lubridate)  # simple date handling
library(ggsci)      # custom color palettes
library(ggpubr)     # publication-ready plots

# load the per-state longitudinal data from covid19us pkg
states.data <- 'https://covidtracking.com/api/v1/states/daily.csv' %>%
    read_csv() %>%
    select(c('state','date','positive','negative','death','total'))

# load state name-abbreviation pairs from https://worldpopulationreview.com/
states.abbrev <- 'https://worldpopulationreview.com/static/states/abbr-name-list.csv' %>%
    read_csv() %>%
    mutate(name = stringr::str_replace(name, 'Of', 'of')) %>%
    add_row(name = 'Puerto Rico', abbreviation = 'PR')

# load state populations from local file (from https://worldpopulationreview.com/)
states.pop <- 'us.state.population.csv' %>%
    read_csv() %>%
    select(c('rank','State','Pop'))

raw.testing.data <- states.data %>%
    left_join(states.abbrev, by = c('state' = 'abbreviation')) %>%
    left_join(states.pop, by = c('name' = 'State')) %>%
    rename(state.name = 'name') %>%
    # remove 4 extra territories
    filter(!is.na(state.name))

testing.data <- raw.testing.data %>%
    # 1. using purrr shorthand
    mutate_at(vars(date), ~ as_date(ymd(.))) %>%
        # 2. using anonymous function
        # mutate_at(vars(date), function(x) {
        #     x %>%
        #         ymd() %>%
        #         as_date() }) %>%
        # 3. using stepwise mutate commands
        # mutate(date = ymd(date)) %>%
        # mutate(date = as_date(date)) %>%
    filter(date > '2020-03-09') %>%
    # create columns for counts per 100k residents
    mutate(pop.factor = Pop/100000) %>%
    mutate(tests.std = total/pop.factor) %>%
    mutate(deaths.std = death/pop.factor) %>%
    mutate(positive.std = positive/pop.factor) %>%
    mutate(negative.std = negative/pop.factor) %>%
    # select variables needed for simple longitudinal scatterplots for each state
    select(c('state.name','date','tests.std','deaths.std','positive.std','negative.std'))

plot.data <- testing.data %>%
    # sort by standardized testing rate, decreasing order
    arrange(desc(tests.std)) %>%
    group_by(state.name) %>%
    # collapse variables except for state.name
    nest() %>%
    ungroup() %>%
    # give each state.name a ranking/order
    mutate(ordered = 1:n()) %>%
    unnest(data) %>%
    # reorder state.name according to ranking/order determined from tests.std
    mutate_at(vars(state.name), ~ fct_reorder(., ordered)) %>%
    select(-ordered) %>%
    # collapse 4 testing variables into key-value pairs for ggplot
    pivot_longer(cols = -c('date','state.name'),
                 names_to = 'std.metric',
                 values_to = 'std.value') %>%
    # use forcats to manipulate factors so our final plot is more visually appealing
    mutate_at(vars(std.metric), ~ fct_relevel(., 'tests.std','negative.std',
                                              'positive.std','deaths.std')) %>%
    # and has more intutive variable names
    mutate_at(vars(std.metric), ~ fct_recode(., 
                                             `Total Test Results` = 'tests.std',
                                             `Negative Tests` = 'negative.std',
                                             `Positive Tests` = 'positive.std',
                                             `Deaths` = 'deaths.std')) 

testing.plot <- plot.data %>%
    ggplot(aes(x = date, y = std.value)) +
    geom_point(aes(color = std.metric, shape = std.metric)) +
    # ticks every 3 weeks and show dates in 'dd.mm' format
    scale_x_date(date_breaks = '3 weeks',
                 date_labels = '%d.%m') +
    # favorite color palette from ggsci
    scale_color_futurama() +
    facet_wrap(~ state.name) +
    # fix title, axes, legends
    labs(title = 'COVID-19 Testing and Prognosis in US States',
         x = '', y = '', color = 'Per 100k Residents:', shape = 'Per 100k Residents:') +
    # favorite theme from ggpubr, especially for this data
    theme_pubclean()