setwd('~/Dropbox/ecrc-data-science/2020.05.25_tidyverse-intro/')

library(tidyverse)
library(magrittr) 
library(ggsci) 
library(ggrepel)
library(ggpubr)

## alpha diversity
meta <- '~/Dropbox/ecrc-data-science/2020.05.25_tidyverse-intro/meta.tbl.rds' %>%
  readRDS() %>%
  print() 

# play around
meta %>%
  ## avg shannon div - group by method
  # group_by(Cohort) %>%
  # mutate(avg.shann = mean(Shannon))
  ## avg shannon div - nest method
  # nest(data = -Cohort) %>%        # purrr shorthand
  # mutate(avg.shann = map_dbl(data, ~ mean(.$Shannon))) %>% 
  # unnest(data)
  # filter for SpA phenotype only
  filter(str_detect(Phenotype, 'SpA'))

# simple boxplot
meta %>%
  ggplot(aes(x = Cohort, y = Shannon)) +
  geom_boxplot(aes(color = Cohort, fill = Cohort), alpha = 0.5) +
  geom_jitter(aes(shape = Cohort))

# clean up strings for labels
plot.data <- meta %>%
  mutate_at(vars(Cohort), ~ fct_recode(., 
                                       Collect = 'Collect',
                                       `Crohn's\nDisease` = 'IBD',
                                       Spondyloarthritis = 'SpA',
                                       Uveitis = 'AAU')) %>%
  mutate_at(vars(Cohort), ~ fct_relevel(., 
                                        'Collect',
                                        'Spondyloarthritis',
                                        "Crohn's\nDisease",
                                        'Uveitis')) %>%
  print()

# list of groups between which wilcoxon test will be run
to.compare <- list(c('Collect','Spondyloarthritis'),
                   c('Collect',"Crohn's\nDisease"),
                   c('Collect','Uveitis'))


# nice diversity plot
plot.data %>%
  ggplot(aes(x = Cohort, y = Shannon)) +
  geom_boxplot(aes(fill = Cohort), alpha = 0.5) +
  geom_jitter(aes(shape = Cohort), alpha = 0.7) +
  ggsci::scale_fill_futurama() +
  labs(title = 'Shannon Diversity by Cohort', x = '') +
  ggpubr::theme_pubr(legend = 'none') +
  ggpubr::stat_compare_means(comparisons = to.compare, size = 4,
                             tip.length = 0, step.increase = 0.15,
                             # ..p.val.. for regular p value
                             aes(label = ..p.signif..))

## beta diversity
taxa <- '~/Dropbox/ecrc-data-science/2020.05.25_tidyverse-intro/taxa.tbl.rds' %>%
  readRDS() %>%
  print()

# play around
taxa %>%
  # check out mean abundance per cohort
  group_by(Cohort) %>%
  summarize_if(is.double, mean) %>%
  # pivot + ignore unknown genera
  pivot_longer(-Cohort,
               names_to = 'genus',
               values_to = 'abundance') %>%
  filter(!str_detect(genus, 'unknown')) 
  

# calculate bray-curtis dissimiliarities + 
taxa.pcoa <- taxa %>%
  # select(-Cohort) %>%
  select_if(is.double) %>%
  vegan::vegdist(method = 'bray', 
                 diag = TRUE, upper = TRUE) %>%
  # directly pipe distance object, or:
  # as.matrix() %>%
  # as_tibble() # check out distance matrix
  stats::cmdscale(k = 2) %>%
  as_tibble() %>%
  add_column(Cohort = taxa$Cohort, .before = 1) %>%
  print()

# calculate centroids
taxa.centroids <- taxa.pcoa %>%
  nest(data = -Cohort) %>%
  mutate(V1 = map_dbl(data, ~ mean(.$V1))) %>% 
  mutate(V2 = map_dbl(data, ~ mean(.$V2))) %>% 
  select(-data) %>%
  add_column(Pt.Type = 'Group Mean') %>% 
  print()

# clean up data for plotting
plot.data <- taxa.pcoa %>%
  add_column(Pt.Type = 'Individual') %>%
  # . says "pipe the argument here instead"
  bind_rows(taxa.centroids, .) %>%
  # relevel to get desired colors
  mutate_at(vars(Cohort), ~ fct_relevel(., 'SpA','IBD','AAU','Collect')) %>%
  print()

# simple PCoA plot
plot.data %>%
  ggplot(aes(x = V1, y = V2)) +
  # add ellipse around cohort groups
  stat_ellipse(geom = 'polygon', alpha = 0.2, 
               aes(color = Cohort, fill = Cohort)) +
  # PCoA coordinates
  geom_point(aes(shape = Pt.Type, size = Pt.Type,
                 color = Cohort, fill = Cohort)) +
  # 22 = filled sq, 4 = X
  scale_shape_manual(values = c(22,4)) +
  # make centroids 2x bigger
  scale_size_manual(values = c(4,2)) +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() +
  labs(title = 'Beta Diversity between Cohorts, genus-level',
       shape = 'Point Type', size = 'Point Type', 
       x = 'PCoA Axis 1', y = 'PCoA Axis 2') + 
  geom_label_repel(dplyr::filter(plot.data, Pt.Type == 'Group Mean'),
                   mapping = aes(label = Cohort), size = 9/.pt,
                   min.segment.length = unit(0, 'lines'), force = 5) +
  theme_bw()

  