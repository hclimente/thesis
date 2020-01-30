library(tidyverse)

theme_transparent <- theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                           panel.grid.major = element_blank(), # get rid of major grid
                           panel.grid.minor = element_blank(), # get rid of minor grid
                           legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
                           legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
                           strip.background = element_rect(fill="transparent", color = NA)
)

n <- 100

bind_rows(
  tibble(id = seq(1:n),
         cohort = 'Controls',
         Darbury = abs(rnorm(n, 20)),
         Sparte = abs(rnorm(n, 10)),
         Shar = abs(rnorm(n, 10))),
  tibble(id = seq(1:n),
         cohort = 'Cases',
         Darbury = abs(rnorm(n, 30)),
         Sparte = abs(rnorm(n, 5)),
         Shar = abs(rnorm(n, 5)))) %>%
  gather(key = 'component', value = 'value', -id, -cohort) %>%
  arrange(component, value) %>%
  mutate(id = factor(id, levels = unique(id))) %>%
  ggplot(aes(x = id, y = value, fill = component)) +
    geom_bar(stat = 'identity', position = 'fill') + 
    scale_y_continuous(labels = scales::percent) +
    labs(fill = 'Ancestral population', y = 'Contribution to the genome', x = 'Individual') +
    facet_grid(. ~ cohort) +
    scale_fill_manual(values = c('Shar' = '#66c2a5', 'Darbury' = '#fc8d62', 'Sparte' = '#8da0cb')) +
    theme_bw() +
    theme(legend.position = 'bottom',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'white'),
          text = element_text(size = 20),
          axis.title.y = element_text(size = 25, margin = margin(r = 15)),
          axis.title.x = element_text(size = 25, margin = margin(t = 15)),
          strip.text = element_text(size = 25)) +
  theme_transparent

ggsave('slides/figs/population_structure.pdf', width=10, height=6, bg = "transparent")
