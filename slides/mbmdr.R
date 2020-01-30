library(tidyverse)

theme_transparent <- theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                           panel.grid.major = element_blank(), # get rid of major grid
                           panel.grid.minor = element_blank(), # get rid of minor grid
                           legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
                           legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
                           strip.background = element_rect(fill="transparent", color = NA)
)

tibble(genotype_1 = factor(rep(c('AA','Aa','aa'), 3), levels = c('AA','Aa','aa')),
       genotype_2 = factor(c('BB','Bb','bb','Bb','bb','BB','bb','BB','Bb'), levels = c('BB','Bb','bb')),
       H = c(12, 10, 7, 18, 12, 5, NA, 19, 7),
       L = c(4, 20, 7, 20, 3, 4, NA, 11, 13),
       cell_type = c('H', 'L', 'O', 'O', 'H', 'O', 'O', 'H', 'L')) %>%
  gather('key', 'value',-genotype_1, -genotype_2, -cell_type) %>%
  ggplot(aes(x = key, y = value, fill = cell_type)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = value), nudge_y = 2) +
    geom_text(aes(label = cell_type, size = 4), x = 0.65, y = 25, fontface = "bold") +
    facet_grid(genotype_2 ~ genotype_1, switch = 'y') +
    scale_y_continuous(limits = c(0,27)) +
    labs(x = '', y = '') +
    theme_bw() +
    theme(legend.position = 'none',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'white'),
          text = element_text(size = 20),
          axis.title.y = element_text(size = 25, margin = margin(r = 15)),
          axis.title.x = element_text(size = 25, margin = margin(t = 15)),
          strip.text = element_text(size = 25),
          strip.text.y = element_text(angle = 180, margin = margin(r = 10)),
          strip.text.x = element_text(margin = margin(b = 10))) +
    theme_transparent +
    scale_fill_manual(values = c('H' = '#ef8a62', 'L' = '#67a9cf', 'O' = '#bdbdbd'))

ggsave('slides/figs/mbmdr.pdf', width=6, height=6, bg = "transparent")
