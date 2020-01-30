library(tidyverse)

theme_transparent <- theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
                           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                           panel.grid.major = element_blank(), # get rid of major grid
                           panel.grid.minor = element_blank(), # get rid of minor grid
                           legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
                           legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
                           strip.background = element_rect(fill="transparent", color = NA)
)

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

tibble(method = factor(c('Standard','Physical','Chromatin','eQTL'), levels = c('Standard','Physical','eQTL','Chromatin')),
       Tested = c(7.2e+08, 4.6e+06, 8.9e+07, 2.2e+07),
       Significant = 1e7 * c(57, NA, 19, 64)) %>%
  gather(key = 'what', value ='num', -method) %>%
  mutate(what = factor(what, levels = c('Tested', 'Significant'))) %>%
  ggplot(aes(x = method, y = num, fill = what)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_y_continuous(sec.axis = sec_axis(~ ./1e7 + 10, breaks = seq(0, 90, 10),
                                      name = "Significant pairs")) +
    labs(x = '', y = 'Tested pairs', fill = 'What') +
    scale_fill_manual(values = c('Tested' = '#bdbdbd', 'Significant' = '#41ab5d')) +
    theme_bw() +
    theme(legend.position = 'left',
          text = element_text(size = 33),
          axis.title.y.left = element_text(size = 35, margin = margin(r = 15)),
          axis.title.y.right = element_text(size = 35, margin = margin(l = 15)),
          axis.title.x = element_text(size = 35, margin = margin(t = 15))) +
    theme_transparent

ggsave('slides/figs/num_tests.pdf', width=15, height=6.5, bg = "transparent")
