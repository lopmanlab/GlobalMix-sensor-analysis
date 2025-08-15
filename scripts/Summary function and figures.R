# Package
pacman::p_load(tidyr, dplyr, ggplot2, knitr, gridExtra, eulerr, grid, ggpubr, purrr, here, 
               #glmnet, vip, boot
               )

# Function ----
## Venn diagram function ----
create_proportional_venn <- function(diary_set, sensor_set) {
  venn_data <- list(Diary = diary_set, Sensor = sensor_set)
  fit <- euler(venn_data)
  
  # Calculate counts and percentages
  only_sensor <- length(setdiff(sensor_set, diary_set))
  only_diary <- length(setdiff(diary_set, sensor_set))
  both <- length(intersect(sensor_set, diary_set))
  total <- only_sensor + only_diary + both
  only_sensor_pct <- round(only_sensor / total * 100, 1)
  only_diary_pct <- round(only_diary / total * 100, 1)
  both_pct <- round(both / total * 100, 1)
  
  # Create Venn diagram as a grob object
  venn_plot <- plot(fit, fills = c( "#F8766D", "#00BFC4"), alpha = 0.5, labels = NULL)
  
  # Create title above
  dataset_labels <- grobTree(
    textGrob("Sensor", x = 0.85, y = 0.85, gp = gpar(fontsize = 14, fontface = "bold")),
    textGrob("Diary", x = 0.15, y = 0.85, gp = gpar(fontsize = 14, fontface = "bold"))
  )
  
  # Create text annotations
  annotations <- grobTree(
    textGrob(paste0(only_sensor, "\n(", only_sensor_pct, "%)"), x = 0.85, y = 0.5, gp = gpar(fontsize = 14)), # Guatemala 0.91, India 0.92, Mozambique 0.85
    textGrob(paste0(only_diary, "\n(", only_diary_pct, "%)"), x = 0.1, y = 0.5, gp = gpar(fontsize = 14)), # Guatemala 0.08, India 0.08, Mozambique 0.1
    textGrob(paste0(both, "\n(", both_pct, "%)"), x = 0.5, y = 0.5, gp = gpar(fontsize = 14))
  )
  
  # Combine Venn diagram and annotations
  combined_grob <- gTree(children = gList(dataset_labels, venn_plot, annotations))
  return(combined_grob)
}

# Figures ----
## Figure 1 ----
row_label_A <- textGrob("A", gp = gpar(fontsize = 16, fontface = "bold"), x = 0.05, hjust = 0, y = 0.98, vjust = 1)
row_label_B <- textGrob("B", gp = gpar(fontsize = 16, fontface = "bold"), x = 0.05, hjust = 0, y = 0.98, vjust = 1)

fig1 <- grid.arrange(
  arrangeGrob(row_label_A, gt_num_plot, in_num_plot, mo_num_plot, pa_num_plot, ncol = 5, widths = c(0.05, 1, 1, 1, 1)),
  arrangeGrob(row_label_B, gt_venn, in_venn, mo_venn, pa_venn, ncol = 5, widths = c(0.05, 1, 1, 1, 1)),
  ncol = 1
)

## Figure 2 ----
fig2 <- ggarrange(ggarrange(gt.method.plot, in.method.plot, mo.method.plot, pa.method.plot, nrow = 1, ncol = 4, common.legend = TRUE, legend = "right"),
                  ggarrange(gt.method.plot.sex, in.method.plot.sex, mo.method.plot.sex, pa.method.plot.sex, nrow = 1, ncol = 4, common.legend = TRUE, legend = "right"),
                  nrow = 2, ncol = 1)

## Figure 3 ----
fig3 <- ggarrange(gt_dur_fig, in_dur_fig, mo_dur_fig, pa_dur_fig, nrow = 1, ncol = 4,
                  common.legend = T, legend = "right")


## Supplemental Figure 2 ------
sup2 <- ggarrange(gt.method.plot.rel, in.method.plot.rel, mo.method.plot.rel, pa.method.plot.rel, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right")


## Main text ----
rbind(gt.unique.con.long, in.unique.con.long, mo.unique.con.long, pa.unique.con.long)%>%
  group_by(Method) %>%
  summarise(
    mean_contacts = mean(contacts),
    sd_contacts = sd(contacts),
    total_contacts = sum(contacts)
  )
