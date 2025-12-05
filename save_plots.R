# ------------------------------------------------------------------------------
# SAVE HIGH-RESOLUTION PLOTS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Plot 1: Survey-weighted Kaplan-Meier Plot + Life Table
# ------------------------------------------------------------------------------
# Reconstruct the plot to ensure it uses the latest updated labels
plot_km_composite <- p_km / p_table + 
  patchwork::plot_layout(heights = c(3, 1))

ggsave(
  filename = "figures/Figure1_KaplanMeier.png",
  plot = plot_km_composite,
  width = 10, 
  height = 9, 
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------------------------
# Plot 2: Interaction Plot 
# ------------------------------------------------------------------------------
plot_interaction <- ggplot(plot_dat_revised,
                           aes(x = HR, y = user_status, color = poly_group, group = poly_group)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = LCL, xmax = UCL),
                width = 0.2, position = position_dodge(width = 0.6), linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_text(aes(x = UCL, label = text_label),
            position = position_dodge(width = 0.6),
            hjust = -0.15, size = 3, show.legend = FALSE) +
  facet_wrap(~ flag_clean, ncol = 2) +
  scale_x_log10(breaks = c(0.5, 1, 2, 4, 8), limits = c(0.2, 16)) +
  labs(
    title = "Hazard ratios for polypharmacy by therapeutic class usage",
    subtitle = "Interaction models (Reference: 1-4 meds, Non-user)",
    x     = "Hazard Ratio (log scale)",
    y     = "",
    color = "Medication Count"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

ggsave(
  filename = "figures/Figure2_Interactions.png",
  plot = plot_interaction,
  width = 14, 
  height = 9, 
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------------------------
# Plot 3: Grand Forest Plot
# ------------------------------------------------------------------------------
ggsave(
  filename = "figures/Figure3_GrandForestPlot.png",
  plot = plot.forest,
  width = 16, 
  height = 10, 
  dpi = 300,
  bg = "white"
)

message("Plots saved to the 'figures/' folder.")