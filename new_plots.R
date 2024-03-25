df_plot <- df[grepl("group|oa_side|Knee", names (df))]
df_plot <- df_plot[grepl("group|oa_side|X", names (df_plot))]

df_plot <- df_plot[-c(1,2)] %>%
  map (as.data.frame) %>%
  bind_rows(.id = "varz") %>%
  mutate (group = rep (df_plot$group, nrow (.)/length(df_plot$group)),
          oa_side = rep (df_plot$oa_side, nrow (.)/length(df_plot$oa_side))) %>%
  select (group, oa_side, everything()) %>%
  pivot_longer(cols = starts_with("t"),
               names_to = "cycle",
               values_to = "angle")%>%
  mutate (cycle = str_remove(cycle, "t") %>% as.numeric)

df_hea <- df_plot %>%
  filter (group == "HEA") %>%
  mutate (varz = ifelse(grepl ("R_", varz), "Ipsi", "Contra"))

df_roa <- df_plot %>%
  filter (group == "HOA" & oa_side == "R") %>%
  mutate (varz = ifelse(grepl ("R_", varz), "Ipsi", "Contra"))

df_loa <- df_plot %>%
  filter (group == "HOA" & oa_side == "L") %>%
  mutate (varz = ifelse(grepl ("L_", varz), "Ipsi", "Contra"))

df_plot <- bind_rows(df_hea, df_roa, df_loa) %>%
  group_by(group, varz, cycle) %>%
  summarise (Mean = mean (angle),
             Sd = sd (angle))


f2 <- ggplot (df_plot) +
  geom_line(aes(x = cycle, y = Mean, color = group)) +
  geom_ribbon(aes (x = cycle, ymin = Mean -Sd, ymax = Mean + Sd, fill = group), alpha = 0.2) +
  facet_wrap( ~ varz, scales = "free") +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual (values = c("blue", "red")) +
  labs (y = "Knee angle (°)",
        x = "Stride (%)",
        color = "Group") +
  guides (fill = "none") +
  theme_cowplot()
f2
