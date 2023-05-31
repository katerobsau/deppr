
date_val = "2013-11-26" #ecc_reordered$date[i];

day_pp <- ecc_data %>%
  filter(date == date_val) %>%
  mutate(dep_type = "pp") %>% # pp for marginal post-processed, without dependence
  dplyr::select(-starts_with("tm")) %>%
  pivot_longer(cols = starts_with("dm"),
               names_to = "member", values_to = "wsur")

day_ecc <- ecc_reordered %>%
  filter(date == date_val) %>%
  mutate(dep_type = "ecc") %>% # ecc for post-processed with dependence
  pivot_longer(cols = starts_with("rm"),
               names_to = "member", values_to = "wsur")

day_raw <- raw_ensemble %>%
  filter(date == date_val) %>%
  mutate(dep_type = "raw") %>% # raw for raw ensemble
  pivot_longer(cols = starts_with("fc"),
               names_to = "member", values_to = "wsur")

day_obs <- ensemble_winter %>%
  filter(date == date_val) %>%
  mutate(lead_time = t) %>%
  select(date, lead_time, sur) %>%
  distinct()

plot_data = full_join(day_pp, day_ecc) %>%
  full_join(day_raw) %>%
  mutate(member = member %>% str_match_all("[0-9]+") %>% as.numeric()) %>%
  mutate(dep_type = as.factor(dep_type))
levels(plot_data$dep_type) = c(ecc = "ECC", pp = "Post-processed",
                               raw = "Raw Ensemble")

compare_plot <- ggplot(data = plot_data) +
  geom_point(aes(x = lead_time, y = wsur, group = member,
                 col = as.factor(member)),
             alpha = 0.5) +
  geom_line(aes(x = lead_time, y = wsur, group = member,
                col = as.factor(member)),
            alpha = 0.5) +
  geom_line(data = rbind(day_obs %>% mutate(dep_type = "ECC"),
                         day_obs %>% mutate(dep_type = "Post-processed"),
                         day_obs %>% mutate(dep_type = "Raw Ensemble")) %>%
              mutate(member =  "Obs"),
            aes(x = lead_time,
                y = sur), col = "black") +
  facet_wrap(~dep_type) +
  theme_bw() +
  theme(legend.position = "none")

compare_plot
