library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
library(lubridate)
library(patchwork)  # for combining plots


df <- read.csv("output/dataset.csv")
df <- df %>% 
  dplyr::mutate(
    sex = factor(sex, levels = c("Laki-laki", "Perempuan")),
    date_of_birth = as.Date(date_of_birth),
    state = factor(state, levels = c("daftar", "hadir", "pelayanan", "selesai"))
  ) %>%  
  dplyr::mutate(
    current_date = lubridate::today(), # Get today's date
    age = floor(time_length(interval(date_of_birth, current_date), unit = "years"))
  )


theme_bgsi <- theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "Montserrat"),
    axis.text.x = element_text(
      family = "Montserrat",
      face   = "bold.italic",
      size   = 12,
      angle  = 45,
      vjust  = 0.7
    ),
    axis.title.y = element_text(
      family = "Montserrat",
      face   = "bold",
      size   = 12
    ),
    axis.text.y = element_text(
      family = "Montserrat",
      face   = "bold",
      size   = 10
    ),
    plot.title = element_text(
      family     = "Montserrat",
      face       = "bold.italic",
      size       = 22,
      lineheight = .1
    ),
    panel.grid = element_blank()
  )

## =====================================================
## 1) Province distribution 
## =====================================================
prov_counts <- df %>% 
  dplyr::group_by(province) %>% 
  dplyr::summarise(n = n_distinct(patient_id), .groups = "drop")

top3_prov <- prov_counts %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 3) %>% 
  pull(province)


prov_palette <- c(
  "#43B178FF",
  "#79C79FFF",
  "#AEDEC5FF",
  "grey80"
)
names(prov_palette) <- c(top3_prov, "OTHER")

p_province <- ggplot(
  prov_counts,
  aes(
    x = fct_reorder(province, n, .desc = TRUE),
    y = n,
    fill  = ifelse(province %in% top3_prov, province, "OTHER"),
    color = ifelse(province %in% top3_prov, province, "OTHER")
  )
) +
  geom_col() +
  geom_text(
    aes(label = comma(n), group = province),
    vjust = -0.8,
    color = "black"
  ) +
  scale_fill_manual(values = prov_palette) +
  scale_color_manual(values = prov_palette) +
  labs(
    title = "Distribusi Peserta menurut Provinsi",
    x = "",
    y = "# Peserta"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_bgsi

## ======================================
## 2) Age distribution by sex
## ======================================
age_sex_counts <- df %>% 
  dplyr::group_by(age, sex) %>% 
  dplyr::summarise(n = n_distinct(patient_id), .groups = "drop")

p_age_sex <- ggplot(
  age_sex_counts,
  aes(
    x = factor(age),
    y = n,
    fill  = sex,
    color = sex
  )
) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = comma(n)),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    color = "black",
    size = 3
  ) +
  scale_fill_manual(
    values = c(
      "Laki-laki" = "#07baae",
      "Perempuan" = "#bcc245"
    )
  ) +
  scale_color_manual(
    values = c(
      "Laki-laki" = "#07baae",
      "Perempuan" = "#bcc245"
    )
  ) +
  labs(
    title = "Distribusi Usia menurut Jenis Kelamin",
    x = "Usia (tahun)",
    y = "# Peserta",
    fill = "Jenis Kelamin",
    color = "Jenis Kelamin"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_bgsi +
  theme(axis.title.x = element_text(vjust=-0.2) ) + 
  theme(legend.position = "top")

## ======================================
## 3) Count by state
## ======================================
state_counts <- df %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarise(n = n_distinct(patient_id), .groups = "drop")


base_state_palette <- c(
  "#43B178FF",
  "#d2db2fff",  
  "#AEDEC5FF",
  "#F4A259FF" 
)


state_levels <- unique(state_counts$state)

state_palette <- c()
if ("selesai" %in% state_levels) {
  state_palette["selesai"] <- "#43B178FF"
}
if ("daftar" %in% state_levels) {
  state_palette["daftar"] <- "#e3872cff"
}

other_states <- setdiff(state_levels, names(state_palette))
if (length(other_states) > 0) {
  remaining_cols <- base_state_palette[!(base_state_palette %in% state_palette)]
  remaining_cols <- remaining_cols[seq_len(min(length(remaining_cols), length(other_states)))]
  names(remaining_cols) <- other_states
  state_palette <- c(state_palette, remaining_cols)
}

p_state <- ggplot(
  state_counts,
  aes(
    x = fct_reorder(state, n, .desc = TRUE),
    y = n,
    fill  = state,
    color = state
  )
) +
  geom_col() +
  geom_text(
    aes(label = comma(n), group = state),
    vjust = -0.8,
    color = "black"
  ) +
  scale_fill_manual(values = state_palette) +
  scale_color_manual(values = state_palette) +
  labs(
    title = "Jumlah Peserta menurut Status",
    x = "",
    y = "# Peserta"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_bgsi

## ==========================
## Combine into one figure
## ==========================
combined_plot <- (p_province | p_age_sex) / p_state

combined_plot
png(file = "output/plots.png", width = 2000, height = 970, units = "px", res = 100)
plot(combined_plot)
dev.off()

#ggsave( plot = combined_plot)
write.table(top3_prov, "output/top3_prov.csv")
write.table(age_sex_counts, "output/age_sex_counts.csv")

