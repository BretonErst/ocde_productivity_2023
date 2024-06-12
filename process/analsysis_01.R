###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



# librerías
library(tidyverse)
library(styleBreton)
library(ggrepel)
library(tidytext)



# adquisición de datos
prod_00 <- 
  read_csv("prep/productivity_00.csv")



# USD constant PPPs
prod_const_ppp <- 
  prod_00 |> 
  filter(measure == "USD, constant prices, 2015 PPPs") |> 
  pivot_wider(names_from = code, 
              values_from = value)



# GDP Per Hour Worked
# evolución
# países seleccionados
paises_evol <- 
  c("MEX", "KOR", "FRA", "CHL", "CAN", "DNK", "DEU", "USA", "BRA", "CHN", 
    "COL", "IRL", "ESP", "NLD", "RUS", "TUR", "CRI", "IND", "PRT", "JPN")


# dataframe para evolución en tiempo
gdphrs_evol <- 
  prod_const_ppp |> 
  filter(description == "GDP per hour worked") |> 
  filter(location %in% paises_evol) |> 
  mutate(max_val = if_else(time == max(time), 
                           country, NA),
         destaca = location == "MEX")


gdphrs_evol |> 
  filter(location %in% c("MEX", "USA") &
           time == 2022) |> 
  select(country, GDPHRS)

# plot de evolución en tiempo 1970 - 2022
gdphrs_evol |> 
  ggplot(aes(x = time, 
             y = GDPHRS,
             color = destaca, 
             group = country)) +
  geom_line(alpha = 0.65) +
  geom_text_repel(aes(label = max_val),
                  na.rm = TRUE,
                  size = 2.6,
                  direction = "y",
                  hjust = -1,
                  segment.size = 0.7,
                  segment.alpha = 0.5,
                  segment.linetype = "dotted") +
  theme_breton() +
  theme(legend.position = "none") +
  scale_color_manual(breaks = c(FALSE, TRUE),
                     values = c("#B8BBBC", "darkred")) +
  # scale_color_hue(l = 45, c = 85) +
  scale_x_continuous(breaks = seq(min(prod_curr_ppp$time), 
                                  max(prod_curr_ppp$time),
                                  4),
                     limits = c(min(prod_curr_ppp$time), 
                                max(prod_curr_ppp$time) + 10)) +
  labs(title = "¿Cómo Ha Evolucionado La Productividad del Sistema Laboral Mexicano?",
       subtitle = "Producto interno bruto por hora trabajada",
       x = NULL,
       y = "USD constant PPPs",
       caption = "Source: OECD Stats. Gross Domestic Product Per Hour Worked
         1970 - 2022. Selected countries.
         <br>Visualization: Juan L. Bretón, PMP | @juanlbreton") +
  scale_y_continuous(labels = scales::dollar_format())
  

# guarda gráfica
ggsave(filename = "figures/gdphrs_evolution.jpg", 
       device = "jpeg", 
       dpi = "retina")




# spread
paises_spread <- 
  c("MEX", "KOR", "FRA", "CHL", "CAN", "DNK", "DEU", "USA", "BRA", "CHN", 
    "COL", "IRL", "ESP", "NLD", "RUS", "TUR", "CRI", "IND", "PRT", "JPN")

# dataframe para spread
gdphrs_spread <- 
  prod_const_ppp |> 
  filter(time > 1989) |>
  filter(description == "GDP per hour worked") |> 
  filter(location %in% paises_spread) |> 
  mutate(max_val = if_else(time == max(time), 
                           country, NA),
         ult_val = if_else(time == max(time),
                           GDPHRS, NA),
         destaca = (location == "MEX"))

# valor mediano
val_median <- 
  gdphrs_spread |> 
  summarize(medi = median(ult_val, na.rm = TRUE)) |> 
  pull()


# gráfica de spread
gdphrs_spread |> 
  ggplot(aes(x = GDPHRS, 
             y = country, 
             color = destaca)) +
  geom_vline(xintercept = val_median,
             alpha = 0.15,
             color = "midnightblue",
             linewidth = 2.5) +
  geom_jitter(alpha = 0.35,
              height = 0.15,
              width = 0.01,
              size = 2.5) +
  geom_point(aes(x = ult_val),
             pch = 21,
             size = 3.5,
             color = "midnightblue",
             fill = "midnightblue",
             alpha = 0.4) +
  geom_label(aes(label = scales::dollar(val_median), 
                 x = val_median, 
                 y = 17.35),
             family = "Encode Sans Condensed",
             size = 4.45) +
  theme_breton() +
  theme(legend.position = "none") +
  labs(title = "¿Cuánto Ha Crecido La Productividad del Sistema Laboral Mexicano?",
       subtitle = "Dispersión del producto interno bruto por hora trabajada de 1990 a 2022",
       y = NULL,
       x = "USD constant PPPs",
       caption = "Source: OECD Stats. Gross Domestic Product Per Hour Worked
         1990 - 2022. Selected countries. Value for 2022 in blue. Median value for 2022 in line.
         <br>Visualization: Juan L. Bretón, PMP | @juanlbreton") +
  scale_color_manual(breaks = c(FALSE, TRUE),
                     values = c("#B8BBBC", "#C64F45")) +
  scale_x_continuous(breaks = seq(min(gdphrs_spread$GDPHRS),
                                  max(gdphrs_spread$GDPHRS),
                                  8),
                     labels = scales::dollar_format())
  

# guarda gráfica
ggsave(filename = "figures/gdphrs_spread.jpg", 
       device = "jpeg", 
       dpi = "retina")



# HOURS WORKED
# data frame para horas trabajadas
prod_hrwd <- 
  prod_00 |> 
  filter(code == "HRSAV")


# máximos por año
prod_hrwd_max <- 
  prod_hrwd |> 
  group_by(time) |> 
  slice_max(order_by = value, n = 5) |> 
  arrange(-time) |> 
  select(location, country, time, value) 


# plot de máximos
prod_hrwd_max |> 
  filter(time >= 2018) |> 
  ggplot(aes(x = value, 
             y = reorder_within(country, value, as_factor(time)), 
             fill = as_factor(time))) +
  geom_col(alpha = 0.75) +
  geom_text(aes(label = scales::number(value, accuracy = 1, big.mark = ",")),
            family = "Encode Sans Condensed",
            color = "grey30",
            size = 3.90,
            hjust = 1.1) +
  facet_grid(rows = vars(fct_rev(as_factor(time))), 
             scales = "free_y") +
  theme_breton() +
  theme(legend.position = "none") +
  labs(title = "¿En Dónde Se Trabajan Más Horas?",
       subtitle = "Promedio de horas trabajadas cada año por persona empleada",
       y = NULL,
       x = "Hours worked annually",
       caption = "Source: OECD Stats. Average hours worked per person employed
         1970 - 2022.
         <br>Visualization: Juan L. Bretón, PMP | @juanlbreton") +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0, 50),
                     labels = scales::number_format(big.mark = ",")) +
  scale_fill_hue(c = 65, h = c(300, 140))


# guarda gráfica
ggsave(filename = "figures/hrwd_max.jpg", 
       device = "jpeg", 
       dpi = "retina")


prod_hrwd_max |> 
  pivot_wider(names_from = time, 
              values_from = value, 
              names_prefix = "y_") |> 
  filter(location %in% c("MEX", "COL", "KOR")) |> 
  reframe(change = (y_2022 - y_2020) / y_2020) * 100








