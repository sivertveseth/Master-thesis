# Figur: Pustearbeid og ventilasjon under arbeid - inspirert av Guenette & Sheel (2007)

# Benytter meg av ligningene fra Aron et al. (1992)

## Pustearbeid som funksjon av ventilasjon:
  # W_b = -80.041 + 1.459 * VE + 0.011 * VE^2

## Oksygenkostnad av pustearbeid:
  # VO_2 = 0.081 + 0.001 * (W_exercise - W_rest)

library(ggplot2)

# Lager et fiktivt datasett

VE <- seq(50, 200, by = 1)
Wb <- -80.041 + 1.459 * VE + 0.011 * VE^2

VE_rest <- 5 # Setter hvileverdier for ventilasjon, her 5 L/min
WB_rest <- -80.041 + 1.459 * VE_rest + 0.011 * VE_rest^2

# Oksygenkostnad av pustearbeid
VO2_resp <- 0.081 + 0.001 * (Wb- WB_rest)

df <- data.frame(
  VE = VE,
  Wb = Wb,
  VO2_resp = VO2_resp
)

# Skalering av figur
  # Venstre y-akse: 0-700 J/min
  # Høgre y-akse: 0-0.8 L/min
scale_factor <- 700 / 0.8

# Figur
ggplot(df, aes(x = VE)) +
  geom_line(aes(y = Wb), linewidth = 1) +
  geom_line(aes(y =VO2_resp * scale_factor),
            linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Work of breathing (J/min)",
    limits = c(0, 700),
    sec.axis = sec_axis(
      ~ . / scale_factor * 1000,
      name = expression(dot(V) * O[2] ~ "(ml/min)")
    )
  ) +
  scale_x_continuous(
    name = "Ventilation (L/min)",
    limits = c(0,250)
  ) + 
  theme_classic(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "grey90", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )
