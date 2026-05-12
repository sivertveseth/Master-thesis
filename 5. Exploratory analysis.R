# Eksplorativ analyse av frisk subgruppe ----

# En av inklusjonskriteriene for å være med i studien var CLE-score lik eller høyere enn 2 ved glottisk eller supraglottisk nivå under maksimal anstrengelse (C eller D)

# Eg ønsker derfor å se om hvor mange som har ved studieslutt har blitt "frisk" for EILO i forhold til inklusjonskriterie i studien. 

# ---- (1) Forberedelser ----

## 1.0) Pakker ----
library(dplyr)
library(tidyr)
library(here)
library(gt)
library(ggplot2)
library(stringr)
library(slider)
library(effectsize)

## 1.1) Fil til mappen output skal lagres ----
out_sav <- here("R", "master_output")

## 1.2) Innlesing av datasett ----
data <- readRDS(here("R","master_output", "bearbeidet datasett", "fullt_datasett_komplett_2026-04-21_22-53.rds")) 

## 1.3) Enkel rydding ----
# Fordi det er blitt gjort en matematisk utregning tidligere, ref kodeblokk 4.1, der alle radene for den testen hadde NA også ble det gjort en utregning 
# der man brukte na.rm =TRUE, så vil man gjøre utregningen på ingenting og få NaN. Eg velger derfor å endre alle NaN til NA for å ikkje miste data i analysene. 

data <- data %>% 
  mutate(
    across(
      where(is.numeric),
      ~ na_if(., NaN)
    ) 
  ) %>% 
  mutate(
    phase =factor(phase, levels = c("t1", "t2", "t3"))
  )

# (2) Finne frisk subgruppe ----

# Kriterier
## 1) Registrert test ved tidspunkt 1 og 3
## 2) Gyldige, ikkje manglende scorer på cle_c og cle_d ved begge tidspunktene
## 3) ≥ 2 i C eller D ved t1
## 4) < 2 i både C og D ved t3


## 2.1) Avgrenser til dei som har tidspunkt 1 og 3, samt gyldige målinger + legger til inspiratorisk flow

data_frisk <- data %>%
  filter(phase %in% c("t1", "t3"),
         !is.na(cle_sub_c),
         !is.na(cle_sub_d)) %>%
  group_by(eilo_id) %>%
  filter(all(c("t1", "t3") %in% phase)) %>%
  filter(
    any(phase == "t1" & (cle_sub_c >= 2 | cle_sub_d >= 2)),
    any(phase == "t3" & (cle_sub_c < 2 & cle_sub_d < 2))
  ) %>%
  ungroup() %>%
  mutate(insp_flow = vtin / ti) %>%
  pivot_wider(
    id_cols = eilo_id,
    names_from = phase,
    values_from = c(vo2, vo2kg,ve, bf, vtex, vtin, ti, titot, rer, tid, insp_flow)
  )

data_frisk

# ---- (2) Hovedanalyse ----

## 2.1) Normalfordeling av datasettet -------

### Lager differanse variabel
data_friske_diff <- data_frisk %>%
  mutate(
    diff_vo2 = vo2_t3 - vo2_t1,
    diff_vo2kg = vo2kg_t3 - vo2kg_t1,
    diff_ve = ve_t3 - ve_t1,
    diff_bf = bf_t3 - bf_t1,
    diff_vtex = vtex_t3 - vtex_t1,
    diff_vtin = vtin_t3 - vtin_t1,
    diff_ti = ti_t3 - ti_t1,
    diff_titot = titot_t3 - titot_t1,
    diff_rer = rer_t3 - rer_t1,
    diff_tid = tid_t3 - tid_t1,
    diff_insp_flow = insp_flow_t3 - insp_flow_t1
  ) %>% 
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  mutate(
    variable = recode(
      variable,
      diff_vo2 = "VO2",
      diff_vo2kg = "VO2kg",
      diff_ve = "VE",
      diff_bf = "BF",
      diff_vtex = "VTex",
      diff_vtin = "VTin",
      diff_ti = "Ti",
      diff_titot = "Ti/Ttot",
      diff_rer = "RER",
      diff_tid = "Tid til utmattelse",
      diff_insp_flow = "Inspiratorisk flow"
    )
  )

# Histogrammene
ggplot(data_friske_diff, aes(x = value)) +
  geom_histogram(bins = 20, fill = "grey", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histogram of differences (T3 - T1)",
       x = "Difference",
       y = "Frequency") +
  theme_minimal()

# Q-Q-plottene
ggplot(data_friske_diff, aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Q-Q plots of differences (T3 - T1)") +
  theme_minimal()

# Noen var preget av avvik i halene
## Gjelder VE, BF, VO2kg, VO2 og tid til utmattelse - > sjekker derfor med boksplott om snitt og median blir påvirket av outliers

data_long_box <- data_friske_diff %>%
  filter(variable %in% c("VE", "BF", "VO2", "VO2kg", "Tid til utmattelse"))

## Boksplott
ggplot(data_long_box, aes(y = value)) +
  geom_boxplot(fill = "grey") +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Boxplots of differences (T3 - T1)",
    y = "Difference"
  ) +
  theme_minimal()

## 2.2) Ved peak  ----

# Variabler som skal analyseres
vars_peak <- c("tid", "rer", "vo2", "vo2kg", "ve", "bf", "vtex", "ti", "titot", "insp_flow")

# Funksjon for paret oppsummering
summary_paired <- function(data, var) {
  t1_col <- paste0(var, "_t1")
  t3_col <- paste0(var, "_t3")
  
  t1 <- data[[t1_col]]
  t3 <- data[[t3_col]]
  
  # Behold kun komplette par
  complete_idx <- complete.cases(t1, t3)
  t1 <- t1[complete_idx]
  t3 <- t3[complete_idx]
  
  diff <- t3 - t1
  test <- t.test(t3, t1, paired = TRUE)
  
  # Cohen’s d (pooled SD)
  sd_pooled <- sqrt((sd(t1)^2 + sd(t3)^2) / 2)
  cohen_d <- mean(diff) / sd_pooled
  
  # Bootstrap CI (samme rolle som CI fra repeated_measures_d)
  set.seed(123)
  boot_d <- replicate(5000, {
    idx <- sample(seq_along(diff), replace = TRUE)
    
    t1_b <- t1[idx]
    t3_b <- t3[idx]
    diff_b <- t3_b - t1_b
    
    sd_pooled_b <- sqrt((sd(t1_b)^2 + sd(t3_b)^2) / 2)
    mean(diff_b) / sd_pooled_b
  })
  
  ci_d <- quantile(boot_d, c(0.025, 0.975), na.rm = TRUE)
  
  data.frame(
    variable = var,
    n = length(t1),
    mean_t1 = mean(t1),
    sd_t1 = sd(t1),
    mean_t3 = mean(t3),
    sd_t3 = sd(t3),
    mean_diff = mean(diff),
    sd_diff = sd(diff),
    ci_low = test$conf.int[1],
    ci_high = test$conf.int[2],
    p_value = test$p.value,
    cohen_d = cohen_d,
    ci_low_d = ci_d[1],
    ci_high_d = ci_d[2]
  )
}

# Kjør analysene for alle variabler
results <- bind_rows(
  lapply(vars_peak, function(x) summary_paired(data_frisk, x))
) %>%
  mutate(
    p_adj_bh = p.adjust(p_value, method = "BH"),
    significant_bh = p_adj_bh < 0.05,
    
    variable_label = recode(
      variable,
      tid = "Time~to~exhaustion",
      rer = "RER",
      vo2 = "VO[2]",
      vo2kg = "VO[2]/kg",
      ve = "VE",
      bf = "BF",
      vtex = "VT",
      ti = "Ti",
      titot = "Ti/Ttot",
      insp_flow = "Inspiratory~flow"
    )
  )

# Evt. lage en avrundet tabellversjon
results_table <- results %>%
  mutate(across(where(is.numeric), ~ round(.x,3)))

# Se resultater
results_table

### Tabell av resultatet -------------

tabell <- results %>% 
  mutate(
    label = case_when(
      variable == "tid" ~ "Tid til utmattelse [sek]",
      variable == "rer" ~ "RER",
      variable == "vo2" ~ "VO2 [ml/min]",
      variable == "vo2kg" ~ "VO2kg [ml/kg/min]",
      variable == "ve" ~ "VE [L/min]", 
      variable == "bf" ~ "BF [1/min]", 
      variable == "vtex" ~ "VT [L]",
      variable == "ti" ~ "Ti [sek]",
      variable == "titot" ~ "Ti/Ttot [%]",
      variable == "insp_flow" ~ "Inspiratorisk flow [L/s]",
      TRUE ~ variable
    ),
    T1 = sprintf("%.1f (%.1f)", mean_t1, sd_t1),
    T3 = sprintf("%.1f (%.1f)", mean_t3, sd_t3),
    Endring = sprintf("%.1f (%.1f til %.1f)", mean_diff, ci_low, ci_high),
    p_value = if_else(
      p_value < 0.001,
      "<0.001",
      sprintf("%.3f", p_value)
    ),
    p_adj_fmt = if_else(
      p_adj_bh < 0.001,
      "<0.001",
      sprintf("%.3f", p_adj_bh)
    )
  ) %>% 
  select(
    Variabel = label,
    n,
    `Test 1, mean (SD)` = T1,
    `Test 3, mean (SD)` = T3,
    `Endring, mean (95 % KI)` = Endring,
    `p-ujustert` = p_value,
    `p-justert` = p_adj_fmt
  )

gt_tabell <- tabell %>% 
  gt() %>% 
  tab_header(
    title = md("**Tabell X. Endringer (Test 3 — Test 1) i fysiologiske parametere hos den friske subgruppen ved peak**")
  ) %>% 
  cols_align(
    align = "left",
    columns = Variabel
  ) %>% 
  cols_align(
    align = "center",
    columns = c(n, `Test 1, mean (SD)`, `Test 3, mean (SD)`, `Endring, mean (95 % KI)`, `p-ujustert`, `p-justert`)
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_source_note(
    source_note = "Data presenteres som gjennomsnitt (SD). Endringer er vist som gjennomsnittlig differanse med 95 % konfidensintervall. P-verdier er justert med Benjamini-Hochberg for multiple tester."
  ) %>% 
  tab_options(
    table.font.size = px(12),
    heading.align = "left",
    column_labels.font.weight = "bold",
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.border.bottom.width = px(1),
    source_notes.font.size = px(10)
  )

gt_tabell

### Forrest plot med cohen d ----
plot_peak <- ggplot(
  results, 
  aes(x = cohen_d, y = reorder(variable_label, cohen_d))
) +
  geom_errorbar(aes(xmin = ci_low_d, xmax = ci_high_d), height = 0.2) +
  geom_point(aes(shape = significant_bh), size = 2.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_shape_manual(values = c(1, 16), labels = c("No", "Yes")) +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  labs(
    x = "Standardized effect (Cohen's d, pooled SD)",
    y = NULL,
    shape = "Significant after BH"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

plot_peak

## 2.3) Sensitivitetsanalyse: ved samme tidspunkt ----

# CPET-filen med de valide 10 sek målingene (kontrollert for tid) - se kolonne "tid_reset" for kva 10-sek måling det er i testen)
cpet_10_sek <- readRDS(here("R","master_output", "bearbeidet datasett", "10_sek_CPET_2026-04-21_22-51.rds")) 

# Henter ut dei ID-ene fra den friske subgruppen:
friske_id <- data_frisk %>% 
  mutate(eilo_id = str_squish(str_to_lower(eilo_id))) %>% 
  distinct(eilo_id)

# Filtrer dermed ut dei friske-IDene fra det fulle datasettet
cpet_valid_frisk <- cpet_10_sek %>%
  mutate(eilo_id = str_squish(str_to_lower(eilo_id))) %>%
  semi_join(friske_id, by = "eilo_id")

# Sammenligne på samme tidspunkt:

# Finner den siste 10 sekunders målingen som er lik på begge testene (isotid) - vil være peak på en av testene. 

isotime <- cpet_valid_frisk %>%
  filter(phase %in% c("t1", "t3")) %>%
  group_by(eilo_id, phase) %>%
  summarise(max_tid = max(tid_reset), .groups = "drop") %>%
  pivot_wider(names_from = phase, values_from = max_tid) %>%
  filter(complete.cases(t1, t3)) %>%
  mutate(isotime = pmin(t1, t3)) %>%
  select(eilo_id, isotime)

cpet_iso_30sek <- cpet_valid_frisk %>%
  filter(phase %in% c("t1", "t3")) %>%
  inner_join(isotime, by = "eilo_id") %>%
  group_by(eilo_id, phase) %>%
  arrange(tid_reset, .by_group = TRUE) %>%
  mutate(
    roll_mean3 = slide_dbl(
      vo2kg,
      .f = ~ if (all(!is.na(.x))) mean(.x) else NA_real_,
      .before = 2,
      .complete = TRUE
    ),
    end_tid = tid_reset  # sluttpunkt for vinduet
  ) %>%
  group_modify(~{
    g <- .x
    
    valid <- which(!is.na(g$roll_mean3))
    if (length(valid) == 0) return(g[0, ])
    
    # Finner vinduet som sluttar nærmast isotime
    best_i <- valid[which.min(abs(g$end_tid[valid] - g$isotime[1]))]
    
    s <- best_i - 2
    e <- best_i
    
    if (s < 1) return(g[0, ])
    
    g[s:e, , drop = FALSE]
  }) %>%
  ungroup()

# Finner snittet for hver deltager per test
data_iso <- cpet_iso_30sek %>%
  group_by(eilo_id, phase) %>%
  summarise(
    across(vo2kg:vtin, ~ round(mean(.x, na.rm = TRUE), 1)),
    n_rows = n(),
    .groups = "drop"
  ) %>%   
  mutate(insp_flow = vtin / ti) %>% 
  pivot_wider(
    id_cols = eilo_id,
    names_from = phase,
    values_from = c(vo2, vo2kg,ve, bf, vtex, vtin, ti, titot, rer, insp_flow)
  )

# Én deltaker (ID 2432) ble identifisert med ekstreme verdier på tvers av flere fysiologiske parametere. Ved nærmere inspeksjon ble dette vurdert å skyldes feil i registrert tidspunkt for målingene, slik at data sannsynligvis var hentet etter at testen var avsluttet. Dette førte til systematiske avvik i flere variabler. Observasjonen ble derfor ekskludert fra videre analyse.

# Eksluderer ID 2432 på grunn av det som virker å feil i nedskrevet tid
data_iso_clean <- data_iso %>%
  filter(eilo_id != 2432)

### Lage differanse variabel
options(scipen = 999)

data_iso_diff <- data_iso_clean %>%
  mutate(
    diff_vo2 = vo2_t3 - vo2_t1,
    diff_vo2kg = vo2kg_t3 - vo2kg_t1,
    diff_ve = ve_t3 - ve_t1,
    diff_bf = bf_t3 - bf_t1,
    diff_vtex = vtex_t3 - vtex_t1,
    diff_vtin = vtin_t3 - vtin_t1,
    diff_ti = ti_t3 - ti_t1,
    diff_titot = titot_t3 - titot_t1,
    diff_rer = rer_t3 - rer_t1,
    diff_insp_flow = insp_flow_t3 - insp_flow_t1
  ) %>% 
  mutate(across(starts_with("diff_"), ~ round(.x, 2))) %>% 
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  mutate(
    variable = recode(
      variable,
      diff_vo2 = "VO2",
      diff_vo2kg = "VO2kg",
      diff_ve = "VE",
      diff_bf = "BF",
      diff_vtex = "VTex",
      diff_vtin = "VTin",
      diff_ti = "Ti",
      diff_titot = "Ti/Ttot",
      diff_rer = "RER",
      diff_insp_flow = "Inspiratorisk flow"
    )
  )

### Normalfordeling ----

# Histogrammene
ggplot(data_iso_diff, aes(x = value)) +
  geom_histogram(bins = 20, fill = "grey", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histogram of differences (T3 - T1)",
       x = "Difference",
       y = "Frequency") +
  theme_minimal()

# Q-Q-plottene
ggplot(data_iso_diff, aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Q-Q plots of differences (T3 - T1)") +
  theme_minimal()

## Konklusjon: Ingen tydelige brudd på normalitetsprinsippet. Histogrammene er relativt symmetriske (sentrert rundt 0), og de fleste punktene i Q-Q-plottene ligger langs linjen. Det er noen haler og outliers slik som ved peak, men anses som normalt da det er ikkje unormalt at det er noen outliers ved fysiologiske målinger.

### Tabell -----

# Variabler som skal analyseres
vars <- c("rer", "vo2", "vo2kg", "ve", "bf", "vtex", "ti", "titot", "insp_flow")

# Funksjon for paret oppsummering
summary_paired <- function(data, var, R = 5000, seed = 123) {
  
  t1_col <- paste0(var, "_t1")
  t3_col <- paste0(var, "_t3")
  
  t1 <- data[[t1_col]]
  t3 <- data[[t3_col]]
  
  # Behold kun komplette par
  complete_idx <- complete.cases(t1, t3)
  t1 <- t1[complete_idx]
  t3 <- t3[complete_idx]
  
  diff <- t3 - t1
  n <- length(diff)
  
  # Paired t-test for mean difference og p-verdi
  test <- t.test(t3, t1, paired = TRUE)
  
  # Cohen's d med pooled SD fra T1 og T3
  sd_pooled <- sqrt((sd(t1)^2 + sd(t3)^2) / 2)
  cohen_d <- mean(diff) / sd_pooled
  
  # Bootstrap 95 % CI for Cohen's d pooled
  set.seed(seed)
  
  boot_d <- replicate(R, {
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    
    t1_b <- t1[idx]
    t3_b <- t3[idx]
    diff_b <- t3_b - t1_b
    
    sd_pooled_b <- sqrt((sd(t1_b)^2 + sd(t3_b)^2) / 2)
    mean(diff_b) / sd_pooled_b
  })
  
  ci_d <- quantile(
    boot_d,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
  
  data.frame(
    variable = var,
    n = n,
    
    mean_t1 = mean(t1),
    sd_t1 = sd(t1),
    mean_t3 = mean(t3),
    sd_t3 = sd(t3),
    
    mean_diff = mean(diff),
    sd_diff = sd(diff),
    ci_low = test$conf.int[1],
    ci_high = test$conf.int[2],
    p_value = test$p.value,
    
    cohen_d = cohen_d,
    ci_low_d = ci_d[1],
    ci_high_d = ci_d[2]
  )
}

# Kjør analysene for alle variabler
results_iso <- bind_rows(
  lapply(vars, function(x) summary_paired(data_iso_clean, x))
) %>%
  mutate(
    p_adj_bh = p.adjust(p_value, method = "BH"), # BH korreksjonen
    significant_bh = p_adj_bh < 0.05,
    
    # Finere navn til tabell
    variable_label = recode(
      variable,
      tid = "Time~to~exhaustion",
      rer = "RER",
      vo2 = "VO[2]",
      vo2kg = "VO[2]/kg",
      ve = "VE",
      bf = "BF",
      vtex = "VT",
      ti = "Ti",
      titot = "Ti/Ttot",
      insp_flow = "Inspiratory~flow"
    )
  )

# Evt. lage en avrundet tabellversjon
results_table_iso <- results_iso %>%
  mutate(across(where(is.numeric), ~ round(.x,3)))

# Se resultater
results_table_iso

### Tabell av resultatet

tabell_iso <- results_iso %>% 
  mutate(
    label = case_when(
      variable == "rer" ~ "RER",
      variable == "vo2" ~ "VO2 [ml/min]",
      variable == "vo2kg" ~ "VO2kg [ml/kg/min]",
      variable == "ve" ~ "VE [L/min]", 
      variable == "bf" ~ "BF [1/min]", 
      variable == "vtex" ~ "VTex [L]",
      variable == "vtin" ~ "VTin [L]",
      variable == "ti" ~ "Ti [sek]",
      variable == "titot" ~ "Ti/Ttot [%]",
      variable == "insp_flow" ~ "Inspiratorisk flow [L/s]",
      TRUE ~ variable
    ),
    T1 = sprintf("%.1f (%.1f)", mean_t1, sd_t1),
    T3 = sprintf("%.1f (%.1f)", mean_t3, sd_t3),
    Endring = sprintf("%.1f (%.1f til %.1f)", mean_diff, ci_low, ci_high),
    p_adj_fmt = if_else(
      p_adj_bh < 0.001,
      "<0.001",
      sprintf("%.3f", p_adj_bh)
    )
  ) %>% 
  select(
    Variabel = label,
    n,
    `Test 1, mean (SD)` = T1,
    `Test 3, mean (SD)` = T3,
    `Endring, mean (95 % KI)` = Endring,
    `p-justert` = p_adj_fmt
  )

gt_tabell_iso <- tabell_iso %>% 
  gt() %>% 
  tab_header(
    title = md("**Tabell X. Endringer (Test 3 — Test 1) i fysiologiske parametere hos den friske subgruppen ved samme tidspunkt**")
  ) %>% 
  cols_align(
    align = "left",
    columns = Variabel
  ) %>% 
  cols_align(
    align = "center",
    columns = c(n, `Test 1, mean (SD)`, `Test 3, mean (SD)`, `Endring, mean (95 % KI)`, `p-justert`)
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_source_note(
    source_note = "Data presenteres som gjennomsnitt (SD). Endringer er vist som gjennomsnittlig differanse med 95 % konfidensintervall. P-verdier er justert med Benjamini-Hochberg for multiple tester."
  ) %>% 
  tab_options(
    table.font.size = px(12),
    heading.align = "left",
    column_labels.font.weight = "bold",
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.border.bottom.width = px(1),
    source_notes.font.size = px(10)
  )

gt_tabell_iso

### Forest plot med cohen d -----

plot_iso <- ggplot(results_iso, 
                   aes(x = cohen_d, y = reorder(variable_label, cohen_d))) +
  geom_errorbar(aes(xmin = ci_low_d, xmax = ci_high_d), height = 0.2) +
  geom_point(aes(shape = significant_bh), size = 2.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_shape_manual(values = c(1, 16), labels = c("No", "Yes")) +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  labs(
    x = "Standardized effect (Cohen's d, pooled SD)",
    y = NULL,
    shape = "Significant after BH"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

plot_iso
