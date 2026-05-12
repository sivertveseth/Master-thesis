# ---- Forslag til R-script for analyser ----

# ---- (1) Forberedelser ----

# 1.0) Pakker
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(viridis)
library(here)
library(patchwork)
library(gt)

# 1.1) Velge hvor det skal lagres
grunnlag <- "master_output"      # eksempler: "pilot/n30" | "endelig_data/xls_raw"

out_sav <- here("R", "master_output")

# 1.2) Innlesing av datasett
data <- readRDS(here("R","master_output", "bearbeidet datasett", "fullt_datasett_komplett_2026-04-21_22-53.rds")) 

# 1.3)
# Fordi det er blitt gjort en matematisk utregning tidligere, der alle radene for den testen hadde NA også ble det gjort en utregning 
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

# ---- (2) Demografisk oversikt ----

# Bare en sikkerhetssjekk at det er ei rad per deltaker per fase - noko det er
demografi <- data %>% 
  distinct(eilo_id, phase, .keep_all = TRUE) 

# Sammendrag per fase og kjønn
sammendrag_kjonn <- demografi %>% 
  group_by(phase, kjonn) %>% 
  summarise(
    n = n(),
    hoyde = sprintf("%.1f \u00B1 %.1f", round(mean(hoyde, na.rm = TRUE), 1), round(sd(hoyde, na.rm = TRUE), 1)),
    vekt = sprintf("%.1f \u00B1 %.1f", round(mean(vekt, na.rm = TRUE), 1), round(sd(vekt, na.rm = TRUE), 1)),
    alder = sprintf("%.1f \u00B1 %.1f", round(mean(alder, na.rm = TRUE), 1), round(sd(alder, na.rm = TRUE), 1)),
    .groups = "drop"
  ) 

# Sammendrag per fase (uavhengig av kjønn)
sammendrag_total <- demografi %>% 
  group_by(phase) %>% 
  summarise(
    n = n(),
    hoyde = sprintf("%.1f \u00B1 %.1f", round(mean(hoyde, na.rm = TRUE), 1), round(sd(hoyde, na.rm = TRUE), 1)),
    vekt = sprintf("%.1f \u00B1 %.1f", round(mean(vekt, na.rm = TRUE), 1), round(sd(vekt, na.rm = TRUE), 1)),
    alder = sprintf("%.1f \u00B1 %.1f", round(mean(alder, na.rm = TRUE), 1), round(sd(alder, na.rm = TRUE), 1)),
    .groups = "drop"
  ) %>% 
  mutate(kjonn = "Total")

# Slår sammen total + kjønnsgrupper
sammendrag <- bind_rows(sammendrag_total, sammendrag_kjonn)

# Gjør om til long format som en del av å gjøre datsettet klar til tabell - dette slik at eg får kombinasjonene phase + kjonn
demografisk_long <- sammendrag %>% 
  mutate(n = as.character(n)) %>%  # så alt blir tekst
  select(phase, kjonn, n, hoyde, vekt, alder) %>% 
  pivot_longer(
    cols      = c(n, hoyde, vekt, alder),
    names_to  = "Variabel",
    values_to = "Verdi"
  ) %>% 
  mutate(
    Variabel = recode(
      Variabel,
      "n"     = "Participants (n)",
      "hoyde" = "Height (cm)",
      "vekt"  = "Weight (kg)",
      "alder" = "Age (years)"
    )
  )

# Gjør til wide-format slik at eg får kombinasjonen phase_kjonn - det gjør at eg kan skille på kjonn og fase i tabellen
demografisk_wide <- demografisk_long %>% 
  mutate(
    phase = factor(phase, levels = c("t1", "t2", "t3"))  # pre først, så post
  ) %>% 
  unite("col_name", phase, kjonn, sep = "_") %>%   # f.eks. "pre_Kvinne"
  pivot_wider(
    names_from  = col_name,
    values_from = Verdi
  )

# Tabell
gt_demografisk <- demografisk_wide %>% 
  gt(rowname_col = "Variabel") %>% 
  tab_spanner(
    label = "T1",
    columns = starts_with("t1_")
  ) %>% 
  tab_spanner(
    label = "T2",
    columns = starts_with("t2_")
  ) %>% 
  tab_spanner(
    label = "T3",
    columns = starts_with("t3_")
  ) %>% 
  
  cols_label(
    t1_Total  = "Total",
    t1_kvinne = "Women",
    t1_mann   = "Men",
    t2_Total  = "Total",
    t2_kvinne = "Women",
    t2_mann   = "Men",
    t3_Total  = "Total",
    t3_kvinne = "Women",
    t3_mann   = "Men"
  ) %>% 
  
  cols_width(
    stub() ~ px(120),
    everything() ~ px(75)
  ) %>% 
  
  tab_style(
    style = cell_text(weight = "normal"),
    locations = cells_column_labels()
  ) %>% 
  
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>% 
  
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stub()
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "white",
      weight = px(10)
    ),
    locations = cells_body(columns = c(t1_mann, t2_mann))
  ) %>% 
  
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(5),
    column_labels.font.weight = "normal",
    heading.title.font.size = px(14),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1)
  )

gt_demografisk

## VIKTIG! ----

# Her er det viktig å nevne at selv om det er blitt inkludert 303 stk i RCT-studien og det er 303 unike ID-er i datasettet, så er det ikkje alle som har alle tre testene. Noen mangler f.eks t1, t2 eller t3. Ved å bruke kodene under, får man en oversikt over dei som mangler ved verdt tidspunkt

# T1
mangler_t1 <- demografi %>%
  distinct(eilo_id, phase) %>%
  filter(phase == "t1") %>%
  distinct(eilo_id)

demografi %>%
  distinct(eilo_id) %>%
  anti_join(mangler_t1, by = "eilo_id")

# T2
mangler_t2 <- demografi %>%
  distinct(eilo_id, phase) %>%
  filter(phase == "t2") %>%
  distinct(eilo_id)

demografi %>%
  distinct(eilo_id) %>%
  anti_join(mangler_t2, by = "eilo_id")

# T3
mangler_t3 <- demografi %>%
  distinct(eilo_id, phase) %>%
  filter(phase == "t3") %>%
  distinct(eilo_id)

demografi %>%
  distinct(eilo_id) %>%
  anti_join(mangler_t3, by = "eilo_id")

# ---- (2) Oversikt over gjennomførte tester per ID ----
missing_test <- data %>% 
  group_by(eilo_id) %>% 
  summarise(
    har_t1 = any(phase == "t1"),
    har_t2 = any(phase == "t2"),
    har_t3 = any(phase == "t3"),
    .groups = "drop"
  ) %>% 
  mutate(kategori = case_when(
    har_t1 & har_t2 & har_t3 ~ "alle",
    har_t1 & !har_t2 & !har_t3 ~ "kun_t1",
    !har_t1 & har_t2 & !har_t3 ~ "kun_t2",
    !har_t1 & !har_t2 & har_t3 ~ "kun_t3",
    har_t1 & har_t2 & !har_t3 ~ "t1_t2",
    !har_t1 & har_t2 & har_t3 ~ "t2_t3",
    har_t1 & !har_t2 & har_t3 ~ "t1_t3",
    TRUE ~ "ingen"
  ))


# Teller over hvor mange som har kun_pre målinger og hvor mange som har alle
oversikt_test <- missing_test %>% 
  count(kategori, name = "antall")

# Lager en tabell
gt_test <- oversikt_test %>% 
  gt()%>% 
  cols_label(
    kategori = "Kategori",
    antall = "Antall deltakere"
  ) %>% 
  tab_header(
    title = "Oversikt over tilgjengelige tester",
    subtitle = "Fordelt mellom de ulike testtidspunktene"
  )

gt_test

# 2.2) Antall NA per variabel
missing_totalt <- data %>% 
  summarise(across(everything(), ~sum(is.na(.)))) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "variabel",
    values_to = "n_missing"
  ) 

missing_totalt


# 2.3) Bygger videre på missing_totalt. Formål er å kunne gå spesifikt inn å se kven som mangler f.eks BORG-score

variabler <- setdiff(names(data), c("eilo_id", "phase"))

missing_detaljer <- data %>% 
  select(eilo_id, phase, all_of(variabler)) %>%
  mutate(across(all_of(variabler), as.character)) %>% 
  pivot_longer(
    cols = all_of(variabler),
    names_to = "variabel",
    values_to = "verdi"
  ) %>% 
  filter(is.na(verdi) | verdi == "") %>% 
  arrange(variabel, eilo_id, phase)

# 2.4) Skriver ut oversikten av manglende verdier
write.csv(missing_test, "missing_test_komplett.csv")
write.csv(missing_totalt, "missing_totalt_komplett.csv")
write.csv(missing_detaljer, "missing_detaljer_komplett.csv")

# 2.5) Beskrivende oversikt av tid og CLE

## 2.5.1) Oppsummering av hver fase for tid og CLE-score på gruppenivå
data_summary <- data %>% 
  group_by(phase) %>% 
  summarise(
    n = n_distinct(eilo_id),
    mean_tid = mean(tid, na.rm =TRUE),
    sd_tid =sd(tid, na.rm = TRUE),
    mean_cle_a = mean(cle_sub_a, na.rm = TRUE),
    sd_cle_a = sd(cle_sub_a, na.rm=TRUE),
    mean_cle_b = mean(cle_sub_b, na.rm = TRUE),
    sd_cle_b = sd(cle_sub_b, na.rm=TRUE),
    mean_cle_c = mean(cle_sub_c, na.rm = TRUE),
    sd_cle_c = sd(cle_sub_c, na.rm=TRUE),
    mean_cle_d = mean(cle_sub_d, na.rm = TRUE),
    sd_cle_d = sd(cle_sub_d, na.rm=TRUE),
    mean_cle_tot = mean(cle_tot, na.rm = TRUE),
    sd_cle_tot = sd(cle_tot, na.rm=TRUE)
  )
# ---- (3) Endring ----

# 3.1) Finner alle dei numeriske variablene (de eg ønsker å kjøre endring på)
variabler_delta <- data %>% 
  # Tar vekk dei kolonnene som eg ikkje tenker å kjøre endringsanalyse på
  select(-eilo_id, -phase, -dato, -starttid) %>% 
  select(where(is.numeric)) %>% 
  # Lagrer navnene på kolonnene i en vektor for senere bruk
  names()

# 3.2) Lager også en liste med variabler som eg ønsker å ha med, men uten delta
# Begrunnelse: Ønsker en komplett oversikt 

variabler_andre <- c("dato", "starttid")

# 3.3) Lager delta 
delta_long <- data %>% 
  select(eilo_id, phase, all_of(variabler_delta)) %>%
  
  # Gjør om til long format for å behandle alle variablene likt
  pivot_longer(
    cols = -c(eilo_id, phase),
    names_to = "variabel",
    values_to = "verdi"
  ) %>%
  
  # Gjør om til wide format for å gjøre det enkelt å regne ut delta. Har en rad per id + variabel
  pivot_wider(
    id_cols = c(eilo_id,variabel),
    names_from = phase,
    values_from = verdi
  ) %>% 
  
  # Lager en kolonne delta som tar for seg alle dei ønskede variablene og regner ut endring
   # delta_1 = t2 - t1
   # delta_2 = t3 - t2
  mutate(
    delta_1 = t2 - t1,
    delta_2 = t3 - t2
  )

# 3.4) For å gjøre tabellen mer oversiktlig
delta_tabell <- delta_long %>% 
  
  # Legger testidspunktene og deltaene i en kolonne
  pivot_longer(
    cols = c(t1, t2, t3, delta_1, delta_2),
    names_to = "phase2",
    values_to = "verdi"
  ) %>% 
  
  # Ønsker at for kvar id og fase, får eg 5 rader og alle variablene ligg bortover som kolonner
  pivot_wider(
    id_cols = c(eilo_id, phase2),
    names_from = variabel,
    values_from = verdi
  ) %>% 
  
  # Sørger for at rekkefølgen blir t1 -> t2 -> delta_1 -> t3 -> delta_2
  mutate(
    phase2 = factor(phase2, levels = c("t1", "t2", "t3", "delta_1", "delta_2"))
  ) %>% 
  rename(
    phase = "phase2"
  ) %>% 
  arrange(eilo_id, phase) # Gir ei ryddig sortering per id

# 3.5) Lager en fullstendig tabell med gjennomsnittlig endring fra pre til post med standardavvik
delta_summary <- delta_tabell %>% 
  filter(phase %in% c("delta_1", "delta_2")) %>% 
  group_by(phase) %>% 
  summarise(
    across(
      alder:vtin,
      list(
        mean = ~ round(mean(.x, na.rm = TRUE), 2),
        sd   = ~ round(sd(.x, na.rm = TRUE), 2),
        n    = ~ sum(!is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  ) %>% 
  pivot_longer(
    cols = -phase,
    names_to = c("variabel", ".value"),
    names_sep = "__"
  )

# 3.1) Sjekke om høyden er relativt likt mellom fasene ----

delta_tabell2 <- delta_tabell %>%
  mutate(
    hoyde = na_if(hoyde, 0),
    vekt  = na_if(vekt, 0)
  )

p <- delta_tabell2 %>%
  filter(phase %in% c("t1","t2","t3")) %>% 
  mutate(phase = factor(phase, levels = c("t1","t2","t3"))) %>%
  pivot_longer(cols = c(vekt, hoyde),
               names_to = "variabel",
               values_to = "verdi") %>%
  filter(!is.na(verdi)) %>%
  ggplot(aes(x = phase, y = verdi)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ variabel, scales = "free_y") +
  theme_minimal() +
  labs(x = NULL, y = NULL, title = "Vekt og høyde per tidspunkt (t1–t3)")

print(p)

# ---- (4) Oppsummering av endringen av CLE og tid ----

# 4.1) Prestasjon (tid)

# 4.1.1) Filtrerer ut slik at eg kun tar for meg endring i tid
delta_tid <- delta_tabell %>% 
  select(eilo_id, phase, tid) %>% 
  pivot_wider(
    names_from  = phase,
    values_from = tid
  ) %>% 
  mutate(
    bedring_1 = case_when(
      delta_1 > 0 ~ "Bedre",
      delta_1 < 0 ~ "Verre",
      delta_1 == 0 ~ "Ingen endring",
      is.na(delta_1) ~ "Ingen data"
    ),
    bedring_2 = case_when(
      delta_2 > 0 ~ "Bedre",
      delta_2 < 0 ~ "Verre",
      delta_2 == 0 ~ "Ingen endring",
      is.na(delta_2) ~ "Ingen data"
    ),
    
    bedring_1     = factor(bedring_1,     levels = c("Ingen data", "Verre", "Ingen endring", "Bedre")),
    bedring_2     = factor(bedring_2,     levels = c("Ingen data", "Verre", "Ingen endring", "Bedre"))
  ) %>% 
  select(eilo_id, bedring_1, bedring_2)

# 4.4) Tabell som viser t1, t2, t3 og deltaene

# Variablene eg ønsker å hente ut fra delta_tabell
vars <- c("vo2kg","vo2", "tid", "cle_sub_a", "cle_sub_b", "cle_sub_c", "cle_sub_d", "cle_tot")

# Rydder slik at eg får det i longformat
delta_cle_tid <- delta_tabell %>% 
  pivot_longer(
    cols = all_of(vars),
    names_to = "variabel",
    values_to = "verdi"
  ) %>% 
  group_by(variabel, phase) %>% 
  summarise(
    n    = sum(!is.na(verdi)),
    mean = round(mean(verdi, na.rm = TRUE), digits = 2),
    sd   = sd(verdi,   na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    mean_sd = sprintf("%.2f \u00B1 %.2f", mean, sd) # Lager formatet for snitt ± sd
  )

tabell_cle_tid <- delta_cle_tid %>% 
  select(variabel, phase, mean_sd) %>% 
  
  pivot_wider(
    names_from = phase,
    values_from = mean_sd
  ) %>%
  
  select(variabel, t1, t2, t3, delta_1, delta_2) %>% 
  
  mutate(variabel = recode(variabel,
                           "cle_sub_a" = "CLE subscore A",
                           "cle_sub_b" = "CLE subscore B",
                           "cle_sub_c" = "CLE subscore C",
                           "cle_sub_d" = "CLE subscore D",
                           "cle_tot"   = "CLE total",
                           "tid"       = "Tid (sek)",
                           "vo2kg"     = "VO₂peak (ml·kg⁻¹·min⁻¹)",
                           "vo2"       = "VO₂peak (ml·min⁻¹)"
  ))

# Selve tabellen

gt_cle_tid <- tabell_cle_tid %>% 
  gt() %>% 
  
  tab_header(
    title = "Oversikt CLE-score, tid og VO₂peak") %>% 
    
  # Gir navn til hver kolonne
  cols_label(
    variabel = "Variabel",
    t1 = "T1 (mean \u00B1 SD)",
    t2 = "T2 (mean \u00B1 SD)",
    t3 = "T3 (mean \u00B1 SD)",
    delta_1 = "\u0394 T2 - T1 (mean \u00B1 SD)",
    delta_2 = "\u0394 T3 - T2 (mean \u00B1 SD)"
  ) %>% 
  
  # Grupperer kolonnene visuelt
  tab_spanner(
    label = "M\u00E5linger",
    columns = c(t1, t2, t3)
  ) %>% 
  tab_spanner(
    label = "Endring",
    columns = c(delta_1, delta_2)
  ) %>%
  
  # Gjør at kolonnene med variablene er flyttet til venstre, og dei andre er sentrert
  cols_align(align = "left",  columns = variabel) %>%
    cols_align(align = "center", columns = c(t1, t2, t3, delta_1, delta_2)) %>% 
  
  # Lager bakgrunnfarge på annenhver rad
  opt_row_striping() %>%
  
  # Lik skriftstørrelse på hele tabellen, og gir litt padding - luft - mellom hver rad
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(4)
  ) %>% 
  tab_source_note(
    source_note = "Note: Antall observasjoner varierer mellom tidspunktene p\u00E5 grunn av manglende data."
  )


gt_cle_tid
