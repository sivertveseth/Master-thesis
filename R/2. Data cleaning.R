# ---- Forslag til R-script for datarydding ----

# ---- (1) Forberedelser ----

# Pakker
library(dplyr)
library(tidyr)
library(haven)
library(here)
library(slider)
library(stringr)

# 1.0) Velge hvor det skal lagres
grunnlag <- "pilot/master_output"      # eksempler: "pilot/n30" | "endelig_data/xls_raw"

# 1.1) Fil til mappen output skal lagres
out_sav <- here("R", "master_output", "bearbeidet datasett")


# 1.2) Innlesing av datasett

## 1.2.1) CPET-filen som er laget fra datauttrekket - filen endres etter om det er pilot eller endelig ----
cpet_raw <- readRDS(here("R","master_output", "bearbeidet datasett", "CPET_full_data_2026-04-21_22-45.rds")) 

# CPET_full_data_2026-04-21_22-45.rds

## 1.2.2) Meta-dataen som ble hentet ut fra SeS-eksporten fra det samme uttrekket som CPET-dataen over er fra - justeres etter behov ----
metadata_raw <- readRDS(here("R","master_output", "bearbeidet datasett", "meta_full_data_2026-04-21_22-45.rds"))

# meta_full_data_2026-04-21_22-45.rds

## 1.2.3) Nedskriven informasjon om CLE-score og prestasjonsmålene tid og distanse fra test 1 og 3 ----
cle_perf_raw <- read_sav(here("R", "WP1_masterfil_kopi.sav")) %>% 
  select(eilo_id = ID, CLE_subscore_A_1:CLE_subscore_D_1,CLE_subscore_A_2:CLE_subscore_D_2,CLE_subscore_A_3:CLE_subscore_D_3, BORG_score_1:BORG_score_3, tid_1, distanse_1, tid_2, distanse_2, tid_3, distanse_3)

cle_perf_renset <- cle_perf_raw %>% 
  mutate(across(everything(), zap_formats)) %>%                # fjerner format.spss
  mutate(across(everything(), zap_label)) %>%                  # fjerner variabel-label
  mutate(eilo_id = str_squish(str_to_lower(eilo_id))) %>%      # rens ID
  mutate(across(starts_with("CLE_subscore"), as.integer)) %>%  # blir heltall
  mutate(across(starts_with("tid_"), as.numeric))              # gjør om tid til sekunder

cle_perf_long <- cle_perf_renset %>% 
  pivot_longer(
    cols = c(-eilo_id),                             # tar for meg alle kolonnene bortsett fra ID
    names_to = c(".value", "phase"),             # splitter kolonnenavna i to deler
    names_pattern = "(.*)_(\\d+)"                # alt før _ = variabelnavn, tallet etter = fase
  ) %>% 
  mutate(                                        # Legger til kolonnen phase, for å få frem hva test verdiene er fra
    phase =recode(phase,
                  "1" = "t1",
                  "2" = "t2",
                  "3" = "t3")
  ) %>% 
  
  # Gjør at det blir i riktig rekkefølge: t1 -> t2 -> t3
  mutate(
    phase = factor(phase, levels = c("t1", "t2", "t3"))
  ) %>% 
  rename_with(~str_replace(.x, "CLE_subscore_", "cle_sub_")) %>% 
  rename_with(tolower) %>% 
  mutate(
    cle_tot = cle_sub_a + cle_sub_b + cle_sub_c + cle_sub_d
  ) %>% 
  relocate(cle_tot, .after = cle_sub_d)

# ---- (2) Definere start og slutt i hver test ----

## 2.1) Definerer start og slutt ----

cpet_clean <- cpet_raw %>%
  select(-test_number, -kildefil, -source_type) %>% 
  group_by(eilo_id, phase) %>%
  mutate(                                      
    valid = !is.na(hastighet) & hastighet != 0      # Definerer "gyldig" som hastighet != 0 og ikke NA
  ) %>%
  
  # Start: TRUE fra og med første gyldige rad. Vil fortsette å være true til slutten av testen der hastighet er 0 igjen
  mutate(
    started = cumsum(valid) > 0,                    
    ended   = rev(cumsum(rev(valid)) > 0),          # Slutt: TRUE til og med siste gyldige rad. Her blir det lest baklengs, slik at alt vil være TRUE frem til der hastighet blir 0.
    keep    = started & ended                       # Er den delen av testen vi er interessert i - der det er blitt gjort fulle målinger med hastighet
  ) %>%
  filter(keep) %>% # behold kun gyldige målinger 
  mutate(                             # Lager en kolonne der tid blir reset ved start for hver test
    tid_reset = row_number() * 10
  ) %>% 
  select(-valid, -started, -ended, -keep) %>%
  ungroup()

## 2.2) Kontrollere for nedskrevet tid----

### 2.2.1) Legge til prestasjonsvariabelen tid----

cpet_tid <- cpet_clean %>%
  mutate(eilo_id = str_squish(str_to_lower(eilo_id))) %>%      # Sørger for at det ikkje er noen mellomrom som skjuler seg og derfor gjør eilo_idene ulike
  left_join(
    cle_perf_renset %>%
      select(eilo_id, tid_1, tid_2, tid_3),
    by = "eilo_id"
  ) %>%
  rename(
    tid_t1 = tid_1,
    tid_t2 = tid_2,
    tid_t3 = tid_3
  ) %>%
  select(eilo_id, phase, tph, tid, tid_t1, tid_t2, tid_t3, everything())

## 2.2.2) Bruker nedskrevet tid som ref for dei ulike pre og post testene hos deltagerne-----

cpet_tid_ref <- cpet_tid %>% 
  
  # Lager en kolonne som henter den nedskrevne tiden for hver fase: t1, t2 eller t3. Viss fasen er t1 så henter den verdien i tid_t1 osv..
  mutate(
    tid_ref = case_when(
      phase == "t1" ~ tid_t1,
      phase == "t2" ~ tid_t2,
      phase == "t3" ~ tid_t3,
      TRUE ~ NA_real_
    )
  )

## 2.2.3) Bevarer dei målingene som er valide------
cpet_valid <- cpet_tid_ref %>% 
  filter(tid_reset <= tid_ref)

## Velger å lagre datasettet med 10 sek før eg begynner med å gjøre gjennomsnitt på 30 sek

# Lager datostempel for å hindre overlagring av filene - kan ta vekk timer viss eg ikkje lagrer nye versjoner hyppig. 
stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")

# Lagrer det data-settet som RDS-fil for å beholde nøyaktig slik det var i R
valid_rds <- file.path (out_sav, paste0("10_sek_CPET_", stamp, ".rds"))
{saveRDS(cpet_valid, valid_rds)  # Denne kan åpnes i R, men er god for videre behandling. CSV-filen er bra for dokumentasjon og deling. 
  message("Skreiv RDS-fil:", valid_rds)}


## 2.2.4) Lagrer dei radene som er fjernet for 100% transparent-----
cpet_fjernet <- cpet_tid_ref %>% 
  filter(tph > tid_ref)

# ---Valgfritt---

# 2.3) Lagre dei fjernede radene i en CSV-fil
write.csv(cpet_fjernet,
          file.path(out_sav, "fjernede_rader_cpet.csv"),
          row.names = FALSE)

# ---- (3) Finne dei 30 sekundene der peak oksygenforbruk er høyest ----

# 3.1) Hente ut det rullerende halvminuttet der oksygenopptaket er størst

# En forutsetning for at det skal være et gyldig halvminutt er at det ikkje er nokon NA i målingene som blir med i utregningen

cpet_30sek <- cpet_valid %>% 
  group_by(eilo_id, phase) %>% 
  mutate(
    # Lager en ny kolonne som gjør en rullerende beregning av vo2kg,
    # og gir snittet for hvert vindu/halvminutt (3 påfølgende rader
    roll_mean3 = slide_dbl(
      vo2kg,
      
      # Funksjonen for selve utregningen:
       # Beregner rullerende gjennomsnitt av vo2kg over 3 påfølgende målinger,
       # men bare hvis ingen av de 3 er NA.
       # "~" = er det samme som function(x), ".x" = vinduet som slide jobber på.
      .f = ~ if (all(!is.na(.x))) mean(.x) else NA_real_,  
      .before = 2,        # Vindusdefinisjon: nåværende rad + 2 forrige = 3 rader totalt
      .complete = TRUE    # Bare beregn hvis vinduet er komplett, ellers NA
    ),
    end_rad = row_number(),         # Sluttraden i hvert vindu
    start_rad = end_rad - 2         # Startraden i vinduet (3 rader totalt)
  ) %>%  
  
  # group_modify gjør at funksjonen under kjøres separat for hver eilo_id × phase
  group_modify(~{
    g <- .x   # .x er gruppedataene (alle rader for én deltaker per test)
    
    # Finner radnumrene der roll_mean3 er gyldig (ikke NA)
    valid <- which(!is.na(g$roll_mean3))
    
    # Hvis ingen gyldige vinduer → returner tom tibble for denne gruppen
    if (length(valid) == 0) return(g[0, ])
    
    # Finn maksimal rullerende VO2/kg i denne testen
    best_val <- max(g$roll_mean3[valid])
    
    # Hent alle vinduer som deler denne maksimumsverdien
    cand <- valid[g$roll_mean3[valid] == best_val]
    
    # Hvis flere vinduer har samme maksimum → velg den siste (senest i testen)
    best_i <- max(cand)
    
    # Finn start og slutt for selve 30 sek-vinduet
    s <- g$start_rad[best_i]
    e <- g$end_rad[best_i]
    
    # Returnerer de 3 originalradene som utgjør topp-vinduet
    out <- g[s:e, , drop = FALSE]
    
    # Legger til info om hvor mange vinduer som hadde samme maksverdi
    out$n_ties <- length(cand)
    out$tied   <- out$n_ties > 1
    
    out   # Returner resultatet for denne gruppen
  }) %>% 
  
  ungroup()


## 3.1) Ordner det slik at pre kommer før post - gjør det enklere å lese----

cpet_30sek<-cpet_30sek %>% 
  mutate(phase = factor(phase, levels = c("t1", "t2", "t3"))) %>% 
  arrange(eilo_id, phase)

# ---- (4) Regne ut gjennomsnitt per deltaker og fase ----

## 4.1) Regner ut gjennomsnittet ----
cpet_gj <- cpet_30sek %>% 
  group_by(eilo_id, phase) %>% 
  summarise(
    across(vo2kg:vtin, ~ round(mean(.x, na.rm = TRUE), 1)),
    n_rows = n(),
    .groups = "drop"
  )

# ---- (5) Samle all data i en felles dataframe ----

## 5.1)----

cle_score <- readRDS(here("R", "cle_score", "cle_score_output", "cle_score_2025-11-19_21-14.rds"))

# 5.1.1) Rense datasettet til CLE-score
cle_renset <- cle_score %>% 
  rename(                                # Endrer navn på kolonnene for å matche dei andre data.frames - enklere å left_join da
    eilo_id = id,
    phase = tidspunkt
  ) %>% 
  select(eilo_id, phase, score_type, score) %>% 
  pivot_wider(
    names_from = score_type,
    values_from = score
  )

# 5.2) For at eg skal kunne koble df-ene sammen så må eilo_id og phase være lik. Siden det er en ID som er repetert to ganger er det blitt laget en suffix med "a" og "b"
# Dette er for å vise at dei er to ulike ID, og skal være det.
## Man må derfor sørge for at dei er like på tvers

str(metadata_raw)
str(cle_perf_long)
str(cpet_gj)

# 5.3) Legge til metadata og demografisk data
full_data <- cpet_gj %>% 
  left_join(metadata_raw %>% select(eilo_id, phase:barotrykk), by = c("eilo_id", "phase")) %>% 
  left_join(cle_perf_long %>% select(eilo_id, phase, cle_sub_a:distanse), by = c("eilo_id", "phase")) %>% 
  select(eilo_id, phase, kjonn:barotrykk, borg_score:distanse, cle_sub_a:cle_tot, everything(), -n_rows)

# 5.4) Lage en oversikt over miljøvariabler
miljo <- full_data %>% 
  group_by(phase) %>% 
  summarise(
    across(
      c(temperatur, luftfuktighet),
      list(
      mean = ~mean(.x, na.rm =TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE),
      min = ~min(.x, na_rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )
)

# ---- (6) Output ----

# 6.1) Lager datostempel for å hindre overlagring av filene - kan ta vekk timer viss eg ikkje lagrer nye versjoner hyppig. 
stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")

# 6.2) Lagrer det fulle-datasettet som CSV (Lesbar og delbar)
full_csv <- file.path(out_sav, paste0("fult_datasett_komplett_", stamp, ".csv")) # Hele filstien for hvor filen skal lagres
{write.csv(full_data, full_csv)                                            # Lagrer resultatet fra den ferdigprosesserte daten i dat til CSV-filen "CPET_master.csv"
  message("Skreiv full_datasett: ", full_csv)}  

# 6.3) Lagrer det fulle data-settet som RDS-fil for å beholde nøyaktig slik det var i R
full_rds <- file.path (out_sav, paste0("fullt_datasett_komplett_", stamp, ".rds"))
{saveRDS(full_data, full_rds)  # Denne kan åpnes i R, men er god for videre behandling. CSV-filen er bra for dokumentasjon og deling. 
  message("Skreiv RDS-fil:", full_rds)}
