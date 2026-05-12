# ---- Forslag til R-script for uttrekk ----

# ---- (1) Forberedelser ----

# 1.1) Pakker
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(readxl)
library(here)
library(tidyr)
library(lubridate)
library(janitor)

# 1.2) Velg kilde
grunnlag <- "endelig_data/xml_eksport"      # eksempler: "pilot/n30" | "endelig_data/xlm_output"

# 1.3) Inndata
data_dir <- here("CPET", grunnlag) # Avhenger hvor prosjektfilen ligger

# 1.4) Utdata: pilot -> R/pilotering_CPET/n#, ellers -> R/master_output
{if (grepl("^pilot/", grunnlag)) {
  out_dir <- here("R", "pilotering_CPET", basename(grunnlag))  # R/pilotering_CPET/n#
} else {
  out_dir <- here("R", "master_output")                         # fast mappe for endelig data
}
  
  message("Inndata: ", data_dir)
  message("Utdata : ", out_dir)
}

# ---- (2) Oppsett til innlesing av filer ----

# 2.1) Navnerydding
clean_names <- function(x) {
  x %>% 
    tolower() %>% 
    gsub("\\s+", "_", x = .) %>%          # erstatter alle mellomrom med _
    gsub("[^a-z0-9_]", "", x = .) %>%     # fjerner alt som ikke er bokstav (a-z), tall (0-9) eller _ 
    gsub("_+", "_", x = .) %>%            # gjør flere understreker etter hverandre om til bare en
    gsub("^_|_$", "", x = .)              # fjerner _ helt i starten eller helt på slutten
}

# 2.2) Identifisere om det er en ekte Excel-fil (XLS/XLSX.fil) og ikkje bare en tekstfil med.xls som filendelse

real_excel <- function(path) {   # Definerer en funksjon som tar for seg filstien til filen (kommer igjen senere når man leser                                  alle filene)
  
  ok <- TRUE                    # Antar at filen er en ekte Excel-fil
  
  tryCatch({excel_sheets(path); }, error = function(e) ok <<- FALSE)    # Prøver å lese arbeidsarkene med excel_sheets
  
  # Viss sann så betyr det at filen er en gyldig (enten .xls eller .xlsx)
  # Men viss excel_sheets feiler, så blir det feil i koden
  # Men fordi man bruker tryCatch så fanger koden opp feilen og går videre til
  # error = function(e).. som sier at man skal retunere den som FALSE
  
  ok                                                                    # Her returneres verdien av ok
}

# 2.3) Leser en fil (enten ekte Excel (XLS/XLSX) eller "tekstforkledd .xls)

# Definerer en funksjon som tar en parameter path - som er filsiten til filen som skal leses
read_sentry_file <- function(path) {  
  # Får bare filnavnet og ikkje hele filstien
  fn <- basename(path)
  
  # Leser filen
  if (real_excel(path)) {   # Sjekker først om filen oppfører seg som en ekte Excel fil gjennom funksjonen real_excel
    df <- read_excel(path)                  
    source_type <- "excel"    # Lager også en hjelpevariabel som sier om det det er excel for sporing seinere
  } else {                    
    # Viss fila ikke virker å være ekte Excel (tekstfil med .xls) prøver den å lese den som tekstfil
    # Filen prøves å leses på tre forskjellige måter: 

    # Forsøk 1: les som tab-separert (\t) - Om kolonnene er skilt med tegn som tab, semikolon eller komma
    try_tab <- try(
      read_delim(path, delim = "\t", escape_double = FALSE, 
                 trim_ws = TRUE,locale = locale(encoding = "UTF-8")),
      silent = TRUE
    )
    if (!inherits(try_tab, "try-error") && ncol(try_tab) > 1) {               
      # Sjekk 1: Var det ikkje en try-feil?
      # Sjekk 2: Ble det faktisk flere kolonner? (hvis bruk av feil separasjon, ender alt ofte i en kolonne)
      
      # Viss begge er ja, legg det inn under df
      df <- try_tab                                                           
      
      # Forsøk 2: Prøv semikolon (;)
    } else {
      try_semicolon <- try(
        read_delim(path, delim = ";", escape_double = FALSE, trim_ws = TRUE,
                   locale = locale(encoding = "UTF-8")),
        silent = TRUE
      )
      if (!inherits(try_semicolon, "try-error") && ncol(try_semicolon) > 1) {
        df <- try_semicolon
        
      # Forsøk 3: Om semikolon også feiler, så prøver man komma (,)
      } else {
        df <- read_delim(path, delim = ",", escape_double = FALSE, trim_ws = TRUE, 
        # Hvis også siste read_delim (komma) feiler, kastes feil – fanges opp i tryCatch() under map() senere         i scriptet og  logges i qc_read_errors.csv
                         locale = locale(encoding = "UTF-8"))
      }
    }
    # Markerer at den aktuelle filen ikkje var "ekte Excel", men tekstlig variant
    source_type <- "textlike_delim"                                           
  }
  
  # Normaliser kolonnenavn
  names(df) <- clean_names(names(df))  # Uansett hvilken tekst-deler som fungerte så standardiserer kolonnene etter clean_names
  
  # Henter ut EILO-ID og testnummer fra filnavnet
  m <- str_match(fn, "^([0-9]+[A-Za-z]?)_([0-9]+)\\.[Xx][Ll][Ss][Xx]?$") 
  # Lager en matrise, m, der:
    # Kolonne nr 1: hele filnavnet - 1234_1.xls
    # Kolonne nr 2: EILO-nr - 1234 | Fordi eg veit fra tidligere grovanalyse at det er et duplikat som vil bli markert med en liten bokstav på slutten
  # Kolonne nr 3: test-nummeret = 1 | 2 | 3
  eilo_id_chr <- if(!is.na(m[1,2])) m[1,2] else NA_character_
  test_number_chr <- if (!is.na(m[1,3])) m[1,3] else NA_character_
  test_number <- suppressWarnings(as.integer(test_number_chr))
  
  # Map testnum til fase
  phase <- recode(
    as.character(test_number),
    "1" = "t1",
    "2" = "t2",
    "3" = "t3",
    .default = NA_character_
  )
  # Returner en tibble med viktige informasjonsdata
  df <- as_tibble(df) %>%              # Lager df som en tibble, som er en ryddigere versjon av en data.frame
    mutate(
      kildefil = fn,                   # En kolonne som heter kildefil som tar for seg bare filnavnet
      source_type = source_type,       # En kolonne som viser hvordan fila blei lest: enten "Excel" eller                                            "textlike_delim"
      eilo_id = eilo_id_chr,           # En kolonne med EILO-ID
      test_number = test_number,       # En kolonne med kva test det er
      phase = phase                    # En kolonne som sier kva fase det er: t1 = test 1, t2 = test 2, t3 = test 3
    )
  
  # Små QC-varsler 
  if (is.na(eilo_id_chr) || is.na(test_number_chr)) {
    warning("Klarte ikkje å tolke eilo_id/test_number frå filnamn: ", fn)
  }
  if (!is.na(test_number_chr) && !test_number_chr %in% c("1","3")) {
    warning("Uventa test_number=", test_number_chr, " i fil: ", fn, " (forventa 1=pre, 3=post)")
  }
  
  df
}

# ---- (3) Les alle filer ----

# 3.1) Gir en vektor med filbaner

# Leter etter filer i mappa som eg har satt som data_dir
{files <- list.files(data_dir, pattern = "\\.xlsx?$", full.names = TRUE, ignore.case = TRUE) 
# pattern = "\\.xls$" - bare ta med filer som slutter på .xls
# full.names = TRUE - gir hele filstien
# ignore.case = TRUE - gjør søket case-sensitiv: tar med både .xls, .XLS eller .Xls osv..

# Om det er ingen filer som ble funnet stopper koden med en feilmelding
if (length(files) == 0L) {
  stop("Fann ingen .xls-filer i ", data_dir,
       ". Sjekk SentrySuite-eksporten (Del 2) og kjøyr skriptet på nytt.")
}
}

# 3.3) Leser inn hver fil i "files"-listen en og en

# Går gjennom alle elementene i "files"-listen og gjør det samme med hver av dem og lager en ny liste med resultatene
raw_list <- map(files, ~tryCatch(read_sentry_file(.x),          
                                 error = function(e) e))        
# Ved å bruke tryCatch så vil de filene det går fint med få en tibble med resultat
# mens viss det skjer en feil så vil ikkje koden stoppe, men lagre selve feilobjektet
# filen vil få en feilmelding inn i listen, mens resten leses som normalt


# 3.4) Logg eventuelle feil ved lesing av filer
{read_errors <- tibble(
  
  # Lager en kolonne "file" med filnavnene som ble forsøkt lest
  file = files,
  # Sjekker her for hver fil i "raw_list", og lagrer resultatet i kolonnen "ok"
  ok = !vapply(raw_list, inherits, logical(1), "error"),    
  # Hvis filen ble lest lest uten feil og blir TRUE (grunnet ! før vapply)
  # Hvis derimot, at filen feilet, og ikkje kan leses i read_sentry_file, da får man en feil og blir FALSE
  
  # Lages en kolonne som heter "msg", som sier hvliken feilmelding som oppstod, hvis filen ikkje kunne leses
  msg = vapply(raw_list, function(x) if (inherits(x, "error"))        
    conditionMessage(x) else NA_character_, character(1))             
  # Sjekker for hver fil i "raw_list" om det er en fil som ikke kunne leses (x = error)
  # Er dette tilfelle så returnerer funksjonen "function(x)" den faktisk feilmeldingen, 
  # Om x ikkje er "error" så blir det NA.
)
  
  # Viss det finnes minst en fil som ikkje blei lest korrekt
  if (any(!read_errors$ok)) {           
    
    # så lagre read_errors som en CSV-fil - qc_read_errors.csv
    write_csv(read_errors, file.path(out_dir, "qc_read_errors.csv"))    
    message("Nokre filer feila ved lesing. Sjå qc_read_errors.csv")     
    # Får også en beskjed i konsollen om noen filer feilet.
  }
}

# 3.5) Filtrer til berre dei som vart lese OK

# Plukker kun ut vellykkede innlesninger
dfs <- raw_list[vapply(raw_list,                                                         
                       function(x) inherits(x, c("tbl_df", "data.frame")), logical(1))]  
# altså dei som kunne leses (for dei er lagret enten som tibbles eller data.frame fra read_sentry_file)

# 3.6) Gi navn til lista basert på filnavnene
names(dfs) <- basename(files[vapply(raw_list, inherits, logical(1), "tbl_df")])

# 3.7) Finner alle dei unike kolonnenavna

# Henter alle kolonnenavnene fra alle filene gjennom lapply(dfs, names)
all_names <- Reduce(union, lapply(dfs, names)) 
# union() sørger for at det ikkje blir noen duplikater
# Reduce (union,...): Finner kolonnenavnene fra dei første to filene -> finner union
# Tar resultatet og finner union med kolonnenavnene fra neste fil
# Fortsetter til alle filer er tatt med - får dermed alle dei unike kolonnenavnene på tvers av alle filer
# Fungerer som en sikkerhetssjekk

# 3.8) Sørger for at hvert enkelt tibble(data.frame) har samme kolonne-oppsett og rekkefølge

dfs_aligned <- lapply(dfs, function(d) {  
  # lapply(dfs,..) kjører funksjonen på hver av tabellene (data.frames) i dfs, og lager en liste med resultatene
  # lapply() vil ta hver tabell en og en, kalle den "d", og kjøre funksjonen inni {...}
  # Finner dei kolonnene som mangler. all_names er "fasitlisten", mens kolonnen names(d) er kolonnene som finnes i den aktuelle filen som lese
  
  # I kolonnen missing er dei kolonnene som finnes i fasitlisten "all_names", men ikkje i filen som lese.
  missing <- setdiff(all_names, names(d)) 

  if (length(missing)) d[missing] <- NA   
  # Sjekker om det finnes noen kolonner fra fasitlisten "all_names" som ikkje finnes i filen som leses.
  # Hvis ja, opprettes dei kolonnene i d, og fyller hele kolonnen med NA. Da har "d" alle kolonnene som trengs, selv om noen består kun av NA
  
  # Sikrer at kolonnene kommer i nøyaktig samme rekkefølge som i all_names
  d[, all_names]                          
})

## Resultatet fra koden er en ny liste med like mange elementer som dfs, der hvert element er den justerte versjonen av orginaltabellen i dfs. 


# 3.9) Bind saman og behold info om kva for fil kvar rad kommer fra
dat <- bind_rows(dfs_aligned) # Slår sammen alle tibbles til en stor tibble

# 3.10) Standardiser kolonnenamn (igjen, i tilfelle)
names(dat) <- clean_names(names(dat))

# Sikrer også at eilo_id leses som bokstaver/character på grunn av suffixen som er i to ID: 2129a og 2129b
dat <- dat %>% mutate(eilo_id = as.character(eilo_id))

# 3.11.1) Lagrer metadata i en df
metadata <- dat %>%
  group_by(eilo_id) %>% 
  filter(str_detect(tph, "^(Kjønn|Kjonn|Alder|Starttid|Temperatur|Baro)\\b")) %>% 
  ungroup()

## 3.11.2) Rydder i kolonner for å få oversiktlig df
metadata <- metadata %>% 
  group_by(eilo_id, test_number, phase) %>% 
  summarise(
    kjonn = tid[str_detect(tph, "Kjønn|Kjonn")][1],
    alder = tid[str_detect(tph, "Alder")][1],
    hoyde = elev[str_detect(hastighet, "Høyde|Hoyde")][1],
    vekt = elev[str_detect(hastighet, "Vekt")][1],
    starttid = tid[str_detect(tph, "Starttid")][1],
    dato = elev[str_detect(hastighet, "Dato")][1],
    temperatur = tid[str_detect(tph, "Temperatur")][1],
    luftfuktighet = elev[str_detect(hastighet, "Luftfuktighet")][1],
    barotrykk = tid[str_detect(tph, "Baro")][1], 
    .groups = "drop"
  ) %>% 
  mutate(
    eilo_id = as.character(eilo_id),
    alder = as.numeric(str_remove_all(alder, "[^0-9,.]")),  # Fjerner alt som ikke er tall, punktum eller komma og gjør om til numerisk verdi- ^ = ikke
    hoyde = as.numeric(str_remove_all(hoyde, "[^0-9,.]")),
    vekt = as.numeric(str_remove_all(vekt, "[^0-9,.]")),
    temperatur = as.numeric(str_remove_all(temperatur,"[^0-9,.]")),
    luftfuktighet = as.numeric(str_remove_all(luftfuktighet, "[^0-9,.]")),
    barotrykk = as.numeric(str_remove_all(barotrykk, "[^0-9,.]"))
  )

# 3.11.3) Fjerner metadata fra SentrySuite + enhetsrad for hver fil
dat_no_meta <- dat %>%
  group_by(kildefil) %>%
  filter(!str_detect(tph, "^(Kjønn|Kjonn|Alder|Starttid|Temperatur|Baro|min)\\b")) %>%
  ungroup()

## 3.11.4) En enkel kvalitetssjekk for å se kva rader som er blitt fjernet

# Definer mønsteret (radene som skal bort)
pat <- "^(Kjønn|Kjonn|Alder|Starttid|Temperatur|Baro|min)\\b" 

removed <- dat %>% filter(str_detect(tph, pat))
kept    <- dat_no_meta  # samme som 'dat' uten metadata

# Antall fjernet per fil
qc_counts <- removed %>%
  group_by(kildefil) %>%
  summarise(
    n_removed = n(),
    examples = paste(head(tph, 3), collapse = " | "),  # viser typiske verdier
    .groups = "drop"
  )

# Totalt før / etter
qc_total <- tibble(
  n_before = nrow(dat),
  n_removed = nrow(removed),
  n_after  = nrow(kept)
)

print(qc_counts)
print(qc_total)

# Sikkerhetssjekk – verifiser at ingenting gjenstår
stopifnot(sum(str_detect(kept$tph, pat), na.rm = TRUE) == 0)

# ---- (4) Slå sammen CPET-data ----

# Det er en grense i SeS for hvor mange parametre man kan ha med i hver tabell.
# For å øke antall variabler som en del av kvalitetssjekk, minske manuell beregning og mulighet for å utforske andre variabler som man først ikkje hadde tenkt på
# ble det derfor gjort en endring med å legge til en ny tabell under den opprinnlige i eksporten.
# Utfordringen med dette blei at den nye tabellen la seg under den gamle og ble derfor lest inn som enkelt rader i data.framen og fikk ikkje unike kolonner.


# 4.1.1) Lager egen tabell for dei nye variablene som nå ikkje ligger med unike kolonner
dat_no_meta <- dat_no_meta %>% 
  mutate(
    tph = iconv(as.character(tph), from = "", to = "UTF-8", sub = "")
  )

tab2 <- dat_no_meta %>%
  
  # Grupperer på ID og test slik at man kan slå det sammen senere, og gjør det mulig å sørge for at man kan skille mellom dei ulike tabellene som nå ligger som 1
  group_by(eilo_id, test_number, phase) %>%       
  
  # Lager en hjelpekolonne 'header_row' som sier *hvilket radnummer* i gruppa
  mutate(header_row = which(str_detect(tolower(trimws(tph)),           
                                       "^t\\s*[-_]?\\s*ph$"))[1]) %>%
  
  # Beholder kun rader fra og med header-raden og nedover (altså tabell 2-området).
  filter(row_number() >= header_row) %>%      
  
  # Fjern hjelpekolonnen, den trengs ikke i utdata.
  select(-header_row) %>%  
  
  # Bruk første rad i gruppas gjenværende data som kolonnenavn
  group_modify(~{                                                     
    x <- row_to_names(.x, row_number = 1, remove_row = TRUE)
    names(x) <- make.names(names(x), unique = TRUE)
    names(x)[1] <- "tph"
    x %>% mutate(tph = as.character(tph))
  }) %>%
  ungroup()

# 4.1.2) # Renser kolonner som er konstante eller ikkje skal være med videre grunnet NA
tab2 <- tab2 %>% 
  
  # Dropper alle som heiter noko med .XLS, startar på NA og kolonnen som heter "textlike_delim".
  select(-matches("\\.XLS$"), -matches("^NA\\."), -textlike_delim)  

# 4.1.3) Standardiserer kolonnenamn 
names(tab2) <- clean_names(names(tab2))

# 4.2) Rydder i den opprinnlige eksporten for å gjøre klar til merge
tab1 <- dat_no_meta %>%
  group_by(eilo_id, test_number, phase) %>%
  mutate(header_row = which(str_detect(tolower(trimws(tph)), "^t\\s*[-_]?\\s*ph$"))[1]) %>%
  filter(row_number() < header_row) %>%
  ungroup() %>%
  select(-header_row)

# 4.3) Slår sammen tabellene til en stor cpet-tabell
cpet_full <- tab1 %>%
  left_join(tab2, by = c("eilo_id", "test_number", "phase", "tph", "tid"))

# 4.4) Rask sjekk om det har blitt gjort riktig:
count(tab1,eilo_id, test_number, phase) %>% head()
count(tab2,eilo_id, test_number, phase) %>% head()
count(cpet_full,eilo_id, test_number, phase) %>% head()

# ---- (5) Rydde i datasettet ----

# 5.1) Rydder rekkefølgen på kolonnene
cpet_full<- cpet_full %>% 
  
  # Flytter kolonnene som låg sist i datasettet først, for å gjøre det letter å orientere seg i datasettet
  select(eilo_id, test_number, phase, kildefil, source_type, everything()) 

# 5.2) Konverterer trygt til numeriske der det gir meining

## 5.2.1) Lager en liste med dei aktuell kolonnene
cols_num <- c(
  "hastighet", "elev", "vo2kg","hf","bf","vtex","ve","vo2","vco2",
  "tex", "ti","titot","o2puls","peto2","petco2","spo2","fico2","fio2","rer","br","vtexvc","vtinti","vtinstpd", "vtin")

## 5.2.2) Konverter disse til tall
cpet_full <- cpet_full %>%
  mutate(across(any_of(cols_num),
                ~ parse_number(.x, locale = locale(decimal_mark=".", grouping_mark=","), na = c("", "NA"))
  ))

## 5.2.3) Konverterer tidskolonnene til sekunder
cpet_full<- cpet_full %>% 
  mutate(across(c(tph, tid), ~ period_to_seconds(ms(.x))))

## 3.12.4) Sjekk om det er riktig
str(cpet_full)


# ---- (6) Output ----

# 6.1) Lager datostempel for å hindre overlagring av filene - kan ta vekk timer viss eg ikkje lagrer nye versjoner hyppig. 
stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")

# 6.2) Lagrer cpet-data som CSV (Lesbar og delbar)
master_path <- file.path(out_dir, paste0("CPET_full_data_", stamp, ".csv")) # Hele filstien for hvor filen skal lagres
{write_csv(cpet_full, master_path)                                            # Lagrer resultatet fra den ferdigprosesserte daten i dat til CSV-filen "CPET_master.csv"
  message("Skreiv masterfil: ", master_path)}                             # Gir en beskjed i konsollen når den er skrevet og lagret hvor

# 6.3) Lagrer cpet-data som RDS-fil for å beholde nøyaktig slik det var i R
master_rds <- file.path (out_dir, paste0("CPET_full_data_", stamp, ".rds"))
{saveRDS(cpet_full, master_rds)  # Denne kan åpnes i R, men er god for videre behandling. CSV-filen er bra for dokumentasjon og deling. 
  message("Skreiv RDS-fil:", master_rds)}

# 6.4) Lagrer metadata som RDS-fil for å gjøre det enkelt å ta med i videre analyse
meta_rds <- file.path(out_dir, paste0("meta_full_data_", stamp, ".rds"))
{saveRDS(metadata, meta_rds)
  message("Skreiv RDS-fil:", meta_rds)}

# 6.3) Oppsummering til konsoll
{cat("\n--- Oppsummering ---\n")
  cat("Filer lest: ", length(files), "\n")
  cat("Rader i master: ", nrow(cpet_full), "\n")
  cat("Kolonnar i master: ", ncol(cpet_full), "\n")
  cat("Master CSV: ", master_path, "\n")
  cat("QC-filer ligg i: ", out_dir, "\n")}
