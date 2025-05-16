# Library
library(tidyverse)

################################################################################
# 1: Last inn data


Sys.setenv(MYSQL_HOST = "localhost") # for mobilt kontor
Sys.setenv(MYSQL_DB_DATA = "noric_bergen")

noricHentTabell <- function(tabellnavn = "surgeonform") {
  registryName <- "data"
  dbType <- "mysql"
  query <- paste0("SELECT * FROM ", tabellnavn)
  
  tabell <- rapbase::loadRegData(registryName, query, dbType)
  return(tabell)
}




################################################################################
#--- ANDRE PROSEDYRER VARNUM---#
################################################################################
# 1: Last inn data: 
andre <- noricHentTabell("andreprosedyrervarnum")

# 2: Lag dataramme med mapping

# From andreprosedyrevarnum - APVN
# liste over de som ikke har blitt med pga spesielle felt:
# Regtype
# AndreProsOperatorer


APVN_varnavn_kobl <- 
  data.frame(
    kol = 
      c("other.MCEID AS ForlopsID",
        "other.CENTREID AS AvdRESH",
        "other.PROCEDUREDATE AS ProsedyreDato",
        "other.PROCEDUREDATE_TIME AS ProsedyreTid",
        "other.PROCEDUREDATE_TIME_MISSING AS ProsedyreTidUkjent",
        "other.PROCEDURETYPE AS AnnenProsType",
        "other.LABKOMP AS Komplikasjon",
        "other.LABALLERGILATT AS LettAllergi",
        "other.LABALLERGIALLV AS ModeratAllergi",
        "other.LABBEHARYTMI AS Arytmi",
        "other.LABHEMO AS HemodynKomp",
        "other.LABNEURO AS NeuroKomp",
        "other.LABVASK AS VaskulKomp",
        "other.LABPERF AS Perforasjon",
        "other.LABTAMP AS Tamponade",
        "other.LABAKUTCABG AS AkuttACB",
        "other.LABANNANALLV AS AnnenAlvorligKomp",
        "other.LABDODSFALL AS Dod",
        "other.LABPROCEDURDOD AS ProsRelatertDod",
        "other.STATUS AS SkjemaStatus")
    ) %>%  tidyr::separate(col="kol", 
                          into=c("dbnavn", "rapporteket"), 
                          sep = " AS ") %>%  
  tidyr::separate(col="dbnavn", 
                  into=c("tabell", "var_navn"), 
                  extra = "merge") %>%  
  dplyr::mutate(rapporteket = ifelse(is.na(rapporteket), 
                                     var_navn, rapporteket),
                tabell = dplyr::case_when(tabell == "P" ~ "PATIENT",
                                          tabell == "A" ~ "REGANGIO",
                                          tabell == "I" ~ "INITIALCARE",
                                          tabell == "IL" ~ "INITIALCARE_LAB",
                                          tabell == "F" ~ "FINDINGSTATIC",
                                          tabell == "C" ~ "ANGIOPCICOMP",
                                          tabell == "D" ~ "DISCHARGE",
                                          tabell == "other" ~ "OTHER",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  ) 


kodebok <- read.csv2(system.file("extdata/NORIC_klokeboken_18.03.2025.csv", 
                                 package = "noric")) %>%  
  dplyr::select(type, listeverdier, listetekst, tabell, 
                fysisk_feltnavn, variabel_id)


APVN_mangler_kodebok_katvar <- APVN_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(APVN_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="OTHER") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

## Sjekk om disse er viktige
#### Her er det tre - MCEID, CENTREID, PROCEDURE_TIME 
#### Disse leveres allerede i nummerformat og trengs ikke legges inn

APVN_map_num_tekst <- merge(kodebok,
                            APVN_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

# Sjekk om noen ikke har verdi
APVN_map_num_tekst_uVerdi <- APVN_map_num_tekst %>% 
  filter(is.na(verdi))

# Sjekk om noen av disse må legges inn 
### Her er det tre: Prosedyredato, ProsedyretidUkjent (dette er en avkrysningsboks) og SkjemaStatus
### Disse brukes allerede som tall og trenger ikke tekstverdi

APVN_map_num_tekst <- APVN_map_num_tekst %>% 
  filter(!is.na(verdi))
  
usethis::use_data(APVN_map_num_tekst, overwrite = TRUE)

# 3: Sjekk av mappinga stemmer - er det noen kolonner som ikke er med i koblinga som bør være med i koblinga (=> hvis det er tekstvariabler trenger de ikke være med)
#------------------------------------------------------------------------------#

# Alle variablene som ikke er med: 

# Regtype
# AndreProsOperatorer
# Prosedyredato
# Prosedyretidukjent
# Skjemastatus
# MCEID
# CENTREID
# Prosedyretid 


#------------------------------------------------------------------------------#
#### Sjekk at det fungerer med erstatt_koder_m_etiketter() 
#------------------------------------------------------------------------------#

# new <- erstatt_koder_m_etiketter(data, mapping = noric::APVN_map_num_tekst) #

################################################################################
#--- Annendiagnostikk varnum ---#
################################################################################

# Last inn data
annendiag <- noricHentTabell("annendiagnostikkvarnum")


# 2: Lag dataramme med mapping

ADVN_varnavn_kobl <- 
  data.frame(
    kol = 
      c("centre.ID AS AvdResh",
      "P.BIRTH_DATE AS FodselsDato",
      "diag.MCEID AS ForlopsID",
      "R.INDIKATION AS Indikasjon",
      "R.INTERDAT AS ProsedyreDato",
      "R.HEIGHT AS Hoyde",
      "R.WEIGHT AS Vekt",
      "diag.SEGMENT AS segment",
      "diag.GRAFT AS graft",
      "diag.METHODUSED AS metode",
      "diag.FFR_BEFORE AS FfrFoer",
      "diag.FFR_AFTER AS FfrEtter",
      "diag.IFR_BEFORE AS IfrFoer",
      "diag.IFR_AFTER AS IfrEtter",
      "diag.CSA_BEFORE AS CsaFoer",
      "diag.CSA_AFTER AS CsaEtter",
      "diag.MLD_BEFORE AS MldFoer",
      "diag.MLD_EAFTER AS MldEtter",
      "diag.MLA_BEFORE AS MlaFoer",
      "diag.MLA_AFTER AS MlaEtter",
      "diag.MXLCBI_BEFORE AS MxlcbiFoer",
      "diag.MXLCBI_AFTER AS MxlcbiEtter",
      "diag.CFR_BEFORE AS CfrFoer",
      "diag.CFR_AFTER AS CfrEtter",
      "diag.IMR_BEFORE AS ImrFoer",
      "diag.IMR_AFTER AS ImrEtter",
      "diag.PDPA_BEFORE AS PdpaFoer",
      "diag.PDPA_AFTER AS PdpaEtter",
      "diag.PAH_BEFORE AS PahFoer",
      "diag.PAH_AFTER AS PahEtter",
      "diag.PDH_BEFORE AS PdhFoer",
      "diag.PDH_AFTER AS PdhEtter")
  ) %>%  tidyr::separate(col="kol", 
                         into=c("dbnavn", "rapporteket"), 
                         sep = " AS ") %>%  
  tidyr::separate(col="dbnavn", 
                  into=c("tabell", "var_navn"), 
                  extra = "merge") %>%  
  dplyr::mutate(rapporteket = ifelse(is.na(rapporteket), 
                                     var_navn, rapporteket),
                tabell = dplyr::case_when(tabell == "P" ~ "PATIENT",
                                          tabell == "A" ~ "REGANGIO",
                                          tabell == "I" ~ "INITIALCARE",
                                          tabell == "IL" ~ "INITIALCARE_LAB",
                                          tabell == "F" ~ "FINDINGSTATIC",
                                          tabell == "C" ~ "ANGIOPCICOMP",
                                          tabell == "D" ~ "DISCHARGE",
                                          tabell == "other" ~ "OTHER",
                                          tabell == "diag" ~ "DIAG",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  )

# Lag dataramme med de som mangler i kodeboka 

ADVN_mangler_kodebok_katvar <- ADVN_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(ADVN_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="DIAG") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

# Sjekk de som mangler i kodeboka
### Her er det veldig mange... 
# Jeg sjekker hvilke i tabellen "diag" som ikke er i ADVN_varnavn_kobl

in_kodebok <- kodebok %>%
  filter(tabell == "diag") %>% 
  select(-c(listeverdier, listetekst)) %>% 
  unique()


in_kodebok_notSQL <- in_kodebok %>% 
  filter(!in_kodebok$variabel_id %in% ADVN_varnavn_kobl$variabel_id & type != "Tallvariabel")


# Det er kun 2 variabler som ikke er tallvariabler
# Disse finner jeg i tabellen "diag" i kodeboken, men de ser ut til å være utledete
# Jeg legger dem inn med nye navn slik at vi kan bruke kodeboken på dette 
# Dette gjelder diag.GRAFT -> diag.SEGMENT.GRAFT og diag.METHODUSED -> diag.SEGMENT.METHOD


# Runde 2 - oppdatere ADVN_varnavn_kobl
ADVN_varnavn_kobl2 <- 
  data.frame(
    kol = 
      c("centre.ID AS AvdResh",
        "P.BIRTH_DATE AS FodselsDato",
        "diag.MCEID AS ForlopsID",
        "R.INDIKATION AS Indikasjon",
        "R.INTERDAT AS ProsedyreDato",
        "R.HEIGHT AS Hoyde",
        "R.WEIGHT AS Vekt",
        "diag.SEGMENT AS segment",
        "diag.SEGMENT_GRAFT AS graft",
        "diag.SEGMENT_METHOD AS metode",
        "diag.FFR_BEFORE AS FfrFoer",
        "diag.FFR_AFTER AS FfrEtter",
        "diag.IFR_BEFORE AS IfrFoer",
        "diag.IFR_AFTER AS IfrEtter",
        "diag.CSA_BEFORE AS CsaFoer",
        "diag.CSA_AFTER AS CsaEtter",
        "diag.MLD_BEFORE AS MldFoer",
        "diag.MLD_EAFTER AS MldEtter",
        "diag.MLA_BEFORE AS MlaFoer",
        "diag.MLA_AFTER AS MlaEtter",
        "diag.MXLCBI_BEFORE AS MxlcbiFoer",
        "diag.MXLCBI_AFTER AS MxlcbiEtter",
        "diag.CFR_BEFORE AS CfrFoer",
        "diag.CFR_AFTER AS CfrEtter",
        "diag.IMR_BEFORE AS ImrFoer",
        "diag.IMR_AFTER AS ImrEtter",
        "diag.PDPA_BEFORE AS PdpaFoer",
        "diag.PDPA_AFTER AS PdpaEtter",
        "diag.PAH_BEFORE AS PahFoer",
        "diag.PAH_AFTER AS PahEtter",
        "diag.PDH_BEFORE AS PdhFoer",
        "diag.PDH_AFTER AS PdhEtter")
  ) %>%  tidyr::separate(col="kol", 
                         into=c("dbnavn", "rapporteket"), 
                         sep = " AS ") %>%  
  tidyr::separate(col="dbnavn", 
                  into=c("tabell", "var_navn"), 
                  extra = "merge") %>%  
  dplyr::mutate(rapporteket = ifelse(is.na(rapporteket), 
                                     var_navn, rapporteket),
                tabell = dplyr::case_when(tabell == "P" ~ "PATIENT",
                                          tabell == "A" ~ "REGANGIO",
                                          tabell == "I" ~ "INITIALCARE",
                                          tabell == "IL" ~ "INITIALCARE_LAB",
                                          tabell == "F" ~ "FINDINGSTATIC",
                                          tabell == "C" ~ "ANGIOPCICOMP",
                                          tabell == "D" ~ "DISCHARGE",
                                          tabell == "other" ~ "OTHER",
                                          tabell == "diag" ~ "DIAG",
                                          tabell == "R" ~ "REGANGIO",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  )

# Lager dataramme med de som mangler i kodeboka
ADVN_mangler_kodebok_katvar2 <- ADVN_varnavn_kobl2 %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(ADVN_varnavn_kobl2$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="DIAG") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

# Sjekker om det er noen som ikke er tallvariabler som er i kodeboka og ikke i ADVN_varnavn_kobl2
in_kodebok_notSQL2 <- in_kodebok %>% 
  filter(!in_kodebok$variabel_id %in% ADVN_varnavn_kobl2$variabel_id & type != "Tallvariabel")

# Nå er det ingen variabler som er listervariabler som ikke er med

# Merge ny ADVN_varnavn_kobl med kodeboka

ADVN_map_num_tekst <- merge(kodebok,
                            ADVN_varnavn_kobl2[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

# Sjekk om noen ikke har verdi

ADVN_map_num_tekst_uVerdi <- ADVN_map_num_tekst %>% 
  filter(is.na(verdi))

# Dette gjelder kun fødselsdato
# Jeg tar denne ut fra mappinga: 

ADVN_map_num_tekst <- ADVN_map_num_tekst %>% 
  filter(!is.na(verdi))

# Lagre dataen 
usethis::use_data(ADVN_map_num_tekst, overwrite = TRUE)

# 3: Sjekk av mappinga stemmer - er det noen kolonner som ikke er med i koblinga som bør være med i koblinga (=> hvis det er tekstvariabler trenger de ikke være med)
#------------------------------------------------------------------------------#

## Nei, alt i orden


#------------------------------------------------------------------------------#
#### Sjekk at det fungerer med erstatt_koder_m_etiketter() 
#------------------------------------------------------------------------------#

#new_diag <- erstatt_koder_m_etiketter(annendiag, mapping = noric::ADVN_map_num_tekst) #


################################################################################
#---      ctangio varnum     ---#
################################################################################


# 1: Last inn data
ctangio <- noricHentTabell("ctangiovarnum")

# 2: Lag dataramme med mapping

# Variabler som lages med en komplisert spørring i SQL

# Lokalsykehus -> leveres som tekst
# Studie -> leveres som tekst
# Granskere -> leveres som tekst
# FunnConcat -> leveres som tekst 

# Side disse leveres som tekst trenger vi ikke gjøre noe med disse
# Lagt inn F.SEGMENT som F.SEGMENT_LAYER for å få disse med 

CTANG_varnavn_kobl <- 
  data.frame(
    kol = 
      c("MCE.MCEID AS ForlopsID",
        "P.ID AS PasientID",
        "P.SSN_TYPE AS FnrType",
        "P.GENDER AS Kjonn",
        "P.BIRTH_DATE AS FodselsDato",
        "P.DECEASED AS Avdod",
        "P.DECEASED_DATE AS AvdodDato",
        "P.REGISTERED_DATE AS PasientRegDato",
        "CT.CENTREID AS AvdRESH",
        "CT.CTDAT AS UndersokDato",
        "CT.HEIGHT AS Hoyde",
        "CT.WEIGHT AS Vekt",
        "CT.SKREATININ AS SKreatinin",
        "CT.TIDPCI AS TidligerePCI",
        "CT.TIDCABG AS TidligereACB",
        "CT.DIABETES AS Diabetes",
        "CT.DIABETESINSULIN AS Insulin",
        "CT.TOBAK AS RoykingFoer2009",
        "CT.SMOKING_STATUS AS Royking",
        "CT.HYPERTON AS Hypertoni",
        "CT.HYPERLIP AS Lipidsenkende",
        "CT.TIDINF AS TidligereInfarkt",
        "CT.INDIKATION AS Indikasjon",
        "CT.VIKTIGSYMP AS ViktigsteSymptomer",
        "CT.TIDLIGSLUTT AS SluttForKontrast",
        "CT.HIGHCALCIUMSCORE AS HoyCalciumScore",
        "CT.TIDLIGSLUTTANNET AS SluttForKontrastAndreGrunner",
        "CT.CABG AS StenoseOkklusjonABC",
        "CT.CALCIUMSCORE AS CalciumScore",
        "CT.CORONARY_SURVEY AS KoronarundersokelseUtfort",
        "CT.KONKLUSIVUND AS Konklusiv",
        "CT.STENOS AS StenoseOkklusjonTidligere",
        "CT.FYND AS Funn",
        "F.SEGMENT1_LAYER AS SEGMENT1_LAYER",
        "F.SEGMENT2_LAYER AS SEGMENT2_LAYER",
        "F.SEGMENT3_LAYER AS SEGMENT3_LAYER",
        "F.SEGMENT4_LAYER AS SEGMENT4_LAYER",
        "F.SEGMENT5_LAYER AS SEGMENT5_LAYER",
        "F.SEGMENT6_LAYER AS SEGMENT6_LAYER",
        "F.SEGMENT7_LAYER AS SEGMENT7_LAYER",
        "F.SEGMENT8_LAYER AS SEGMENT8_LAYER",
        "F.SEGMENT9_LAYER AS SEGMENT9_LAYER",
        "F.SEGMENT10_LAYER AS SEGMENT10_LAYER",
        "F.SEGMENT11_LAYER AS SEGMENT11_LAYER",
        "F.SEGMENT12_LAYER AS SEGMENT12_LAYER",
        "F.SEGMENT13_LAYER AS SEGMENT13_LAYER",
        "F.SEGMENT14_LAYER AS SEGMENT14_LAYER",
        "F.SEGMENT15_LAYER AS SEGMENT15_LAYER",
        "F.SEGMENT16_LAYER AS SEGMENT16_LAYER",
        "F.SEGMENT17_LAYER AS SEGMENT17_LAYER",
        "F.SEGMENT18_LAYER AS SEGMENT18_LAYER",
        "F.SEGMENT19_LAYER AS SEGMENT19_LAYER",
        "F.SEGMENT20_LAYER AS SEGMENT20_LAYER",
        "CT.MYOCARDIAL_MASS AS Myokardmasse",
        "CT.CORONARY_ANOMALY AS KoronarAnomali",
        "CT.FUNCTIONAL_SURVEY AS FunksjonsUndersokelse",
        "CT.DIASTOLIC_VOLUME AS EndediastoliskVolum",
        "CT.END_SYSTOLIC_VOLUME AS EndesystoliskVolum",
        "CT.EJECTION_FRACTION AS EjeksjonsFraksjon",
        "CT.CARDIAC_OUTPUT AS HjerteminuttVolum",
        "CT.FURTHER_ASSESS_TREAT AS VidereUtredning",
        "CT.ADJUVANT AS CTAngioMedikamenter",
        "CT.ADJBETA AS Betablokkere",
        "CT.ADJNITRO AS Nitroglycerin",
        "CT.ADJANN AS AnnetAdjuvant",
        "CT.CTTEKNIK AS CTTeknikk",
        "CT.CTTEKNIK_SPECIFY AS ProspektivEKG",
        "CT.DLP AS DLP",
        "CT.KONTRASTMEDEL AS Kontrastmiddel",
        "CT.KONTRASTMEDELMANGD AS KontrastmiddelMengde",
        "CT.LABKOMP AS Komplikasjon",
        "CT.LABALLERGILATT AS LettAllergisk",
        "CT.LABALLERGIALLV AS AlvorligAllergisk",
        "CT.LABHEMO AS Hemodynamisk",
        "CT.STATUS AS SkjemaStatus")
  ) %>%  tidyr::separate(col="kol", 
                         into=c("dbnavn", "rapporteket"), 
                         sep = " AS ") %>%  
  tidyr::separate(col="dbnavn", 
                  into=c("tabell", "var_navn"), 
                  extra = "merge") %>%  
  dplyr::mutate(rapporteket = ifelse(is.na(rapporteket), 
                                     var_navn, rapporteket),
                tabell = dplyr::case_when(tabell == "P" ~ "PATIENT",
                                          tabell == "F" ~ "FINDINGSTATIC",
                                          tabell == "CT" ~ "CTANGIO",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  )

# Sjekke om noen mangler i kodeboka

CTANG_mangler_kodebok_katvar <- CTANG_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(CTANG_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="ctangio" | tabell = "findingstatic") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

# Ingen mangler

# Sjekker om det er noen som ikke er tallvariabler som er i kodeboka og ikke i ADVN_varnavn_kobl2

CTANG_in_kodebok <- kodebok %>%
  filter(tabell == "ctangio") %>% 
  select(-c(listeverdier, listetekst)) %>% 
  unique()

CTANG_in_kodebok_notSQL2 <- in_kodebok %>% 
  filter(!in_kodebok$variabel_id %in% CTANG_varnavn_kobl$variabel_id & type != "Tallvariabel")

# Det er noen, men ingen tas med: 
# STUDYPARTICIP -> jeg vet ikke hvordan denne evt. skal kodes
# STUDY_START_DATE -> dato
# CTANGIO_ANSVARIGGRANSKARE -> jeg vet ikke hvordan denne evt. skal kodes
# de andre er kommentarer (altså ikke listevariabler)

# Merge ADVN_varnavn_kobl med kodeboka

CTANG_map_num_tekst <- merge(kodebok,
                            CTANG_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

# Sjekk om noen ikke har verdi

CTANG_map_num_tekst_uVerdi <- CTANG_map_num_tekst %>% 
  filter(is.na(verdi))

# Dette gjelder 17 variabler 
# Disse ser ut som tallverdier
# Jeg tar disse ut fra mappinga: 

CTANG_map_num_tekst <- CTANG_map_num_tekst %>% 
  filter(!is.na(verdi))

usethis::use_data(CTANG_map_num_tekst, overwrite = TRUE)

# 3: Sjekk av mappinga stemmer - er det noen kolonner som ikke er med i koblinga som bør være med i koblinga (=> hvis det er tekstvariabler trenger de ikke være med)
#------------------------------------------------------------------------------#
# Det ser bra ut


#------------------------------------------------------------------------------#
#### Sjekk at det fungerer med erstatt_koder_m_etiketter() 
#------------------------------------------------------------------------------#

new <- erstatt_koder_m_etiketter(ctangio, mapping = noric::CTANG_map_num_tekst) #

################################################################################
#---      segmentstentnum    ---#
################################################################################

# ADVN -> annendiagnostikk varnum
segm <- noricHentTabell("segmentstentnum")

# 2: Lag dataramme med mapping

# Variabler som lages med en komplisert spørring i SQL

# Ingen


segm_varnavn_kobl <- 
  data.frame(
    kol = 
      c("S.ID AS SegmentID",
        "S.MCEID AS ForlopsID",
        "P.BIRTH_DATE AS FodselsDato",
        "R.INDIKATION  AS Indikasjon",
        "R.INTERDAT AS ProsedyreDato",
        "R.HEIGHT AS Hoyde",
        "R.WEIGHT AS Vekt",
        "S.SEGMENT AS Segment",
        "S.STENT AS StentID",
        "ST.STENTNAMN AS Stentnavn",
        "ST.DES  AS StentType",
        "S.BALLONGLANGD AS BallongLengde",
        "S.DEBDIAM AS DEBDiameter",
        "S.DIAM AS Diameter",
        "S.EFTERDILATATION  AS Etterdilatasjon",
        "S.FRAMGANG  AS LokalSuksess",
        "S.GRAFT  AS Graft",
        "S.LAKEMEDELSBALLONG  AS MedikamentellBallong",
        "S.MAXTRYCKVIDDEB AS MaksTrykkDEB",
        "S.OCKL  AS Okklusjon",
        "S.PROCTYP  AS ProsedyreType",
        "S.SEGMENT_STENT_TROMBOSE_TYPE  AS Stenttrombosetype",
        "S.STENOSKLASS  AS Stenoseklasse",
        "S.STENOSTYP  AS StenoseType",
        "S.STENTLANGD AS Stentlengde",
        "S.STENTSLUT  AS StentSlut",
        "S.UPPBLASNINGSTIDDEB AS InflasjonsTidDEB",
        "S.IVL_DIAM AS IVLDiameter",
        "S.NUMBERPULSES AS AntallPulser",
        "centre.ID AS AvdRESH"
        )
  ) %>%  tidyr::separate(col="kol", 
                         into=c("dbnavn", "rapporteket"), 
                         sep = " AS ") %>%  
  tidyr::separate(col="dbnavn", 
                  into=c("tabell", "var_navn"), 
                  extra = "merge") %>%  
  dplyr::mutate(rapporteket = ifelse(is.na(rapporteket), 
                                     var_navn, rapporteket),
                tabell = dplyr::case_when(tabell == "P" ~ "PATIENT",
                                          tabell == "S" ~ "SEGMENT",
                                          tabell == "ST" ~ "STENT",
                                          tabell == "M" ~ "MCE",
                                          tabell == "R" ~ "REGANGIO",
                                          tabell == "centre" ~ "CENTRE",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  )

# Sjekke om noen mangler i kodeboka

segm_mangler_kodebok_katvar <- segm_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(segm_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="SEGMENT" | tabell == "STENT") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

# 4 mangler: 
# ID -> skal leveres som tall
# MCEID -> skal leveres som tall
# STENTNAMN -> skal leveres som tekst
# STENT_DES -> jeg finner ikke ut hva dette er 
# Ingen av disse legges inn manuelt 

# Sjekker om det er noen som ikke er tallvariabler som er i kodeboka og ikke i ADVN_varnavn_kobl2

segm_in_kodebok <- kodebok %>%
  filter(tabell == "segment" | tabell == "stent") %>% 
  select(-c(listeverdier, listetekst)) %>% 
  unique()

segm_in_kodebok_notSQL2 <- segm_in_kodebok %>% 
  filter(!segm_in_kodebok$variabel_id %in% segm_varnavn_kobl$variabel_id & type != "Tallvariabel")

# Det er ikke noen

# Merge varnavn_kobl-fila med kodeboka

segm_map_num_tekst <- merge(kodebok,
                             segm_varnavn_kobl[, c("variabel_id", "rapporteket")],
                             by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

# Sjekk om noen ikke har verdi

segm_map_num_tekst_uVerdi <- segm_map_num_tekst %>% 
  filter(is.na(verdi))

# Dette gjelder 13 variabler 
# Disse ser ut som tallverdier eller tekstverdier
# Jeg tar disse ut fra mappinga: 

segm_map_num_tekst <- segm_map_num_tekst %>% 
  filter(!is.na(verdi))

usethis::use_data(segm_map_num_tekst, overwrite = TRUE)

# 3: Sjekk av mappinga stemmer - er det noen kolonner som ikke er med i koblinga som bør være med i koblinga (=> hvis det er tekstvariabler trenger de ikke være med)
# Jeg sjekker hvilke variabler fra varnavn-koblinga som ikke ble med i mappinga

segm_test <- segm_varnavn_kobl %>% 
  filter(!rapporteket %in% segm_map_num_tekst$variabel_id)

# Dette ser bra ut 

#------------------------------------------------------------------------------#
#### Sjekk at det fungerer med erstatt_koder_m_etiketter() 
#------------------------------------------------------------------------------#

segm <- erstatt_koder_m_etiketter(segm, mapping = noric::segm_map_num_tekst) #
#### Obs! stentType har ikke fått mapping siden denne variabelen ikke finnes i kodeboka!


################################################################################
#---      mitralklaffoppfvarnum    ---#
################################################################################

# ADVN -> annendiagnostikk varnum
mitr <- noricHentTabell("mitralklaffvarnum")

# 2: Lag dataramme med mapping

# Variabler som lages med en komplisert spørring i SQL
# Studie
# HovedOperator
# AndreOperator
# TredjeOperator 
# Operatorer
# TypeKlaffeprotese
# Skjemastatus


mitr_varnavn_kobl <- 
  data.frame(
    kol = 
      c("TF.MCEID AS ForlopsID",
        "TF.CENTREID AS AvdRESH",
        
        #-- Mitralklaff
        "T.SCREENING AS BasisScreeningBeslutning",
        "T.MITRAL_VALVE_TYPE AS Prosedyre",
        "T.PROCEDUREDATE AS ProsedyreDato",
        
        #-- Kliniske bakgrunnsdata
        "T.HEIGHT AS Hoyde",
        "T.HEIGHT_MISS AS HoydeUkjent",
        "T.WEIGHT AS Vekt",
        "T.WEIGHT_MISS AS VektUkjent",
        "T.SMOKING AS Royker",
        "T.S_CREATININ AS KreatininFoer",
        "T.S_CREATININ_MISS AS KreatininUkjent",
        "T.PROBNP AS ProBNP",
        "T.PROBNPNT AS NTProBNPFoer",
        "T.PROBNP_MISS AS ProBNPUkjent",
        "T.HEMOGLOBIN AS HemoglobinFoer", #-- Added in v1.13 as NOR-1345
        
        #-- Tidligere sykdommer/behandling
        "T.HYPERTENSION AS BehHypertoni",
        "T.DIABETES AS Diabetes",
        "T.DIABETESINSULIN AS Insulin",
        "T.ATRIAL_FIBRILLATION AS Atrieflimmer",
        "T.PREVIOUS_MI AS InfarktSiste90d",
        "T.HEARTFAILURE AS AntInnleggelserSisteAar",
        "T.HEARTFAILURE_MISS AS AntInnleggelserSisteAarUkjent",
        "T.PRIOR_CARDIAC_SURGERY AS TidlHjerteoperasjon",
        "T.PRIOR_CARDIAC_SURGERY_ACB AS TidlACB",
        "T.PRIOR_CARDIAC_SURGERY_AVR AS TidlAVR",
        "T.PRIOR_CARDIAC_SURGERY_MITRALPLASTIKK AS TidlMitralplastikk",
        "T.PRIOR_CARDIAC_SURGERY_MVR AS TidlMVR",
        "T.PRIOR_CARDIAC_SURGERY_OTHER AS TidlAnnet",
        "T.PRIORCORRECTION AS TidlKorrigering",
        "T.PREVIOUS_PCI AS TidlPCI",
        "T.PREVIOUS_STROKE AS TidlHjerneslag",
        "T.CHRONIC_PULMONARY_DISEASE AS KOLS",
        "T.PERIF_VESSELDISEASE AS PeriferKarsykdom",
        
        #-- Aktuell preoperativ status
        "T.NYHA AS NYHAKlasse",
        "T.FRAILTY AS Frailty",
        "T.NEUROLOGIC_DIS AS RedusertMobilitet",
        "T.WALKINGTEST AS Gangtest",
        "T.WALKINGTEST_MISS AS GangtestIkkeUtfort",
        "T.WALKINGTESTSPEED AS GangHastigtest",
        "T.WALKINGTESTSPEED_MISS AS GangHastigtestIkkeUtfort",
        "T.GRIPTEST AS Gripestyrke",
        "T.EURO2_DIALYSIS AS DialyseFoerOp",
        "T.KRITISKT AS KritiskPreopTilstand",
        "T.EURO2_URGENCY AS Hastegrad",
        
        #-- Kontraindikasjon mot kirurgi
        "T.COUNTERINDICATION AS Kontraindikasjon",
        "T.OTHERMORBREASON0 AS Porselenaorta",
        "T.OTHERMORBREASON1 AS Malignitet",
        "T.OTHERMORBREASON3 AS UgunstigAnatomi",
        "T.OTHERMORBREASON2 AS Steroidbehandling",
        "T.OTHERMORBREASON4 AS Stralebehandling",
        "T.OTHERMORBREASON5 AS Thoraxdeformitet",
        "T.AVBOJD AS AvslaattForThorax",
        "T.PRESENT_HEALTH_STAT AS Helsetilstand",
        
        #-- EKKO-funn før prosedyre
        "T.LEFT_VENTRICULAR_FUNCTION_EURO2 AS PreVenstreVentrikkelFunksjon",
        "T.AORTAINS AS PreAortainsuffisiens",
        "T.AORTASTENOS AS PreAortastenose",
        "T.TRICUSINSUFF AS PreTricuspidal",
        "T.MITRALISSTENOS AS PreMitralstenose",
        "T.MITRINSUFF AS PreMitralinsuffisiens",
        "T.MAXSPEED_MSEK AS PreMaxHastighet",
        "T.AVERAGEGRADIENT AS PreMiddelgradient",
        "T.MREROA AS PreMREROA",
        "T.MREROA_MISS AS PreMREROAUkjent",
        "T.PULMONELLHYPERTENSION AS PreHoyreVentTrykk",
        
        #-- Prosedyrevariabler
        "T.PUNCTIONTIME AS Punksjonstid",
        "T.ENDTIME AS Avslutningstid",
        "T.INTRODUCER AS IntroducerStr",
        "T.INTRODUCER_MISS AS IntroducerStrUkjent",
        "T.VESSELSEAL AS Karlukning",
        "T.ANESTHESIA AS Anestesi",
        "T.EKODURINGPROC AS ProsedyreEkko",
        "T.SUCCESSFULPROC AS VellykketProsedyre",
        
        #-- Stråledata og kontrast
        "T.LABNO AS Labnr",
        "T.BEAMDOSE AS Straaledose",
        "T.BEAMDOSE_MISS AS StraaledoseUkjent",
        "T.LIGHTTIME AS GjennomLysTid",
        "T.CONTRASTAGENT AS Kontrastmiddel",
        "T.CONTRASTAMOUNT AS Kontrastmengde",
        "T.CONTRASTAMOUNT_MISS AS KontrastmengdeUkjent",
        
        
        #-- Komplikasjoner på lab
        "T.LABKOMP AS LabKomplikasjon",
        "T.LABBEHARYTMI AS LabKompArytmi",
        "T.LABNEURO AS LabKompNeurologi",
        "T.LABTAMP AS LabKompTamponade",
        "T.LABPACEMAKER AS LabKompPacemaker",
        "T.LABVASCULAR AS LabKompVaskular",
        "T.LABBLEEDING AS LabKompBlodning",
        "T.LABSURGVESSEL AS LabKompAkuttKlaff",
        "T.LABSURGVASC AS LabKompAkuttVaskular",
        "T.LABANESTHESI AS LabKompAnestesi",
        "T.LABHLMACHINE AS LabKompHLMaskin",
        "T.LABEMBOLDEVICE AS LabKompEmbolisering",
        "T.LABOTHER AS LabKompAnnenKomp",
        "T.LABDECEASED AS LabKompDod",
        "P.DECEASED_DATE AS Dodsdato",
        
        
        #-- Komplikasjoner på avdelingen
        "TD.AVDCOMP AS AvdKomplikasjon",
        "TD.AVDSTROKE AS AvdKompHjerneslag",
        "TD.AVDSTROKE_DEGREE AS AvdKompHjerneslagGrad",
        "TD.AVDTIA AS AvdKompTIA",
        "TD.AVDTAMPONAD AS AvdKompTamponade",
        "TD.AVDPACEMAKER AS AvdKompPacemaker",
        "TD.AVDATRIALFIB AS AvdKompAtrieflimmer",
        "TD.AVDMI AS AvdKompHjerteinfarkt",
        "TD.AVDVASCULAR AS AvdKompVaskular",
        "TD.AVDBLEEDING AS AvdKompBlodning",
        "TD.AVDBLEEDING_DEGREE AS AvdKompBlodningGrad",
        "TD.AVDINFECTION AS AvdKompInfeksjon",
        "TD.AVDDIALYSIS AS AvdKompDialyse",
        "TD.AVDOTHER AS AvdKompAnnenKomp",
        "TD.AVDDECEASED AS AvdKompDod",
        
        #-- Postoperative EKKO-funn
        "TD.POSTPERFORMED AS PostUndersokelseForetatt",
        "TD.POSTQUALCLASS AS PostKlassifisering",
        "TD.POSTPROLAPSA1 AS PostProlapsA1",
        "TD.POSTPROLAPSA2 AS PostProlapsA2",
        "TD.POSTPROLAPSA3 AS PostProlapsA3",
        "TD.POSTPROLAPSP1 AS PostProlapsP1",
        "TD.POSTPROLAPSP2 AS PostProlapsP2",
        "TD.POSTPROLAPSP3 AS PostProlapsP3",
        "TD.POSTLEFT_VENTRICULAR_FUNCTION_EURO2 AS PostVenstreVentrikkelFunksjon",
        "TD.POSTTRICUS_INSUFF AS PostTricuspidal",
        "TD.POSTMITRALISSTENOS AS PostMitralstenose",
        "TD.POSTMITR_INSUFF AS PostMitralinsuffisiens",
        "TD.POSTMAXGRADIENT AS PostMaxgradient",
        "TD.POSTMAXSPEED_MSEK AS PostMaxHastighet",
        "TD.POSTAVERAGEGRADIENT AS PostMiddelgradient",
        "TD.POSTMRPISA AS PostMRPISA",
        "TD.POSTMRPISA_MISS AS PostMRPISAUkjent",
        "TD.POSTMREROA AS PostMREROA",
        "TD.POSTMREROA_MISS AS PostMREROAUkjent",
        "TD.POSTVCONTRACTA AS PostVContracta",
        "TD.POSTVCONTRACTA_MISS AS PostVContractaUkjent",
        "TD.POSTLUNGVENREVERSE AS PostReversFlow",
        "TD.POSTPULMONELLHYPERTENSION AS PostHoyreVentrikkelTrykk",
        "TD.POSTPULMONELLHYPERTENSION_MISS AS PostHoyreVentrikkelTrykkUkjent",
        
        
        #-- Utskrivelse
        "TD.CREATININMAX AS MaxKreatinin",
        "TD.CREATININMAX_MISS AS MaxKreatininUkjent",
        "TD.DISCHARGEDATE AS UtskrDato",
        "TD.DISCHARGETO AS UtskrevetTil",
        
        #-- Antikoagulantia og platehemmere ved utskrivelse
        "TD.ASA_DISCHARGE AS ASAVedUtskrivelse",
        "TD.ANTICOAGULANTS_DISCHARGE AS AntikoagulantiaVedUtskrivelse",
        "TD.OTHER_ANTIPLATELET_DISCHARGE AS AndrePlatehemmereVedUtskrivelse",
        
        "T.STATUS AS SkjemaStatusHovedskjema",
        "TD.STATUS AS SkjemaStatusKomplUtskr"
      )
  ) %>%  tidyr::separate(col="kol", 
                         into=c("dbnavn", "rapporteket"), 
                         sep = " AS ") %>%  
  tidyr::separate(col="dbnavn", 
                  into=c("tabell", "var_navn"), 
                  extra = "merge") %>%  
  dplyr::mutate(rapporteket = ifelse(is.na(rapporteket), 
                                     var_navn, rapporteket),
                tabell = dplyr::case_when(tabell == "mce" ~ "MCE",
                                          tabell == "P" ~ "PATIENT",
                                          tabell == "TD" ~ "TAVIMITRALISDISCHARGE",
                                          tabell == "T" ~ "TAVIMITRALIS",
                                          tabell == "C" ~ "CENTRE",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  )

# Sjekke om noen mangler i kodeboka

mitr_mangler_kodebok_katvar <- mitr_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(mitr_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="TAVIMITRALISDISCHARGE" | tabell == "TAVIMITRALIS") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

# 3 mangler: 
# PRESENT_HEALT_STAT ("Helsetilstand") -> numerisk variabel
# PUNCTIONTIME -> datovariabel
# ENDTIME -> datovariabel
# Ingen av disse legges inn manuelt 

# Sjekker om det er noen som ikke er tallvariabler som er i kodeboka og ikke i ADVN_varnavn_kobl2

mitr_in_kodebok <- kodebok %>%
  filter(tabell == "tavimitralis" | tabell == "tavimitralisdischarge") %>% 
  unique()

mitr_in_kodebok_notSQL2 <- mitr_in_kodebok %>% 
  filter(!mitr_in_kodebok$variabel_id %in% mitr_varnavn_kobl$variabel_id & type != "Tallvariabel")

# Det er flere her som er listevariabler
# Jeg legger dem DERIMOT ikke inn siden disse er "utkommentert" i sql-fila og 
# dermed ser ut som de ikke skal være med i spørringen

# Merge varnavn_kobl-fila med kodeboka

mitr_map_num_tekst <- merge(kodebok,
                            mitr_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

# Sjekk om noen ikke har verdi

mitr_map_num_tekst_uVerdi <- mitr_map_num_tekst %>% 
  filter(is.na(verdi))

# Dette gjelder 53 variabler 
# Disse ser ut som numeriske eller tekstlige variabler. Det er også noen som er av typen "avkrysningsboks"
# Jeg tar disse ut fra mappinga: 

mitr_map_num_tekst <- mitr_map_num_tekst %>% 
  filter(!is.na(verdi))

usethis::use_data(mitr_map_num_tekst, overwrite = TRUE)

# 3: Sjekk av mappinga stemmer - er det noen kolonner som ikke er med i koblinga som bør være med i koblinga (=> hvis det er tekstvariabler trenger de ikke være med)
# Jeg sjekker hvilke variabler fra varnavn-koblinga som ikke ble med i mappinga

mitr_test <- mitr_varnavn_kobl %>% 
  filter(!rapporteket %in% mitr_map_num_tekst$variabel_id)

# Dette ser bra ut 

#------------------------------------------------------------------------------#
#### Sjekk at det fungerer med erstatt_koder_m_etiketter() 
#------------------------------------------------------------------------------#

mitr <- erstatt_koder_m_etiketter(mitr, mapping = noric::mitr_map_num_tekst) #
# Dette ser rett ut 
# Jeg lurer litt på alle som har type "avkrysningsboks" og som dermed ikke blir med... 

################################################################################
#---      aortaklaffvarnum    ---#
################################################################################

# ADVN -> annendiagnostikk varnum
aort <- noricHentTabell("aortaklaffvarnum")

# 2: Lag dataramme med mapping

# Variabler som lages med en komplisert spørring i SQL
# Studie
# Operatorer - hoved osv (4 variabler)
# TypeKlaffeprotese
# Skjemastatus


aort_varnavn_kobl <- 
  data.frame(
    kol = 
      c("T.MCEID AS ForlopsID",
        "P.ID AS PasientID",
        "P.SSN_TYPE AS FnrType",
        "P.DECEASED AS AvdodFReg",
        "P.DECEASED_DATE AS DodsdatoFReg",
        "T.CENTREID AS AvdRESH",
        
        #-- Perkutane aortaklaffer
        "T.SCREENING AS ScreeningBeslutning",
        "T.COMPLETED_PROCEDURE AS Prosedyre",
        "T.INDICATION AS Indikasjon",
        "T.SCREENINGDATE AS BeslutningsDato",
        "T.PROCEDUREDATE AS ProsedyreDato",
        "T.AKUTOP AS AkuttOperasjon",
      
        
        #-- Kliniske bakgrunnsdata
        "T.HEIGHT AS Hoyde",
        "T.HEIGHT_MISS AS HoydeUkjent",
        "T.WEIGHT AS Vekt",
        "T.WEIGHT_MISS AS VektUkjent",
        "T.SMOKING AS Royker",
        "T.S_CREATININ AS SKreatininFoer",
        "T.S_CREATININ_MISS AS SKreatininIkkeUtfort",
        "T.PROBNP AS ProBNPFoer",
        "T.PROBNPNT AS NTProBNPFoer",
        "T.HEMOGLOBIN AS HemoglobinFoer", #-- Added in v1.13 as NOR-1345
        
        #-- Tidligere sykdommer/behandling
        "T.HYPERTENSION AS BehHypertoni",
        "T.DIABETES AS Diabetes",
        "T.DIABETESINSULIN AS Insulin",
        "T.ATRIAL_FIBRILLATION AS Atrieflimmer",
        "T.PREVIOUS_MI AS InfarktSiste90d",
        "T.PRIOR_CARDIAC_SURGERY AS TidlHjerteoperasjon",
        "T.PRIOR_CARDIAC_SURGERY_ACB AS TidlACB",
        "T.PRIOR_CARDIAC_SURGERY_AVR AS TidlAVR",
        "T.PRIOR_CARDIAC_SURGERY_MITRALPLASTIKK AS TidlMitralplastikk",
        "T.PRIOR_CARDIAC_SURGERY_MVR AS TidlMVR",
        "T.PRIOR_CARDIAC_SURGERY_OTHER AS TidlAnnet",
        "T.PREVIOUS_PCI AS TidlPCI",
        "T.PREVIOUS_STROKE AS TidlHjerneslag",
        "T.PACEMACER_IMPLANT AS Pacemaker",
        "T.CHRONIC_PULMONARY_DISEASE AS KOLS",
        "T.PERIF_VESSELDISEASE AS PeriferKarsykdom",
        "T.OTHER_SERIOUS_ILLNESS AS AnnenAlvorligSykdom",
        
        
        #-- Aktuell preoperativ status
        "T.NYHA AS NYHAKlasse",
        "T.CANADIAN AS CanadianClass",
        "T.FRAILTY AS Frailty",
        "T.NEUROLOGIC_DIS AS RedusertMobilitet",
        "T.WALKINGTEST AS Gangtest",
        "T.WALKINGTEST_MISS AS GangtestIkkeUtfort",
        "T.WALKINGTESTSPEED AS GangHastigtest",
        "T.WALKINGTESTSPEED_MISS AS GangHastigtestIkkeUtfort",
        "T.GRIPTEST AS Gripestyrke",
        "T.EURO2_DIALYSIS AS DialyseFoerOp",
        "T.KRITISKT AS KritiskPreopTilstand",
        "T.EURO2_URGENCY AS Hastegrad",
        
        #-- Kontraindikasjon mot kirurgi
        "T.PERC_VALVE_DUE_TO_RISK AS PerkKlaffPgaRisiko",
        "T.PERC_VALVE_RISK_AGE AS PerkKlaffPgaRisikoAlder",
        "T.PERC_VALVE_RISK_SPIRO AS PerkKlaffPgaRisikoSpiro",
        "T.PERC_VALVE_RISK_FORMER_ACB AS PerkKlaffPgaRisikoACB",
        "T.COUNTERINDICATION AS PerkKlaffPgaRisikoSpesiell",
        "T.OTHERMORBREASON0 AS Porselenaorta",
        "T.OTHERMORBREASON1 AS Malignitet",
        "T.OTHERMORBREASON3 AS UgunstigAnatomi",
        "T.OTHERMORBREASON2 AS Steroidbehandling",
        "T.OTHERMORBREASON4 AS Stralebehandling",
        "T.OTHERMORBREASON5 AS Thoraxdeformitet",
        "T.OTHERMORB AS AnnenAlvorligSykdomKirRisiko",
        "T.PERC_VALVE_DUE_TO_PATIENT AS PerkKlaffPgaPasient",
        "T.PRESENT_HEALTH_STAT AS Helsetilstand",
        
        #-- EKKO-funn før prosedyre
        "T.VESSELAREA AS Klaffeareal",
        "T.VESSELAREA_MISS AS KlaffearealIkkeBeregnet",
        "T.ADJUSTED_VESSELAREA AS KorrigertKlaffeareal",
        "T.MAXGRADIENT AS PreMaxgradient",
        "T.MAXGRADIENT_MSEK AS PreMaxHastighet",
        "T.MAXGRADIENT_MSEK_MISS AS PreMaxHastMangler",
        "T.AVERAGEGRADIENT AS PreMiddelgradient",
        "T.AVERAGEGRADIENT_MISS AS PreMiddelgradientMangler",
        "T.LEFT_VENTRICULAR_FUNCTION AS PreVenstreVentrikkelFunksjon2011",
        "T.LEFT_VENTRICULAR_FUNCTION_EURO2 AS PreVenstreVentrikkelFunksjon",
        "T.PULMHYPERTENSION AS PulmonalHypertensjon",
        "T.AORTAINS AS PreAortainsuffisiens",
        "T.MITRALISINS AS PreMitralisinsuffisiens",
        
        #-- CT-funn
        "T.CTPERFORMED AS ErCTForetatt",
        "T.OSTIEDIAM AS AnnulusdiameterVedCT",
        "T.ANNULUSAORTA AS AnnulusAreal",
        "T.ANNULUSAORTA_MM AS AnnulusArealmm",
        "T.PERIMETER AS AnnulusPerimeter",
        "T.AORTA_CALCIFICATION AS Aortaforkalk",
        "T.ASCENDING_AORTA_CALCIFICATION AS AortaKalkAscendens",
        "T.TYPEOF_FLAP AS TypeKlaff",
        
        #-- Prosedyrevariabler
        "T.PUNCTIONTIME AS Punksjonstid",
        "T.ENDTIME AS Avslutningstid",
        "T.MITRALISINS AS Mitralisinsuffisiens",
        "T.PROCEDURETYPE AS OperativTilgang",
        "T.VESSELINVESSEL AS KlaffIKlaff",
        "T.PROSTHESIS_SIZE AS ProteseStr",
        "T.PREDILATATION AS Predilatasjon",
        "T.BALLOONSIZEPRE AS PredilBallongStr",
        "T.AFTERDILATATION AS Postdilatasjon",
        "T.BALLOONSIZEPOST AS PostdilBallongStr",
        "T.PROTECTIONDEVICE AS ProtectionDevice",
        "T.SEALING AS Karlukning",
        "T.RAPIDPACING AS RapidPacing",
        "T.ANESTHESIA AS Anestesi",
        "T.SUCCESSFULPROC AS VellykketProsedyre",
        
        #-- Stråledata og kontrast
        "T.LABNO AS Labnr",
        "T.BEAMDOSE AS Straaledose",
        "T.LIGHTTIME AS GjennomLysTid",
        "T.CONTRASTAGENT AS Kontrastmiddel",
        "T.CONTRASTAMOUNT AS Kontrastmengde",
        
        #-- Komplikasjoner på lab
        "T.LABKOMP AS LabKomplikasjon",
        "T.LABBEHARYTMI AS LabKompArytmi",
        "T.LABNEURO AS LabKompNeurologi",
        "T.LABTAMP AS LabKompTamponade",
        "T.LABVASCULAR AS LabKompVaskular",
        "T.LABBLEEDING AS LabKompBlodning",
        "T.LABSURGVESSEL AS LabKompAkuttKlaff",
        "T.LABSURGVASC AS LabKompAkuttVaskular",
        "T.LABCORONAR AS LabKompOkklusjon",
        "T.LABANESTHESI AS LabKompAnestesi",
        "T.LABHLMACHINE AS LabKompHLMaskin",
        "T.LABKLAFF AS LabKompProtese",
        "T.LABEMBOLKLAFF AS LabKompEmboli",
        "T.LABOTHER AS LabKompAnnenKomp",
        "T.LABDECEASED AS LabKompDod",
        "T.LAB_DECEASED_DATE AS LabKompDodsdato",
        
        #-- Komplikasjoner på avdelingen
        "TD.AVDCOMP AS AvdKomplikasjon",
        "TD.AVDSTROKE AS AvdKompHjerneslag",
        "TD.AVDSTROKE_DEGREE AS AvdKompHjerneslagGrad",
        "TD.AVDTIA AS AvdKompTIA",
        "TD.AVDTAMPONAD AS AvdKompTamponade",
        "TD.AVDPACEMAKER AS AvdKompPacemaker",
        "TD.AVDATRIALFIB AS AvdKompAtrieflimmer",
        "TD.AVDMI AS AvdKompHjerteinfarkt",
        "TD.AVDVASCULAR AS AvdKompVaskular",
        "TD.AVDBLEEDING AS AvdKompBlodning",
        "TD.AVDBLEEDING_DEGREE AS AvdKompBlodningGrad",
        "TD.AVDINFECTION AS AvdKompInfeksjon",
        "TD.AVDDIALYSIS AS AvdKompDialyse",
        "TD.AVDOTHER AS AvdKompAnnenKomp",
        "TD.AVDDECEASED AS AvdKompDod",
        "TD.AVD_DECEASED_DATE AS AvdKompDodsdato",
        
        #-- Postoperative EKKO-funn
        "TD.POSTPERFORMED AS PostUndersokelseForetatt",
        "TD.POSTMAXGRADIENT AS PostMaxgradient",
        "TD.POSTMAXGRADIENT_MSEK AS PostMaxHastighet",
        "TD.POSTAVERAGEGRADIENT AS PostMiddelgradient",
        "TD.POSTAVERAGEGRADIENT_MISS AS PostMiddelgradientMangler",
        "TD.POSTLEFT_VENTRICULAR_FUNCTION AS PostVenstreVentrikkelFunksjon",
        "TD.POSTPULMHYPERTENSION AS PostPulmonalHypertensjon",
        "TD.VALVULAR_POSTAORTAINS AS PostAortainsuffisiens",
        "TD.PARAVALVULAR_LEAK AS ParavalvularLekkasje",
        "TD.PARAVALVULAR_INSUFFICIENCY AS ParavalvularInsuffisiens",
        "TD.POSTMITRALISINS AS PostMitralisinsuffisiens",
        
        #-- Utskrivelse
        "TD.CREATININMAX AS UtskrKreatinin",
        "TD.CREATININMAX_MISS AS UtskrKreatininIkkeUtfort",
        "TD.DISCHARGEDATE AS UtskrDato",
        "TD.DISCHARGETO AS UtskrevetTil",
        
        #-- Antikoagulantia og platehemmere ved utskrivelse
        "TD.ASA_DISCHARGE AS ASAVedUtskrivelse",
        "TD.ANTICOAGULANTS_DISCHARGE AS AntikoagulantiaVedUtskrivelse",
        "TD.OTHER_ANTIPLATELET_DISCHARGE AS AndrePlatehemmereVedUtskrivelse",
        
        "T.STATUS AS SkjemaStatusHovedskjema",
        "TD.STATUS AS SkjemaStatusKomplUtsk"
      )
  ) %>%  tidyr::separate(col="kol", 
                         into=c("dbnavn", "rapporteket"), 
                         sep = " AS ") %>%  
  tidyr::separate(col="dbnavn", 
                  into=c("tabell", "var_navn"), 
                  extra = "merge") %>%  
  dplyr::mutate(rapporteket = ifelse(is.na(rapporteket), 
                                     var_navn, rapporteket),
                tabell = dplyr::case_when(tabell == "mce" ~ "MCE",
                                          tabell == "P" ~ "PATIENT",
                                          tabell == "TD" ~ "TAVIDISCHARGE",
                                          tabell == "T" ~ "TAVIPERC",
                                          tabell == "C" ~ "CENTRE",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  )

# Sjekke om noen mangler i kodeboka

aort_mangler_kodebok_katvar <- aort_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(aort_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="TAVIDISCHARGE" | tabell == "TAVIPERC") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

# 7 mangler: 
# MCEID -> numerisk 
# CENTREID -> numerisk
# PRESENT_HEALT_STAT ("Helsetilstand") -> numerisk variabel
# PUNCTIONTIME -> datovariabel
# ENDTIME -> datovariabel
# BALLOONSIZE PRE og POST -> tallvariabel
# Ingen av disse legges inn manuelt 

# Sjekker om det er noen som ikke er tallvariabler som er i kodeboka og ikke i aort_varnavn_kobl2
###############LEGG DISSE INN!
aort_in_kodebok <- kodebok %>%
  filter(tabell == "tavidischarge" | tabell == "taviperc") %>% 
  unique()

aort_in_kodebok_notSQL <- aort_in_kodebok %>% 
  filter(!aort_in_kodebok$variabel_id %in% aort_varnavn_kobl$variabel_id & type != "Tallvariabel")

# Det er flere her som er listevariabler og 4 som ser ut som de bør legges inn





# Merge varnavn_kobl-fila med kodeboka

mitr_map_num_tekst <- merge(kodebok,
                            mitr_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

# Sjekk om noen ikke har verdi

mitr_map_num_tekst_uVerdi <- mitr_map_num_tekst %>% 
  filter(is.na(verdi))

# Dette gjelder 53 variabler 
# Disse ser ut som numeriske eller tekstlige variabler. Det er også noen som er av typen "avkrysningsboks"
# Jeg tar disse ut fra mappinga: 

mitr_map_num_tekst <- mitr_map_num_tekst %>% 
  filter(!is.na(verdi))

usethis::use_data(mitr_map_num_tekst, overwrite = TRUE)

# 3: Sjekk av mappinga stemmer - er det noen kolonner som ikke er med i koblinga som bør være med i koblinga (=> hvis det er tekstvariabler trenger de ikke være med)
# Jeg sjekker hvilke variabler fra varnavn-koblinga som ikke ble med i mappinga

mitr_test <- mitr_varnavn_kobl %>% 
  filter(!rapporteket %in% mitr_map_num_tekst$variabel_id)

# Dette ser bra ut 

#------------------------------------------------------------------------------#
#### Sjekk at det fungerer med erstatt_koder_m_etiketter() 
#------------------------------------------------------------------------------#

mitr <- erstatt_koder_m_etiketter(mitr, mapping = noric::mitr_map_num_tekst) #
# Dette ser rett ut 
# Jeg lurer litt på alle som har type "avkrysningsboks" og som dermed ikke blir med... 





























#################### evt mitralklaffoppfvarnum #################################









"TF.FOLLOWUPDATE AS OppfDato",
"TF.FOLLOWUP_TYPE AS OppfType",
"TF.DECEASED AS OppfAvdod",
"TF.DECEASEDDATE AS OppfAvdodDato",
"TF.PROC_RELATED_DEATH AS OppfDodProsRelatert",

#-- Oppfølging
"TF.HEIGHT AS Hoyde",
"TF.HEIGHT_MISS AS HoydeUkjent",
"TF.WEIGHT AS Vekt",
"TF.WEIGHT_MISS AS VektUkjent",
"TF.NYHA AS NYHA",
"TF.WALKINGTEST AS Gangtest",
"TF.WALKINGTEST_MISS AS GangtestIkkeUtfort",
"T.WALKINGTESTSPEED AS GangHastigtest",
"T.WALKINGTESTSPEED_MISS AS GangHastigtestIkkeUtfort",
"TF.S_CREATININ AS Kreatinin",
"TF.S_CREATININ_MISS AS KreatininUkjent",
"TF.PROBNP AS ProBNP",
"TF.PROBNPNT AS NTProBNP",

#-- EKKO-funn
"TF.PROLAPSA1 AS ProlapsA1",
"TF.PROLAPSA2 AS ProlapsA2",
"TF.PROLAPSA3 AS ProlapsA3",
"TF.PROLAPSP1 AS ProlapsP1",
"TF.PROLAPSP2 AS ProlapsP2",
"TF.PROLAPSP3 AS ProlapsP3",
"TF.LEFT_VENTRICULAR_FUNCTION AS VenstreVentrikkelFunksjon",
"TF.AORTAINS AS Aortainsuffisiens",
"TF.AORTAINS AS Aortastenose",
"TF.TRICUSINS AS Tricuspidal",
"TF.MITRALISINS AS Mitralinsuffisiens",
"TF.MITRALISSTENOS AS Mitralstenose",
"TF.MAXVELOCITY AS MaxHastighet",
"TF.AVGGRADIENT_MSEK AS MiddelgradientMPS",
"TF.AVERAGEGRADIENT AS Middelgradient",
"TF.MRPISA AS MRPISA",
"TF.MREROA AS MREROA",
"TF.VCONTRACTA AS VContracta",
"TF.LUNGVENSREVERS AS FlowRevers",
"TF.PULMHYPERTENSION AS HoyreVentrikkelTrykk",
"TF.PULMHYPERTENSION_MISS AS HoyreVentrikkelTrykkUkjent",
"TF.PRESENT_HEALTH_STAT AS Helsetilstand",

#-- Komplikasjoner
"TF.AVDCOMP AS Komplikasjoner",
"TF.AVDSTROKE AS Hjerneslag",
"TF.AVDTIA AS TIA",
"TF.AVDTAMPONAD AS Tamponade",
"TF.AVDPACEMAKER AS Pacemaker",
"TF.AVDATRIALFIB AS Atrieflimmer",
"TF.AVDMI AS Hjerteinfarkt",
"TF.AVDVASCULAR AS Vaskular",
"TF.AVDBLEEDING AS Blodning",
"TF.AVDINFECTION AS Infeksjon",
"TF.AVDDIALYSIS AS Dialyse",
"TF.AVDDEVICE AS DeviceRelKomp",
"TF.AVDOTHER AS AnnenKomp",
"TF.STATUS AS SkjemaStatus"