#### Dette er et script som lagrer en rekke datasett når pakken bygges.

### Scriptet lager først en mapping mellom rapporteketnavn og databasenavn"
### Deretter sjekkes det om noen av variablene mangler i kodeboka eller om 
### noen av variablene finnes i kodeboka og ikke i databasen. Hvis det er 
### noen som mangler legges disse inn manuelt.
### Deretter kobles kodeboka og mappingen slik at alle variabler som skal 
### mappes får verdien som oppgitt i kodeboka. 
### Tilslutt lagres et datasett med "usethis" som har informasjon om 
### Rapporteketnavnene, verdiene og verditekstene. Disse datasettene har navn
### _map_num_tekst og inneholder kun mapping og ikke data. 
### Datasettene gjør det mulig å bruke funksjonen erstatt_koder_m_etiketter()
### for å erstatte listevariabler som 1, 0 med "ja" og "nei".


# Laste inn kodeboka (ligger i pakken)
kodebok <- read.csv2(system.file("extdata/NORIC_klokeboken_18.03.2025.csv", 
                                 package = "noric")) %>%  
  dplyr::select(type, listeverdier, listetekst, tabell, 
                fysisk_feltnavn, variabel_id)
 

# Lage datafiler ---------------------------------------------------------------
# pr varnum-tabell med navn i rapporteket og listeverdier i tekst og num


## ANGIOPCI NUM ----------------------------------------------------------------



# 1: Lag mapping 

angp_varnavn_kobl <- 
  data.frame(
    kol = 
      c("MCE.MCEID AS ForlopsID",
        "MCE.PARENT_MCEID AS PrimaerForlopsID",
        "P.ID AS PasientID",
        "P.SSN_TYPE AS FnrType",
        "A.CENTREID AS AvdRESH",
        "A.INTERDAT AS ProsedyreDato",
        "A.INTERDAT_TIME AS ProsedyreTid",
        "A.INTERDAT_TIME_MISSING AS ProsedyreTidUkjent",
        "A.REGTYP AS ProsedyreType",
        "MCE.ACUTE_ELECTIVE AS Hastegrad",
        "P.GENDER AS Kjonn",
        "P.BIRTH_DATE AS FodselsDato",
        "P.DECEASED  AS AvdodFReg",
        "P.DECEASED_DATE AS AvdodDato",
        "P.REGISTERED_DATE AS PasientRegDato",
        "A.SYMPTOM_ONSET_DATE AS SymptomDato",
        "A.SYMPTOM_ONSET_TIME AS SymptomTid",
        "A.PREHOSPITAL_ECG_DATE AS BesUtlEKGDato",
        "A.PREHOSPITAL_ECG_TIME AS BesUtlEKGTid",
        "A.PREHOSPITAL_ECG_TIME_MISSING AS BesUtlEKGTidUkjent",
        "A.JOURTID AS Vakttid",
        "A.HEIGHT AS Hoyde",
        "A.WEIGHT AS Vekt",
        "A.SKREATININ AS SKreatinin",
        "A.TIDPCI  AS TidlPCI",
        "A.TIDCABG  AS TidlABC",
        "A.SMOKING_STATUS  AS RoykeStatus",
        "A.HYPERTON  AS BehHypertoni",
        "A.STATINS AS Statiner",
        "A.TIDINF  AS TidlInfarkt",
        "A.HISTORY_OF_CHF  AS KjentNedsattVenstVentr",
        "A.DIABETES  AS Diabetes",
        "A.DIABETESINSULIN AS Insulin",
        "A.PREVIOUS_STROKE AS TidligereSlag",
        "A.PERIPHERAL_VASCULAR_DISEASE  AS PeriferKarsykdom",
        "A.INDIKATION  AS Indikasjon",
        "A.MYOKARD  AS Myokardskademarkor",
        "A.STSEGSANK  AS STSegmentSenkning",
        "A.STAGED_PROCEDURE  AS StegvisProsedyre",
        "A.CSS AS CanadianClass",
        "A.NYHA AS NYHA",
        "A.CARD AS KardiogentSjokk",
        "A.KILLIPKLASS AS KillipKlasse",
        "A.FYND AS Funn",
        "A.ADMISSION_ER AS AnkomstPCIDato",
        "A.ADMISSION_ER_TIME AS AnkomstPCITid",
        "A.ADMISSION_ER_TIME_MISSING AS AnkomstPCITidUkjent",
        "A.PUNKT AS Stikksted",
        "A.STENOS AS StenoseTidlBehSegment",
        "A.CABG AS StenoseACBGraft",
        "A.STRESSKARDIOMYOPATI  AS Stresskardiomyopati",
        "A.PRIMBES AS PrimarBeslutning",
        "A.DIAGNOSTIK AS AnnenDiagHovedSpm",
        "A.DIATRYCK AS FFR",
        "A.DIADOP AS Doppler",
        "A.DIAIVUS AS IVUS",
        "A.DIAOCT AS OCT",
        "A.DIAIFR AS IFR",
        "A.DIANIRS AS NIRS",
        "A.DIACFR AS CFR",
        "A.DIAIMR AS IMR",
        "A.DIAPDPA AS PDPA",
        "A.DIAPAHYPEREMI AS PA_Hyperemi",
        "A.DIAPDHYPEREMI AS PD_Hyperemi",
        "A.DIAANN AS AnnenDiag",
        "A.EXTRAPROC AS Tilleggsprosedyrer",
        "A.CARDIAC_CATHETERIZATION AS HoyreHjerteKat",
        "A.VALVE_RECORDING AS Ventilfilming",
        "A.PERICARDIOCENTESIS AS Perikardiocentese",
        "A.ADJUVANT AS AdjuvantTerapi",
        "A.ADJPUMP AS Aortaballongpumpe",
        "A.ADJIMPELLA AS Impella",
        "A.ADJECMO AS ECMO",
        "A.ADJKAM AS AnnenVenstreKammerAssist",
        "A.ADJLUK AS MekKompr",
        "PCI.ADJDISTAL AS DistalProtectionDevice",
        "A.ADJPACE AS Pacemaker",
        "PCI.ADJTROMB AS Trombectomy",
        "A.ADJPTA AS PTAHalskar",
        "A.ADJPTSMA AS PTSMA",
        "A.ADJANN AS AnnenAdj",
        "A.OPEN_CAPILLARY AS ApningKarDato",
        "A.OPEN_CAPILLARY_TIME AS ApningKarTid",
        "A.OPEN_CAPILLARY_TIME_MISSING AS ApningKarTidUkjent",
        "A.OPEN_CAP_OPEN_ANGIO AS KarAapentVedAngio",
        "A.OPEN_CAP_OPEN_UNSUCCESSFUL AS KarIkkeAapnet",
        "PCI.SUCCESS AS GenereltSuksess",
        "A.LABNO AS LabNummer",
        "A.STRALDOS AS Straledose",
        "A.GENOMLYSNINGSTID AS GjenLysTidSek",
        "A.KONTRASTMEDEL AS Kontrastmiddel",
        "A.KONTRASTMEDELMANGD AS KontrastMengdeMl",
        "A.ANNANKONTRASTUNDERSOK  AS AnnKonMidUndersokelse",
        "A.VASCCLOSUREDEV AS ArteriellLukning",
        "A.LABKOMP  AS LabKomplikasjon",
        "A.LABALLERGILATT  AS LabKompAllergiskLettModerat",
        "A.LABALLERGIALLV  AS LabKompAllergiskAlvorlig",
        "A.LABBEHARYTMI  AS LabKompBehkrevendeArytmi",
        "A.LABHEMO  AS LabKompHemodynamisk",
        "A.LABNEURO  AS LabKompNeurologisk",
        "A.LABVASK  AS LabKompVaskulaerIkkeKoronar",
        "A.LABTAPPAT  AS LabKompMistetStent",
        "A.LABBESTSIDO  AS LabKompVedvarSidegrensokkl",
        "A.LABPERF  AS LabKompPerforasjon",
        "A.LABTAMP  AS LabKompTamponade",
        "A.LABAKUTCABG  AS LabKompAkuttACBOperasjon",
        "A.LABANNANALLV  AS LabKompAnnenAlv",
        "A.LABDODSFALL  AS LabKompDod",
        "A.LABPROCEDURDOD  AS LabKompProsedyrerelatertDod",
        "I.TRANSFERREDPATIENT  AS OverflyttetFra",
        "I.REFERRING_HOSP_ADMISSIONDATE AS InnleggelseHenvisendeSykehusDato",
        "I.REFERRING_HOSP_ADMISSIONDATE_MISSING AS InnleggelseHenvisendeSykehusDatoUkjent",
        "I.REFERRING_HOSP_ADMISSIONDATE_TIME AS InnleggelseHenvisendeSykehusTid",
        "I.REFERRING_HOSP_ADMISSIONDATE_TIME_MISSING AS InnleggelseHenvisendeSykehusTidUkjent",
        "I.PRESENTING_SYMPTOMS AS Innkomstarsak",
        "I.SYMPTOM_ONSET_DATE AS SymptomdebutDato",
        "I.SYMPTOM_ONSET_TIME AS SymptomdebutTid",
        "I.CPR_BEFORE_HOSPITAL  AS HLRForSykehus",
        "I.ECG_RHYTHM AS EKGRytme",
        "I.ECG_QRS_ANNOTATION AS EKGQRS",
        "I.ECG_STT_CHANGES AS EKGSTT",
        "I.LEFT_BLOCK  AS VenstreGrenblokk",
        "I.ECG_DECISION_TRIGGERING AS BeslutningsutlosendeEKG",
        "I.PREHOSPITAL_ECG_DATE AS BeslEKGDato",
        "I.PREHOSPITAL_ECG_TIME AS BeslEKGTid",
        "I.PREHOSPITAL_ECG_TIME_MISSING AS BeslEKGUkjent",
        "I.HEART_RATE AS Hjertefrekvens",
        "I.SYSTOLIC_BLOOD_PRESSURE AS SystoliskBlodtrykk",
        "I.DIASTOLIC_BLOOD_PRESSURE AS DiastoliskBlodtrykk",
        "I.KILLIPKLASS AS KillipKlasseAnkomst",
        "I.CARDIAC_SHOCK  AS KardiogentSjokkAnkomst",
        "A.REPERTREATMENT AS GittTrombolyse",
        "A.TYPE_THROMB_THERAPY_ADM AS TrombolyseMedikament",
        "A.THROMB_GIVEN_DATE AS TrombolyseDato",
        "A.THROMB_GIVEN_TIME AS TrombolyseTid",
        "A.THROMB_GIVEN_TIME_MISSING AS TrombolyseUkjent",
        "I.PREVIOUS_ACB AS TidligereACBOp",
        "I.PRIOR_CARDIAC_SURGERY  AS AnnenTidlKirurgi",
        "I.HYPERTENSION AS Hypertoni",
        "I.ASPIRIN_REG AS InitASA",
        "I.ORAL_ANTICOAGULANTS_REG AS InitAntikoagulantia",
        "I.OTHER_ANTIPLATELET_REG AS InitAndrePlatehemmere",
        "I.STATINS_REG AS InitStatiner",
        "I.NSAID_REG AS InitNSAID",
        "I.ACE_INHIBITORS_REG AS InitACEHemmere",
        "I.ANGIOTENSIN_II_BLOCK_REG AS InitA2Blokkere",
        "I.BETA_BLOCKERS_REG AS InitBetaBlokkere",
        "I.CALCIUM_ANTAGONIST_REG AS InitCaHemmere",
        "I.DIAB_MED_ORAL AS InitDiabetesPrOral",
        "I.DIGITALIS_REG AS InitDigitalis",
        "I.DIURETICS_REG AS InitDiuretika",
        "I.ALDOSTERONBLOCKAD_IC AS InitAldosteronantagonist",
        "I.OTHER_LIPID_LOW_AGENTS_REG AS InitOvrigLipid",
        "I.NITRATES_REG AS InitNitroglycerin",
        "IL.BIOCHEMICAL_MARKER_TYPE AS Infarktmarkoer",
        "IL.BIOCHEMICAL_MARKER_VALUE AS InfarktMarkoerMax",
        "IL.CHOLESTEROL_TOTAL AS Kolesterol",
        "IL.TRIGLYCERIDES AS Triglycerider",
        "IL.HDL_CHOLESTEROL AS HDL",
        "IL.LDL_MEASURED AS MaaltLDL",
        "IL.B_GLUCOSE AS SGlukose",
        "IL.HBA1CMOL AS HbA1c",
        "IL.S_CREATININ AS Kreatinin",
        "IL.CRP AS CRP",
        "IL.HEMOGLOBIN AS Hemoglobin",
        "F.SEGMENT1_LAYER AS SEGMENT1_LAYER", # Disse het kun F1_LAYER i sql-fila
        "F.SEGMENT2_LAYER AS SEGMENT2_LAYER", # lagt til SEGMENT for å få disse med
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
        "PCI.SEKBESLUT AS SekundaerBeslutning",
        "PCI.KOMPREV AS KomplettRevaskularisering",
        "PCI.ANTIFORE AS AntitrombotiskFor",
        "PCI.TROFORE AS TrombolyseFor",
        "PCI.ASAFORE AS ASAFor",
        "PCI.CLOFORE AS ClopidogrelFor",
        "PCI.PRAFORE AS PrasugrelFor",
        "PCI.TICFORE AS TicagrelorFor",
        "PCI.HEPFORE AS HeparinFor",
        "PCI.DALFORE AS DalteparinFor",
        "PCI.ENOFORE AS EnoxaparinFor",
        "PCI.ANNFORE AS AnnetLavmolHeparinFor",
        "PCI.BIVFORE AS BivalirudinFor",
        "PCI.FONFORE AS FondaparinuxFor",
        "PCI.ABCFORE AS AbciximabFor",
        "PCI.EPTFORE AS EptifibatidFor",
        "PCI.TIRFORE AS TirofibanFor",
        "PCI.WARFORE AS WarfarinFor",
        "PCI.DABFORE AS DabigatranFor",
        "PCI.APIFORE AS ApiksabanFor",
        "PCI.RIVFORE AS RivaroksabanFor",
        "PCI.EDOFORE AS EdoksabanFor",
        "PCI.KANGRELORFORE AS KangrelorFor",
        "PCI.OVRFORE AS AnnetAntitrombotiskFor",
        "PCI.ANTIFORE AS AntitrombotiskUnder",
        "PCI.TROUND AS TrombolyseUnder",
        "PCI.ASAUND AS ASAUnder",
        "PCI.CLOUND AS ClopidogrelUnder",
        "PCI.PRAUND AS PrasugrelUnder",
        "PCI.TICUND AS TicagrelorUnder",
        "PCI.HEPUND AS HeparinUnder",
        "PCI.DALUND AS DalteparinUnder",
        "PCI.ENOUND AS EnoxaparinUnder",
        "PCI.ANNUND AS AnnetLavmolHeparinUnder",
        "PCI.BIVUND AS BivalirudinUnder",
        "PCI.FONUND AS FondaparinuxUnder",
        "PCI.ABCUND AS AbciximabUnder",
        "PCI.EPTUND AS EptifibatidUnder",
        "PCI.TIRUND AS TirofibanUnder",
        "PCI.WARUND AS WarfarinUnder",
        "PCI.KANUND AS KangrelorUnder",
        "PCI.OVRFORE AS AnnetAntitrombotiskUnder",
        "C.AVDKOMP AS AvdKomp",
        "C.AVDALLERGISK AS AvdKompAllergisk",
        "C.AVDBLODNING AS AvdKompBlodning",
        "C.AVDBLODMAJOR AS AvdKompBlodningMajor",
        "C.AVDBLODMINOR AS AvdKompBlodningMinor",
        "C.AVDBEHPSEUDO AS AvdKompPseudoaneurysme",
        "C.AVDHEMATOM AS AvdKompHematomStor",
        "C.AVDHBFALL AS AvdKompHbFallStor",
        "C.AVDFORLANGDKOMPTID AS AvdKompForlengetTidStor",
        "C.AVDVARDTID AS AvdKompForlengetOppholdStor",
        "C.AVDULTRALJUD AS AvdKompUltralydCT",
        "C.AVDBLODTRANSFUSION AS AvdKompBlodtransfusjon",
        "C.AVDKIRURGISKATGARD AS AvdKompKirurgiskBeh",
        "C.AVDANNANBEHUTOVERKOMP AS AvdKompAnnenBehUtoverKompresjon",
        "C.AVDFORTIDAUTSATTNING AS AvdKompTidligUtsettelse",
        "C.AVDANNANVASK AS AvdKompVaskulaer",
        "C.AVDNEURO AS AvdKompNeurologiskKomp",
        "C.AVDNJURINSUFF AS AvdKompNyNyreinsuffisiens",
        "C.AVDTAMP AS AvdKompTamponade",
        "C.AVDREPCI AS AvdKompPCI",
        "C.AVDCABG AS AvdKompACB",
        "C.AVDHJARTINFARKT AS AvdKompHjerteinfarkt",
        "C.AVDANNANALLV AS AvdKompAnnenAlvorlig",
        "C.AVDDODSFALL  AS AvdKompDod",
        "C.AVDPROCEDURDOD AS AvdKompProsedyrerelatertDod",
        "C.CKMBFORE AS CKMBFor",
        "C.CKMBEFTER AS CKMBEtter",
        "C.TROPMETFORE AS TroponinMetFor",
        "C.TROPVARDEFORE AS TroponinVerdiFor",
        "C.TROPMETEFTER AS TroponinMetEtter",
        "C.TROPVARDEEFTER AS TroponinVerdiEtter",
        "D.DISCHARGE_DATE AS Utskrivningsdato",
        "D.DEATH  AS UtskrevetDod",
        "D.DECEASED_DATE AS UtskrevetDodsdato",
        "D.DISCHARGETO AS UtskrevetTil",
        "D.ASPIRIN_DISCHARGE AS ASA",
        "D.ORAL_ANTICOAGULANTS_DISCHARGE AS Antikoagulantia",
        "D.OTHER_ANTIPLATELET_DISCHARGE AS AndrePlatehemmere",
        "D.STATINS_DISCHARGE AS UtskrStatiner",
        "D.NSAID_DISCHARGE AS NSAID",
        "D.ACE_INHIBITORS_DISCHARGE AS ACEHemmere",
        "D.ANGIOTENSIN_II_BLOCK_DISCHARGE AS A2Blokkere",
        "D.BETA_BLOCKERS_DISCHARGE AS Betablokkere",
        "D.CALCIUM_ANTAGONIST_DISCHARGE  AS CaBlokkere",
        "D.DIAB_MED_INSULIN_DC  AS DiabetesBehandlingInsulin",
        "D.DIAB_MED_ORAL_DC AS DiabetesBehandlingPerOral",
        "D.DIGITALIS_DISCHARGE AS Digitalis",
        "D.DIURETICS_DISCHARGE AS Diuretika",
        "D.ALDOSTERONBLOCKAD_DC AS Aldosteronantagonister",
        "D.OTHER_LIPID_LOW_AGENTS_DISCHARGE AS OvrigeLipidsenkere",
        "D.NITRATES_DISCHARGE AS NitroglycerinLangtid",
        "D.OTHER_SERIOUS_DISEASE AS AnnenAlvorligSykdom",
        "D.INFARCTTYPE AS InfarktType",
        "D.INFARCTCLASSIFICATION AS InfarktSubklasse",
        "I.STATUS AS SkjemaStatusStart",
        "A.STATUS AS SkjemastatusHovedskjema",
        "D.STATUS AS SkjemaStatusUtskrivelse",
        "C.STATUS AS SkjemaStatusKomplikasjoner")
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
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  ) 

# 2: Sjekk om noen variabler mangler i kodeboka

angp_mangler_kodebok_katvar <- angp_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(angp_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%  
  dplyr::filter(tabell=="ANGIOPCICOMP") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

### Det er 3 som mangler
### Disse legges inn manuelt:

angp_mangler_kodebok_katvar <- dplyr::bind_rows(angp_mangler_kodebok_katvar,
                                                angp_mangler_kodebok_katvar,
                                                angp_mangler_kodebok_katvar) %>%  
  dplyr::arrange(variabel_id)

angp_mangler_kodebok_katvar <- dplyr::bind_cols(
  data.frame(type = rep("Listevariabel", 9),
             listeverdier = rep(c(0,1,9), 3),
             listetekst = rep(c("Nei", "Ja", "Ukjent"))),
  angp_mangler_kodebok_katvar)

angp_kodebok <- dplyr::bind_rows(kodebok, angp_mangler_kodebok_katvar)

# 3; Merge kodeboka og varnavn_kobl

angp_map_num_tekst <- merge(kodebok,
                            angp_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>%  
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(verdi),
                !is.na(variabel_id))


# 4: Lagre som data i pakken

usethis::use_data(angp_map_num_tekst, overwrite = TRUE)

## ANDRE PROSEDYRER VARNUM -----------------------------------------------------

# 1: Lag dataramme med mapping

# Liste over de som ikke har blitt med pga spesielle felt:
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


# 2: Sjekk om noen av variablene mangler i kodeboka

APVN_mangler_kodebok_katvar <- APVN_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(APVN_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="OTHER") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

#### Her er det tre som mangler -> MCEID, CENTREID, PROCEDURE_TIME 
#### Disse er tallverdier og trenger derfor ikke legges inn

# 3: Trekk ut informasjon fra kodeboka 

APVN_map_num_tekst <- merge(kodebok,
                            APVN_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

## Sjekk om noen ikke har verdi
APVN_map_num_tekst_uVerdi <- APVN_map_num_tekst %>% 
  filter(is.na(verdi))

## Sjekk om noen av disse må legges inn 
### Her er det tre: Prosedyredato, ProsedyretidUkjent (dette er en avkrysningsboks) og SkjemaStatus
### Disse brukes allerede som tall og trenger ikke tekstverdi

## De uten verdi tas ut

APVN_map_num_tekst <- APVN_map_num_tekst %>% 
  filter(!is.na(verdi))
  
# 4: Lagre dataramma i pakken 
usethis::use_data(APVN_map_num_tekst, overwrite = TRUE)


## ANNEN DIAGNOSTIKK VARNUM ----------------------------------------------------


# 1: Lag dataramme med mapping

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

# 2: Lag dataramme med de som mangler i kodeboka 

ADVN_mangler_kodebok_katvar <- ADVN_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(ADVN_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="DIAG") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

## Sjekk de som mangler i kodeboka
### Her er det veldig mange... 
### Sjekk hvilke i tabellen "diag" i kodeboka som ikke er i ADVN_varnavn_kobl

in_kodebok <- kodebok %>%
  filter(tabell == "diag") %>% 
  select(-c(listeverdier, listetekst)) %>% 
  unique()


in_kodebok_notSQL <- in_kodebok %>% 
  filter(!in_kodebok$variabel_id %in% ADVN_varnavn_kobl$variabel_id & type != "Tallvariabel")


### Det er 2 variabler som bør være med. 
### Disse finner jeg i tabellen "diag" i kodeboken. De ser ut til å være utledete.
### Disse legges inn med nye navn slik at vi kan bruke kodeboken på dette 
### Dette gjelder:
########### diag.GRAFT -> diag.SEGMENT.GRAFT 
########### diag.METHODUSED -> diag.SEGMENT.METHOD

# RUNDE 2 ADVN #
# 1: Oppdatere ADVN_varnavn_kobl
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
        "diag.SEGMENT AS segment", #DENNE ER NY
        "diag.SEGMENT_GRAFT AS graft", #DENNE ER NY
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

# 2: Lage dataramme med de som mangler i kodeboka
ADVN_mangler_kodebok_katvar2 <- ADVN_varnavn_kobl2 %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(ADVN_varnavn_kobl2$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="DIAG") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

## Sjekke om det er noen som ikke er tallvariabler som er i kodeboka og ikke i ADVN_varnavn_kobl2
in_kodebok_notSQL2 <- in_kodebok %>% 
  filter(!in_kodebok$variabel_id %in% ADVN_varnavn_kobl2$variabel_id & type != "Tallvariabel")

### Nå er det ingen variabler som er listervariabler som ikke er med

# 3: Merge ny ADVN_varnavn_kobl med kodeboka

ADVN_map_num_tekst <- merge(kodebok,
                            ADVN_varnavn_kobl2[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

### Sjekk om noen ikke har verdi

ADVN_map_num_tekst_uVerdi <- ADVN_map_num_tekst %>% 
  filter(is.na(verdi))

# Dette gjelder kun fødselsdato
# Denne tas ut fra mappinga: 

ADVN_map_num_tekst <- ADVN_map_num_tekst %>% 
  filter(!is.na(verdi))

# 4: Lagre dataen 
usethis::use_data(ADVN_map_num_tekst, overwrite = TRUE)


## CTANGIO VARNUM --------------------------------------------------------------

# 1: Lag dataramme med mapping

### Variabler som lages med en komplisert spørring i SQL: 

# Lokalsykehus -> leveres som tekst
# Studie -> leveres som tekst
# Granskere -> leveres som tekst
# FunnConcat -> leveres som tekst 

# Siden disse leveres som tekst trenger vi ikke gjøre noe med disse

# Lagt inn F.SEGMENT som F.SEGMENT_LAYER for å få disse med 

CTANG_varnavn_kobl <- 
  data.frame(
    kol = 
      c("MCE.MCEID AS ForlopsID",
        "P.ID AS PasientID",
        "P.SSN_TYPE AS FnrType",
        "P.GENDER AS Kjonn",
        "P.BIRTH_DATE AS FodselsDato",
        "P.DECEASED AS AvdodFReg",
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
        "F.SEGMENT1_LAYER AS SEGMENT1_LAYER", # Disse het kun F1_LAYER i sql-fila
        "F.SEGMENT2_LAYER AS SEGMENT2_LAYER", # lagt til SEGMENT for å få disse med
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

# 2: Sjekk om noen mangler i kodeboka

CTANG_mangler_kodebok_katvar <- CTANG_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(CTANG_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="ctangio" | tabell == "findingstatic") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

### Ingen mangler

## Sjekk om det er noen som ikke er tallvariabler som er i kodeboka og ikke i ADVN_varnavn_kobl2

CTANG_in_kodebok <- kodebok %>%
  filter(tabell == "ctangio") %>% 
  select(-c(listeverdier, listetekst)) %>% 
  unique()

CTANG_in_kodebok_notSQL2 <- CTANG_in_kodebok %>% 
  filter(!CTANG_in_kodebok$variabel_id %in% CTANG_varnavn_kobl$variabel_id & type != "Tallvariabel")

### Det er noen, men ingen tas med: 
### STUDYPARTICIP -> Usikkert hvordan denne evt. skal kodes
### STUDY_START_DATE -> dato
### CTANGIO_ANSVARIGGRANSKARE -> Usikkert hvordan denne evt. skal kodes
### de andre er kommentarer (altså ikke listevariabler)

# 3: Merge varnavn_kobl-fila med kodeboka

CTANG_map_num_tekst <- merge(kodebok,
                            CTANG_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

### Sjekk om noen ikke har verdi

CTANG_map_num_tekst_uVerdi <- CTANG_map_num_tekst %>% 
  filter(is.na(verdi))

### Dette gjelder 17 variabler 
### Disse ser ut som tallverdier
### Alle som ikke har verdi tas ut av mappinga: 

CTANG_map_num_tekst <- CTANG_map_num_tekst %>% 
  filter(!is.na(verdi))

# 4: Lagre data i pakken
usethis::use_data(CTANG_map_num_tekst, overwrite = TRUE)


# SEGMENTSTENTNUM---------------------------------------------------------------

# 1: Lag dataramme med mapping

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

# 2: Sjekk om noen mangler i kodeboka

segm_mangler_kodebok_katvar <- segm_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(segm_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="SEGMENT" | tabell == "STENT") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

### 4 mangler: 
### ID -> skal leveres som tall
### MCEID -> skal leveres som tall
### STENTNAMN -> skal leveres som tekst
### Ingen av disse legges inn manuelt

### STENT_DES -> dette er StentType og den skal legges inn manuelt 

# stentType får manuell mapping etter bestilling fra registeret #

segm_manuell_map <- data.frame(tabell = "stent", fysisk_feltnavn = "DES", variabel_id = "STENT_DES")

segm_manuell_map <- dplyr::bind_cols(
  data.frame(type = rep("Listevariabel", 3),
             listeverdier = c(0,1,8),
             listetekst = c("DES", "BMS", "Annet")),
  segm_manuell_map)

segm_kodebok <- dplyr::bind_rows(kodebok, segm_manuell_map)


### Sjekk om det er noen som ikke er tallvariabler som er i kodeboka og ikke i varnavn_kobl

segm_in_kodebok <- segm_kodebok %>%
  filter(tabell == "segment" | tabell == "stent") %>% 
  select(-c(listeverdier, listetekst)) %>% 
  unique()

segm_in_kodebok_notSQL2 <- segm_in_kodebok %>% 
  filter(!segm_in_kodebok$variabel_id %in% segm_varnavn_kobl$variabel_id & type != "Tallvariabel")

### Det er ikke noen

# 3: Merge varnavn_kobl-fila med kodeboka

segm_map_num_tekst <- merge(segm_kodebok,
                             segm_varnavn_kobl[, c("variabel_id", "rapporteket")],
                             by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

### Sjekk om noen ikke har verdi

segm_map_num_tekst_uVerdi <- segm_map_num_tekst %>% 
  filter(is.na(verdi))

### Dette gjelder 13 variabler 
### Disse ser ut som tallverdier eller tekstverdier
### Alle uten verdi tas ut: 

segm_map_num_tekst <- segm_map_num_tekst %>% 
  filter(!is.na(verdi))

# 4: Lagre data i pakken
usethis::use_data(segm_map_num_tekst, overwrite = TRUE)




# MITRALKLAFFVARNUM-------------------------------------------------------------

# 1: Lag dataramme med mapping

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

# 2: Sjekk om noen mangler i kodeboka

mitr_mangler_kodebok_katvar <- mitr_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(mitr_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="TAVIMITRALISDISCHARGE" | tabell == "TAVIMITRALIS") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

### 3 mangler: 
### PRESENT_HEALT_STAT ("Helsetilstand") -> numerisk variabel
### PUNCTIONTIME -> datovariabel
### ENDTIME -> datovariabel
### Ingen av disse legges inn manuelt 

### Sjekk om det er noen som ikke er tallvariabler som er i kodeboka og ikke i varnavn_kobl

mitr_in_kodebok <- kodebok %>%
  filter(tabell == "tavimitralis" | tabell == "tavimitralisdischarge") %>% 
  unique()

mitr_in_kodebok_notSQL2 <- mitr_in_kodebok %>% 
  filter(!mitr_in_kodebok$variabel_id %in% mitr_varnavn_kobl$variabel_id & type != "Tallvariabel")

### Det er flere her som er listevariabler
### De legges DERIMOT ikke inn siden disse er "utkommentert" i sql-fila og 
### dermed ser ut som de ikke skal være med i spørringen

# 3: Merge varnavn_kobl-fila med kodeboka

mitr_map_num_tekst <- merge(kodebok,
                            mitr_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

### Sjekk om noen ikke har verdi

mitr_map_num_tekst_uVerdi <- mitr_map_num_tekst %>% 
  filter(is.na(verdi))

### Dette gjelder 53 variabler 
### Disse ser ut som numeriske eller tekstlige variabler. Det er også noen som er av typen "avkrysningsboks"
### Disse tas ut av mappinga

mitr_map_num_tekst <- mitr_map_num_tekst %>% 
  filter(!is.na(verdi))

# 4: Lagre data i pakken

usethis::use_data(mitr_map_num_tekst, overwrite = TRUE)


#! Obs Det er flere med type "avkrysningsboks" som ikke blir med i mappinga


# AORTAKLAFFVARNUM--------------------------------------------------------------

# 1: Lag dataramme med mapping

# Variabler som lages med en komplisert spørring i SQL
# Disse blir ikke med i mappinga:
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

# 2: Sjekk om noen mangler i kodeboka

aort_mangler_kodebok_katvar <- aort_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(aort_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="TAVIDISCHARGE" | tabell == "TAVIPERC") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

### 7 mangler: 
### MCEID -> numerisk 
### CENTREID -> numerisk
### PRESENT_HEALT_STAT ("Helsetilstand") -> numerisk variabel
### PUNCTIONTIME -> datovariabel
### ENDTIME -> datovariabel
### BALLOONSIZE PRE og POST -> tallvariabel
### Ingen av disse legges inn manuelt 

### Sjekk om det er noen som ikke er tallvariabler som er i kodeboka og ikke i varnavn_kobl

aort_in_kodebok <- kodebok %>%
  filter(tabell == "tavidischarge" | tabell == "taviperc") %>% 
  unique()

aort_in_kodebok_notSQL <- aort_in_kodebok %>% 
  filter(!aort_in_kodebok$variabel_id %in% aort_varnavn_kobl$variabel_id & type != "Tallvariabel")

### Det er flere her som er listevariabler og 4 som ser ut som de bør legges inn
### Siden de ikke finnes i kodeboka er rapporteket-navnet utkjent, 
### men et "rapporteket"-navn lages i små bokstaver basert på fysisk_feltnavn

### Kode for å få lage dataramme med rader som må legges inn i varnavn_kobl helt lik varnavn_kobl:

aort_in_kodebok_hente_ut <- aort_in_kodebok_notSQL %>% 
  filter(type == "Listevariabel") %>% 
  select(-c(type, listeverdier, listetekst)) %>% 
  rename(var_navn = fysisk_feltnavn) %>% 
  mutate(rapporteket = tolower(var_navn),
         tabell = toupper(tabell))

aort_in_kodebok_hente_ut <- aort_in_kodebok_hente_ut[,c(1,2,4,3)] # endre rekkefølge

### Radene legges til i fila varnavn_kobl:

aort_varnavn_kobl <- rbind(aort_varnavn_kobl, aort_in_kodebok_hente_ut)


# 3: Merge varnavn_kobl-fila med kodeboka

aort_map_num_tekst <- merge(kodebok,
                            aort_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

### Sjekk om noen ikke har verdi

aort_map_num_tekst_uVerdi <- aort_map_num_tekst %>% 
  filter(is.na(verdi))

### Dette gjelder 53 variabler 
### Disse ser ut som numeriske eller tekstlige variabler. 
### Det er også noen som er av typen "avkrysningsboks"
### Disse tas ut fra mappinga: 

aort_map_num_tekst <- aort_map_num_tekst %>% 
  filter(!is.na(verdi))

# 4: Lagre data som pakke 

usethis::use_data(aort_map_num_tekst, overwrite = TRUE)


# MITRALKLAFFOPPFVARNUM----------------------------------------------------

# 1: Lag dataramme med mapping

# mitr_oppf <- MITRALKLAFFOPPFVARNUM


mitr_oppf_varnavn_kobl <- 
  data.frame(
    kol = 
      c("TF.MCEID AS ForlopsID",
        "T.MCEID AS BasisForlopsID",
        "TF.CENTREID AS AvdRESH",
        "T.SCREENING AS BasisScreeningBeslutning",
        "T.PROCEDUREDATE AS BasisProsedyreDato",
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
        "TF.AORTASTENOS AS Aortastenose", #OBS!!!!! I SQL-spørringa står det her "TF.AORTAINS AS Aortastenose" 
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
                                          tabell == "TF" ~ "TAVIMITRALISFOLLOWUP",
                                          tabell == "T" ~ "TAVIMITRALIS",
                                          tabell == "C" ~ "CENTRE",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  )

# 2: Sjekk om noen mangler i kodeboka

mitr_oppf_mangler_kodebok_katvar <- mitr_oppf_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(mitr_oppf_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="TAVIMITRALISFOLLOWUP" | tabell == "TAVIMITRALIS") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

### 3 mangler: 
### PRESENT_HEALT_STAT ("Helsetilstand") -> numerisk variabel
### MCEID x2 -> ikke kategorisk variabel
### CENTREID -> ikke kategorisk variabel 
### Ingen av disse legges inn manuelt 

### Sjekk om det er noen som ikke er tallvariabler som er i kodeboka og ikke i varnavn_kobl

mitr_oppf_in_kodebok <- kodebok %>%
  filter(tabell == "tavimitralis" | tabell == "tavimitralisfollowup") %>% 
  unique()

mitr_oppf_in_kodebok_notSQL2 <- mitr_oppf_in_kodebok %>% 
  filter(!mitr_oppf_in_kodebok$variabel_id %in% mitr_oppf_varnavn_kobl$variabel_id & type != "Tallvariabel")

### Det er mange her fra tabellen tavimitralis. Det er som forventet siden denne 
### tabellen er veldig stor og det er veldig få variabler som er med i mitralklaffoppfvarnum. 
### Det er én listevariabel som ikke er med fra tavimitralisfollowup. 
### Dette er aortastenos og den ser viktig ut. Jeg legger den inn varnavn-koblinga.
### Se kommentar der.


# 3: Merge varnavn_kobl-fila med kodeboka

mitr_oppf_map_num_tekst <- merge(kodebok,
                            mitr_oppf_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

### Sjekk om noen ikke har verdi

mitr_oppf_map_num_tekst_uVerdi <- mitr_oppf_map_num_tekst %>% 
  filter(is.na(verdi))

### Dette gjelder 30 variabler 
### Disse ser ut som numeriske eller tekstlige variabler. 
### Det er også noen som er av typen "avkrysningsboks" (f.eks., PROLAPS-variablene)
### Disse tas ut av mappinga

mitr_oppf_map_num_tekst <- mitr_oppf_map_num_tekst %>% 
  filter(!is.na(verdi))

# 4: Lagre data i pakken

usethis::use_data(mitr_oppf_map_num_tekst, overwrite = TRUE)


# AORTAKLAFFOPPFVARNUM ---------------------------------------------------------


# 1: Lag dataramme med mapping

# aort_oppf <- AORTAKLAFFOPPFVARNUM

aort_oppf_varnavn_kobl <- 
  data.frame(
    kol = 
      c("TF.MCEID AS ForlopsID",
        "T.MCEID AS BasisForlopsID",
        "TF.CENTREID AS AvdRESH",
        "T.SCREENING AS BasisScreeningBeslutning",
        "T.SCREENINGDATE AS BasisBeslutningsDato",
        "T.PROCEDUREDATE AS BasisProsedyreDato",
        "TF.FOLLOWUPDATE AS OppfDato",
        "TF.FOLLOWUP_TYPE AS OppfType",
        "TF.AVDDECEASED AS OppfAvdod",
        "TF.DECEASEDDATE AS OppfAvdodDato",
        "TF.PROC_RELATED_DEATH AS OppfDodProsRelatert",
        
        
        #-- Oppfølging
        "TF.HEIGHT AS Hoyde",
        "TF.HEIGHT_MISS AS HoydeUkjent",
        "TF.WEIGHT AS Vekt",
        "TF.WEIGHT_MISS AS VektUkjent",
        "TF.NYHA AS NYHA",
        "TF.CANADIAN AS CanadianClass", ### OBS!!! I SQL-spørringa står det T.CANADIAN
        "TF.WALKINGTEST AS Gangtest",
        "TF.WALKINGTEST_MISS AS GangtestIkkeUtfort",
        "TF.WALKINGTESTSPEED AS GangHastigtest",
        "TF.WALKINGTESTSPEED_MISS AS GangHastigtestIkkeUtfort",
        "TF.S_CREATININ AS SKreatinin",
        "TF.S_CREATININ_MISS AS SKreatininIkkeUtfort",
        "TF.PROBNP AS ProBNP",
        "TF.PROBNPNT AS NTProBNP",
        "TF.HEMOGLOBIN AS Hemoglobin",
        "TF.HEMOGLOBIN_MISS AS HemoglobinUkjent",
        
        #-- EKKO-funn
        "TF.MAXGRADIENT AS MaxGradient",
        "TF.MAXGRADIENT_MSEK AS MaxGradientMPS",
        "TF.AVERAGEGRADIENT AS Middelgradient",
        "TF.AVERAGEGRADIENT_MISS AS MiddelgradientMangler",
        "TF.LEFT_VENTRICULAR_FUNCTION AS VenstreVentrikkelFunksjon",
        "TF.PULMHYPERTENSION AS PulmonalHypertensjon",
        "TF.AORTAINS AS Aortainsuffisiens",
        "TF.PARAVALVULAR_LEAK AS ParavalvularLekkasje",
        "TF.PARAVALVULAR_INSUFFICIENCY AS ParavalvularInsuffisiens",
        "TF.MITRALISINS AS Mitralinsuffisiens",
        "TF.PRESENT_HEALTH_STAT AS Helsetilstand",
        
        #-- Komplikasjoner
        "TF.COMP AS Komplikasjoner",
        "TF.ENDOCARDIT AS Endokarditt",
        "TF.DIALYSIS AS Dialyse",
        "TF.MI AS Hjerteinfarkt",
        "TF.STROKE AS Hjerneslag",
        "TF.PACEMAKER AS Pacemaker",
        "TF.PROSTHESIS AS ProteseDysfunk",
        "TF.VESSEL AS SenKarKomp",
        "TF.COMPOTHER AS AnnenKomp",
        "TF.STATUS AS SkjemaStatus"
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
                                          tabell == "TF" ~ "TAVIPERCFOLLOWUP",
                                          tabell == "T" ~ "TAVIPERC",
                                          tabell == "C" ~ "CENTRE",
                                          .default = tabell),
                variabel_id = trimws(paste0(tabell, "_", var_navn))
  )

# 2: Sjekk om noen mangler i kodeboka

aort_oppf_mangler_kodebok_katvar <- aort_oppf_varnavn_kobl %>%  
  dplyr::filter(variabel_id %in% 
                  setdiff(aort_oppf_varnavn_kobl$variabel_id, kodebok$variabel_id)) %>%
  dplyr::filter(tabell=="TAVIDISCHARGE" | tabell == "TAVIPERC") %>%  
  dplyr::mutate(tabell=tolower(tabell)) %>%  
  dplyr::rename(fysisk_feltnavn = var_navn) %>%  
  dplyr::select(-rapporteket)

### 1 mangler: 
### MCEID -> numerisk 
### Legges ikke inn manuelt 

### Sjekk om det er noen som ikke er tallvariabler som er i kodeboka og ikke i varnavn_kobl

aort_oppf_in_kodebok <- kodebok %>%
  filter(tabell == "tavipercfollowup" | tabell == "taviperc") %>% 
  unique()

aort_oppf_in_kodebok_notSQL <- aort_oppf_in_kodebok %>% 
  filter(!aort_oppf_in_kodebok$variabel_id %in% aort_oppf_varnavn_kobl$variabel_id & type != "Tallvariabel")

### Det er mange her. De fleste kommer fra tabellen "taviperc" og legges ikke inn
### siden disse ikke hører til tabellen aortaklaffoppfvar num. 
### Det er to fra tabllen "tavipercfollowup":
### SUCCESSFULLPROC og CANADIAN 
### Begge disse er listevariabler
### CANADIAN er ikke med i varnavn_kobl siden det står i SQL-spørringa at den kommer
### fra tabellen "taviperc". I kodeboka står det at den kommer fra tabellen 
### "tavipercfollowup". Jeg legger den inn som "tavipercfollowup" i varnavn_kobl over.
### Se egen kommentar der. 
### SUCCESSFULLPROC ser ikke ut til å brukes i denne tabellen. Ingenting gjøres 
### med denne variabelen. 

# 3: Merge varnavn_kobl-fila med kodeboka

aort_oppf_map_num_tekst <- merge(kodebok,
                            aort_oppf_varnavn_kobl[, c("variabel_id", "rapporteket")],
                            by = "variabel_id", all.x = TRUE) %>% 
  dplyr::arrange(variabel_id, rapporteket, listeverdier) %>%  
  dplyr::select(rapporteket, listeverdier, listetekst) %>%  
  dplyr::rename(variabel_id = rapporteket,
                verdi = listeverdier,
                verditekst = listetekst) %>%
  dplyr::filter(!is.na(variabel_id))

### Sjekk om noen ikke har verdi

aort_oppf_map_num_tekst_uVerdi <- aort_oppf_map_num_tekst %>% 
  filter(is.na(verdi))

### Dette gjelder 23 variabler 
### Disse ser ut som numeriske eller tekstlige variabler. 
### Det er også noen som er av typen "avkrysningsboks"
### Disse tas ut fra mappinga: 

aort_oppf_map_num_tekst <- aort_oppf_map_num_tekst %>% 
  filter(!is.na(verdi))

# 4: Lagre data som pakke 

usethis::use_data(aort_oppf_map_num_tekst, overwrite = TRUE)




