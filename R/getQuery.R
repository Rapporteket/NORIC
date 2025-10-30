#' Fetch queries to NORIC raw data tables
#'
#' Replace old views
#'
#' @param fromDate Character string of format YYYY-MM-DD with start date. Value
#' NULL if no filter on date.
#' @param toDate Character string of format YYYY-MM-DD with end date. Value
#' NULL if no filter on date.

#' @return query as string
#' @name getQuery
#' @aliases queryAngiopcinum
#' queryCtangiovarnum
#' queryAortaklaffvarnum
#' queryAortaklaffoppfvarnum
#' queryAndreprosedyrervarnum
#' queryAnnendiagnostikkvarnum
#' querySegmentstentnum
#' queryMitralklaffvarnum
#' queryMitralklaffoppfvarnum
#' queryTaviprom
#' queryForlopsoversikt
#' querySkjemaoversikt
#' queryPasienterstudier
#' queryApLight
#' queryDiagnose
NULL



#' @rdname getQuery
#' @export
queryAngiopcinum <- function(){
  paste0("
  SELECT
    A.CENTREID AS AvdRESH,
    MCE.MCEID AS ForlopsID,
    P.ID AS PasientID,

    A.REGTYP AS ProsedyreType,
    MCE.MCETYPE AS Hastegrad,
    CASE
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NOT NULL THEN 'Sekundær'
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NULL THEN 'Primær'
      ELSE NULL
    END AS Regtype,
    A.INTERDAT AS ProsedyreDato,
    A.INTERDAT_TIME AS ProsedyreTid,
    CASE A.INTERDAT_TIME_MISSING
      WHEN 1 THEN 'Ja'
      ELSE 'Nei'
    END AS ProsedyreTidUkjent,
    CASE (P.LOCAL_HOSPITAL) 
      WHEN 999 THEN P.LOCAL_HOSPITAL_OTHER
      ELSE (SELECT NAME FROM hospital WHERE hospital.ID = P.LOCAL_HOSPITAL)
    END AS Lokalsykehus,
   
    P.GENDER AS Kjonn,
    P.BIRTH_DATE FodselsDato,

     A.SYMPTOM_ONSET_DATE AS SymptomDato,
     A.SYMPTOM_ONSET_TIME AS SymptomTid,
   -- 	getCheckText(A.SYMPTOM_ONSET_TIME_MISSING) AS SymptomTidUkjent, // Removed in NOR-1053 for v1.11
     A.PREHOSPITAL_ECG_DATE AS BesUtlEKGDato,
     A.PREHOSPITAL_ECG_TIME AS BesUtlEKGTid,
     CASE A.PREHOSPITAL_ECG_TIME_MISSING
      WHEN 1 THEN 'Ja'
      ELSE 'Nei'
      END AS BesUtlEKGTidUkjent,
     A.JOURTID AS Vakttid,
     A.HEIGHT AS Hoyde,
     A.WEIGHT AS Vekt,
     A.SKREATININ AS SKreatinin,
     A.TIDPCI  AS TidlPCI,
     A.TIDCABG  AS TidlABC,
     A.SMOKING_STATUS  AS RoykeStatus,
     A.HYPERTON  AS BehHypertoni,
     A.STATINS AS Statiner,
     A.TIDINF  AS TidlInfarkt,
     A.HISTORY_OF_CHF  AS KjentNedsattVenstVentr,
     A.DIABETES  AS Diabetes,
     A.DIABETESINSULIN AS Insulin,
     A.PREVIOUS_STROKE AS TidligereSlag,
     A.PERIPHERAL_VASCULAR_DISEASE  AS PeriferKarsykdom,
     A.INDIKATION  AS Indikasjon,
     A.MYOKARD  AS Myokardskademarkor,
     A.STSEGSANK  AS STSegmentSenkning,
     A.STAGED_PROCEDURE  AS StegvisProsedyre,
     A.CSS AS CanadianClass,
     A.NYHA AS NYHA,
     A.CARD AS KardiogentSjokk,
     A.KILLIPKLASS AS KillipKlasse,
     A.FYND AS Funn,
     A.ADMISSION_ER AS AnkomstPCIDato,
     A.ADMISSION_ER_TIME AS AnkomstPCITid,
     CASE A.ADMISSION_ER_TIME_MISSING
      WHEN 1 THEN 'Ja'
      ELSE 'Nei'
      END AS AnkomstPCITidUkjent,
      
     A.PUNKT AS Stikksted,
     A.STENOS AS StenoseTidlBehSegment,
     A.CABG AS StenoseACBGraft,
     A.STRESSKARDIOMYOPATI  AS Stresskardiomyopati,
     A.PRIMBES AS PrimarBeslutning,
     A.DIAGNOSTIK AS AnnenDiagHovedSpm,
     A.DIATRYCK AS FFR,
     A.DIADOP AS Doppler,
     A.DIAIVUS AS IVUS,
     A.DIAOCT AS OCT,
     A.DIAIFR AS IFR,
     A.DIANIRS AS NIRS,
     A.DIACFR AS CFR,
     A.DIAIMR AS IMR,
     A.DIAPDPA AS PDPA,
     A.DIAPAHYPEREMI AS PA_Hyperemi,
     A.DIAPDHYPEREMI AS PD_Hyperemi,
     A.DIAANN AS AnnenDiag,
     A.EXTRAPROC AS Tilleggsprosedyrer,
     A.CARDIAC_CATHETERIZATION AS HoyreHjerteKat,
     A.VALVE_RECORDING AS Ventilfilming,
     A.PERICARDIOCENTESIS AS Perikardiocentese,
     A.ADJUVANT AS AdjuvantTerapi,
     A.ADJPUMP AS Aortaballongpumpe,
     A.ADJIMPELLA AS Impella,
     A.ADJECMO AS ECMO,
     A.ADJKAM AS AnnenVenstreKammerAssist,
     A.ADJLUK AS MekKompr,
     PCI.ADJDISTAL AS DistalProtectionDevice,
     A.ADJPACE AS Pacemaker,
     PCI.ADJTROMB AS Trombectomy,
     A.ADJPTA AS PTAHalskar,
     A.ADJPTSMA AS PTSMA,
     A.ADJANN AS AnnenAdj,
     A.OPEN_CAPILLARY AS ApningKarDato,
     A.OPEN_CAPILLARY_TIME AS ApningKarTid,
     CASE A.OPEN_CAPILLARY_TIME_MISSING
      WHEN 1 THEN 'Ja'
      ELSE 'Nei'
      END AS ApningKarTidUkjent,
      
     CASE A.OPEN_CAP_OPEN_ANGIO
      WHEN 1 THEN 'Ja'
      ELSE 'Nei'
      END AS KarAapentVedAngio,
     CASE A.OPEN_CAP_OPEN_UNSUCCESSFUL
      WHEN 1 THEN 'Ja'
      ELSE 'Nei'
      END AS KarIkkeAapnet,
     PCI.SUCCESS AS GenereltSuksess,
     A.LABNO AS LabNummer,
     A.STRALDOS AS Straledose,
     A.GENOMLYSNINGSTID AS GjenLysTidSek,
     A.KONTRASTMEDEL AS Kontrastmiddel,
     A.KONTRASTMEDELMANGD AS KontrastMengdeMl,
     A.ANNANKONTRASTUNDERSOK  AS AnnKonMidUndersokelse,
     A.VASCCLOSUREDEV AS ArteriellLukning,
     A.LABKOMP  AS LabKomplikasjon,
     A.LABALLERGILATT  AS LabKompAllergiskLettModerat,
     A.LABALLERGIALLV  AS LabKompAllergiskAlvorlig,
     A.LABBEHARYTMI  AS LabKompBehkrevendeArytmi,
     A.LABHEMO  AS LabKompHemodynamisk,
     A.LABNEURO  AS LabKompNeurologisk,
     A.LABVASK  AS LabKompVaskulaerIkkeKoronar,
     A.LABTAPPAT  AS LabKompMistetStent,
     A.LABBESTSIDO  AS LabKompVedvarSidegrensokkl,
     A.LABPERF  AS LabKompPerforasjon,
     A.LABTAMP  AS LabKompTamponade,
     A.LABAKUTCABG  AS LabKompAkuttACBOperasjon,
     A.LABANNANALLV  AS LabKompAnnenAlv,
     A.LABDODSFALL  AS LabKompDod,
     A.LABPROCEDURDOD  AS LabKompProsedyrerelatertDod,


     I.TRANSFERREDPATIENT  AS OverflyttetFra,
     (select h.NAME FROM hospital h where I.TRANSFERREDFROM = h.ID) AS OverflyttetFraSykehus,
     I.REFERRING_HOSP_ADMISSIONDATE AS InnleggelseHenvisendeSykehusDato,
     CASE I.REFERRING_HOSP_ADMISSIONDATE_MISSING
       WHEN 1 THEN 'Ja'
       ELSE 'Nei'
       END AS InnleggelseHenvisendeSykehusDatoUkjent,
     I.REFERRING_HOSP_ADMISSIONDATE_TIME AS InnleggelseHenvisendeSykehusTid,
     CASE I.REFERRING_HOSP_ADMISSIONDATE_TIME_MISSING
       WHEN 1 THEN 'Ja'
       ELSE 'Nei'
      END AS InnleggelseHenvisendeSykehusTidUkjent,
     

    I.PRESENTING_SYMPTOMS AS Innkomstarsak,
    I.SYMPTOM_ONSET_DATE AS SymptomdebutDato,
    I.SYMPTOM_ONSET_TIME AS SymptomdebutTid,
  --	getCheckText(I.SYMPTOM_ONSET_TIME_MISSING) as SymptomdebutUkjent, // Removed in NOR-1053 for v1.11
    I.CPR_BEFORE_HOSPITAL  AS HLRForSykehus,

    I.ECG_RHYTHM AS EKGRytme,
    I.ECG_QRS_ANNOTATION AS EKGQRS,
    I.ECG_STT_CHANGES AS EKGSTT,
    I.LEFT_BLOCK  AS VenstreGrenblokk,
    I.ECG_DECISION_TRIGGERING AS BeslutningsutlosendeEKG,
    I.PREHOSPITAL_ECG_DATE AS BeslEKGDato,
    I.PREHOSPITAL_ECG_TIME AS BeslEKGTid,
    CASE I.PREHOSPITAL_ECG_TIME_MISSING
       WHEN 1 THEN 'Ja'
       ELSE 'Nei'
      END AS BeslEKGUkjent,   

    I.HEART_RATE AS Hjertefrekvens,
    I.SYSTOLIC_BLOOD_PRESSURE AS SystoliskBlodtrykk,
    I.DIASTOLIC_BLOOD_PRESSURE AS DiastoliskBlodtrykk,
    I.KILLIPKLASS AS KillipKlasseAnkomst,
    I.CARDIAC_SHOCK  AS KardiogentSjokkAnkomst,

    A.REPERTREATMENT AS GittTrombolyse,
    A.TYPE_THROMB_THERAPY_ADM AS TrombolyseMedikament,
    A.THROMB_GIVEN_DATE AS TrombolyseDato,
    A.THROMB_GIVEN_TIME AS TrombolyseTid,
    CASE A.THROMB_GIVEN_TIME_MISSING
       WHEN 1 THEN 'Ja'
       ELSE 'Nei'
      END AS TrombolyseUkjent,   

    I.PREVIOUS_ACB AS TidligereACBOp,
    I.PRIOR_CARDIAC_SURGERY  AS AnnenTidlKirurgi,
    I.HYPERTENSION AS Hypertoni,
   
    I.ASPIRIN_REG AS InitASA,
    I.ORAL_ANTICOAGULANTS_REG AS InitAntikoagulantia,
    I.OTHER_ANTIPLATELET_REG AS InitAndrePlatehemmere,
    I.STATINS_REG AS InitStatiner,
  
    I.NSAID_REG AS InitNSAID,
    I.ACE_INHIBITORS_REG AS InitACEHemmere,
    I.ANGIOTENSIN_II_BLOCK_REG AS InitA2Blokkere,
    I.BETA_BLOCKERS_REG AS InitBetaBlokkere,
    I.CALCIUM_ANTAGONIST_REG AS InitCaHemmere,
    I.DIAB_MED_ORAL AS InitDiabetesPrOral,
    I.DIGITALIS_REG AS InitDigitalis,
    I.DIURETICS_REG AS InitDiuretika,
    I.ALDOSTERONBLOCKAD_IC AS InitAldosteronantagonist,
    I.OTHER_LIPID_LOW_AGENTS_REG AS InitOvrigLipid,
    I.NITRATES_REG AS InitNitroglycerin,

    IL.BIOCHEMICAL_MARKER_TYPE AS Infarktmarkoer,
    IL.BIOCHEMICAL_MARKER_VALUE AS InfarktMarkoerMax,
    IL.CHOLESTEROL_TOTAL AS Kolesterol,
    IL.TRIGLYCERIDES AS Triglycerider,
    IL.HDL_CHOLESTEROL AS HDL,
    IL.LDL_MEASURED AS MaaltLDL,
    IL.B_GLUCOSE AS SGlukose,
    IL.HBA1CMOL AS HbA1c,
    IL.S_CREATININ AS Kreatinin,
    IL.CRP AS CRP,
    IL.HEMOGLOBIN AS Hemoglobin,
  -- Vanlige segment
    F.SEGMENT1,
    F.SEGMENT2,
    F.SEGMENT3,
    F.SEGMENT4,
    F.SEGMENT5,
    F.SEGMENT6,
    F.SEGMENT7,
    F.SEGMENT8,
    F.SEGMENT9,
    F.SEGMENT10,
    F.SEGMENT11,
    F.SEGMENT12,
    F.SEGMENT13,
    F.SEGMENT14,
    F.SEGMENT15,
    F.SEGMENT16,
    F.SEGMENT17,
    F.SEGMENT18,
    F.SEGMENT19,
    F.SEGMENT20,
  -- Vene segment
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '1' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT1VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '2' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT2VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '3' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT3VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '4' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT4VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '5' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT5VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '6' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT6VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '7' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT7VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '8' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT8VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '9' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT9VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '10' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT10VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '11' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT11VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '12' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT12VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '13' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT13VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '14' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT14VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '15' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT15VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '16' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT16VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '17' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT17VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '18' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT18VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '19' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT19VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '20' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT20VeneStenosgrad,

  -- Arterie segment
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '1' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT1ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '2' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT2ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '3' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT3ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '4' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT4ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '5' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT5ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '6' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT6ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '7' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT7ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '8' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT8ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '9' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT9ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '10' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT10ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '11' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT11ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '12' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT12ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '13' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT13ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '14' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT14ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '15' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT15ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '16' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT16ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '17' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT17ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '18' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT18ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '19' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT19ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '20' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT20ArterieStenosgrad,
  
    PCI.SEKBESLUT AS SekundaerBeslutning,
    PCI.KOMPREV AS KomplettRevaskularisering,
    PCI.ANTIFORE AS AntitrombotiskFor,
    PCI.TROFORE AS TrombolyseFor,
    PCI.ASAFORE AS ASAFor,
    PCI.CLOFORE AS ClopidogrelFor,
    PCI.PRAFORE AS PrasugrelFor,
    PCI.TICFORE AS TicagrelorFor,
    PCI.HEPFORE AS HeparinFor,
    PCI.DALFORE AS DalteparinFor,
    PCI.ENOFORE AS EnoxaparinFor,
    PCI.ANNFORE AS AnnetLavmolHeparinFor,
    PCI.BIVFORE AS BivalirudinFor,
    PCI.FONFORE AS FondaparinuxFor,
    PCI.ABCFORE AS AbciximabFor,
    PCI.EPTFORE AS EptifibatidFor,
    PCI.TIRFORE AS TirofibanFor,
    PCI.WARFORE AS WarfarinFor,
    PCI.DABFORE AS DabigatranFor,
    PCI.APIFORE AS ApiksabanFor,
    PCI.RIVFORE AS RivaroksabanFor,
    PCI.EDOFORE AS EdoksabanFor,
    PCI.KANGRELORFORE AS KangrelorFor,
    PCI.OVRFORE AS AnnetAntitrombotiskFor,
    PCI.ANTIFORE AS AntitrombotiskUnder,
    PCI.TROUND AS TrombolyseUnder,
    PCI.ASAUND AS ASAUnder,
    PCI.CLOUND AS ClopidogrelUnder,
    PCI.PRAUND AS PrasugrelUnder,
    PCI.TICUND AS TicagrelorUnder,
    PCI.HEPUND AS HeparinUnder,
    PCI.DALUND AS DalteparinUnder,
    PCI.ENOUND AS EnoxaparinUnder,
    PCI.ANNUND AS AnnetLavmolHeparinUnder,
    PCI.BIVUND AS BivalirudinUnder,
    PCI.FONUND AS FondaparinuxUnder,
    PCI.ABCUND AS AbciximabUnder,
    PCI.EPTUND AS EptifibatidUnder,
    PCI.TIRUND AS TirofibanUnder,
    PCI.WARUND AS WarfarinUnder,
    PCI.KANUND AS KangrelorUnder,
    PCI.OVRFORE AS AnnetAntitrombotiskUnder,
  
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = PCI.MAIN_OPERATOR ) AS PCIHovedOperator,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = PCI.SECOND_OPERATOR ) AS PCIAndreOperator,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = PCI.THIRD_OPERATOR ) AS PCITredjeOperator,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = A.MAIN_ANGIOGRAFOR ) AS Angiografor1,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = A.SECOND_ANGIOGRAFOR ) AS Angiografor2,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = A.THIRD_ANGIOGRAFOR ) AS Angiografor3,
    CAST((SELECT GROUP_CONCAT(CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME)) FROM pci_operator_mapping aom, people peo where aom.PEOPLEID = peo.PEOPLEID and aom.MCEID = A.MCEID) AS CHAR(300)) AS PCIOperatorer,
    CAST((SELECT GROUP_CONCAT(CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME)) FROM angio_operator_mapping aom, people peo where aom.PEOPLEID = peo.PEOPLEID and aom.MCEID = A.MCEID) AS CHAR(300)) AS AngioOperatorer,

    C.AVDKOMP AS AvdKomp,
    C.AVDALLERGISK AS AvdKompAllergisk,
    C.AVDBLODNING AS AvdKompBlodning,
    C.AVDBLODMAJOR AS AvdKompBlodningMajor,
    C.AVDBLODMINOR AS AvdKompBlodningMinor,
    C.AVDBEHPSEUDO AS AvdKompPseudoaneurysme,
    C.AVDHEMATOM AS AvdKompHematomStor,
    C.AVDHBFALL AS AvdKompHbFallStor,
    C.AVDFORLANGDKOMPTID AS AvdKompForlengetTidStor,
    C.AVDVARDTID AS AvdKompForlengetOppholdStor,
    C.AVDULTRALJUD AS AvdKompUltralydCT,
    C.AVDBLODTRANSFUSION AS AvdKompBlodtransfusjon,
    C.AVDKIRURGISKATGARD AS AvdKompKirurgiskBeh,
    C.AVDANNANBEHUTOVERKOMP AS AvdKompAnnenBehUtoverKompresjon,
    C.AVDFORTIDAUTSATTNING AS AvdKompTidligUtsettelse,
    C.AVDANNANVASK AS AvdKompVaskulaer,
    C.AVDNEURO AS AvdKompNeurologiskKomp,
    C.AVDNJURINSUFF AS AvdKompNyNyreinsuffisiens,
    C.AVDTAMP AS AvdKompTamponade,
    C.AVDREPCI AS AvdKompPCI,
    C.AVDCABG AS AvdKompACB,
    C.AVDHJARTINFARKT AS AvdKompHjerteinfarkt,
    C.AVDANNANALLV AS AvdKompAnnenAlvorlig,
    C.AVDDODSFALL  AS AvdKompDod,
    C.AVDPROCEDURDOD AS AvdKompProsedyrerelatertDod,
  
    C.CKMBFORE AS CKMBFor,
    C.CKMBEFTER AS CKMBEtter,
    C.TROPMETFORE AS TroponinMetFor,
    C.TROPVARDEFORE AS TroponinVerdiFor,
    C.TROPMETEFTER AS TroponinMetEtter,
    C.TROPVARDEEFTER AS TroponinVerdiEtter,
    -- Here comes numerous variables in the ANGIOPCICOMP SQL. Never used?
  
    D.DISCHARGE_DATE AS Utskrivningsdato,
    D.DEATH  AS UtskrevetDod,
    D.DECEASED_DATE AS UtskrevetDodsdato,
    D.DISCHARGETO AS UtskrevetTil,
    D.ASPIRIN_DISCHARGE AS ASA,
    D.ORAL_ANTICOAGULANTS_DISCHARGE AS Antikoagulantia,
    D.OTHER_ANTIPLATELET_DISCHARGE AS AndrePlatehemmere,
    D.STATINS_DISCHARGE AS UtskrStatiner,
    D.NSAID_DISCHARGE AS NSAID,
    D.ACE_INHIBITORS_DISCHARGE AS ACEHemmere,
    D.ANGIOTENSIN_II_BLOCK_DISCHARGE AS A2Blokkere,
    D.BETA_BLOCKERS_DISCHARGE AS Betablokkere,
    D.CALCIUM_ANTAGONIST_DISCHARGE  AS CaBlokkere,
    D.DIAB_MED_INSULIN_DC  AS DiabetesBehandlingInsulin,
    D.DIAB_MED_ORAL_DC AS DiabetesBehandlingPerOral,
    D.DIGITALIS_DISCHARGE AS Digitalis,
    D.DIURETICS_DISCHARGE AS Diuretika,
    D.ALDOSTERONBLOCKAD_DC AS Aldosteronantagonister,
    D.OTHER_LIPID_LOW_AGENTS_DISCHARGE AS OvrigeLipidsenkere,
    D.NITRATES_DISCHARGE AS NitroglycerinLangtid,
    D.OTHER_SERIOUS_DISEASE AS AnnenAlvorligSykdom,
    D.INFARCTTYPE AS InfarktType,
    D.INFARCTCLASSIFICATION AS InfarktSubklasse,
  
    P.SSN_TYPE AS FnrType,
    P.SSNSUBTYPE AS FnrSubtype,
    P.DECEASED  AS AvdodFReg,
    P.DECEASED_DATE as AvdodDatoFReg,
    P.MUNICIPALITY_NAME AS Kommune,
    P.MUNICIPALITY_NUMBER AS KommuneNr,
	  CAST(NULL AS CHAR(50)) AS Fylke,
  	CAST(NULL AS CHAR(2)) AS Fylkenr,
  	MCE.PARENT_MCEID as KobletForlopsID,
  	MCE.PARENT_MCEID AS PrimaerForlopsID,

    -- Study information
     CAST((SELECT
            GROUP_CONCAT(
              IF ((DATEDIFF(P.REGISTERED_DATE, PS.PasInklDato) > 0) AND (DATEDIFF(P.REGISTERED_DATE, PS.StudieAvsluttDato) < 0 OR PS.StudieAvsluttDato IS NULL), CONCAT(PS.StudieNavn), NULL))
          FROM pasienterstudier PS
          WHERE PS.PasientID = MCE.PATIENT_ID) AS CHAR(75))
      AS Studie,  

    I.STATUS AS SkjemaStatusStart,
    A.STATUS AS SkjemastatusHovedskjema,
    D.STATUS AS SkjemaStatusUtskrivelse,
    C.STATUS AS SkjemaStatusKomplikasjoner
    
    FROM mce MCE
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      INNER JOIN regangio A ON MCE.MCEID = A.MCEID
      LEFT JOIN initialcare I ON MCE.MCEID = I.MCEID
      LEFT JOIN initialcarelab IL ON MCE.MCEID=IL.MCEID
      LEFT JOIN findingstatic F ON MCE.MCEID = F.MCEID
      LEFT JOIN pci PCI ON MCE.MCEID = PCI.MCEID
      LEFT JOIN angiopcicomp C ON MCE.MCEID = C.MCEID
      LEFT JOIN discharge D ON MCE.MCEID = D.MCEID
  ")
}  



#' @rdname getQuery
#' @export
queryCtangiovarnum <-function(){
  
  paste0("
  SELECT
    CT.CENTREID AS AvdRESH,
    MCE.MCEID AS ForlopsID,
	  P.ID AS PasientID,
	  
	  CASE WHEN MCE.INTERVENTION_TYPE = 4 THEN 'CT-Angio' END AS ForlopsType1,
	  CASE 
      WHEN MCE.MCETYPE = 1 THEN 'Planlagt'
      WHEN MCE.MCETYPE = 2 THEN 'Akutt'
      WHEN MCE.MCETYPE = 3 THEN 'Subakutt'
    END AS Hastegrad,
    
	  CASE (P.LOCAL_HOSPITAL) WHEN 999
		  THEN P.LOCAL_HOSPITAL_OTHER
		  ELSE (SELECT NAME FROM hospital WHERE hospital.ID = P.LOCAL_HOSPITAL)
	    END AS Lokalsykehus,
	    
	  CT.CTDAT AS UndersokDato,
	  
	  CASE
      WHEN IFNULL(P.GENDER,0) = 0 THEN 'Ikke angitt'
      WHEN P.GENDER = 1 THEN 'Mann'
      WHEN P.GENDER = 2 THEN 'Kvinne'
      ELSE 'Ukjent'
    END AS PasientKjonn,	 
    P.BIRTH_DATE FodselsDato,

	  CT.HEIGHT AS Hoyde,
	  CT.WEIGHT AS Vekt,
	  CT.SKREATININ AS SKreatinin,
	  CT.TIDPCI AS TidligerePCI,
	  CT.TIDCABG AS TidligereACB,
	  CT.DIABETES AS Diabetes,
	  CT.DIABETESINSULIN AS Insulin,
	  CT.TOBAK AS RoykingFoer2009,
	  CT.SMOKING_STATUS AS Royking,
	  CT.HYPERTON AS Hypertoni,
	  CT.HYPERLIP AS Lipidsenkende,
	  CT.TIDINF AS TidligereInfarkt,

   (SELECT GROUP_CONCAT(CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME)) FROM ctangio_operator_mapping ctop, people peo where ctop.PEOPLEID = peo.PEOPLEID and ctop.MCEID = CT.MCEID) AS Granskere,

	  CT.INDIKATION AS Indikasjon,
	  CT.VIKTIGSYMP AS ViktigsteSymptomer,
	  CT.TIDLIGSLUTT AS SluttForKontrast,
	  CT.HIGHCALCIUMSCORE AS HoyCalciumScore,
	  CT.TIDLIGSLUTTANNET AS SluttForKontrastAndreGrunner,

	  CT.CABG AS StenoseOkklusjonABC,
  	CT.CALCIUMSCORE AS CalciumScore,
    CT.CORONARY_SURVEY AS KoronarundersokelseUtfort,
	  CT.KONKLUSIVUND AS Konklusiv,
	  CT.STENOS AS StenoseOkklusjonTidligere,

  	CT.FYND AS Funn,
	  CAST((SELECT GROUP_CONCAT(CONCAT('Graft: ', CTF.GRAFT, '. Segment: ', CTF.SEGMENT, '. Grad: ', CTF.STENOSGRAD, '.  ')) FROM ctfinding CTF where CTF.MCEID = CT.MCEID) AS CHAR(400)) AS FunnConcat,

  	F.SEGMENT1,
  	F.SEGMENT2,
  	F.SEGMENT3,
  	F.SEGMENT4,
  	F.SEGMENT5,
  	F.SEGMENT6,
  	F.SEGMENT7,
  	F.SEGMENT8,
  	F.SEGMENT9,
  	F.SEGMENT10,
  	F.SEGMENT11,
  	F.SEGMENT12,
  	F.SEGMENT13,
  	F.SEGMENT14,
  	F.SEGMENT15,
  	F.SEGMENT16,
  	F.SEGMENT17,
  	F.SEGMENT18,
  	F.SEGMENT19,
  	F.SEGMENT20,

  	CT.MYOCARDIAL_MASS AS Myokardmasse,
   	CT.CORONARY_ANOMALY AS KoronarAnomali,
	  CT.FUNCTIONAL_SURVEY AS FunksjonsUndersokelse,
  	CT.DIASTOLIC_VOLUME AS EndediastoliskVolum,
  	CT.END_SYSTOLIC_VOLUME AS EndesystoliskVolum,
  	CT.EJECTION_FRACTION AS EjeksjonsFraksjon,
  	CT.CARDIAC_OUTPUT AS HjerteminuttVolum,
  
  	CT.FURTHER_ASSESS_TREAT AS VidereUtredning,
  
  	CT.ADJUVANT AS CTAngioMedikamenter,
  	CT.ADJBETA AS Betablokkere,
  	CT.ADJNITRO AS Nitroglycerin,
  	CT.ADJANN AS AnnetAdjuvant,
  
    CT.CTTEKNIK AS CTTeknikk,
    CT.CTTEKNIK_SPECIFY AS ProspektivEKG,
  	CT.DLP AS DLP,
  	CT.KONTRASTMEDEL AS Kontrastmiddel,
  	CT.KONTRASTMEDELMANGD AS KontrastmiddelMengde,
  
    CT.LABKOMP AS Komplikasjon,
    CT.LABALLERGILATT AS LettAllergisk,
    CT.LABALLERGIALLV AS AlvorligAllergisk,
    CT.LABHEMO AS Hemodynamisk,
    
   	P.SSN_TYPE AS FnrType,
   	P.SSNSUBTYPE AS FnrSubtype,
   	P.DECEASED AS AvdodFReg,
	  P.DECEASED_DATE as AvdodDatoFReg,

   	-- Study information
    (SELECT
      GROUP_CONCAT(
         IF ((DATEDIFF(P.REGISTERED_DATE, PS.PasInklDato) > 0) AND (DATEDIFF(P.REGISTERED_DATE, PS.StudieAvsluttDato) < 0 OR PS.StudieAvsluttDato IS NULL), CONCAT(PS.StudieNavn), NULL))
      FROM pasienterstudier PS
      WHERE PS.PasientID = MCE.PATIENT_ID)
    AS Studie,
    
    P.MUNICIPALITY_NAME AS Kommune,
    P.MUNICIPALITY_NUMBER AS KommuneNr,
    CAST(NULL AS CHAR(50)) AS Fylke,
    CAST(NULL AS CHAR(2)) AS Fylkenr,
    MCE.PARENT_MCEID as KobletForlopsID, 
    CT.STATUS AS SkjemaStatus 
    
    FROM mce MCE
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      INNER JOIN ctangio CT ON MCE.MCEID = CT.MCEID
      LEFT JOIN ctfindingstatic F ON MCE.MCEID = F.MCEID
      WHERE MCE.INTERVENTION_TYPE = 4
  ")
}


#' @rdname getQuery
#' @export
queryAortaklaffvarnum <- function(){
  paste0("
  SELECT
    T.CENTREID AS AvdRESH,
    T.MCEID AS ForlopsID,
    P.ID AS PasientID,
    CASE WHEN MCE.INTERVENTION_TYPE = 5 THEN 'Aortaklaff' END AS ForlopsType1,
    CASE 
      WHEN MCE.MCETYPE = 1 THEN 'Planlagt'
      WHEN MCE.MCETYPE = 2 THEN 'Akutt'
      WHEN MCE.MCETYPE = 3 THEN 'Subakutt'
    END AS ForlopsType2,
  
    -- Perkutane aortaklaffer
    T.SCREENING AS ScreeningBeslutning,
    T.COMPLETED_PROCEDURE AS Prosedyre,
    T.INDICATION AS Indikasjon,
    T.SCREENINGDATE AS BeslutningsDato,
    T.PROCEDUREDATE AS ProsedyreDato,
    T.AKUTOP AS AkuttOperasjon,
  
    -- Kliniske bakgrunnsdata
    CASE
      WHEN IFNULL(P.GENDER,0) = 0 THEN 'Ikke angitt'
      WHEN P.GENDER = 1 THEN 'Mann'
      WHEN P.GENDER = 2 THEN 'Kvinne'
      ELSE 'Ukjent'
    END AS PasientKjonn,
    P.BIRTH_DATE AS FodselsDato,

    T.HEIGHT AS Hoyde,
    T.HEIGHT_MISS AS HoydeUkjent,
    T.WEIGHT AS Vekt,
    T.WEIGHT_MISS AS VektUkjent,
    T.SMOKING AS Royker,
    T.S_CREATININ AS SKreatininFoer,
    T.S_CREATININ_MISS AS SKreatininIkkeUtfort,
    T.PROBNP AS ProBNPFoer,
    T.PROBNPNT AS NTProBNPFoer,
    T.HEMOGLOBIN AS HemoglobinFoer, -- Added in v1.13 as NOR-1345
  
    -- Tidligere sykdommer/behandling
    T.HYPERTENSION AS BehHypertoni,
    T.DIABETES AS Diabetes,
    T.DIABETESINSULIN AS Insulin,
    T.ATRIAL_FIBRILLATION AS Atrieflimmer,
    T.PREVIOUS_MI AS InfarktSiste90d,
    T.PRIOR_CARDIAC_SURGERY AS TidlHjerteoperasjon,
    T.PRIOR_CARDIAC_SURGERY_ACB AS TidlACB,
    T.PRIOR_CARDIAC_SURGERY_AVR AS TidlAVR,
    T.PRIOR_CARDIAC_SURGERY_MITRALPLASTIKK AS TidlMitralplastikk,
    T.PRIOR_CARDIAC_SURGERY_MVR AS TidlMVR,
    T.PRIOR_CARDIAC_SURGERY_OTHER AS TidlAnnet,
    T.PREVIOUS_PCI AS TidlPCI,
    T.PREVIOUS_STROKE AS TidlHjerneslag,
    T.PACEMACER_IMPLANT AS Pacemaker,
    T.CHRONIC_PULMONARY_DISEASE AS KOLS,
    T.PERIF_VESSELDISEASE AS PeriferKarsykdom,
    T.OTHER_SERIOUS_ILLNESS AS AnnenAlvorligSykdom,
    
    -- Aktuell preoperativ status
    T.NYHA AS NYHAKlasse,
    T.CANADIAN AS CanadianClass,
    T.FRAILTY AS Frailty,
    T.NEUROLOGIC_DIS AS RedusertMobilitet,
    T.WALKINGTEST AS Gangtest,
    T.WALKINGTEST_MISS AS GangtestIkkeUtfort,
    T.WALKINGTESTSPEED AS GangHastigtest,
    T.WALKINGTESTSPEED_MISS AS GangHastigtestIkkeUtfort,
    T.GRIPTEST AS Gripestyrke,
    T.EURO2_DIALYSIS AS DialyseFoerOp,
    T.KRITISKT AS KritiskPreopTilstand,
    T.EURO2_URGENCY AS Hastegrad,
    
    -- Kontraindikasjon mot kirurgi
    T.PERC_VALVE_DUE_TO_RISK AS PerkKlaffPgaRisiko,
    T.PERC_VALVE_RISK_AGE AS PerkKlaffPgaRisikoAlder,
    T.PERC_VALVE_RISK_SPIRO AS PerkKlaffPgaRisikoSpiro,
    T.PERC_VALVE_RISK_FORMER_ACB AS PerkKlaffPgaRisikoACB,
    T.COUNTERINDICATION AS PerkKlaffPgaRisikoSpesiell,
    T.OTHERMORBREASON0 AS Porselenaorta,
    T.OTHERMORBREASON1 AS Malignitet,
    T.OTHERMORBREASON3 AS UgunstigAnatomi,
    T.OTHERMORBREASON2 AS Steroidbehandling,
    T.OTHERMORBREASON4 AS Stralebehandling,
    T.OTHERMORBREASON5 AS Thoraxdeformitet,
    T.OTHERMORB AS AnnenAlvorligSykdomKirRisiko,
    T.PERC_VALVE_DUE_TO_PATIENT AS PerkKlaffPgaPasient,
    T.PRESENT_HEALTH_STAT AS Helsetilstand,
    
    -- EKKO-funn fC8r prosedyre
    T.VESSELAREA AS Klaffeareal,
    T.VESSELAREA_MISS AS KlaffearealIkkeBeregnet,
    T.ADJUSTED_VESSELAREA AS KorrigertKlaffeareal,
    T.MAXGRADIENT AS PreMaxgradient,
    T.MAXGRADIENT_MSEK AS PreMaxHastighet,
    T.MAXGRADIENT_MSEK_MISS AS PreMaxHastMangler,
    T.AVERAGEGRADIENT AS PreMiddelgradient,
    T.AVERAGEGRADIENT_MISS AS PreMiddelgradientMangler,
    T.LEFT_VENTRICULAR_FUNCTION AS PreVenstreVentrikkelFunksjon2011,
    T.LEFT_VENTRICULAR_FUNCTION_EURO2 AS PreVenstreVentrikkelFunksjon,
    T.PULMHYPERTENSION AS PulmonalHypertensjon,
    T.AORTAINS AS PreAortainsuffisiens,
    T.MITRALISINS AS PreMitralisinsuffisiens,
  
    -- CT-funn
    T.CTPERFORMED AS ErCTForetatt,
    T.OSTIEDIAM AS AnnulusdiameterVedCT,
    T.ANNULUSAORTA AS AnnulusAreal,
    T.ANNULUSAORTA_MM AS AnnulusArealmm,
    T.PERIMETER AS AnnulusPerimeter,
    T.AORTA_CALCIFICATION AS Aortaforkalk,
    T.ASCENDING_AORTA_CALCIFICATION AS AortaKalkAscendens,
    
    T.TYPEOF_FLAP AS TypeKlaff,
    
    -- OperatC8rer
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = T.MAIN_OPERATOR ) AS HovedOperator,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = T.SECOND_OPERATOR ) AS AndreOperator,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = T.THIRD_OPERATOR ) AS TredjeOperator,
    (SELECT GROUP_CONCAT(CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME)) FROM taviperc_operator_mapping tavi_op, people peo where tavi_op.PEOPLEID = peo.PEOPLEID and tavi_op.MCEID = T.MCEID) AS Operatorer,
    
    -- Prosedyrevariabler
    T.PUNCTIONTIME AS Punksjonstid,
    T.ENDTIME AS Avslutningstid,
    T.MITRALISINS AS Mitralisinsuffisiens,
    T.PROCEDURETYPE AS OperativTilgang,
    T.VESSELINVESSEL AS KlaffIKlaff,
    (SELECT v.NAME FROM valve v WHERE T.INSTRUMENTTYPE = v.ID) AS TypeKlaffeprotese,
    T.PROSTHESIS_SIZE AS ProteseStr,
    T.PREDILATATION AS Predilatasjon,
    T.BALLOONSIZEPRE AS PredilBallongStr,
    T.AFTERDILATATION AS Postdilatasjon,
    T.BALLOONSIZEPOST AS PostdilBallongStr,
    T.PROTECTIONDEVICE AS ProtectionDevice,
    T.SEALING AS Karlukning,
    T.RAPIDPACING AS RapidPacing,
    T.ANESTHESIA AS Anestesi,
    T.SUCCESSFULPROC AS VellykketProsedyre,
    
    -- StrC%ledata og kontrast
    T.LABNO AS Labnr,
    T.BEAMDOSE AS Straaledose,
    T.LIGHTTIME AS GjennomLysTid,
    T.CONTRASTAGENT AS Kontrastmiddel,
    T.CONTRASTAMOUNT AS Kontrastmengde,
    
    -- Komplikasjoner pC% lab
    T.LABKOMP AS LabKomplikasjon,
    T.LABBEHARYTMI AS LabKompArytmi,
    T.LABNEURO AS LabKompNeurologi,
    T.LABTAMP AS LabKompTamponade,
    T.LABVASCULAR AS LabKompVaskular,
    T.LABBLEEDING AS LabKompBlodning,
    T.LABSURGVESSEL AS LabKompAkuttKlaff,
    T.LABSURGVASC AS LabKompAkuttVaskular,
    T.LABCORONAR AS LabKompOkklusjon,
    T.LABANESTHESI AS LabKompAnestesi,
    T.LABHLMACHINE AS LabKompHLMaskin,
    T.LABKLAFF AS LabKompProtese,
    T.LABEMBOLKLAFF AS LabKompEmboli,
    T.LABOTHER AS LabKompAnnenKomp,
    T.LABDECEASED AS LabKompDod,
    T.LAB_DECEASED_DATE AS LabKompDodsdato,
  
    -- Komplikasjoner pC% avdelingen
    TD.AVDCOMP AS AvdKomplikasjon,
    TD.AVDSTROKE AS AvdKompHjerneslag,
    TD.AVDSTROKE_DEGREE AS AvdKompHjerneslagGrad,
    TD.AVDTIA AS AvdKompTIA,
    TD.AVDTAMPONAD AS AvdKompTamponade,
    TD.AVDPACEMAKER AS AvdKompPacemaker,
    TD.AVDATRIALFIB AS AvdKompAtrieflimmer,
    TD.AVDMI AS AvdKompHjerteinfarkt,
    TD.AVDVASCULAR AS AvdKompVaskular,
    TD.AVDBLEEDING AS AvdKompBlodning,
    TD.AVDBLEEDING_DEGREE AS AvdKompBlodningGrad,
    TD.AVDINFECTION AS AvdKompInfeksjon,
    TD.AVDDIALYSIS AS AvdKompDialyse,
    TD.AVDOTHER AS AvdKompAnnenKomp,
    TD.AVDDECEASED AS AvdKompDod,
    TD.AVD_DECEASED_DATE AS AvdKompDodsdato,
    
    -- Postoperative EKKO-funn
    TD.POSTPERFORMED AS PostUndersokelseForetatt,
    TD.POSTMAXGRADIENT AS PostMaxgradient,
    TD.POSTMAXGRADIENT_MSEK AS PostMaxHastighet,
    TD.POSTAVERAGEGRADIENT AS PostMiddelgradient,
    TD.POSTAVERAGEGRADIENT_MISS AS PostMiddelgradientMangler,
    TD.POSTLEFT_VENTRICULAR_FUNCTION AS PostVenstreVentrikkelFunksjon,
    TD.POSTPULMHYPERTENSION AS PostPulmonalHypertensjon,
    TD.VALVULAR_POSTAORTAINS AS PostAortainsuffisiens,
    TD.PARAVALVULAR_LEAK AS ParavalvularLekkasje,
    TD.PARAVALVULAR_INSUFFICIENCY AS ParavalvularInsuffisiens,
    TD.POSTMITRALISINS AS PostMitralisinsuffisiens,
    
    -- Utskrivelse
    TD.CREATININMAX AS UtskrKreatinin,
    TD.CREATININMAX_MISS AS UtskrKreatininIkkeUtfort,
    TD.DISCHARGEDATE AS UtskrDato,
    TD.DISCHARGETO AS UtskrevetTil,
  
    -- Antikoagulantia og platehemmere ved utskrivelse
    TD.ASA_DISCHARGE AS ASAVedUtskrivelse,
    TD.ANTICOAGULANTS_DISCHARGE AS AntikoagulantiaVedUtskrivelse,
    TD.OTHER_ANTIPLATELET_DISCHARGE AS AndrePlatehemmereVedUtskrivelse,
    
    -- Pasientinfo
    P.SSN_TYPE AS FnrType,
    P.SSNSUBTYPE AS FnrSubType,
    P.DECEASED AS AvdodFReg,
    P.DECEASED_DATE AS DodsdatoFReg,
    P.MUNICIPALITY_NAME AS Kommune,
    P.MUNICIPALITY_NUMBER AS KommuneNr,
	  CAST(NULL AS CHAR(50)) AS Fylke,
	  CAST(NULL AS CHAR(2)) AS Fylkenr,
    MCE.PARENT_MCEID as KobletForlopsID,
    
     -- Study information
    (SELECT
      GROUP_CONCAT(
        IF ((DATEDIFF(P.REGISTERED_DATE, PS.PasInklDato) > 0) AND (DATEDIFF(P.REGISTERED_DATE, PS.StudieAvsluttDato) < 0 OR PS.StudieAvsluttDato IS NULL), CONCAT(PS.StudieNavn), NULL))
      FROM pasienterstudier PS
      WHERE PS.PasientID = MCE.PATIENT_ID)
    AS Studie,
  
    LEAST(T.STATUS, TD.STATUS) AS SkjemaStatus,
    T.STATUS AS SkjemaStatusHovedskjema,
    TD.STATUS AS SkjemaStatusKomplUtsk
  
    FROM mce MCE
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      INNER JOIN taviperc T ON MCE.MCEID = T.MCEID
      INNER JOIN tavidischarge TD ON MCE.MCEID = TD.MCEID
")
}



#' @rdname getQuery
#' @export
queryAortaklaffoppfvarnum <- function(){
  paste0("
  SELECT
	  TF.CENTREID AS AvdRESH,
	  TF.MCEID AS ForlopsID,
	  T.MCEID AS BasisForlopsID,

    T.SCREENING AS BasisScreeningBeslutning,
	  T.SCREENINGDATE AS BasisBeslutningsDato,
	  T.PROCEDUREDATE AS BasisProsedyreDato,
	  TF.FOLLOWUPDATE AS OppfDato,
    TF.FOLLOWUP_TYPE AS OppfType,
    TF.AVDDECEASED AS OppfAvdod,
	  TF.DECEASEDDATE AS OppfAvdodDato,
    TF.PROC_RELATED_DEATH AS OppfDodProsRelatert,

  	-- OppfC8lging
	  TF.HEIGHT AS Hoyde,
    TF.HEIGHT_MISS AS HoydeUkjent,
    TF.WEIGHT AS Vekt,
    TF.WEIGHT_MISS AS VektUkjent,
    TF.NYHA AS NYHA,
    T.CANADIAN AS CanadianClass,
	  TF.WALKINGTEST AS Gangtest,
    TF.WALKINGTEST_MISS AS GangtestIkkeUtfort,
    TF.WALKINGTESTSPEED AS GangHastigtest,
    TF.WALKINGTESTSPEED_MISS AS GangHastigtestIkkeUtfort,
  	TF.S_CREATININ AS SKreatinin,
    TF.S_CREATININ_MISS AS SKreatininIkkeUtfort,
    TF.PROBNP AS ProBNP,
    TF.PROBNPNT AS NTProBNP,
  	TF.HEMOGLOBIN AS Hemoglobin,
	  TF.HEMOGLOBIN_MISS AS HemoglobinUkjent,

  	-- EKKO-funn
  	TF.MAXGRADIENT AS MaxGradient,
  	TF.MAXGRADIENT_MSEK AS MaxGradientMPS,
  	TF.AVERAGEGRADIENT AS Middelgradient,
  	TF.AVERAGEGRADIENT_MISS AS MiddelgradientMangler,
    TF.LEFT_VENTRICULAR_FUNCTION AS VenstreVentrikkelFunksjon,
    TF.PULMHYPERTENSION AS PulmonalHypertensjon,
    TF.AORTAINS AS Aortainsuffisiens,
    TF.PARAVALVULAR_LEAK AS ParavalvularLekkasje,
    TF.PARAVALVULAR_INSUFFICIENCY AS ParavalvularInsuffisiens,
    TF.MITRALISINS AS Mitralinsuffisiens,

    TF.PRESENT_HEALTH_STAT AS Helsetilstand,

    -- Komplikasjoner
    TF.COMP AS Komplikasjoner,
    TF.ENDOCARDIT AS Endokarditt,
    TF.DIALYSIS AS Dialyse,
    TF.MI AS Hjerteinfarkt,
    TF.STROKE AS Hjerneslag,
    TF.PACEMAKER AS Pacemaker,
    TF.PROSTHESIS AS ProteseDysfunk,
    TF.VESSEL AS SenKarKomp,
    TF.COMPOTHER AS AnnenKomp,

	  TF.STATUS AS SkjemaStatus
    FROM mce MCE
      INNER JOIN tavipercfollowup TF ON MCE.MCEID = TF.MCEID
      LEFT JOIN taviperc T ON MCE.PARENT_MCEID = T.MCEID
 ")
}


#' @rdname getQuery
#' @export
queryAndreprosedyrervarnum <-function(){
  paste0("
  SELECT
    other.CENTREID AS AvdRESH,
    other.MCEID AS ForlopsID,
    P.ID AS PasientID,

    CASE WHEN MCE.INTERVENTION_TYPE = 7 THEN 'Andre prosedyrer' END AS ForlopsType1,
    CASE 
      WHEN MCE.MCETYPE = 1 THEN 'Planlagt'
      WHEN MCE.MCETYPE = 2 THEN 'Akutt'
      WHEN MCE.MCETYPE = 3 THEN 'Subakutt'
    END AS Hastegrad,
    CASE
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NOT NULL THEN 'Sekundær'
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NULL THEN 'Primær'
      ELSE NULL
    END AS Regtype,
    
    CASE
      WHEN IFNULL(P.GENDER,0) = 0 THEN 'Ikke angitt'
      WHEN P.GENDER = 1 THEN 'Mann'
      WHEN P.GENDER = 2 THEN 'Kvinne'
      ELSE 'Ukjent'
    END AS PasientKjonn,
    P.BIRTH_DATE AS FodselsDato,
    
    -- Andre prosedyrer
    other.PROCEDUREDATE AS ProsedyreDato,
    other.PROCEDUREDATE_TIME AS ProsedyreTid,
    other.PROCEDUREDATE_TIME_MISSING AS ProsedyreTidUkjent,
    other.PROCEDURETYPE AS AnnenProsType,
    
    -- OperatC8r
    (SELECT GROUP_CONCAT(CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME)) FROM other_operator_mapping oom, people peo where oom.PEOPLEID = peo.PEOPLEID and oom.MCEID = other.MCEID) AS AndreProsOperatorer,
    
    -- Komplikasjon pC% lab
    other.LABKOMP AS Komplikasjon,
    other.LABALLERGILATT AS LettAllergi,
    other.LABALLERGIALLV AS ModeratAllergi,
    other.LABBEHARYTMI AS Arytmi,
    other.LABHEMO AS HemodynKomp,
    other.LABNEURO AS NeuroKomp,
    other.LABVASK AS VaskulKomp,
    other.LABPERF AS Perforasjon,
    other.LABTAMP AS Tamponade,
    other.LABAKUTCABG AS AkuttACB,
    other.LABANNANALLV AS AnnenAlvorligKomp,
    other.LABDODSFALL AS Dod,
    other.LABPROCEDURDOD AS ProsRelatertDod,
    
    P.MUNICIPALITY_NAME AS Kommune,
    P.MUNICIPALITY_NUMBER AS KommuneNr,
    CAST(NULL AS CHAR(50)) AS Fylke,
    CAST(NULL AS CHAR(2)) AS Fylkenr,
    MCE.PARENT_MCEID as KobletForlopsID, 
    other.STATUS AS SkjemaStatus

    FROM mce MCE
    INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
    INNER JOIN other ON MCE.MCEID = other.MCEID
  ")
}




#' @rdname getQuery
#' @export
queryAnnendiagnostikkvarnum <-function(){
  
  paste0("
  SELECT
    MCE.CENTREID AS AvdRESH,
    diag.MCEID AS ForlopsID,
    P.ID AS PasientID,
    CASE 
      WHEN MCE.INTERVENTION_TYPE = 1 THEN 'Angio'
      WHEN MCE.INTERVENTION_TYPE = 2 THEN 'PCI'
      WHEN MCE.INTERVENTION_TYPE = 3 THEN 'Angio+PCI'
    END AS ForlopsType1,
    CASE 
     WHEN MCE.MCETYPE = 1 THEN 'Planlagt'
     WHEN MCE.MCETYPE = 2 THEN 'Akutt'
     WHEN MCE.MCETYPE = 3 THEN 'Subakutt'
    END AS Hastegrad,
    CASE
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NOT NULL THEN 'Sekundær'
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NULL THEN 'Primær'
      ELSE NULL
    END AS Regtype,    
    
    CASE
      WHEN IFNULL(P.GENDER,0) = 0 THEN 'Ikke angitt'
      WHEN P.GENDER = 1 THEN 'Mann'
      WHEN P.GENDER = 2 THEN 'Kvinne'
      ELSE 'Ukjent'
    END AS PasientKjonn,
    P.BIRTH_DATE as FodselsDato,
  

    R.INDIKATION  AS Indikasjon,
    R.INTERDAT as ProsedyreDato,
    R.HEIGHT as Hoyde,
    R.WEIGHT as Vekt,
    diag.SEGMENT  AS segment,
    diag.GRAFT  AS graft,
    diag.METHODUSED  AS metode,
    diag.FFR_BEFORE AS FfrFoer,
    diag.FFR_AFTER as FfrEtter,
    diag.IFR_BEFORE as IfrFoer,
    diag.IFR_AFTER as IfrEtter,
    diag.CSA_BEFORE as CsaFoer,
    diag.CSA_AFTER as CsaEtter,
    diag.MLD_BEFORE as MldFoer,
    diag.MLD_EAFTER as MldEtter,
    diag.MLA_BEFORE as MlaFoer,
    diag.MLA_AFTER as MlaEtter,
    diag.MXLCBI_BEFORE as MxlcbiFoer,
    diag.MXLCBI_AFTER as MxlcbiEtter,
    diag.CFR_BEFORE as CfrFoer,
    diag.CFR_AFTER as CfrEtter,
    diag.IMR_BEFORE as ImrFoer,
    diag.IMR_AFTER as ImrEtter,
    diag.PDPA_BEFORE as PdpaFoer,
    diag.PDPA_AFTER as PdpaEtter,
    diag.PAH_BEFORE as PahFoer,
    diag.PAH_AFTER as PahEtter,
    diag.PDH_BEFORE as PdhFoer,
    diag.PDH_AFTER as PdhEtter,
    
        
    (SELECT CONCAT(FIRSTNAME, ' ', LASTNAME) as name from people where people.PEOPLEID = R.MAIN_ANGIOGRAFOR ) AS Angiografor1,
    (SELECT CONCAT(FIRSTNAME, ' ', LASTNAME) as name from people where people.PEOPLEID = R.SECOND_ANGIOGRAFOR ) AS Angiografor2,
    (SELECT CONCAT(FIRSTNAME, ' ', LASTNAME) as name from people where people.PEOPLEID = R.THIRD_ANGIOGRAFOR ) AS Angiografor3,
    
    P.MUNICIPALITY_NAME AS Kommune,
    P.MUNICIPALITY_NUMBER AS KommuneNr,
    CAST(NULL AS CHAR(50)) AS Fylke,
    CAST(NULL AS CHAR(2)) AS Fylkenr,
    MCE.PARENT_MCEID as KobletForlopsID
    
    FROM diagnostics diag
      INNER JOIN mce MCE ON diag.MCEID = MCE.MCEID
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      LEFT JOIN regangio R ON diag.MCEID = R.MCEID
  ")
}




#' @rdname getQuery
#' @export
querySegmentstentnum <-function(){
  paste0("
  SELECT
    MCE.CENTREID AS AvdRESH,
    S.ID AS SegmentID,
    S.MCEID AS ForlopsID,
    P.ID AS PasientID,
    
    CASE 
      WHEN MCE.INTERVENTION_TYPE = 1 THEN 'Angio'
      WHEN MCE.INTERVENTION_TYPE = 2 THEN 'PCI'
      WHEN MCE.INTERVENTION_TYPE = 3 THEN 'Angio+PCI'
    END AS ForlopsType1,
    
    CASE 
     WHEN MCE.MCETYPE = 1 THEN 'Planlagt'
     WHEN MCE.MCETYPE = 2 THEN 'Akutt'
     WHEN MCE.MCETYPE = 3 THEN 'Subakutt'
    END AS Hastegrad,
    
    CASE
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NOT NULL THEN 'Sekundær'
      WHEN  MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NULL THEN 'Primær'
      ELSE NULL
    END AS Regtype,
   
    CASE
      WHEN IFNULL(P.GENDER,0) = 0 THEN 'Ikke angitt'
      WHEN P.GENDER = 1 THEN 'Mann'
      WHEN P.GENDER = 2 THEN 'Kvinne'
      ELSE 'Ukjent'
    END AS PasientKjonn,
    P.BIRTH_DATE as FodselsDato,
    
    (SELECT CONCAT(FIRSTNAME, ' ', LASTNAME) as name from people where people.PEOPLEID = R.MAIN_ANGIOGRAFOR ) AS Angiografor1,
    (SELECT CONCAT(FIRSTNAME, ' ', LASTNAME) as name from people where people.PEOPLEID = R.SECOND_ANGIOGRAFOR ) AS Angiografor2,
    (SELECT CONCAT(FIRSTNAME, ' ', LASTNAME) as name from people where people.PEOPLEID = R.THIRD_ANGIOGRAFOR ) AS Angiografor3,
    
    R.INDIKATION  AS Indikasjon,
    R.INTERDAT as ProsedyreDato,
    R.HEIGHT as Hoyde,
    R.WEIGHT as Vekt,
    
    S.SEGMENT as Segment,
    S.STENT as StentID,
    ST.STENTNAMN AS Stentnavn,
    ST.DES  AS StentType,
    S.BALLONGLANGD as BallongLengde,
    S.DEBDIAM as DEBDiameter,
    S.DIAM as Diameter,
    S.EFTERDILATATION  AS Etterdilatasjon,
    S.FRAMGANG  AS LokalSuksess,
    S.GRAFT  AS Graft,
    S.LAKEMEDELSBALLONG  AS MedikamentellBallong,
    S.MAXTRYCKVIDDEB AS MaksTrykkDEB,
    S.OCKL  AS Okklusjon,
    S.PROCTYP  AS ProsedyreType,
    S.SEGMENT_STENT_TROMBOSE_TYPE  AS Stenttrombosetype,
    S.STENOSKLASS  AS Stenoseklasse,
    S.STENOSTYP  AS StenoseType,
    S.STENTLANGD AS Stentlengde,
    S.STENTSLUT  AS StentSlut,
    S.UPPBLASNINGSTIDDEB AS InflasjonsTidDEB,
    S.IVL_DIAM AS IVLDiameter,
    S.NUMBERPULSES AS AntallPulser,
    
    P.MUNICIPALITY_NAME AS Kommune,
    P.MUNICIPALITY_NUMBER AS KommuneNr,
    CAST(NULL AS CHAR(50)) AS Fylke,
    CAST(NULL AS CHAR(2)) AS Fylkenr,
    MCE.PARENT_MCEID as KobletForlopsID

    FROM segment S
    LEFT  JOIN stent ST ON S.STENT = ST.SID
    INNER JOIN mce MCE ON S.MCEID = MCE.MCEID
    INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
    LEFT JOIN regangio R ON S.MCEID = R.MCEID
")
}





#' @rdname getQuery
#' @export
queryMitralklaffvarnum <-function(){
  paste0("
  SELECT
	  T.CENTREID AS AvdRESH,
	  T.MCEID AS ForlopsID,
	  P.ID AS PasientID,
	  
    CASE WHEN MCE.INTERVENTION_TYPE = 6 THEN 'Mitralklaff' END AS ForlopsType1,
    CASE 
       WHEN MCE.MCETYPE = 1 THEN 'Planlagt'
       WHEN MCE.MCETYPE = 2 THEN 'Akutt'
       WHEN MCE.MCETYPE = 3 THEN 'Subakutt'
     END AS ForlopsType2,
     
     CASE
       WHEN IFNULL(P.GENDER,0) = 0 THEN 'Ikke angitt'
       WHEN P.GENDER = 1 THEN 'Mann'
       WHEN P.GENDER = 2 THEN 'Kvinne'
       ELSE 'Ukjent'
     END AS PasientKjonn,
     P.BIRTH_DATE AS FodselsDato,

    
     -- Mitralklaff
     T.SCREENING AS ScreeningBeslutning,
     T.MITRAL_VALVE_TYPE AS Prosedyre,
     T.PROCEDUREDATE AS ProsedyreDato,
     
     -- Kliniske bakgrunnsdata
     T.HEIGHT AS Hoyde,
     T.HEIGHT_MISS AS HoydeUkjent,
     T.WEIGHT AS Vekt,
     T.WEIGHT_MISS AS VektUkjent,
     T.SMOKING AS Royker,
     T.S_CREATININ AS KreatininFoer,
     T.S_CREATININ_MISS AS KreatininUkjent,
     T.PROBNP AS ProBNP,
     T.PROBNPNT AS NTProBNPFoer,
     T.PROBNP_MISS AS ProBNPUkjent,
     T.HEMOGLOBIN AS HemoglobinFoer, -- Added in v1.13 as NOR-1345

     -- Tidligere sykdommer/behandling
     T.HYPERTENSION AS BehHypertoni,
     T.DIABETES AS Diabetes,
     T.DIABETESINSULIN AS Insulin,
     T.ATRIAL_FIBRILLATION AS Atrieflimmer,
     T.PREVIOUS_MI AS InfarktSiste90d,
     T.HEARTFAILURE AS AntInnleggelserSisteAar,
     T.HEARTFAILURE_MISS AS AntInnleggelserSisteAarUkjent,
     T.PRIOR_CARDIAC_SURGERY AS TidlHjerteoperasjon,
     T.PRIOR_CARDIAC_SURGERY_ACB AS TidlACB,
     T.PRIOR_CARDIAC_SURGERY_AVR AS TidlAVR,
     T.PRIOR_CARDIAC_SURGERY_MITRALPLASTIKK AS TidlMitralplastikk,
     T.PRIOR_CARDIAC_SURGERY_MVR AS TidlMVR,
     T.PRIOR_CARDIAC_SURGERY_OTHER AS TidlAnnet,
     T.PRIORCORRECTION AS TidlKorrigering,
     T.PREVIOUS_PCI AS TidlPCI,
     T.PREVIOUS_STROKE AS TidlHjerneslag,
     T.CHRONIC_PULMONARY_DISEASE AS KOLS,
     T.PERIF_VESSELDISEASE AS PeriferKarsykdom,

	   -- Aktuell preoperativ status
     T.NYHA AS NYHAKlasse,
     T.FRAILTY AS Frailty,
     T.NEUROLOGIC_DIS AS RedusertMobilitet,
     T.WALKINGTEST AS Gangtest,
     T.WALKINGTEST_MISS AS GangtestIkkeUtfort,
     T.WALKINGTESTSPEED AS GangHastigtest,
     T.WALKINGTESTSPEED_MISS AS GangHastigtestIkkeUtfort,
     T.GRIPTEST AS Gripestyrke,
     T.EURO2_DIALYSIS AS DialyseFoerOp,
     T.KRITISKT AS KritiskPreopTilstand,
     T.EURO2_URGENCY AS Hastegrad,

     -- Kontraindikasjon mot kirurgi
     T.COUNTERINDICATION AS Kontraindikasjon,
     T.OTHERMORBREASON0 AS Porselenaorta,
     T.OTHERMORBREASON1 AS Malignitet,
     T.OTHERMORBREASON3 AS UgunstigAnatomi,
     T.OTHERMORBREASON2 AS Steroidbehandling,
     T.OTHERMORBREASON4 AS Stralebehandling,
     T.OTHERMORBREASON5 AS Thoraxdeformitet,
     T.AVBOJD AS AvslaattForThorax,
     T.PRESENT_HEALTH_STAT AS Helsetilstand,

     -- EKKO-funn fC8r prosedyre
     T.LEFT_VENTRICULAR_FUNCTION_EURO2 AS PreVenstreVentrikkelFunksjon,
     T.AORTAINS AS PreAortainsuffisiens,
     T.AORTASTENOS AS PreAortastenose,
     T.TRICUSINSUFF AS PreTricuspidal,
     T.MITRALISSTENOS AS PreMitralstenose,
     T.MITRINSUFF AS PreMitralinsuffisiens,
     T.MAXSPEED_MSEK AS PreMaxHastighet,
     T.AVERAGEGRADIENT AS PreMiddelgradient,
     T.MREROA AS PreMREROA,
     T.MREROA_MISS AS PreMREROAUkjent,
     T.PULMONELLHYPERTENSION AS PreHoyreVentTrykk,

     -- OperatC8rer
     (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = T.MAIN_OPERATOR ) AS HovedOperator,
     (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = T.SECOND_OPERATOR ) AS AndreOperator,
     (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = T.THIRD_OPERATOR ) AS TredjeOperator,
     (SELECT GROUP_CONCAT(CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME)) FROM tavimitralis_operator_mapping tavi_op, people peo where tavi_op.PEOPLEID = peo.PEOPLEID and tavi_op.MCEID = T.MCEID) AS Operatorer,

      -- Prosedyrevariabler
     T.PUNCTIONTIME AS Punksjonstid,
     T.ENDTIME AS Avslutningstid,
     T.INTRODUCER AS IntroducerStr,
     T.INTRODUCER_MISS AS IntroducerStrUkjent,
     T.VESSELSEAL AS Karlukning,
     T.ANESTHESIA AS Anestesi,
     T.EKODURINGPROC AS ProsedyreEkko,
     (SELECT v.NAME FROM valve v WHERE T.INSTRUMENTTYPE = v.ID) AS TypeKlaffeprotese,
     T.SUCCESSFULPROC AS VellykketProsedyre,
	
     -- These are outcommented from the form, due to NOR-733:
     -- TAVIMITRALIS_CRT
     -- TAVIMITRALIS_ICD
     -- TAVIMITRALIS_QUALCLASS
     -- TAVIMITRALIS_PROLAPS, 6 variables
     -- TAVIMITRALIS_MRPISA (with _MISS)
     -- TAVIMITRALIS_VCONTRACTA (with MISS)
     -- TAVIMITRALIS_LUNGVENREVERSE
     -- TAVIMITRALIS_NRCLIP_ROW (with MISS)
     -- TAVIMITRALIS_ACCESS

      -- StrC%ledata og kontrast
     T.LABNO AS Labnr,
     T.BEAMDOSE AS Straaledose,
     T.BEAMDOSE_MISS AS StraaledoseUkjent,
     T.LIGHTTIME AS GjennomLysTid,
     T.CONTRASTAGENT AS Kontrastmiddel,
     T.CONTRASTAMOUNT AS Kontrastmengde,
     T.CONTRASTAMOUNT_MISS AS KontrastmengdeUkjent,

     -- Komplikasjoner pC% lab
     T.LABKOMP AS LabKomplikasjon,
     T.LABBEHARYTMI AS LabKompArytmi,
     T.LABNEURO AS LabKompNeurologi,
     T.LABTAMP AS LabKompTamponade,
     T.LABPACEMAKER AS LabKompPacemaker,
     T.LABVASCULAR AS LabKompVaskular,
     T.LABBLEEDING AS LabKompBlodning,
     T.LABSURGVESSEL AS LabKompAkuttKlaff,
     T.LABSURGVASC AS LabKompAkuttVaskular,
     T.LABANESTHESI AS LabKompAnestesi,
     T.LABHLMACHINE AS LabKompHLMaskin,
     T.LABEMBOLDEVICE AS LabKompEmbolisering,
     T.LABOTHER AS LabKompAnnenKomp,
     T.LABDECEASED AS LabKompDod,

     -- Komplikasjoner pC% avdelingen
     TD.AVDCOMP AS AvdKomplikasjon,
     TD.AVDSTROKE AS AvdKompHjerneslag,
     TD.AVDSTROKE_DEGREE AS AvdKompHjerneslagGrad,
     TD.AVDTIA AS AvdKompTIA,
     TD.AVDTAMPONAD AS AvdKompTamponade,
     TD.AVDPACEMAKER AS AvdKompPacemaker,
     TD.AVDATRIALFIB AS AvdKompAtrieflimmer,
     TD.AVDMI AS AvdKompHjerteinfarkt,
     TD.AVDVASCULAR AS AvdKompVaskular,
     TD.AVDBLEEDING AS AvdKompBlodning,
     TD.AVDBLEEDING_DEGREE AS AvdKompBlodningGrad,
     TD.AVDINFECTION AS AvdKompInfeksjon,
     TD.AVDDIALYSIS AS AvdKompDialyse,
     TD.AVDOTHER AS AvdKompAnnenKomp,
     TD.AVDDECEASED AS AvdKompDod,

     -- Postoperative EKKO-funn
     TD.POSTPERFORMED AS PostUndersokelseForetatt,
     TD.POSTQUALCLASS AS PostKlassifisering,
     TD.POSTPROLAPSA1 AS PostProlapsA1,
     TD.POSTPROLAPSA2 AS PostProlapsA2,
     TD.POSTPROLAPSA3 AS PostProlapsA3,
     TD.POSTPROLAPSP1 AS PostProlapsP1,
     TD.POSTPROLAPSP2 AS PostProlapsP2,
     TD.POSTPROLAPSP3 AS PostProlapsP3,
     TD.POSTLEFT_VENTRICULAR_FUNCTION_EURO2 AS PostVenstreVentrikkelFunksjon,
     TD.POSTTRICUS_INSUFF AS PostTricuspidal,
     TD.POSTMITRALISSTENOS AS PostMitralstenose,
     TD.POSTMITR_INSUFF AS PostMitralinsuffisiens,
     TD.POSTMAXGRADIENT AS PostMaxgradient,
     TD.POSTMAXSPEED_MSEK AS PostMaxHastighet,
     TD.POSTAVERAGEGRADIENT AS PostMiddelgradient,
     TD.POSTMRPISA AS PostMRPISA,
     TD.POSTMRPISA_MISS AS PostMRPISAUkjent,
     TD.POSTMREROA AS PostMREROA,
     TD.POSTMREROA_MISS AS PostMREROAUkjent,
	   TD.POSTVCONTRACTA AS PostVContracta,
     TD.POSTVCONTRACTA_MISS AS PostVContractaUkjent,
     TD.POSTLUNGVENREVERSE AS PostReversFlow,
     TD.POSTPULMONELLHYPERTENSION AS PostHoyreVentrikkelTrykk,
     TD.POSTPULMONELLHYPERTENSION_MISS AS PostHoyreVentrikkelTrykkUkjent,

     -- Utskrivelse
     TD.CREATININMAX AS MaxKreatinin,
     TD.CREATININMAX_MISS AS MaxKreatininUkjent,
     TD.DISCHARGEDATE AS UtskrDato,
     TD.DISCHARGETO AS UtskrevetTil,
     -- Antikoagulantia og platehemmere ved utskrivelse
     TD.ASA_DISCHARGE AS ASAVedUtskrivelse,
     TD.ANTICOAGULANTS_DISCHARGE AS AntikoagulantiaVedUtskrivelse,
     TD.OTHER_ANTIPLATELET_DISCHARGE AS AndrePlatehemmereVedUtskrivelse,
     
     P.SSN_TYPE AS FnrType,
     P.DECEASED AS AvdodFReg,
     P.DECEASED_DATE AS DodsdatoFReg,
     P.MUNICIPALITY_NAME AS Kommune,
     P.MUNICIPALITY_NUMBER AS KommuneNr,
     CAST(NULL AS CHAR(50)) AS Fylke,
     CAST(NULL AS CHAR(2)) AS Fylkenr,
     MCE.PARENT_MCEID as KobletForlopsID, 
  
     -- Study information
    (SELECT
      GROUP_CONCAT(
         IF ((DATEDIFF(P.REGISTERED_DATE, PS.PasInklDato) > 0) AND (DATEDIFF(P.REGISTERED_DATE, PS.StudieAvsluttDato) < 0 OR PS.StudieAvsluttDato IS NULL), CONCAT(PS.StudieNavn), NULL))
      FROM pasienterstudier PS
      WHERE PS.PasientID = MCE.PATIENT_ID)
    AS Studie,
  
    T.STATUS AS SkjemaStatusHovedskjema,
    TD.STATUS AS SkjemaStatusKomplUtskr,
	  LEAST(T.STATUS, TD.STATUS) AS SkjemaStatus

    FROM mce MCE
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      INNER JOIN tavimitralis T ON MCE.MCEID = T.MCEID
      INNER JOIN tavimitralisdischarge TD ON MCE.MCEID = TD.MCEID
 ")
}


#' @rdname getQuery
#' @export
queryMitralklaffoppfvarnum <-function(){
  paste0("
  SELECT
	  TF.CENTREID AS AvdRESH,
	  TF.MCEID AS ForlopsID,
	  T.MCEID AS BasisForlopsID,
    T.SCREENING AS BasisScreeningBeslutning,
  	T.PROCEDUREDATE AS BasisProsedyreDato,
  	TF.FOLLOWUPDATE AS OppfDato,
    TF.FOLLOWUP_TYPE AS OppfType,
    TF.DECEASED AS OppfAvdod,
	  TF.DECEASEDDATE AS OppfAvdodDato,
    TF.PROC_RELATED_DEATH AS OppfDodProsRelatert,

    -- OppfC8lging
    TF.HEIGHT AS Hoyde,
    TF.HEIGHT_MISS AS HoydeUkjent,
    TF.WEIGHT AS Vekt,
    TF.WEIGHT_MISS AS VektUkjent,
    TF.NYHA AS NYHA,
  	TF.WALKINGTEST AS Gangtest,
	  TF.WALKINGTEST_MISS AS GangtestIkkeUtfort,
    T.WALKINGTESTSPEED AS GangHastigtest,
    T.WALKINGTESTSPEED_MISS AS GangHastigtestIkkeUtfort,
	  TF.S_CREATININ AS Kreatinin,
  	TF.S_CREATININ_MISS AS KreatininUkjent,
    TF.PROBNP AS ProBNP,
    TF.PROBNPNT AS NTProBNP,

    -- EKKO-funn
    TF.PROLAPSA1 AS ProlapsA1,
    TF.PROLAPSA2 AS ProlapsA2,
    TF.PROLAPSA3 AS ProlapsA3,
    TF.PROLAPSP1 AS ProlapsP1,
    TF.PROLAPSP2 AS ProlapsP2,
    TF.PROLAPSP3 AS ProlapsP3,
    TF.LEFT_VENTRICULAR_FUNCTION AS VenstreVentrikkelFunksjon,
    TF.AORTAINS AS Aortainsuffisiens,
    TF.AORTAINS AS Aortastenose,
    TF.TRICUSINS AS Tricuspidal,
    TF.MITRALISINS AS Mitralinsuffisiens,
    TF.MITRALISSTENOS AS Mitralstenose,
    TF.MAXVELOCITY AS MaxHastighet,
    TF.AVGGRADIENT_MSEK AS MiddelgradientMPS,
    TF.AVERAGEGRADIENT AS Middelgradient,
    TF.MRPISA AS MRPISA,
    TF.MREROA AS MREROA,
    TF.VCONTRACTA AS VContracta,
    TF.LUNGVENSREVERS AS FlowRevers,
	  TF.PULMHYPERTENSION AS HoyreVentrikkelTrykk,
	  TF.PULMHYPERTENSION_MISS AS HoyreVentrikkelTrykkUkjent,

   	TF.PRESENT_HEALTH_STAT AS Helsetilstand,

    	-- Komplikasjoner
    TF.AVDCOMP AS Komplikasjoner,
    TF.AVDSTROKE AS Hjerneslag,
    TF.AVDTIA AS TIA,
    TF.AVDTAMPONAD AS Tamponade,
    TF.AVDPACEMAKER AS Pacemaker,
    TF.AVDATRIALFIB AS Atrieflimmer,
    TF.AVDMI AS Hjerteinfarkt,
    TF.AVDVASCULAR AS Vaskular,
    TF.AVDBLEEDING AS Blodning,
    TF.AVDINFECTION AS Infeksjon,
    TF.AVDDIALYSIS AS Dialyse,
    TF.AVDDEVICE AS DeviceRelKomp,
    TF.AVDOTHER AS AnnenKomp,

   	TF.STATUS AS SkjemaStatus
    FROM mce MCE
      INNER JOIN tavimitralisfollowup TF ON MCE.MCEID = TF.MCEID
      LEFT JOIN tavimitralis T ON MCE.PARENT_MCEID = T.MCEID
  ")
}

#' @rdname getQuery
#' @export
queryTaviprom <- function(){
  
  paste0("
  SELECT
    MCE.CENTREID AS AvdRESH,
    MCE.MCEID AS ForlopsID,
    P.ID AS PasientID,
    proms.REGISTRATION_TYPE AS Registreringstype,
    P.SSN_TYPE AS FnrType,
    tavi.PROCEDUREDATE AS ProsedyreDato,
    
    proms.TSSENDT AS ePromBestillingsdato,
    proms.TSRECEIVED AS ePromMottatt,
    proms.EXPIRY_DATE AS ePromUtloeptDato,
    proms.STATUS AS ePromStatus,
    
    r.Q01 AS rose01,
    r.Q02 AS rose02,
    r.Q03 AS rose03,
    r.Q04 AS rose04,
    r.Q05 AS rose05,
    r.FORM_COMPLETED_VIA_PROMS AS roseFerdigViaProm,
    r.TSUPDATED AS roseDato,
    r.STATUS AS roseStatus,
    
    h.Q01 AS heart01 ,
    h.Q02 AS heart02 ,
    h.Q03 AS heart03 ,
    h.Q04 AS heart04 ,
    h.Q05 AS heart05 ,
    h.Q06 AS heart06 ,
    h.Q07 AS heart07 ,
    h.Q08 AS heart08 ,
    h.Q09 AS heart09 ,
    h.Q10 AS heart10,
    h.Q11 AS heart11,
    h.Q12 AS heart12,
    h.Q13 AS heart13,
    h.Q14 AS heart14,
    h.FORM_COMPLETED_VIA_PROMS AS heartFerdigViaProm,
    h.TSUPDATED AS heartDato,
    h.STATUS AS hearStatus,
    
    m.Q01 AS min01,
    m.Q02 AS min02,
    m.Q03 AS min03,
    m.Q04 AS min04,
    m.Q05 AS min05,
    m.Q06 AS min06,
    m.Q07 AS min07,
    m.Q08 AS min08,
    m.Q09 AS min09,
    m.Q10 AS min10,
    m.Q11 AS min11,
    m.Q12 AS min12,
    m.Q13 AS min13,
    m.Q14 AS min14,
    m.Q15 AS min15,
    m.Q16 AS min16,
    m.Q17 AS min17,
    m.Q18 AS min18,
    m.Q19 AS min19,
    m.Q20 AS min20,
    m.Q21 AS min21,
    m.FORM_COMPLETED_VIA_PROMS AS minFerdigViaProm,
    m.TSUPDATED AS minDato,
    m.STATUS AS minStatus,
    
    tav.Q01 AS tavi01,
    tav.Q01_2 AS tavi01_2,
    tav.Q02 AS tavi02,
    tav.Q02_2 AS tavi02_2,
    tav.Q03 AS tavi03,
    tav.Q04 AS tavi04,
    tav.Q05 AS tavi05,
    tav.Q06 AS tavi06,
    tav.Q06_2 AS tavi06_2,
    tav.Q07 AS tavi07,
    tav.FORM_COMPLETED_VIA_PROMS AS taviFerdigViaProm,
    tav.TSUPDATED AS taviDato,
    tav.STATUS AS taviStatus,
    
    prem.Q01 AS prem01,
    prem.Q02 AS prem02,
    prem.Q03 AS prem03,
    prem.Q04 AS prem04,
    prem.Q05 AS prem05,
    prem.Q06 AS prem06,
    prem.Q07 AS prem07,
    prem.Q08 AS prem08,
    prem.Q09 AS prem09,
    prem.Q10 AS prem10,
    prem.Q11 AS prem11,
    prem.Q12 AS prem12,
    prem.Q13 AS prem13,
    prem.Q14 AS prem14,
    prem.Q15 AS prem15,
    prem.FORM_COMPLETED_VIA_PROMS AS premFerdigViaProm,
    prem.TSUPDATED AS premDato,
    prem.STATUS AS premStatus
    
    FROM 
      proms
      INNER JOIN mce MCE ON proms.MCEID = MCE.MCEID
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      LEFT JOIN rose_dyspnea_scale r ON MCE.MCEID = r.MCEID
      LEFT JOIN heart_qol h ON MCE.MCEID = h.MCEID
      LEFT JOIN minnesota_questionnaire m ON MCE.MCEID = m.MCEID
      LEFT JOIN taviperc tavi ON MCE.MCEID = tavi.MCEID
      LEFT JOIN tavi_additional_questions tav ON MCE.MCEID = tav.MCEID
      LEFT JOIN prem ON MCE.MCEID = prem.MCEID
      WHERE proms.REGISTRATION_TYPE LIKE 'TAVI%'
")
}


#' @rdname getQuery
#' @export
queryForlopsoversikt <-function(){
  
  paste0("
  SELECT
    MCE.CENTREID AS AvdRESH,
    P.ID AS PasientID,
    P.SSN_HASH AS KryptertFnr,
    MCE.MCEID AS ForlopsID,
    MCE.PARENT_MCEID as KobletForlopsID,
    CASE
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NOT NULL THEN 'Sekundær'
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NULL THEN 'Primær'
      ELSE NULL
    END AS Regtype,
    MCE.INTERVENTION_TYPE AS ForlopsType1Num,
    CASE 
      WHEN MCE.INTERVENTION_TYPE = 1 THEN 'Angio'
      WHEN MCE.INTERVENTION_TYPE = 2 THEN 'PCI'
      WHEN MCE.INTERVENTION_TYPE = 3 THEN 'Angio+PCI'
      WHEN MCE.INTERVENTION_TYPE = 4 THEN 'CT-Angio'
      WHEN MCE.INTERVENTION_TYPE = 5 THEN 'Aortaklaff'
      WHEN MCE.INTERVENTION_TYPE = 6 THEN 'Mitralklaff'
      WHEN MCE.INTERVENTION_TYPE = 7 THEN 'Andre prosedyrer'
    END AS ForlopsType1,
    MCE.MCETYPE AS ForlopsType2Num,
    CASE 
     WHEN MCE.MCETYPE = 1 THEN 'Planlagt'
     WHEN MCE.MCETYPE = 2 THEN 'Akutt'
     WHEN MCE.MCETYPE = 3 THEN 'Subakutt'
    END AS ForlopsType2,
    
    MCE.INTERDAT AS Interdat,
    CASE INTERVENTION_TYPE
      WHEN 9 THEN mitralisfop.FOLLOWUPDATE
      WHEN 8 THEN tavifop.FOLLOWUPDATE
      WHEN 7 THEN o.PROCEDUREDATE
      WHEN 6 THEN mitralis.PROCEDUREDATE
      WHEN 5 THEN tavi.PROCEDUREDATE
      WHEN 4 THEN ct.CTDAT
      ELSE a.INTERDAT
    END AS HovedDato,
    
    P.ZIPCODE AS Postnr, -- TODO listed as char 4
    CAST(NULL AS CHAR(50)) AS PostSted,
    P.MUNICIPALITY_NAME AS Kommune,
    P.MUNICIPALITY_NUMBER AS KommuneNr,
    CAST(NULL AS CHAR(50)) AS Fylke,
    CAST(NULL AS CHAR(2)) AS Fylkenr,
    
    CASE
      WHEN IFNULL(P.GENDER,0) = 0 THEN 'Ikke angitt'
      WHEN P.GENDER = 1 THEN 'Mann'
      WHEN P.GENDER = 2 THEN 'Kvinne'
      ELSE 'Ukjent'
    END AS PasientKjonn,
    P.BIRTH_DATE AS FodselsDato,

    P.NORWEGIAN AS Norsktalende,
    CAST(NULL AS CHAR(30)) AS Sivilstatus,
    CAST(NULL AS CHAR(50)) AS UtdanningSSB,
    P.DECEASED  AS AvdodFReg,
    P.DECEASED_DATE as AvdodDatoFReg,

    -- event info
    CASE INTERVENTION_TYPE
      WHEN 9 THEN mitralisfop.STATUS
      WHEN 8 THEN tavifop.STATUS
      WHEN 7 THEN o.STATUS
      WHEN 6 THEN LEAST(mitralis.STATUS, IFNULL(mdisc.STATUS,1))
      WHEN 5 THEN LEAST(tavi.STATUS, IFNULL(tdisc.STATUS,1))
      WHEN 4 THEN ct.STATUS
      ELSE LEAST(IFNULL(i.STATUS,1), a.STATUS, IFNULL(c.STATUS,1), IFNULL(d.STATUS,1))
     END AS BasisRegStatus,
     
    CASE INTERVENTION_TYPE
      WHEN 9 THEN '1'
      WHEN 8 THEN '1'
      ELSE '0'
    END AS ErOppflg,
    CAST(NULL AS CHAR(2))  AS OppflgRegStatus,
    CAST(NULL AS CHAR(30)) AS OppflgStatus,
    CAST(NULL AS CHAR(6)) AS OppflgSekNr
  
    FROM
      mce MCE INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      LEFT OUTER JOIN initialcare i on MCE.MCEID = i.MCEID
      LEFT OUTER JOIN regangio a on MCE.MCEID = a.MCEID
      LEFT OUTER JOIN ctangio ct on MCE.MCEID = ct.MCEID
      LEFT OUTER JOIN taviperc tavi on MCE.MCEID = tavi.MCEID
      LEFT OUTER JOIN tavidischarge tdisc on MCE.MCEID = tdisc.MCEID
      LEFT OUTER JOIN tavipercfollowup tavifop on MCE.MCEID = tavifop.MCEID
      LEFT OUTER JOIN tavimitralis mitralis on MCE.MCEID = mitralis.MCEID
      LEFT OUTER JOIN tavimitralisdischarge mdisc on MCE.MCEID = mdisc.MCEID
      LEFT OUTER JOIN tavimitralisfollowup mitralisfop on MCE.MCEID = mitralisfop.MCEID
      LEFT OUTER JOIN angiopcicomp c on MCE.MCEID = c.MCEID
      LEFT OUTER JOIN discharge d on MCE.MCEID = d.MCEID
      LEFT OUTER JOIN other o on MCE.MCEID = o.MCEID
  ")
}




#' @rdname getQuery
#' @export
querySkjemaoversikt <-function(fromDate, toDate){
  
  
  paste0("
  SELECT
    CAST('Start' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    (select regangio.INTERDAT from regangio where regangio.MCEID = skjema.MCEID) AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    1 AS SkjemaRekkeflg
  FROM
    initialcare skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    regangio.INTERDAT >= '", fromDate, "' AND
    regangio.INTERDAT <= '", toDate, "'
  
  UNION
  SELECT
    CAST('AngioPCI' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    INTERDAT AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    3 AS SkjemaRekkeflg
  FROM
    regangio skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.INTERDAT >= '", fromDate, "' AND
    skjema.INTERDAT <= '", toDate, "'

  UNION
  SELECT
    CAST('Kompl' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    (select regangio.INTERDAT from regangio where regangio.MCEID = skjema.MCEID) AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    5 AS SkjemaRekkeflg
  FROM
    angiopcicomp skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    regangio.INTERDAT >= '", fromDate, "' AND
    regangio.INTERDAT <= '", toDate, "'

  UNION
  SELECT  
    CAST('Utskrivelse' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    (select regangio.INTERDAT from regangio where regangio.MCEID = skjema.MCEID) AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    7 AS SkjemaRekkeflg
  FROM
    discharge skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    regangio.INTERDAT >= '", fromDate, "' AND
    regangio.INTERDAT <= '", toDate, "'

  UNION
  SELECT
    CAST('CT-Angio' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    CTDAT AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    10 AS SkjemaRekkeflg
  FROM
    ctangio skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.CTDAT >= '", fromDate, "' AND
    skjema.CTDAT <= '", toDate, "'

  UNION
  SELECT
    CAST('Aorta' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    PROCEDUREDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    20 AS SkjemaRekkeflg
  FROM
    taviperc skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.PROCEDUREDATE >= '", fromDate, "' AND
    skjema.PROCEDUREDATE <= '", toDate, "'

  UNION
  SELECT
    CAST('AortaUtskrivKompl' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    DISCHARGEDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    22 AS SkjemaRekkeflg
  FROM
    tavidischarge skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.DISCHARGEDATE >= '", fromDate, "' AND
    skjema.DISCHARGEDATE <= '", toDate, "'

  UNION
  SELECT
    CAST('Aorta oppf' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    FOLLOWUPDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    25 AS SkjemaRekkeflg
  FROM
    tavipercfollowup skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.FOLLOWUPDATE >= '", fromDate, "' AND
    skjema.FOLLOWUPDATE <= '", toDate, "'


  UNION
  SELECT
    CAST('Mitral' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    PROCEDUREDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    30 AS SkjemaRekkeflg
  FROM
   tavimitralis skjema,
   centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.PROCEDUREDATE >= '", fromDate, "' AND
    skjema.PROCEDUREDATE <= '", toDate, "'

  UNION
  SELECT
    CAST('MitralUtskrivKompl' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    DISCHARGEDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    32 AS SkjemaRekkeflg
  FROM
    tavimitralisdischarge skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.DISCHARGEDATE >= '", fromDate, "' AND
    skjema.DISCHARGEDATE <= '", toDate, "'

  UNION
  SELECT
    CAST('Mitral oppf' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    FOLLOWUPDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    35 AS SkjemaRekkeflg
  FROM
    tavimitralisfollowup skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.FOLLOWUPDATE >= '", fromDate, "' AND
    skjema.FOLLOWUPDATE <= '", toDate, "'


  UNION
  SELECT
    CAST('Mitral utsk' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    skjema.DISCHARGEDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    40 AS SkjemaRekkeflg
  FROM
    tavimitralisdischarge skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.DISCHARGEDATE >= '", fromDate, "' AND
    skjema.DISCHARGEDATE <= '", toDate, "'

  UNION
  SELECT
    CAST('Tavi utsk' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    skjema.DISCHARGEDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    45 AS SkjemaRekkeflg
  FROM
    tavidischarge skjema,
    centre c
  WHERE
    skjema.CENTREID = c.ID AND
    skjema.DISCHARGEDATE >= '", fromDate, "' AND
    skjema.DISCHARGEDATE <= '", toDate, "'


  UNION
  SELECT
    CAST('Andre pros' AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(5)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(15)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    PROCEDUREDATE AS HovedDato,
    COALESCE((select ca.ATTRIBUTEVALUE from centreattribute ca where ca.ID = c.ID AND ca.ATTRIBUTENAME = 'FRIENDLYNAME'), c.ID) AS Sykehusnavn,
    c.ID AS AvdRESH,
    50 AS SkjemaRekkeflg
  FROM
    other skjema,
    centre c
  WHERE 
    skjema.CENTREID = c.ID AND
    skjema.PROCEDUREDATE >= '", fromDate, "' AND
    skjema.PROCEDUREDATE <= '", toDate, "'
         ")}



#' @rdname getQuery
#' @export
queryPasienterstudier <-function(){
  paste0("
  SELECT
    ps.PATIENT_ID AS PasientID,
    ps.CENTREID AS AvdRESH,
    s.ID AS StudieID,
    s.NAME AS StudieNavn,
    getListText('STUDY_PROCEDURE_TYPE', s.PROCEDURE_TYPE) AS ProsedyreType,
    ps.INCLUSION_DATE AS PasInklDato,
    ps.STOP_INCLUSION_DATE AS PasAvsluttDato,
    s.START_DATE AS StudieStartDato,
    s.STOP_INCLUSION_DATE AS StudieAvsluttDato,
    s.STOP_FOLLOWUP_DATE AS StudieOppflgAvslDato,
    getListText('STUDY_STATUS', s.STATUS) AS StudieStatus
  FROM
    patientstudy ps
    LEFT JOIN study s ON s.ID = ps.STUDY
")
}




#' @rdname getQuery
#' @export
queryApLight <- function(){
  paste0("
  SELECT
    A.CENTREID AS AvdRESH,
    MCE.MCEID AS ForlopsID,
    P.ID AS PasientID,

    A.REGTYP AS ProsedyreType,
    MCE.MCETYPE AS Hastegrad,
    CASE
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NOT NULL THEN 'Sekundær'
      WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NULL THEN 'Primær'
      ELSE NULL
    END AS Regtype,
    A.INTERDAT AS ProsedyreDato,
    A.INTERDAT_TIME AS ProsedyreTid,
  
    CASE (P.LOCAL_HOSPITAL) WHEN 999
                              THEN P.LOCAL_HOSPITAL_OTHER
                            ELSE (SELECT NAME FROM hospital WHERE hospital.ID = P.LOCAL_HOSPITAL)
    END AS Lokalsykehus,
   
    P.GENDER AS Kjonn,
    P.BIRTH_DATE FodselsDato,

     A.SYMPTOM_ONSET_DATE AS SymptomDato,
     A.SYMPTOM_ONSET_TIME AS SymptomTid,
     A.PREHOSPITAL_ECG_DATE AS BesUtlEKGDato,
     A.PREHOSPITAL_ECG_TIME AS BesUtlEKGTid,
     A.JOURTID AS Vakttid,
     A.HEIGHT AS Hoyde,
     A.WEIGHT AS Vekt,
     A.SKREATININ AS SKreatinin,
     A.TIDPCI  AS TidlPCI,
     A.TIDCABG  AS TidlABC,
     A.SMOKING_STATUS  AS RoykeStatus,
     A.HYPERTON  AS BehHypertoni,
     A.STATINS AS Statiner,
     A.TIDINF  AS TidlInfarkt,
     A.HISTORY_OF_CHF  AS KjentNedsattVenstVentr,
     A.DIABETES  AS Diabetes,
     A.DIABETESINSULIN AS Insulin,
     A.PREVIOUS_STROKE AS TidligereSlag,
     A.PERIPHERAL_VASCULAR_DISEASE  AS PeriferKarsykdom,
     A.INDIKATION  AS Indikasjon,
     A.MYOKARD  AS Myokardskademarkor,
     A.STSEGSANK  AS STSegmentSenkning,
     A.STAGED_PROCEDURE  AS StegvisProsedyre,
     A.CSS AS CanadianClass,
     A.NYHA AS NYHA,
     A.KILLIPKLASS AS KillipKlasse,
     A.FYND AS Funn,
     A.ADMISSION_ER AS AnkomstPCIDato,
     A.ADMISSION_ER_TIME AS AnkomstPCITid,

     A.PUNKT AS Stikksted,
     A.STENOS AS StenoseTidlBehSegment,
     A.CABG AS StenoseACBGraft,
     A.STRESSKARDIOMYOPATI  AS Stresskardiomyopati,
     A.PRIMBES AS PrimarBeslutning,
     A.DIAGNOSTIK AS AnnenDiagHovedSpm,
     A.DIATRYCK AS FFR,
     A.DIADOP AS Doppler,
     A.DIAIVUS AS IVUS,
     A.DIAOCT AS OCT,
     A.DIAIFR AS IFR,
     A.DIANIRS AS NIRS,
     A.DIACFR AS CFR,
     A.DIAIMR AS IMR,
     A.DIAPDPA AS PDPA,
     A.DIAPAHYPEREMI AS PA_Hyperemi,
     A.DIAPDHYPEREMI AS PD_Hyperemi,
     A.DIAANN AS AnnenDiag,
     A.EXTRAPROC AS Tilleggsprosedyrer,
     A.CARDIAC_CATHETERIZATION AS HoyreHjerteKat,
     A.VALVE_RECORDING AS Ventilfilming,
     A.PERICARDIOCENTESIS AS Perikardiocentese,
     A.ADJUVANT AS AdjuvantTerapi,
     A.ADJPUMP AS Aortaballongpumpe,
     A.ADJIMPELLA AS Impella,
     A.ADJECMO AS ECMO,
     A.ADJKAM AS AnnenVenstreKammerAssist,
     A.ADJLUK AS MekKompr,
     PCI.ADJDISTAL AS DistalProtectionDevice,
     A.ADJPACE AS Pacemaker,
     PCI.ADJTROMB AS Trombectomy,
     A.ADJPTA AS PTAHalskar,
     A.ADJPTSMA AS PTSMA,
     A.ADJANN AS AnnenAdj,
     A.OPEN_CAPILLARY AS ApningKarDato,
     A.OPEN_CAPILLARY_TIME AS ApningKarTid,

     CASE A.OPEN_CAP_OPEN_ANGIO
      WHEN 1 THEN 'Ja'
      ELSE 'Nei'
      END AS KarAapentVedAngio,
     CASE A.OPEN_CAP_OPEN_UNSUCCESSFUL
      WHEN 1 THEN 'Ja'
      ELSE 'Nei'
      END AS KarIkkeAapnet,
     PCI.SUCCESS AS GenereltSuksess,
     A.LABNO AS LabNummer,
     A.STRALDOS AS Straledose,
     A.GENOMLYSNINGSTID AS GjenLysTidSek,
     A.KONTRASTMEDEL AS Kontrastmiddel,
     A.KONTRASTMEDELMANGD AS KontrastMengdeMl,
     A.ANNANKONTRASTUNDERSOK  AS AnnKonMidUndersokelse,
     A.VASCCLOSUREDEV AS ArteriellLukning,
     A.LABKOMP  AS LabKomplikasjon,
     A.LABALLERGILATT  AS LabKompAllergiskLettModerat,
     A.LABALLERGIALLV  AS LabKompAllergiskAlvorlig,
     A.LABBEHARYTMI  AS LabKompBehkrevendeArytmi,
     A.LABHEMO  AS LabKompHemodynamisk,
     A.LABNEURO  AS LabKompNeurologisk,
     A.LABVASK  AS LabKompVaskulaerIkkeKoronar,
     A.LABTAPPAT  AS LabKompMistetStent,
     A.LABBESTSIDO  AS LabKompVedvarSidegrensokkl,
     A.LABPERF  AS LabKompPerforasjon,
     A.LABTAMP  AS LabKompTamponade,
     A.LABAKUTCABG  AS LabKompAkuttACBOperasjon,
     A.LABANNANALLV  AS LabKompAnnenAlv,
     A.LABDODSFALL  AS LabKompDod,
     A.LABPROCEDURDOD  AS LabKompProsedyrerelatertDod,


     I.TRANSFERREDPATIENT  AS OverflyttetFra,
     (select h.NAME FROM hospital h where I.TRANSFERREDFROM = h.ID) AS OverflyttetFraSykehus,
     I.REFERRING_HOSP_ADMISSIONDATE AS InnleggelseHenvisendeSykehusDato,
     I.REFERRING_HOSP_ADMISSIONDATE_TIME AS InnleggelseHenvisendeSykehusTid,

     

    I.PRESENTING_SYMPTOMS AS Innkomstarsak,
    I.SYMPTOM_ONSET_DATE AS SymptomdebutDato,
    I.SYMPTOM_ONSET_TIME AS SymptomdebutTid,
    I.CPR_BEFORE_HOSPITAL  AS HLRForSykehus,

    I.ECG_RHYTHM AS EKGRytme,
    I.ECG_QRS_ANNOTATION AS EKGQRS,
    I.ECG_STT_CHANGES AS EKGSTT,
    I.LEFT_BLOCK  AS VenstreGrenblokk,
    I.ECG_DECISION_TRIGGERING AS BeslutningsutlosendeEKG,
    I.PREHOSPITAL_ECG_DATE AS BeslEKGDato,
    I.PREHOSPITAL_ECG_TIME AS BeslEKGTid,

    I.HEART_RATE AS Hjertefrekvens,
    I.SYSTOLIC_BLOOD_PRESSURE AS SystoliskBlodtrykk,
    I.DIASTOLIC_BLOOD_PRESSURE AS DiastoliskBlodtrykk,
    I.CARDIAC_SHOCK  AS KardiogentSjokkAnkomst,

    A.REPERTREATMENT AS GittTrombolyse,
    A.TYPE_THROMB_THERAPY_ADM AS TrombolyseMedikament,
    A.THROMB_GIVEN_DATE AS TrombolyseDato,
    A.THROMB_GIVEN_TIME AS TrombolyseTid,

    I.PREVIOUS_ACB AS TidligereACBOp,
    I.PRIOR_CARDIAC_SURGERY  AS AnnenTidlKirurgi,
    I.HYPERTENSION AS Hypertoni,



  -- Vanlige segment
    F.SEGMENT1,
    F.SEGMENT2,
    F.SEGMENT3,
    F.SEGMENT4,
    F.SEGMENT5,
    F.SEGMENT6,
    F.SEGMENT7,
    F.SEGMENT8,
    F.SEGMENT9,
    F.SEGMENT10,
    F.SEGMENT11,
    F.SEGMENT12,
    F.SEGMENT13,
    F.SEGMENT14,
    F.SEGMENT15,
    F.SEGMENT16,
    F.SEGMENT17,
    F.SEGMENT18,
    F.SEGMENT19,
    F.SEGMENT20,
  -- Vene segment
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '1' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT1VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '2' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT2VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '3' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT3VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '4' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT4VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '5' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT5VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '6' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT6VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '7' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT7VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '8' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT8VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '9' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT9VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '10' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT10VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '11' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT11VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '12' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT12VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '13' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT13VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '14' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT14VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '15' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT15VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '16' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT16VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '17' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT17VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '18' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT18VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '19' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT19VeneStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '20' AND finding.GRAFT = '1' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT20VeneStenosgrad,

  -- Arterie segment
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '1' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT1ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '2' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT2ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '3' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT3ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '4' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT4ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '5' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT5ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '6' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT6ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '7' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT7ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '8' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT8ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '9' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT9ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '10' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT10ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '11' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT11ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '12' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT12ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '13' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT13ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '14' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT14ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '15' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT15ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '16' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT16ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '17' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT17ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '18' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT18ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '19' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT19ArterieStenosgrad,
    (SELECT finding.STENOSGRAD from finding where finding.SEGMENT = '20' AND finding.GRAFT = '2' AND finding.MCEID = MCE.MCEID ORDER BY finding.ID DESC LIMIT 1) AS SEGMENT20ArterieStenosgrad,
  
    PCI.SEKBESLUT AS SekundaerBeslutning,
    PCI.KOMPREV AS KomplettRevaskularisering,
    PCI.ANTIFORE AS AntitrombotiskFor,
    PCI.TROFORE AS TrombolyseFor,
    PCI.ASAFORE AS ASAFor,
    PCI.CLOFORE AS ClopidogrelFor,
    PCI.PRAFORE AS PrasugrelFor,
    PCI.TICFORE AS TicagrelorFor,
    PCI.HEPFORE AS HeparinFor,
    PCI.DALFORE AS DalteparinFor,
    PCI.ENOFORE AS EnoxaparinFor,
    PCI.ANNFORE AS AnnetLavmolHeparinFor,
    PCI.BIVFORE AS BivalirudinFor,
    PCI.FONFORE AS FondaparinuxFor,
    PCI.ABCFORE AS AbciximabFor,
    PCI.EPTFORE AS EptifibatidFor,
    PCI.TIRFORE AS TirofibanFor,
    PCI.WARFORE AS WarfarinFor,
    PCI.DABFORE AS DabigatranFor,
    PCI.APIFORE AS ApiksabanFor,
    PCI.RIVFORE AS RivaroksabanFor,
    PCI.EDOFORE AS EdoksabanFor,
    PCI.KANGRELORFORE AS KangrelorFor,
    PCI.OVRFORE AS AnnetAntitrombotiskFor,
    PCI.ANTIFORE AS AntitrombotiskUnder,
    PCI.TROUND AS TrombolyseUnder,
    PCI.ASAUND AS ASAUnder,
    PCI.CLOUND AS ClopidogrelUnder,
    PCI.PRAUND AS PrasugrelUnder,
    PCI.TICUND AS TicagrelorUnder,
    PCI.HEPUND AS HeparinUnder,
    PCI.DALUND AS DalteparinUnder,
    PCI.ENOUND AS EnoxaparinUnder,
    PCI.ANNUND AS AnnetLavmolHeparinUnder,
    PCI.BIVUND AS BivalirudinUnder,
    PCI.FONUND AS FondaparinuxUnder,
    PCI.ABCUND AS AbciximabUnder,
    PCI.EPTUND AS EptifibatidUnder,
    PCI.TIRUND AS TirofibanUnder,
    PCI.WARUND AS WarfarinUnder,
    PCI.KANUND AS KangrelorUnder,
    PCI.OVRFORE AS AnnetAntitrombotiskUnder,
  
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = PCI.MAIN_OPERATOR ) AS PCIHovedOperator,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = PCI.SECOND_OPERATOR ) AS PCIAndreOperator,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = PCI.THIRD_OPERATOR ) AS PCITredjeOperator,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = A.MAIN_ANGIOGRAFOR ) AS Angiografor1,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = A.SECOND_ANGIOGRAFOR ) AS Angiografor2,
    (SELECT CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME) from people peo where peo.PEOPLEID = A.THIRD_ANGIOGRAFOR ) AS Angiografor3,
    CAST((SELECT GROUP_CONCAT(CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME)) FROM pci_operator_mapping aom, people peo where aom.PEOPLEID = peo.PEOPLEID and aom.MCEID = A.MCEID) AS CHAR(300)) AS PCIOperatorer,
    CAST((SELECT GROUP_CONCAT(CONCAT(peo.FIRSTNAME, ' ', peo.LASTNAME)) FROM angio_operator_mapping aom, people peo where aom.PEOPLEID = peo.PEOPLEID and aom.MCEID = A.MCEID) AS CHAR(300)) AS AngioOperatorer,

    C.AVDKOMP AS AvdKomp,
    C.AVDALLERGISK AS AvdKompAllergisk,
    C.AVDBLODNING AS AvdKompBlodning,
    C.AVDBLODMAJOR AS AvdKompBlodningMajor,
    C.AVDBLODMINOR AS AvdKompBlodningMinor,
    C.AVDBEHPSEUDO AS AvdKompPseudoaneurysme,
    C.AVDHEMATOM AS AvdKompHematomStor,
    C.AVDHBFALL AS AvdKompHbFallStor,
    C.AVDFORLANGDKOMPTID AS AvdKompForlengetTidStor,
    C.AVDVARDTID AS AvdKompForlengetOppholdStor,
    C.AVDULTRALJUD AS AvdKompUltralydCT,
    C.AVDBLODTRANSFUSION AS AvdKompBlodtransfusjon,
    C.AVDKIRURGISKATGARD AS AvdKompKirurgiskBeh,
    C.AVDANNANBEHUTOVERKOMP AS AvdKompAnnenBehUtoverKompresjon,
    C.AVDFORTIDAUTSATTNING AS AvdKompTidligUtsettelse,
    C.AVDANNANVASK AS AvdKompVaskulaer,
    C.AVDNEURO AS AvdKompNeurologiskKomp,
    C.AVDNJURINSUFF AS AvdKompNyNyreinsuffisiens,
    C.AVDTAMP AS AvdKompTamponade,
    C.AVDREPCI AS AvdKompPCI,
    C.AVDCABG AS AvdKompACB,
    C.AVDHJARTINFARKT AS AvdKompHjerteinfarkt,
    C.AVDANNANALLV AS AvdKompAnnenAlvorlig,
    C.AVDDODSFALL  AS AvdKompDod,
    C.AVDPROCEDURDOD AS AvdKompProsedyrerelatertDod,
  
  
    -- Here comes numerous variables in the ANGIOPCICOMP SQL. Never used?
  
    D.DISCHARGE_DATE AS Utskrivningsdato,
    D.DEATH  AS UtskrevetDod,
    D.DECEASED_DATE AS UtskrevetDodsdato,
    D.DISCHARGETO AS UtskrevetTil,
    D.ASPIRIN_DISCHARGE AS ASA,
    D.ORAL_ANTICOAGULANTS_DISCHARGE AS Antikoagulantia,
    D.OTHER_ANTIPLATELET_DISCHARGE AS AndrePlatehemmere,
    D.STATINS_DISCHARGE AS UtskrStatiner,
    D.OTHER_LIPID_LOW_AGENTS_DISCHARGE AS OvrigeLipidsenkere,


  
    P.SSN_TYPE AS FnrType,
    P.SSNSUBTYPE AS FnrSubtype,
    P.DECEASED  AS AvdodFReg,
    P.DECEASED_DATE as AvdodDatoFReg,
    P.MUNICIPALITY_NAME AS Kommune,
    P.MUNICIPALITY_NUMBER AS KommuneNr,
	  CAST(NULL AS CHAR(50)) AS Fylke,
  	CAST(NULL AS CHAR(2)) AS Fylkenr,
  	MCE.PARENT_MCEID as KobletForlopsID,
  	MCE.PARENT_MCEID AS PrimaerForlopsID,



    I.STATUS AS SkjemaStatusStart,
    A.STATUS AS SkjemastatusHovedskjema,
    D.STATUS AS SkjemaStatusUtskrivelse,
    C.STATUS AS SkjemaStatusKomplikasjoner
    
    FROM mce MCE
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      INNER JOIN regangio A ON MCE.MCEID = A.MCEID
      LEFT JOIN initialcare I ON MCE.MCEID = I.MCEID
      LEFT JOIN findingstatic F ON MCE.MCEID = F.MCEID
      LEFT JOIN pci PCI ON MCE.MCEID = PCI.MCEID
      LEFT JOIN angiopcicomp C ON MCE.MCEID = C.MCEID
      LEFT JOIN discharge D ON MCE.MCEID = D.MCEID
  ")
}  



#' @rdname getQuery
#' @export
queryDiagnose <- function(){
  paste0("
  SELECT
  mce.CENTREID AS AvdRESH,
  mce.MCEID AS ForlopsID,
  mce.PATIENT_ID AS PasientID,
  mce.INTERDAT AS ProsedyreDato,
  diagnose.CODE,
  diagnose.VERSION

  FROM  diagnose
  LEFT JOIN mce ON diagnose.MCEID = mce.MCEID
  ")}
