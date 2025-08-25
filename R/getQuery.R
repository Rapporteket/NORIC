#' Fetch queries to NORIC raw data tables
#'
#' Replace old views
#'

#' @return query as string
#' @name getQuery
#' @aliases queryTaviprom
#' queryAortaklaffvarnum
#' queryForlopsoversikt
#' queryAndreprosedyrervarnum
NULL

#' @rdname getQuery
#' @export
queryTaviprom <- function(){
  
  paste0("
  SELECT
  
  MCE.MCEID AS ForlopsID,
  proms.REGISTRATION_TYPE AS Registreringstype,
  P.ID AS PasientID,
  P.SSN_TYPE AS FnrType,
  tavi.PROCEDUREDATE AS ProsedyreDato,
  MCE.CENTREID AS AvdRESH,
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
  
INNER JOIN mce MCE ON 
  proms.MCEID = MCE.MCEID
  
INNER JOIN centre C ON 
  C.ID = MCE.CENTREID
  
INNER JOIN patient P ON 
  MCE.PATIENT_ID = P.ID
  
LEFT JOIN rose_dyspnea_scale r ON 
  MCE.MCEID = r.MCEID
  
LEFT JOIN heart_qol h ON 
  MCE.MCEID = h.MCEID
  
LEFT JOIN minnesota_questionnaire m ON 
  MCE.MCEID = m.MCEID
  
LEFT JOIN taviperc tavi ON
  MCE.MCEID = tavi.MCEID
  
LEFT JOIN tavi_additional_questions tav ON 
  MCE.MCEID = tav.MCEID
  
LEFT JOIN prem ON 
  MCE.MCEID = prem.MCEID
  
WHERE 
  proms.REGISTRATION_TYPE LIKE 'TAVI%'
")
}




#' @rdname getQuery
#' @export
queryAortaklaffvarnum <- function(){
  paste0("
  SELECT
  T.MCEID AS ForlopsID,
  P.ID AS PasientID,
  P.SSN_TYPE AS FnrType,
  P.DECEASED AS AvdodFReg,
  P.DECEASED_DATE AS DodsdatoFReg,
  T.CENTREID AS AvdRESH,
  
  -- Perkutane aortaklaffer
  T.SCREENING AS ScreeningBeslutning,
  T.COMPLETED_PROCEDURE AS Prosedyre,
  T.INDICATION AS Indikasjon,
  T.SCREENINGDATE AS BeslutningsDato,
  T.PROCEDUREDATE AS ProsedyreDato,
  T.AKUTOP AS AkuttOperasjon,
  
  -- Study information
  (SELECT
    GROUP_CONCAT(
      IF ((DATEDIFF(P.REGISTERED_DATE, PS.PasInklDato) > 0) AND (DATEDIFF(P.REGISTERED_DATE, PS.StudieAvsluttDato) < 0 OR PS.StudieAvsluttDato IS NULL), CONCAT(PS.StudieNavn), NULL))
    FROM pasienterstudier PS
    WHERE PS.PasientID = MCE.PATIENT_ID)
  AS Studie,
  
  -- Kliniske bakgrunnsdata
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
  
  LEAST(T.STATUS, TD.STATUS) AS SkjemaStatus,
  T.STATUS AS SkjemaStatusHovedskjema,
  TD.STATUS AS SkjemaStatusKomplUtsk
  FROM mce MCE
  INNER JOIN centre C ON C.ID = MCE.CENTREID
  INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
  INNER JOIN taviperc T ON MCE.MCEID = T.MCEID
  INNER JOIN tavidischarge TD ON MCE.MCEID = TD.MCEID
")
}





#' @rdname getQuery
#' @export
queryForlopsoversikt <-function(){
  
  
  # FUNGERER IKKE. 
  # funksjoner getFriendlyName() og getListText()
  
  
  paste0("
  SELECT
  -- hospital and highlevel stuff
  m.CENTREID AS AvdRESH,
  getFriendlyName(m.CENTREID) AS Sykehusnavn,
  -- patient info
  p.ID AS PasientID,
  p.ZIPCODE AS Postnr, -- TODO listed as char 4
  CAST(NULL AS CHAR(50)) AS PostSted,
  p.MUNICIPALITY_NAME AS Kommune,
  p.MUNICIPALITY_NUMBER AS KommuneNr,
  CAST(NULL AS CHAR(50)) AS Fylke,
  CAST(NULL AS CHAR(2)) AS Fylkenr,
  p.SSN_HASH AS KryptertFnr,
  CASE
  WHEN IFNULL(p.GENDER,0) = 0 THEN 'Ikke angitt'
  WHEN p.GENDER = 1 THEN 'Mann'
  WHEN p.GENDER = 2 THEN 'Kvinne'
  ELSE 'Ukjent'
  END AS PasientKjonn,
  CASE
  WHEN p.GENDER = 1 THEN '1'
  WHEN p.GENDER = 2 THEN '0'
  ELSE NULL
  END AS erMann,
  floor(datediff(
    (CASE INTERVENTION_TYPE
     WHEN 9 THEN mitralisfop.FOLLOWUPDATE
     WHEN 8 THEN tavifop.FOLLOWUPDATE
     WHEN 7 THEN o.PROCEDUREDATE
     WHEN 6 THEN mitralis.PROCEDUREDATE
     WHEN 5 THEN tavi.PROCEDUREDATE
     WHEN 4 THEN ct.CTDAT
     ELSE a.INTERDAT
     END),
    p.BIRTH_DATE) / 365.25) AS PasientAlder,
  p.BIRTH_DATE AS FodselsDato,
  CONVERT(IFNULL(getListText('PATIENT_NORWEGIAN', p.NORWEGIAN), 'Ikke angitt') USING utf8) AS Norsktalende,
  CAST(NULL AS CHAR(30)) AS Sivilstatus,
  CAST(NULL AS CHAR(50)) AS UtdanningSSB,
  CONVERT(IFNULL(getListText('PATIENT_DECEASED', p.DECEASED), '') USING utf8) AS Avdod,
  p.DECEASED_DATE AS AvdodDato,
  -- event info
  m.MCEID AS ForlopsID,
  x_cast_to_tinyint(
    (CASE INTERVENTION_TYPE
     WHEN 9 THEN mitralisfop.STATUS
     WHEN 8 THEN tavifop.STATUS
     WHEN 7 THEN o.STATUS
     WHEN 6 THEN LEAST(mitralis.STATUS, IFNULL(mdisc.STATUS,1))
     WHEN 5 THEN LEAST(tavi.STATUS, IFNULL(tdisc.STATUS,1))
     WHEN 4 THEN ct.STATUS
     ELSE LEAST(IFNULL(i.STATUS,1), a.STATUS, IFNULL(c.STATUS,1), IFNULL(d.STATUS,1))
     END)) AS BasisRegStatus,
  CONVERT(IFNULL(getListText('MCE_INTERVENTION_TYPE', m.INTERVENTION_TYPE), 'Ikke angitt') USING utf8) AS ForlopsType1,
  m.INTERVENTION_TYPE AS ForlopsType1Num,
  getListText('MCE_ACUTE_ELECTIVE', m.MCETYPE) AS ForlopsType2,
  m.MCETYPE AS ForlopsType2Num,
  m.PARENT_MCEID as KobletForlopsID,
  (CASE INTERVENTION_TYPE
   WHEN 9 THEN mitralisfop.FOLLOWUPDATE
   WHEN 8 THEN tavifop.FOLLOWUPDATE
   WHEN 7 THEN o.PROCEDUREDATE
   WHEN 6 THEN mitralis.PROCEDUREDATE
   WHEN 5 THEN tavi.PROCEDUREDATE
   WHEN 4 THEN ct.CTDAT
   ELSE a.INTERDAT
   END) AS HovedDato,
  CAST(NULL AS CHAR(2))  AS OppflgRegStatus,
  CASE INTERVENTION_TYPE
  WHEN 9 THEN '1'
  WHEN 8 THEN '1'
  ELSE '0'
  END AS ErOppflg,
  CAST(NULL AS CHAR(30)) AS OppflgStatus,
  CAST(NULL AS CHAR(6)) AS OppflgSekNr
  
  from
  mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID
  LEFT OUTER JOIN initialcare i on m.MCEID = i.MCEID
  LEFT OUTER JOIN regangio a on m.MCEID = a.MCEID
  LEFT OUTER JOIN ctangio ct on m.MCEID = ct.MCEID
  LEFT OUTER JOIN taviperc tavi on m.MCEID = tavi.MCEID
  LEFT OUTER JOIN tavidischarge tdisc on m.MCEID = tdisc.MCEID
  LEFT OUTER JOIN tavipercfollowup tavifop on m.MCEID = tavifop.MCEID
  LEFT OUTER JOIN tavimitralis mitralis on m.MCEID = mitralis.MCEID
  LEFT OUTER JOIN tavimitralisdischarge mdisc on m.MCEID = mdisc.MCEID
  LEFT OUTER JOIN tavimitralisfollowup mitralisfop on m.MCEID = mitralisfop.MCEID
  LEFT OUTER JOIN angiopcicomp c on m.MCEID = c.MCEID
  LEFT OUTER JOIN discharge d on m.MCEID = d.MCEID
  LEFT OUTER JOIN other o on m.MCEID = o.MCEID
")}



#' @rdname getQuery
#' @export
queryAndreprosedyrervarnum <-function(){
  paste0("
  SELECT
    other.MCEID AS ForlopsID,
    other.CENTREID AS AvdRESH,
    (CASE
     WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NOT NULL THEN 'Sekundær'
     WHEN MCE.INTERVENTION_TYPE IN (1,2,3,7) AND MCE.PARENT_MCEID IS NULL THEN 'Primær'
     ELSE NULL
     END) as Regtype,
    
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
    
    other.STATUS AS SkjemaStatus
    FROM mce MCE
    INNER JOIN other ON MCE.MCEID = other.MCEID

"
  )}

