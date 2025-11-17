# noric 3.9.0

Oppdatert håndtering av Lokalsykehus (nå på prosedyreskjema). 
...

# noric 3.8.0: Sammenslåing databaser
 
Overgang til èn felles database for alle sykehusene (noric_bergen). 
Hente innhold i Rapporteket fra denne databasen. Oppdatere rapbaseConfig til 
å bruke kun èn database. Tilpasse innhold og tilganger på Rapporteket til 
nytt databaseoppsett. Bruke eksisterende tilganger. Innføring av parameteren
'singleHospital'.


# noric 3.7.0
 
*Fjerne view for Forlopsoversikt (MERK: problemer med kalender for denne, 
bruker ikke fromDate og toDate)
* Fjerne view for PasienterStudier(MERK: problemer med kalender for denne, 
bruker ikke fromDate og toDate)
MERK2: I gammel håndtering viste PS alle pasienter med studier en gang per
forløp (left_join(fo, ps, by= pasientid)). Nå viser vi bare alle pasienter
med studie. Inge forlopsid
* Fjerne Skjemaoversikt
* noric::mapOrgId, noric::getLatestEntry og getLatestEntryHospital bruker 
skjemaoversikt i noric_nasjonal, tilpasse denne når SO er fjernet



# noric 3.6.0
* Fjerne view for Segment stent.
* Fjerne view for angiopcivarnum.
Variabelen UtskDiagnose var svært ressurskrevende å hente ut med SQL 
(over 10 min). Fjerner denne fra getAp og lager egen tabell i datadump som
inneholder en utndertabell med diagnoser ved utskrivelse (en rad per diagnose).

# noric 3.5.0
Entydig bruk av params$reshId i alle rapportert og reportProcessor. 
Fjerne bruk av view i tavi-rapporten ([#241](https://github.com/Rapporteket/NORIC/issues/241))


# noric 3.4.0
* hente data med SQL fra rådata, og ikke views
* gjelder: AK, MK, AnD, AnP, CT
* fjerne join med forlopsoversikt i R, flytte dette til SQL
* SQL query lages som en funksjon, der sykehus og tidsperiode er gitt som 
parametre. Heller enn å spørre etter 'alt' og bruke filter i etterkant
* utlede alder som egen funksjon

# noric 3.3.0
* Knapp i QA for utsending av rapporter ([#228](https://github.com/Rapporteket/NORIC/pull/228))
* Fiks oppdatering av rapport-liste i abo-fane ([#231](https://github.com/Rapporteket/NORIC/pull/231))
* Staging back ([#192](https://github.com/Rapporteket/NORIC/pull/192))

# noric 3.2.0 Endrer månedsvisning for to av månedsrapportene og andre fiks
* Månedsrapportene "Invasive prosedyrer" og "Angiografør/Operatør" viser nå figurer/tabeller med data fra fjoråret samt hittil i år.
* Har fjernet utkommentert kode som feilet. Denne koden fungerer nå.
* korrigere datagrunnlag (noric_nasjonal) for rapporten "Angiografør/Operatør" etter overgang til v3.x


# noric 3.1.9 bugfix datagrunnlag rapporter
* korrigere datagrunnlag (noric_nasjonal) for rapporter etter overgang til v3.x
* fjerne bruk av staging funksjoner i ki-rapporten
* teste for riktig antall sykehus i report processor før ki-rapporten lages
* Oppdatere utlede_kar_segment_stent() og utlede_kar_graft_segment_stent()
  med tekst-format på variabler
* mapping til txt variabler i getApLight()


# noric 3.1.7 HospitalName fra mapping
* Alltid bruke HospitalName fra mapping. 
* Gjelder hospitalName som gis som param til rmd filer
* slette funksjonen getHospitalName()

# noric 2.17.0 Noric 2.0 bruker mysql 8
* De fleste num-tabellene brukes nå sammen med funksjonen erstatt_koder_m_etiketter.R for å gjøre de kategoriske verdiene om til tekstverdier. 
* Tilgjengeliggjort månedsrapporten Aortaklaff i fanen Abonnement for de sykehusene det gjelder.

# noric 2.16.0 Nordlandssykehuset endrer navn
Nordlandssykehuset Bodø endrer navn til UNN Bodø

# noric 2.15.5 bugfix dupliserte variabelnavn
RMariaDB vil ikke lenger har dupliserte variabelnavn. Ryddet opp i SQL 
for AK-tabellen.

# noric 2.15.4 Buxfix nytt år
For TAVI rapporten. Når kun en måned i inneværende år, tvinge string-format på 
label til cumulative plot. Tvinge y-aksen til cumplot til å starte på 0. 
Tvinge alle tabeller til a vise også måneder med 0 registreringer.

# noric 2.15.3 Bugfix staging
Oppdatere staging data KI. Nå kan denne ogsaa regne ut ventetid. 
Typeklaffeprotese Evolut FX er core valve. 

# noric 2.15.2 Bugfix tavi rapport
3 trykkfeiler i rapporten. Og legge til paravalvulær insuffisiens. 

# noric 2.15.1 Bugfix stemi indikator
Tar hensyn til manglende kopiering mellom BeslEKGDato og BesUtlEKGDato med
tilhørende tidspunkt. Bruker i 1.prioritet BeslEKGDato. 


# noric 2.15.0 TAVI rapport
Resultater fra TAVI/Aortaklaff prosedyrer. Tilgjengelig for nedlasting ved 
nasjonal SC rolle og som månedsrapport/abonnement for lokale LC/SC roller.

# noric 2.14.0 Oppdaterte rapporter
* __avdod__
Etter at sykehusene gikk gjennom forrige rapport, ble det klart at død 1-2 
dager etter prosedyre kan forekomme. Ingen feilregistreringer. Fjerner derfor
dette avsnittet til ny versjon av rapporten. Legg inn sjekk på kun død i NORIC. 

* __kvalitets-indikator__
Samler alle figurene for nstemi på en side. 

* __invasive prosedyrer__
Oppdatert om komplikasjoner. 
Revisjon gammel kode.
Nå tilgjengelig for nedlasting/utsending fra Nasjonal SC.

# noric 

# noric 2.13.1 - bugfix
Staging data bruker også trykkmaling, fikser det. 
Nye metoder i "Andre prosedyrer"(Myokardiell biopsi og Trikuspidal kladd teer). 
Disse er lagt til i invasive prosedyrer rapporten, stand alone prosedyrer. 


# noric 2.13.0 - ny noric release
Ny behandlig av trykkmaalinger i NORIC prod. Nye variabler fra annen 
diagnostikk til AngioPCIVar. Oppdatere noric-rapporteket. 

# noric 2.12.0 - tavi prom
Hente tabellen med PROM-data og tilgjengeliggjøre den for datadump og utforsker.
Kun nasjonal SC. 
Kodebok (metadata) for tavi-prom tabellen

# noric 2.11.4

## bugfix
* Fikset feilen som oppstår når tabeller ikke har data for alle månedene i 
ønsket periode.

# noric 2.11.3

## bugfix
* Fikset feilen i "Angiografør/Operatør"-rapporten

# noric 2.11.2

## bugfix2
Fikset visning av fanen for rollene.

# noric 2.11.1

## bugfix
Endret fanen "Aktivitet" til "Angiografør/Operatør"


# noric 2.11.0
* Har slått sammen månedsrapportene Prosedyre og Stentbruk.
* Gjort endringer i figurer, tabeller, figurtekst og tabelltekst.
* Har endret "last ned"-knappen til å bare laste ned pdf for disse lokale rapportene.
* De lokale månedsrapportene bruker nå reportProcessor til å generere pdf-rapporter.


# noric 2.10.1

## bugfix
Verktøy-meny "staging data". Riktig staging data blir nå slettet når man bruker
"delete"-knappen i listen. noric::makeStagingDataFrame sorterer ikke på dato, 
men lister opp akkurat slik som rapbase::listStagingData.
Litt finjusteringer i teksten for rapporten avdød filvask. 

# noric 2.10.0
* Lagrer en skyggekopi av staging data som kan brukes til generering av 
KI-rapporten.
* Egne noric-funksjoner for staging data: liste alle staging data,
sjekke validitet og finne nyeste, slette gamle staging data. 
* Egen bulletinProcessorStaging for regelmessig (daglig) prosessering av
staging data. 
* Brukergrensesnitt i Verktøy-menyen for nasjonal SC rolle.

# noric 2.9.3

## Bugfix
SQL med singleRow=TRUE må ha _en_ rad i datasettet for å kunne fullføre
databehandlingen i getAndPrep funksjonene. For enkelte sykehus ble den ene
raden slettet av "fjerne_tulleregistreringer" før databehandlignen var fullført
(0 rader i tabell). Dette er rettet opp. 




# noric 2.9.2

## Nytt
Utforsker har nå en kalender. 
Valgfri start og slutt-dato før innlasting av tabell. 
Raskere SQL spørring ved kortere intervall!

## Oppdatering og endringer
Ny variabel _FnrType_ bare for SC rolle i utforsker
Litt mer fix i avdod-rapporten
Legge til Avdod og AvdodDato i Mitralklaff

# noric 2.9.1

Bugfix. Fjerne donorutrednign fra avdod-rapporten


# noric 2.9.0

## Nytt
* Innholdet i stent-rapporten ligger nå under prosedyre-rapporten.
* Stent-rapporten er eksisterer ikke lengre.
* Funksjonene utlede_dod_noric() og avdod_opphold()
* Ny rapport: Kvalitetsforbedring - registrering av død i NORIC
* Tilpasse "Nedlasting Rapporter" til å laste valgfri rapport

# noric 2.8.0

## Nytt
* Ny fane under verktøy - mulighet for nasjonal SC til å laste ned ki-rapporten
for valgt sykehus
* Tilrettelegging i server.R for nedlasting av rapport ved bruk av 
reportProcessor
* reportProcessor viser melding 'Rendering, please wait!' mens rapporten lages
* Fremgang i generering vises
* ki-rapporten returnerer fremgang ved ulike 'sjekkpunter' i rapporten

## Bugfix
* Fjerne mulighet for NA som nivå i NSTEMI-figurer
* Lik fargekode på sykehus/nasjonalt for NSTEMI-figurer


# noric 2.7.0

## Nytt
* Ny fane: Månedsrapporter: Samler alle rapporter - Stent, Prosedyre og Aktivitet (Ny)
* Ny Rapport: Aktivitet – Gir en oversikt over antall prosedyrer pr angiografør/operatør pr måned
* Ny fane: Kodebok
* Kodebok – Lister opp utvalgte variabler fra ny data-fil som beskriver de utledede variablene

## Oppdateringer og endringer
* Ny indikator for Aortaklaff-tabellen (Pacemakerbehov)
* Bruker indikatoren i månedlig utsending
* Bruker indikatoren i Utforsker
* Bugfix i indikatoren «Ferdigstilt komplikasjonsskjemaer»
* Oppdatert startside.


# noric 2.6.0

## Oppdatering kvalitetsindikatorene
* Legge til indikator for Aortaklaff-tabellen (Pacemakerbehov)
* Bruke indikatoren i månedlig utsending
* Legge til indikatoren i Utforsker (aortaklaff)
* Bugfix i datagrunnlag for indikatoren "ferdigstilt komplikasjonsskjema"

## Nytt
* Intern data-fil med beskrivelse av utledete variabler 
* Ny fane i Rapporteket: Kodebok
* Kodeboken lister opp utvalge variabler fra data-filen

## Annet
* Oppdatering startsiden 
* Samle alle månedsrapportene i en fane


# noric 2.5.0

## Oppdatering kvalitetsindikatorer
* Trykkmålinger: 4 nye trykkmålinger skal være med i indikatoren 
(IMR, Pd, Pa, Pd/Pa)
* Foreskrevet kolesterolsenkende og blodfortynnende. Vi har nå 3 nivåer dersom 
forløpet er i datagrunnlaget : ja, nei og ikke ferdigstilt. Synliggjør de 
ikke-ferdigstilte
* Oppdatere månedlig utsending med disse endringene
* Oppdatere månedlig utsendign med bruk av noric funksjonene fra versjon 2.3.0
* Bugfix i IVUS-OCT tabellen i KI-rapporten  (riktig i ki_ivus_oct_ved_stenting_lms())

## Nytt
* Ny funksjon: reportProcessor() 
* bruke rapbase-layout med reportProcessor for utsending av KI-rapporten
* testmiljø for getData-funksjonene

## Bugfix
* Rette getMk og GetPs. Filter på riktig dato-variabel


# noric 2.4.2
* missing export of getPrepSoData() hopefully fixed
* removed spinner from local reports
* use stats now only for national registry with sc role 

# noric 2.4.1

## Bugfix monthly quality indicator-reports
* more than 10 TAVI needed last 3 years to create pacemaker-table
* use mst from noric library

## Improvements
* split get functions in two functions: One function does the SQL query, 
the other processes the tables. 
* both functions have arguments fromDate and toDate. Allows us to ask for 
data in restrained time-interval. 



# noric 2.4.0

## New features
* use statistics report
* database export for development and data processing purposes

## Improvements
* selectable start date for subscriptions
* replaced local functions with general modules from rapbase reducing the number of code lines

# noric 2.3.0

* new table in utforsker : "Angio PCI med utledete variabler, 3 siste aar"
* creation of functions used to ease datamanagment (e.g. fjern_tulleregistrering, fikse_sykehusnavn)
* creation of functions used to add new variables in noric datasets (e.g. year, month, week, waiting times, age-class)
* creation of functions used to add new variables for quality indicators and which data to analyse for each indicator (e.g. indik_trykkmaaling_utfort + indik_trykkmaaling_utfoert_data)
* creation of functions used to add information from "SegmentStent"-table or "annen diagnostikk"-table into "angioPCI"-table (e.g. antall_stent, stent_i_lms, antall_ffr_lms, kar)
* use of testthat/tests for all new functions
* general cleanup in code
* Added a `NEWS.md` file to track changes to the package.
