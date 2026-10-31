# Setzt den Pfad zum Arbeitsverzeichnis, in dem sich die Datensätze befinden
setwd("Bitte_Pfad_einsetzen")

###### Datenaufbereitung

# Zentrale Konstanten, um die wiederholte Angabe desselben Zeitraums und derselben NA-Kennzeichnungen zu vermeiden
YEARS <- 2005:2021 # Legt den für die Analyse verwendeten Zeitraum von 2005 bis 2021 fest
NA_VALUES <- c("", "NA", "..") # Definiert die Zeichenfolgen, die beim Einlesen der Datensätze als fehlende Werte (NA) behandelt werden
# Hilfsfunktion zur Vereinheitlichung und Bereinigung der Ländernamen
# - x ist der Vektor (Spalte) mit den Ländernamen
fix_name <- function(x) {
	# gsub() sucht alle Treffer des Musters und ersetzt sie durch die angegebene Substitution
	x <- gsub("^TÃ¼rkiye$", "Türkiye", x) # Korrigiert zunächst die fehlerhaft codierte Schreibweise von "Türkiye"
	x <- gsub("[^[:alnum:] ÄÖÜäöüß &]", "", x) # Entfernt Zeichen, die keine Buchstaben, Ziffern, Leerzeichen, Umlaute oder & sind
	x <- gsub("^Turkiye$|^Türkiye$", "Turkey", x) # Vereinheitlicht unterschiedliche Schreibweisen der Türkei auf "Turkey"
	x <- gsub("^Korea Rep$", "Korea", x) # Vereinheitlicht die Bezeichnung "Korea Rep" auf "Korea"
	x <- gsub("^Naoero$", "Nauru", x) # Vereinheitlicht die Bezeichnung "Naoero" auf "Nauru"
	x <- gsub("^Somalia Fed Rep$", "Somalia", x) # Vereinheitlicht die Bezeichnung "Somalia Fed Rep" auf "Somalia"
	x <- gsub("^China Peoples Republic of$", "China", x) # Vereinheitlicht die Bezeichnung "China Peoples Republic of" auf "China"
	x # Ausgabe des angepassten Vektors 
}
# Hilfsfunktion, um Datenrahmen für Zeitreihenanalysen vorzubereiten. Für jedes Land wird für jedes Jahr des Untersuchungszeitraums eine Zeile erzeugt. 
# Dadurch entstehen vollständige Länder-Jahr-Zeitreihen, auch wenn für einzelne Jahre keine Beobachtungswerte vorhanden sind.
# - df ist der Datenrahmen
# - years enthält die gewünschten Jahre
prepare_ts <- function(df, years = YEARS) {
	# unique() stellt sicher, dass jede Kombination aus Ländercode und Land nur einmal vorkommt
	countries <- unique(df[c("Laendercode", "Land")])
	# merge(..., by = NULL) bildet das kartesische Produkt aus Ländern und Jahren und erzeugt so für jedes Land eine Zeile für jedes untersuchte Jahr
	complete <- merge(countries, data.frame(Jahr = years), by = NULL)
	# Vorhandene Beobachtungen werden ergänzt, dabei erhalten fehlende Länder-Jahr-Kombinationen NA
	# all.x = TRUE stellt sicher, dass alle erzeugten Länder-Jahr-Kombinationen erhalten bleiben
	out <- merge(complete, df, by = c("Laendercode", "Land", "Jahr"), all.x = TRUE) 
	out <- out[order(out$Laendercode, out$Jahr), ] # Sortiert nach Ländercode und innerhalb jedes Landes aufsteigend nach Jahr
	rownames(out) <- NULL # Entfernt die bisherigen Zeilennamen und vergibt fortlaufende Standard-Zeilennamen entsprechend der neuen Sortierung
	out # Gibt den aufbereiteten Datenrahmen zurück
}
# Vereinheitlicht OECD-Datensätze mit dem Aufbau TIME_PERIOD, REF_AREA, Reference.area und OBS_VALUE
# - df ist der Datenrahmen, value_name ist der gewünschte Name der Messspalte
# - multiplier skaliert die Messwerte. Er ermöglicht so die Umrechnung von Millionen in absolute Werte.
simplify_oecd <- function(df, value_name, multiplier = 1) {
	out <- df[c("TIME_PERIOD", "REF_AREA", "Reference.area", "OBS_VALUE")] # Wählt die benötigten Spalten aus
	names(out) <- c("Jahr", "Laendercode", "Land", value_name) # Benennt die Spalten einheitlich um
	out$Jahr <- as.numeric(out$Jahr) # Wandelt die Einträge in numerische Werte um
	out$Land <- fix_name(out$Land) # Vereinheitlichung und Bereinigung der Ländernamen
	out[[value_name]] <- as.numeric(out[[value_name]]) * multiplier # Messwerte numerisch konvertieren und skalieren
	out 
}
# Überführt Weltbankdaten vom Wide- in das Long-Format und vereinheitlicht die Schlüsselspalten
# - df ist der Datenrahmen, value_name ist der gewünschte Name der Messspalte
wb_to_long <- function(df, value_name) {
	# Identifiziert die Spalten des Untersuchungszeitraums. sub("^X", "", ...) entfernt das von read.csv() ergänzte "X" vor numerischen Spaltennamen.
	year_cols <- names(df)[sub("^X", "", names(df)) %in% as.character(YEARS)]
	# Bricht die Verarbeitung ab, wenn eine erwartete Jahresspalte fehlt
	if (length(year_cols) != length(YEARS)) stop("Nicht alle Jahres-Spalten des Untersuchungszeitraums wurden gefunden.")
	# reshape() überführt die Jahreswerte aus mehreren einzelnen Spalten in eine gemeinsame Jahres- und Wertespalte
	long <- reshape(
		df,
		varying = year_cols, # Gibt die umzuformenden Spalten an
		v.names = value_name, # Name der neuen Spalte, in der die Werte gespeichert werden
		timevar = "Jahr", # Name der neuen "Zeitspalte" 
		times = sub("^X", "", year_cols), # Übernimmt die Jahreszahlen ohne das vorangestellte "X"
		direction = "long" # Legt fest, dass die Umwandlung vom Wide- ins Long-Format erfolgt
	)
	out <- data.frame(Laendercode = long$Country.Code, Land = fix_name(long$Country.Name), Jahr = as.numeric(long$Jahr), 
		Wert = as.numeric(long[[value_name]]))
	names(out)[4] <- value_name # Zum Beschriften der Messspalte 
	out
}
# Prüft, ob der Datensatz mindestens eine Zeile enthält und jede Länder-Jahr-Kombination eindeutig ist
# - df ist der Datenrahmen, name ist die Bezeichnung des Datenrahmens für die Fehlermeldung 
check_dataset <- function(df, name) {
	if (nrow(df) == 0) stop(paste("Keine gültigen Beobachtungen in", name))
	if (anyDuplicated(df[c("Laendercode", "Jahr")])) stop(paste("Doppelte Länder-Jahr-Kombinationen in", name))
}


### 1. Dateien laden

# Lädt die OECD-, Weltbank- (WB) und ICP-Datensätze. Der Weltbank-Internetindikator stammt von der ITU. Die Datensätze befinden sich im Ordner Datensaetze.  
# Durch na.strings = NA_VALUES werden leere Felder, "NA" und ".." als fehlende Werte behandelt
oecd_spending <- read.csv("Datensaetze/oecd_aic.csv", na.strings = NA_VALUES)
oecd_bb <- read.csv("Datensaetze/oecd_internet_access_2005bis2021.csv", na.strings = NA_VALUES)
oecd_pop <- read.csv("Datensaetze/oecd_population_2005bis2021.csv", na.strings = NA_VALUES)
# Die ersten vier Zeilen der Weltbankdateien enthalten Metadaten und werden daher übersprungen
wb_internet <- read.csv("Datensaetze/worldbank_internet.csv", na.strings = NA_VALUES, skip = 4)
wb_spending <- read.csv("Datensaetze/worldbank_household_spending.csv", na.strings = NA_VALUES, skip = 4)
wb_pop <- read.csv("Datensaetze/worldbank_population.csv", na.strings = NA_VALUES, skip = 4)
# ICP-Daten aus der Weltbankdatenbank für AIC 2021
icp_2021 <- read.csv("Datensaetze/icp_2021.csv", check.names = FALSE, na.strings = NA_VALUES)
# Zusätzliche Filter und Prüfungen, um versehentlich unpassende Indikatoren oder Kategorien auszuschließen 
oecd_spending <- oecd_spending[ oecd_spending$TIME_PERIOD %in% YEARS & oecd_spending$TRANSACTION == "P41" & oecd_spending$UNIT_MEASURE == "USD_PPP" 
			& oecd_spending$PRICE_BASE == "V",]
oecd_bb <- oecd_bb[oecd_bb$TIME_PERIOD %in% YEARS & oecd_bb$MEASURE == "B21_HH" & oecd_bb$UNIT_MEASURE == "PT_HH" & oecd_bb$GEO_AREA == "_T" 
			& oecd_bb$INCOME_GROUP == "_T",]
oecd_pop <- oecd_pop[oecd_pop$TIME_PERIOD %in% YEARS & oecd_pop$MEASURE == "POP" & oecd_pop$UNIT_MEASURE == "PS" & oecd_pop$SEX == "_T" 
			& oecd_pop$AGE == "_T",]
icp_2021 <- icp_2021[icp_2021[["Classification Code"]] == "PCAP.PP" & icp_2021[["Series Code"]] == 9020000,]
# ! kehrt die Bedingung um. stop() bricht die Ausführung mit einer Fehlermeldung ab, wenn nicht alle Zeilen den erwarteten Indikator enthalten.
if (!all(wb_internet$Indicator.Code == "IT.NET.USER.ZS")) stop("Falscher Weltbank-Internetindikator.")
if (!all(wb_spending$Indicator.Code == "NE.CON.PRVT.PP.CD")) stop("Falscher Weltbank-Ausgabenindikator.")
if (!all(wb_pop$Indicator.Code == "SP.POP.TOTL")) stop("Falscher Weltbank-Bevölkerungsindikator.")

### 2. OECD: AIC pro Kopf

# Extrahiert und vereinheitlicht die relevanten OECD-Spalten
# Die AIC-Gesamtausgaben werden von Millionen in absolute Werte umgerechnet (mit 1e6=10^6)
oecd_spending_simple <- simplify_oecd(oecd_spending, value_name = "HaushaltsausgabeTotal", multiplier = 1e6 )
oecd_pop_simple <- simplify_oecd(oecd_pop, value_name = "Bevoelkerungsanzahl")
check_dataset(oecd_spending_simple, "oecd_spending_simple") # Prüft, ob der Datensatz nicht leer ist und keine Länder-Jahr-Duplikate enthält
check_dataset(oecd_pop_simple, "oecd_pop_simple")
# Vereinigt AIC und Bevölkerungszahl über Ländercode und Jahr
oecd_spending_pc <- merge(oecd_spending_simple, oecd_pop_simple[c("Laendercode", "Jahr", "Bevoelkerungsanzahl")], by = c("Laendercode", "Jahr"))
# Berechnet AIC pro Kopf
oecd_spending_pc$AusgabeProKopf <- oecd_spending_pc$HaushaltsausgabeTotal / oecd_spending_pc$Bevoelkerungsanzahl
# Behält nur die für die weiteren Analysen benötigten Spalten
oecd_spending_pc_final <- oecd_spending_pc[c("Land", "Laendercode", "Jahr", "AusgabeProKopf", "Bevoelkerungsanzahl")]
# Zum Debuggen
# View(oecd_spending_pc_final)

### 3. OECD: Internetdaten

# Die OECD-Daten beziehen sich auf den Breitbandzugang in Prozent der Haushalte
# Zur Vereinheitlichung der Spaltennamen wird der OECD-Indikator als Internetzugang bezeichnet
oecd_bb_simple <- simplify_oecd(oecd_bb, value_name = "Internetzugang")
check_dataset(oecd_bb_simple, "oecd_bb_simple")
# Zum Debuggen
# View(oecd_bb_simple)

### 4. OECD: Finaler Datensatz

# Full Join (all = TRUE) der Internet-, Ausgaben- und Bevölkerungsdaten. So bleiben Werte erhalten, die nur in einem der Datenrahmen vorhanden sind.
oecd_final <- merge(oecd_bb_simple, oecd_spending_pc_final, by = c("Laendercode", "Land", "Jahr"), all = TRUE)
# Beschränkt den Datensatz auf 2005 bis 2021 und ergänzt fehlende Länder-Jahr-Kombinationen
oecd_final <- prepare_ts(oecd_final)
# Enthält die OECD-Werte von 2021, um festzulegen, für welche Länder OECD-Daten verwendet werden, und liefert Werte für den späteren Scoring-Datensatz
oecd_final_2021 <- oecd_final[oecd_final$Jahr == 2021, c("Laendercode", "Land", "Jahr", "Internetzugang", "AusgabeProKopf", "Bevoelkerungsanzahl")]

### 5. Weltbank: Haushaltsausgaben (Haushalte und NPISH) pro Kopf

# Überführt Ausgaben- und Bevölkerungsdaten in das Long-Format
wb_spending_long <- wb_to_long(wb_spending, "HaushaltsausgabeTotal")
wb_pop_long <- wb_to_long(wb_pop, "Bevoelkerungsanzahl")
check_dataset(wb_spending_long, "wb_spending_long")
check_dataset(wb_pop_long, "wb_pop_long")
# Left Join (all.x = TRUE) der Ausgaben- und Bevölkerungsdaten. Damit bleiben alle Ausgabenzeilen erhalten und passende Bevölkerungswerte werden ergänzt.
wb_spending_pc <- merge(wb_spending_long, wb_pop_long[c("Laendercode", "Jahr", "Bevoelkerungsanzahl")], by = c("Laendercode", "Jahr"), all.x = TRUE)
# Berechnet die Haushaltsausgaben pro Kopf
wb_spending_pc$AusgabeProKopf <- wb_spending_pc$HaushaltsausgabeTotal / wb_spending_pc$Bevoelkerungsanzahl
# Behält nur die für die weiteren Analysen benötigten Spalten
wb_spending_pc_final <- wb_spending_pc[c("Laendercode", "Land", "Jahr", "AusgabeProKopf", "Bevoelkerungsanzahl")]
# Zum Debuggen
# View(wb_spending_pc_final)

### 6. Weltbank: Internetnutzung

# Überführt die Internetnutzung in Prozent der Bevölkerung in das Long-Format
wb_internet_final <- wb_to_long(wb_internet, "Internetzugang")
check_dataset(wb_internet_final, "wb_internet_final")
# Zum Debuggen
# View(wb_internet_final)

### 7. Weltbank und ICP: Enddatensatz für Analyse und Score

# Führt Internet- und Ausgabendaten über einen Full Join zusammen, sodass auch unvollständige Länder-Jahr-Kombinationen erhalten bleiben
wb_final <- merge(wb_internet_final, wb_spending_pc_final, by = c("Laendercode", "Land", "Jahr"), all = TRUE)
# Beschränkt den Datensatz auf 2005 bis 2021 und ergänzt fehlende Länder-Jahr-Kombinationen
wb_final <- prepare_ts(wb_final)
# 2021-Ausschnitt mit Internetnutzung und Bevölkerung. AIC pro Kopf wird anschließend aus ICP ergänzt.
wb_final_2021 <- wb_final[ wb_final$Jahr == 2021, c("Laendercode", "Land", "Jahr", "Internetzugang", "Bevoelkerungsanzahl")]
# Ermittelt ICP-Jahresspalten, deren Namen mit vier Ziffern beginnen, z. B. "2021 [YR2021]"
year_cols <- grep("^[0-9]{4}", names(icp_2021), value = TRUE)
# Vereinheitlicht die Spaltennamen für Land und Ländercode
names(icp_2021)[names(icp_2021) == "Country Name"] <- "Country.Name"
names(icp_2021)[names(icp_2021) == "Country Code"] <- "Country.Code"
# Überführt die ICP-Daten in das Long-Format
icp_2021_long <- reshape(
	icp_2021, varying = year_cols, v.names = "AusgabeProKopf",
	timevar = "Jahr", times = sub("^([0-9]{4}).*$", "\\1", year_cols), # Um aus "2021 [YR2021]" "2021" zu machen
	direction = "long" # Um den Datensatz in das Long-Format zu überführen
)
# Erstellt einen vereinfachten ICP-Datensatz für 2021 mit den Spalten Land, Laendercode, Jahr und AusgabeProKopf
icp_final <- data.frame(Land = fix_name(icp_2021_long$Country.Name), Laendercode = icp_2021_long$Country.Code,
	Jahr = as.numeric(icp_2021_long$Jahr), AusgabeProKopf = as.numeric(icp_2021_long$AusgabeProKopf))
check_dataset(icp_final, "icp_final")
icp_final <- icp_final[icp_final$Jahr == 2021, c("Laendercode", "Land", "Jahr", "AusgabeProKopf")]
# Ergebnis zu Debugging-Zwecken speichern
# write.csv(icp_final, "icp_final.csv", row.names = FALSE)
# Ergänzt die ICP-Ausgaben pro Kopf zu den Weltbankdaten für 2021
wb_final_2021 <- merge(wb_final_2021, icp_final[c("Laendercode", "Jahr", "AusgabeProKopf")], by = c("Laendercode", "Jahr"), all.x = TRUE)
# Zum Debuggen
# write.csv(wb_final_2021, "wb_final_2021.csv", row.names = FALSE)

### 8. OECD und Weltbank zusammenführen

# Die Daten über alle Jahre werden für die Analyse und zur Berechnung der Wachstumsindikatoren im Scoring verwendet.
# OECD-Daten werden bevorzugt, wenn für ein Land im Jahr 2021 vollständige Werte für Breitbandzugang, AIC pro Kopf und Bevölkerung vorliegen.
# Für alle übrigen Länder werden ausschließlich Weltbankdaten verwendet, damit die Quelle innerhalb einer Länderzeitreihe nicht wechselt.
# complete.cases() prüft, ob in allen drei benötigten Variablen Werte vorhanden sind, und berücksichtigt nur vollständige Zeilen
oecd_complete_2021 <- oecd_final_2021[complete.cases(oecd_final_2021[c("Internetzugang", "AusgabeProKopf", "Bevoelkerungsanzahl")]),] 
oecd_codes_2021 <- unique(oecd_complete_2021$Laendercode) # Speichert die Ländercodes der vollständigen OECD-Datensatzländer 
# Wählt für vollständige OECD-Datensatzländer die gesamte OECD-Zeitreihe und sonst die Weltbank-Zeitreihe
oecd_selected_all <- oecd_final[oecd_final$Laendercode %in% oecd_codes_2021, ] # %in% prüft, ob der jeweilige Ländercode in oecd_codes_2021 enthalten ist
wb_filtered_all <- wb_final[!wb_final$Laendercode %in% oecd_codes_2021, ]
oecd_selected_all$Quelle <- "OECD" # Um die Quelle für später zu speichern
wb_filtered_all$Quelle <- "WorldBank"
combined_final_all <- rbind(oecd_selected_all, wb_filtered_all) # rbind() verbindet beide Datenrahmen zeilenweise
# Erstellt für 2021 denselben Ländermix. Fehlen vollständige OECD-Werte, stammen Internet und Bevölkerung aus der Weltbank und AIC pro Kopf aus ICP 2021.
oecd_selected_2021 <- oecd_complete_2021
wb_filtered_2021 <- wb_final_2021[!wb_final_2021$Laendercode %in% oecd_codes_2021, ]
oecd_selected_2021$Quelle <- "OECD"
wb_filtered_2021$Quelle <- "WorldBank"
combined_final_2021 <- rbind(oecd_selected_2021, wb_filtered_2021)

### 9. Filterung aggregierter Einträge

# Entfernt anhand definierter Listen und Namensmuster Regionen, Einkommensgruppen und nicht souveräne Gebiete 
agg <- c(
	"Africa Eastern and Southern", "SubSaharan Africa", "Africa Western and Central", "Middle East North Africa Afghanistan & Pakistan", "OECD",
	"Arab World", "Caribbean small states", "South Asia", "Central Europe and the Baltics", "East Asia & Pacific", "Euro area", "North America", 
	"Europe & Central Asia", "European Union", "Fragile and conflict affected situations", "Fragility Conflict and Violence FCV situations", 
	"Small states", "Latin America & Caribbean", "Pacific island small states", "Not classified", "Sint Maarten Dutch part", "St Martin French part" 
)
non_sovereign <- c(
	"Bermuda", "Cayman Islands", "Hong Kong SAR China", "Macao SAR China", "British Virgin Islands", "Turks and Caicos Islands", "Channel Islands", 
	"Isle of Man", "Puerto Rico US", "Virgin Islands US", "Virgin Islands British",  "American Samoa", "Northern Mariana Islands", "West Bank and Gaza",
	"French Polynesia", "New Caledonia", "Aruba", "Curacao", "Sint Maarten (Dutch part)", "Greenland", "Faroe Islands", "Gibraltar", "Guam"
)
# Weitere aggregierte Einträge werden über Wortmuster erkannt
pattern <- "income|total|dividend|members|only|world|other|countries|\\bIDA\\b|\\bIBRD\\b|\\bHIPC\\b|SubSaharan"
# Hilfsfunktion zum Anwenden der Filter. Übrig bleiben nur Einträge, deren Ländername keines der im regulären Ausdruck 'pattern'
# erfassten Wortmuster enthält und die nicht in 'agg' oder 'non_sovereign' enthalten sind.
# - df ist der Datenrahmen
# - exclude_germany steuert, ob Deutschland ausgeschlossen wird
filter_countries <- function(df, exclude_germany = TRUE) {
	keep <- !grepl(pattern, df$Land, ignore.case = TRUE) & !df$Land %in% c(agg, non_sovereign)
	if (exclude_germany) keep <- keep & df$Land != "Germany" 
	df[keep, , drop = FALSE]
}
# Bereinigter OECD-Datensatz für die Korrelationsanalyse einschließlich Deutschland
oecd_corr <- filter_countries(oecd_final, exclude_germany = FALSE)
# Datensätze für Datenexploration, Prognose und Scoring ohne Deutschland
final_df_2021 <- filter_countries(combined_final_2021)
final_df_all <- filter_countries(combined_final_all)
oecd_final <- filter_countries(oecd_final)
wb_final <- filter_countries(wb_final)
# Optionale Exporte von Zwischenergebnissen für das Debugging
# write.csv(oecd_spending_pc_final, "oecd_spending_pc_final.csv", row.names = FALSE)
write.csv(final_df_all, "final_df_all.csv", row.names = FALSE)
write.csv(final_df_2021, "final_df_2021.csv", row.names = FALSE)
# write.csv(wb_final, "wb_final.csv", row.names = FALSE)
# write.csv(oecd_final, "oecd_final.csv", row.names = FALSE)

###### Datenexploration

# Hilfsfunktion zum Erstellen und Speichern einer PNG-Datei
# - out_file ist der Dateipfad
# - width und height bestimmen die Größe in Pixeln
# - draw ist die Funktion, die den eigentlichen Plot erzeugt
save_png <- function(out_file, width, height, draw) {
	dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE) # Legt den Ausgabeordner an, falls er noch nicht existiert
	# Öffnet das Grafik-Device und stellt mit on.exit(dev.off()) sicher, dass es auch bei einem Fehler geschlossen wird
	png(filename = out_file, width = width, height = height) # PNG-Datei öffnen
	on.exit(dev.off()) 
	draw()  # übergebene Plot-Funktion ausführen
	invisible(out_file) # Gibt den Ausgabepfad zurück, ohne ihn bei vielen erzeugten Grafiken jedes Mal in der Konsole anzuzeigen
}
# Hilfsfunktion zum Auswählen und chronologischen Sortieren der Daten eines Landes
# - df ist der Datenrahmen 
# - country ist das ausgewählte Land 
prepare_country_data <- function(df, country) {
	d <- df[df$Land == country, , drop = FALSE]
	d[order(d$Jahr), , drop = FALSE]
}
# Bestimmt, welche Zeilen Prognosewerte enthalten. Falls die Spalte IstPrognose fehlt, werden alle Zeilen als Beobachtungen behandelt
# - df ist der Datenrahmen
get_forecast_flag <- function(df) {
	# Prüft, ob der Datenrahmen die Spalte "IstPrognose" enthält
	if ("IstPrognose" %in% names(df)) { 
		df$IstPrognose %in% TRUE # Gibt für jede Zeile TRUE zurück, wenn sie als Prognose gekennzeichnet ist
	} else { 
		rep(FALSE, nrow(df)) # Erzeugt für jede Zeile FALSE, sodass alle Werte zunächst als Beobachtungen gelten
	}
}
# Bestimmt gültige Beobachtungs- und Prognosewerte einer Zeitreihe
# - df ist der Datenrahmen, values enthält die Messwerte
# - forecast steuert, ob Prognosewerte berücksichtigt werden
get_series_masks <- function(df, values, forecast = FALSE) {
	forecast_flag <- get_forecast_flag(df) # Kennzeichnet Prognosezeilen
	valid <- !is.na(df$Jahr) & !is.na(values) # Nur vorhandene Jahre und Messwerte
	if (!forecast) valid <- valid & !forecast_flag # Prognosen bei Bedarf ausschließen
	list(
		valid = valid, # alle darzustellenden Werte
		obs = valid & !forecast_flag, # beobachtete Werte
		fc = valid & forecast_flag, # prognostizierte Werte
		forecast_flag = forecast_flag  # ursprüngliche Prognosekennzeichnung
	)
}
# Zeichnet eine Zeitreihe in einen bereits geöffneten Plot. Beobachtungen werden als offene, Prognosen als ausgefüllte Punkte dargestellt.
# - df enthält die Zeitreihendaten, values enthält die Messwerte
# - col legt die Farbe der Punkte fest
# - forecast steuert, ob Prognosewerte dargestellt werden
# - line_col legt die Farbe der Verbindungslinie fest
# - lwd bestimmt die Breite der Linie
# - forecast_cex bestimmt die Größe der Prognosepunkte
draw_time_series <- function(df, values, col, forecast = FALSE, line_col = col, lwd = 2, forecast_cex = 2) {
	masks <- get_series_masks(df, values, forecast)
	if (!any(masks$valid)) return(NULL) # Beendet die Funktion, wenn keine gültigen Werte vorliegen
	# Verbindet gültige Werte und markiert beobachtete Werte durch offene Punkte.
	lines(df$Jahr[masks$valid], values[masks$valid], col = line_col, lwd = lwd)
	points(df$Jahr[masks$obs], values[masks$obs], col = col, pch = 1)
	# Prognosewerte (wenn vorhanden) werden bei aktivierter Prognosedarstellung als ausgefüllte Punkte (pch=16) markiert.
	if (forecast && any(masks$fc)) points(df$Jahr[masks$fc], values[masks$fc], col = col, pch = 16, cex = forecast_cex)
}
# Erstellt und speichert einen einzelnen Zeitreihenplot
# - df enthält die Zeitreihendaten, values enthält die darzustellenden Messwerte
# - out_file ist der Pfad der PNG-Datei
# - ylab ist die Beschriftung der y-Achse
# - main_title ist der Titel des Plots
# - col legt die Farbe der Zeitreihe fest
# - forecast steuert, ob Prognosewerte dargestellt werden
plot_single_time_series <- function(df, values, out_file, ylab, main_title, col, forecast = FALSE) {
	masks <- get_series_masks(df, values, forecast) # Bestimmt gültige Beobachtungs- und Prognosewerte einer Zeitreihe
	if (!any(masks$valid)) return(NULL) # Kein Plot ohne gültige Werte
	valid_years <- !is.na(df$Jahr) # Bestimmt die Zeilen mit vorhandenem Jahr
	if (!forecast) valid_years <- valid_years & !masks$forecast_flag # Prognosejahre gegebenenfalls ausschließen
	save_png(out_file, width = 1000, height = 500, draw = function() { # Zeichenfunktion festlegen
		# Erstellt eine leere Zeichenfläche mit den benötigten Achsengrenzen
		plot(
			NA, # Erstellt eine leere Zeichenfläche, in die die Zeitreihe anschließend eingezeichnet wird
			xlim = range(df$Jahr[valid_years], na.rm = TRUE), # Legt den Bereich der x-Achse anhand der gültigen Jahre fest
			ylim = range(values[masks$valid], na.rm = TRUE), # Legt den Bereich der y-Achse anhand der gültigen Messwerte fest
			xlab = "Jahr", # Beschriftung der x-Achse mit "Jahr"
			ylab = ylab, # Übernimmt die übergebene Beschriftung der y-Achse
			main = main_title, # Übernimmt den übergebenen Plot-Titel
			cex.axis = 1.2, # Schriftgröße der Achsenwerte 
			cex.lab = 1.2, # Schriftgröße der Achsenbeschriftungen
			cex.main = 1.3 # Schriftgröße des Plot-Titels
		)
		grid() # Raster zur besseren Orientierung
		draw_time_series(df, values, col, forecast) # Zeichnet die Zeitreihe
	})
}
# Erstellt und speichert ein horizontales Top-N-Balkendiagramm
# - df enthält die auszuwertenden Daten, value_col ist die darzustellende Messspalte als String
# - out_file ist der Pfad der PNG-Datei
# - xlab ist die Beschriftung der x-Achse
# - main_title ist der Titel des Diagramms
# - col legt die Balkenfarbe fest
# - n bestimmt die maximale Anzahl dargestellter Länder
# - scale ist der Divisor zur Skalierung der Werte, z. B. durch 1e6 für Millionen
# - xlim legt bei Bedarf die Grenzen der x-Achse fest. NULL bedeutet automatische Skalierung.
plot_top_n_bar <- function(df, value_col, out_file, xlab, main_title, col, n = 20, scale = 1, xlim = NULL) {
	d <- df[!is.na(df[[value_col]]), , drop = FALSE] # Entfernt fehlende Messwerte
	d <- head(d[order(d[[value_col]], decreasing = TRUE), ], n) # Wählt die höchsten n Werte
	d <- d[order(d[[value_col]]), , drop = FALSE] # Höchster Balken erscheint oben
	if (nrow(d) == 0) return(NULL) # Bricht die Plot-Erstellung ab, wenn keine gültigen Beobachtungen vorliegen
	save_png(out_file, width = 1000, height = 500, draw = function() {
		par(mar = c(5, 12, 4, 2)) # Ränder (unten, links, oben, rechts) vergrößern, damit lange Ländernamen Platz haben
		barplot( # Erzeugt ein Balkendiagramm
			d[[value_col]] / scale, # Zu zeichnende Werte, die bei Bedarf über scale skaliert werden
			names.arg = d$Land, # Verwendet die Ländernamen als Beschriftung der Balken
			horiz = TRUE, # Zeichnet horizontale Balken
			xlim = xlim, # Verwendet bei Bedarf festgelegte Achsengrenzen 
			las = 1, # Stellt die Achsenbeschriftungen horizontal dar
			col = col, # Legt die Balkenfarbe fest
			main = main_title, xlab = xlab,                
			cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.4, cex.names = 1.3 # Vergrößert die Ländernamen           
		)
	})
}
# Erstellt und speichert einen Zeitreihenplot für die Top-10-Länder. Die Rangbildung erfolgt anhand der beobachteten Werte des Jahres 2021.
# - df enthält die Zeitreihendaten, value_col ist die darzustellende Messspalte als String
# - out_file ist der Pfad der PNG-Datei
# - ylab ist die Beschriftung der y-Achse
# - main_title ist der Titel des Plots
# - forecast steuert, ob Prognosewerte dargestellt werden
plot_top10_ts <- function(df, value_col, out_file, ylab = "Wert", main_title = NULL, forecast = FALSE) {
	# Wählt nur vorhandene, tatsächlich beobachtete Werte des Jahres 2021 für die Rangbildung
	sub2021 <- df[df$Jahr == 2021 & !is.na(df[[value_col]]) & !get_forecast_flag(df), , drop = FALSE]
	# Bestimmt die zehn Länder mit den höchsten Werten im Jahr 2021
	countries_top10 <- unique(sub2021$Land[order(sub2021[[value_col]], decreasing = TRUE)])
	countries_top10 <- head(countries_top10, 10)
	if (!length(countries_top10)) return(NULL)
	# Legt die Farben der Länderlinien fest und macht die Linien leicht transparent
	cols <- c("red", "blue", "darkgreen", "orange", "purple", "brown", "pink", "cyan", "gold", "black")
	cols <- cols[seq_along(countries_top10)]
	cols_a <- adjustcolor(cols, alpha.f = 0.8)
	plot_df <- df[df$Land %in% countries_top10, , drop = FALSE]
	masks <- get_series_masks(plot_df, plot_df[[value_col]], forecast)
	if (!any(masks$valid)) return(NULL)
	save_png(out_file, width = 1100, height = 500, draw = function() {
		# Teilt die Zeichenfläche in Zeitreihenplot und Legende auf
		layout(matrix(c(1, 2), nrow = 2), heights = c(4, 1)) # Teilt die Bildhöhe in 80 % für den Plot und 20 % für den Legendenbereich auf
		par(mar = c(5, 5, 4, 2))
		plot(
			NA, xlim = range(plot_df$Jahr[masks$valid], na.rm = TRUE), ylim = range(plot_df[[value_col]][masks$valid], na.rm = TRUE),
			xlab = "Jahr", ylab = ylab, main = main_title, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5
		)
		grid()
		# Zeichnet die Zeitreihe jedes Top-10-Landes.
		for (i in seq_along(countries_top10)) {
			d <- prepare_country_data(df, countries_top10[i])
			draw_time_series(d, d[[value_col]], cols[i], forecast, cols_a[i])
		}
		# Erstellt die gemeinsame Legende unterhalb des Plots
		par(mar = c(0, 1, 0, 1))
		plot.new() # Erstellt eine leere Zeichenfläche für die Legende
		legend(
			"top", # Positioniert die Legende oben im Legendenbereich
			legend = countries_top10, # Verwendet die Top-10-Länder als Legendeneinträge
			col = cols, # Ordnet den Ländern die verwendeten Farben zu
			lwd = 2, # Legt die Breite der Linien in der Legende fest
			ncol = max(1, ceiling(length(countries_top10) / 2)), # Verteilt die Einträge auf mehrere Spalten
			cex = 1.5, # Vergrößert die Schrift der Legende
			x.intersp = 0.6, # Abstand zwischen Liniensymbol und Text
			y.intersp = 1.1, # Abstand zwischen den Legendenzeilen
			seg.len = 1.6, # Länge der dargestellten Liniensegmente
			bty = "n", # Entfernt den Rahmen der Legende
			title = if (forecast) { # Passt den Titel daran an, ob Prognosen dargestellt werden
				"Top-10-Länder (Rang nach 2021) mit Prognosewerten"
			} else {
				"Top-10-Länder (Rang nach 2021)"
			}
		)
		layout(1) # Setzt die Zeichenfläche wieder auf ein Panel zurück
	})
}

### 10. Beispiel: Top-20-Länder für ein Jahr

# Erzeugt vier Top-20-Balkendiagramme für 2021.
# 1.) Breitbandzugang (OECD)
plot_top_n_bar(
	df = oecd_final[oecd_final$Jahr == 2021, ], value_col = "Internetzugang", out_file = "plots_top20/Top20_Breitbandzugang_2021.png", 
	xlab = "Breitbandzugang (% der Haushalte)", main_title = "Top-20-Länder nach Breitbandzugang (% der Haushalte) im Jahr 2021", col = "blue", xlim = c(0, 100)
)
# 2.) AIC pro Kopf
plot_top_n_bar(
	df = final_df_2021, value_col = "AusgabeProKopf", out_file = "plots_top20/Top20_Ausgaben_2021.png", 
	xlab = "US-$ pro Kopf (KKP-bereinigt)", main_title = "Top-20-Länder nach AIC pro Kopf im Jahr 2021", col = "darkgreen"
)
# 3.) Bevölkerungszahl
plot_top_n_bar(
	df = final_df_2021, value_col = "Bevoelkerungsanzahl", out_file = "plots_top20/Top20_Bevoelkerung_2021.png",
	xlab = "Bevölkerung (in Mio.)", main_title = "Top-20-Länder nach Bevölkerungszahl im Jahr 2021", col = "orange", scale = 1e6
)
# 4.) Internetnutzung (Weltbank)
plot_top_n_bar(
	df = wb_final[wb_final$Jahr == 2021, ], value_col = "Internetzugang", out_file = "plots_top20/Top20_Internetnutzung_WorldBank_2021.png",
	xlab = "Internetnutzung (% der Bevölkerung)", main_title = "Top-20-Länder nach Internetnutzung (% der Bevölkerung) im Jahr 2021", col = "red"
)

### 11. Zeitreihenanalyse für ausgewählte Länder

# Darstellung des AIC pro Kopf für alle Länder im OECD-Datensatz. Diese Abbildung veranschaulicht bewusst, wie unübersichtlich eine Gesamtdarstellung wird.
countries <- sort(unique(oecd_final$Land))
cols <- rainbow(length(countries)) # Die Farbpalette 'rainbow' wurde aufgrund vieler unterscheidbarer Farben gewählt
cols_a <- adjustcolor(cols, alpha.f = 0.35) # Halbtransparente Linien für weniger Überdeckungen
save_png("plots_ts/OECD_all_AICProKopf_mit_Legende.png", 1800, 700, draw = function() {
	layout(matrix(c(1, 2), nrow = 2), heights = c(3, 2)) # Teilt die Bildhöhe in 60 % für den Plot und 40 % für den Legendenbereich auf
	par(mar = c(5, 5, 4, 2))
	plot(
		NA, xlim = range(oecd_final$Jahr, na.rm = TRUE), ylim = range(oecd_final$AusgabeProKopf, na.rm = TRUE),
		xlab = "Jahr", ylab = "US-$ pro Kopf (KKP-bereinigt)", cex.axis = 1.3, cex.lab = 1.3, cex.main = 1.4,
		main = "Entwicklung des AIC pro Kopf für alle Länder des bereinigten OECD-Datensatzes im Zeitraum 2005-2021" 
	)
	# Pro Land werden vorhandene Beobachtungen verbunden und als offene Punkte markiert
	for (i in seq_along(countries)) {
		d <- prepare_country_data(oecd_final, countries[i])
		draw_time_series(d, d$AusgabeProKopf, col = cols_a[i], lwd = 1)
	}
	par(mar = c(0, 1, 0, 1))
	plot.new() # Erstellt eine leere Zeichenfläche für die Legende
	legend(
		"top", legend = countries, col = cols, lwd = 2, ncol = max(1, ceiling(length(countries) / 5)),
		cex = 1.3, x.intersp = 0.5, y.intersp = 1.2, bty = "n", title = "Länderlegende"
	)
	layout(1) # Setzt die Zeichenfläche wieder auf ein einzelnes Panel zurück
})
# Erstellen der fünf Top-10-Zeitreihenplots
population_all <- final_df_all
population_all$BevoelkerungMio <- population_all$Bevoelkerungsanzahl / 1e6 # Bevölkerung in Millionen umrechnen
# 1.) OECD: AIC pro Kopf
plot_top10_ts(
	df = oecd_final, value_col = "AusgabeProKopf", out_file = "plots_ts/TS_Top10_AusgabeProKopf.png",
	ylab = "US-$ pro Kopf (KKP-bereinigt)", main_title = "Entwicklung des AIC pro Kopf der Top-10-Länder im Zeitraum 2005-2021"
)
# 2.) OECD: Breitbandzugang
plot_top10_ts(
	df = oecd_final, value_col = "Internetzugang", out_file = "plots_ts/TS_Top10_OECD_Internetzugang.png",
	ylab = "in % der Haushalte", main_title = "Entwicklung des Breitbandzugangs (% der Haushalte) der Top-10-Länder im Zeitraum 2005-2021"
)
# 3.) Weltbank: Haushaltsausgaben pro Kopf
plot_top10_ts(
	df = wb_final, value_col = "AusgabeProKopf", out_file = "plots_ts/TS_Top10_WB_AusgabeProKopf.png", ylab = "Internationale $ pro Kopf (KKP-bereinigt)", 
	main_title = "Entwicklung der Haushalts- und NPISH-Konsumausgaben pro Kopf der Top-10-Länder im Zeitraum 2005-2021"
)
# 4.) Weltbank: Internetnutzung
plot_top10_ts(
	df = wb_final, value_col = "Internetzugang", out_file = "plots_ts/TS_Top10_WB_Internetzugang.png", ylab = "in % der Bevölkerung",
	main_title = "Entwicklung der Internetnutzung (% der Bevölkerung) der Top-10-Länder im Zeitraum 2005-2021"
)
# 5.) Bevölkerung
plot_top10_ts(
	df = population_all, value_col = "BevoelkerungMio", out_file = "plots_ts/TS_Top10_Bevoelkerung.png", ylab = "Bevölkerung (in Mio.)",
	main_title = "Bevölkerungsentwicklung der Top-10-Länder im Zeitraum 2005-2021"
)

### 12. Beispiel für Normalisierung

# Hilfsfunktion zur Min-Max-Normalisierung. Skaliert x relativ zu Minimum und Maximum der Referenzwerte.
# - x enthält die Werte, die normalisiert werden sollen
# - reference enthält die Referenzwerte, aus denen Minimum und Maximum für die Skalierung bestimmt werden (damit Prognosewerte die Skalierung nicht 
# beeinflussen)
# Fehlende Werte bleiben als NA erhalten. Falls keine Streuung vorliegt, erhalten vorhandene Werte den neutralen Wert 0,5.
minmax <- function(x, reference=x) {
	if (all(is.na(reference))) return(rep(NA_real_, length(x))) # Fehlen alle Referenzwerte, wird ein gleich langer Vektor mit NA-Werten zurückgegeben
	min_x <- min(reference, na.rm = TRUE)
	max_x <- max(reference, na.rm = TRUE)
	if (min_x == max_x) return(ifelse(is.na(x), NA, 0.5))  # Falls alle Referenzwerte gleich sind, werden vorhandene Werte auf 0,5 gesetzt
	(x - min_x) / (max_x - min_x)  # Skaliert die Werte relativ zu Minimum und Maximum der Referenzwerte
}
# Daten für Österreich einmalig filtern und sortieren. Anschließend AIC pro Kopf auf den Bereich 0 bis 100 skalieren.
d <- prepare_country_data(oecd_final, "Austria")
spend_norm <- minmax(d$AusgabeProKopf) * 100
# a.) Darstellung ohne Normalisierung zur Veranschaulichung des Skalenproblems
save_png("Austria_ohne_Normalisierung.png", 1000, 500, draw = function() {
	plot(
		d$Jahr, d$AusgabeProKopf, type = "l", # Stellt die Zeitreihe als durchgehende Linie dar
		col = "blue", lwd = 2, ylab = "Originalwerte (unterschiedliche Einheiten)", xlab = "Jahr", cex.axis = 1.2, cex.lab = 1.1, cex.main = 1.2,
		main = "Vergleich von AIC pro Kopf und Breitbandzugang (% der Haushalte) - Österreich, ohne Normalisierung" 
	)
	lines(d$Jahr, d$Internetzugang, col = "red", lwd = 2) # Der Internetzugang ist wegen der deutlich kleineren Skala visuell nicht erkennbar
	legend("topleft", legend = c("AIC pro Kopf", "Breitbandzugang"), col = c("blue", "red"), lwd = 2, cex = 1.2)
})
# b.) Vergleichbare Darstellung auf einer gemeinsamen Skala von 0 bis 100
save_png("Austria_normierter_Vergleich.png", 1000, 500, draw = function() {
	plot(
		d$Jahr, spend_norm, type = "l", col = "blue", lwd = 2, ylim = c(0, 100), ylab = "Wert auf gemeinsamer Skala", xlab = "Jahr",
		main = "Vergleich von Min-Max-skaliertem AIC pro Kopf und Breitbandzugang (% der Haushalte) - Österreich", cex.axis = 1.2, cex.lab = 1.1, cex.main = 1.2
	)
	lines(d$Jahr, d$Internetzugang, col = "red", lwd = 2)
	legend(
		"topleft", legend = c("AIC pro Kopf (Min-Max-skaliert)", "Breitbandzugang (% der Haushalte)"), col = c("blue", "red"), lwd = 2, cex = 1.2
	)
})
# c.) Alternative Darstellung mit zwei Y-Achsen
save_png("Austria_zwei_Y_Achsen.png", 1000, 500, draw = function() {
	par(mar = c(5, 5, 4, 5)) # Etwas größerer rechter Rand für Beschriftung
	plot(
		d$Jahr, d$AusgabeProKopf, type = "l", col = "blue", lwd = 2, ylab = "US-$ pro Kopf (KKP-bereinigt)", xlab = "Jahr", cex.axis = 1.2, 
		cex.lab = 1.2, cex.main = 1.2, main = "Zwei-Achsen-Darstellung von AIC pro Kopf und Breitbandzugang (% der Haushalte) - Österreich"
	)
	par(new = TRUE)  # Ermöglicht das Überlagern eines zweiten Plots in derselben Zeichenfläche
	# Zeichnet den Breitbandzugang über die bestehende Grafik
	plot( d$Jahr, d$Internetzugang, type = "l", col = "red", lwd = 2,
		axes = FALSE, # Achsen werden nicht gezeichnet 
		xlab = "", ylab = ""
	)
	axis(side = 4, col.axis = "red", col = "red", cex.axis = 1.2) # Ergänzt die rechte y-Achse für den Breitbandzugang
	mtext("Breitbandzugang (% der Haushalte)", side = 4, line = 3, col = "red", cex = 1.3) # Beschriftet die rechte y-Achse
	legend("topleft", legend = c("AIC pro Kopf", "Breitbandzugang"), col = c("blue", "red"), lwd = 2, cex = 1.2 )
})

### 13. Normalisierung und Plots für OECD und Weltbank

# Hilfsfunktion für datenquellenspezifische Beschriftungen und Ordnernamen
# - source_name gibt die Datenquelle an, anhand der passende Titel, Achsenbeschriftungen und Legendentexte zurückgegeben werden
# - forecast steuert die Prognosebeschriftung und die Auswahl des Ausgabeordners
source_plot_config <- function(source_name, forecast = FALSE) {
	if (source_name == "OECD") { # Einstellungen für OECD-Daten
		list( # Gibt die datenquellenspezifischen Plot-Einstellungen gemeinsam als Liste zurück
			folder = if (forecast) "normierte_plots_mit_regression/oecd" else "normierte_plots/oecd",
			main = if (forecast) {
				"Vergleich von Min-Max-skaliertem AIC pro Kopf und Breitbandzugang (% der Haushalte) einschließlich Prognose"
			} else {
				"Vergleich von Min-Max-skaliertem AIC pro Kopf und Breitbandzugang (% der Haushalte)"
			},
			spend_legend = "AIC pro Kopf (Min-Max-skaliert)", internet_legend = "Breitbandzugang (% der Haushalte)",
			internet_ylab = "Breitbandzugang (% der Haushalte)", internet_title = "Breitbandzugang (% der Haushalte)",
			spend_ylab = "US-$ pro Kopf (KKP-bereinigt)", spend_title = "AIC pro Kopf", cex_main = 1.2
		)
	} else { # Einstellungen für Weltbank-Daten
		list(
			folder = if (forecast) "normierte_plots_mit_regression/worldbank" else "normierte_plots/worldbank",
			main = if (forecast) {
				"Vergleich von Min-Max-skalierten Haushalts- und NPISH-Konsumausgaben pro Kopf und Internetnutzung (% der Bevölkerung) einschließlich Prognose"
			} else {
				"Vergleich von Min-Max-skalierten Haushalts- und NPISH-Konsumausgaben pro Kopf und Internetnutzung (% der Bevölkerung)"
			},
			spend_legend = "Haushalts- und NPISH-Konsumausgaben pro Kopf (Min-Max-skaliert)", internet_legend = "Internetnutzung (% der Bevölkerung)",
			internet_ylab = "Internetnutzung (% der Bevölkerung)", internet_title = "Internetnutzung (% der Bevölkerung)",
			spend_ylab = "Internationale $ pro Kopf (KKP-bereinigt)", spend_title = "Haushalts- und NPISH-Konsumausgaben pro Kopf",
			cex_main = 1
		)
	}
}
# Erstellt für jedes Land einen Vergleichsplot von Ausgaben und Internetindikator.
# Die Ausgaben werden relativ zum historischen Min-Max-Bereich skaliert. Der Internetindikator liegt bereits als Prozentwert vor.
# - df ist der Datenrahmen, source_name gibt die Datenquelle an (OECD oder Weltbank)
# - forecast steuert, ob Prognosewerte berücksichtigt werden
plot_normalized_countries <- function(df, source_name = "OECD", forecast = FALSE) {
	config <- source_plot_config(source_name, forecast) # Lädt die zur Datenquelle passenden Beschriftungen und Ausgabeordner
	countries <- sort(unique(na.omit(df$Land))) # Ermittelt alle vorhandenen Länder und sortiert sie alphabetisch
	for (country in countries) {
		d <- prepare_country_data(df, country)
		# Bestimmt gültige Beobachtungs- und Prognosewerte für beide Zeitreihen
		masks_spend <- get_series_masks(d, d$AusgabeProKopf, forecast)
		masks_internet <- get_series_masks(d, d$Internetzugang, forecast)
		# Ohne Prognose werden mindestens vier gemeinsame Beobachtungen vorausgesetzt
		if (!forecast && sum(masks_spend$valid & masks_internet$valid) < 4) next
		# Beide Einzelreihen benötigen mindestens zwei gültige Werte
		if (sum(masks_spend$valid) < 2 || sum(masks_internet$valid) < 2) next
		# Nicht gültige Ausgabenwerte werden vor der Normalisierung auf NA gesetzt
		spend_values <- d$AusgabeProKopf
		spend_values[!masks_spend$valid] <- NA
		# Nur historische Werte bilden die Referenz, damit Prognosen die Min-Max-Skala nicht verändern
		observed_spend <- !masks_spend$forecast_flag & !is.na(spend_values)
		# Skaliert die Ausgaben relativ zum historischen Min-Max-Bereich
		spend_norm <- minmax(spend_values, reference = spend_values[observed_spend]) * 100 
		# Für die x-Achse werden alle Jahre berücksichtigt, in denen mindestens eine der beiden Reihen einen gültigen Wert besitzt
		valid_years <- masks_spend$valid | masks_internet$valid
		save_png(file.path(config$folder, paste0(country, ".png")), 1000, 500, draw = function() {
			plot(
				NA, xlim = range(d$Jahr[valid_years], na.rm = TRUE), ylim = range(c(0, 100, spend_norm[masks_spend$valid]),na.rm = TRUE), 
				xlab = "Jahr", ylab = "Wert auf gemeinsamer Skala", main = paste(config$main, "-", country),cex.axis = 1.2, cex.lab = 1.2, cex.main = config$cex_main
			)
			grid()
			draw_time_series(d, spend_norm, col = "blue", forecast = forecast)
			draw_time_series(d, d$Internetzugang, col = "red", forecast = forecast)
			legend("topleft", legend = c(config$spend_legend, config$internet_legend), col = c("blue", "red"), lwd = 2, cex = 1.2)
		})
	}
}
# Erstellt für jedes Land separate Zeitreihenplots für Internetindikator, Bevölkerung und Ausgaben.
# Die Plot-Einstellungen für die drei Indikatoren werden in einer gemeinsamen Liste gespeichert.
# - df ist der Datenrahmen
# - forecast steuert, ob Prognosewerte berücksichtigt werden
plot_country_indicators <- function(df, forecast = FALSE) {
	version <- if (forecast) "mit_prognose" else "ohne_prognose"
	title_suffix <- if (forecast) "einschließlich Prognose" else "2005-2021"
	base_folder <- file.path("plots_laender_einzeln", version)
	countries <- sort(unique(na.omit(df$Land)))
	for (country in countries) {
		d <- prepare_country_data(df, country)
		data_source <- unique(na.omit(d$Quelle))[1] # Erste nicht fehlende Quelle des Landes verwenden
		config <- source_plot_config(data_source, forecast) # Lädt die zur Datenquelle passenden Beschriftungen
		plot_specs <- list( # Speichert die Plot-Einstellungen der drei Indikatoren
			internet = list(
				values = d$Internetzugang, suffix = "_Internet.png",
				ylab = config$internet_ylab, title = paste(country, "-", config$internet_title), col = "red"
			),
			bevoelkerung = list(
				values = d$Bevoelkerungsanzahl / 1e6, suffix = "_Bevoelkerung.png",
				ylab = "Bevölkerung (in Mio.)", title = paste(country, "- Bevölkerungsentwicklung"), col = "orange"
			),
			ausgaben = list(
				values = d$AusgabeProKopf, suffix = "_Ausgaben.png",
				ylab = config$spend_ylab, title = paste(country, "-", config$spend_title), col = "green"
			)
		)
		for (type in names(plot_specs)) { # Durchläuft die drei Indikatortypen
			spec <- plot_specs[[type]] # Lädt die Einstellungen des aktuellen Indikators
			out_file <- file.path(base_folder, type, paste0(country, spec$suffix))
			plot_single_time_series(d, spec$values, out_file, ylab = spec$ylab,
				main_title = paste(spec$title, "-", title_suffix), col = spec$col, forecast = forecast)
		}
	}
}
# Erstellt die normierten Länderplots und die Einzelplots (ohne Prognosewerte)
plot_normalized_countries(oecd_final, "OECD", FALSE)
plot_normalized_countries(wb_final, "WorldBank", FALSE)
plot_country_indicators(final_df_all, FALSE)

###### Prognose

### 14. Prognose und Modellierung

# Ausgaben und Bevölkerung werden linear prognostiziert. Für den Internetindikator werden ein lineares und ein logistisches Modell verglichen.
# Alle Internetprognosen werden anschließend auf den Bereich 0 bis 100 % begrenzt.

# Hilfsfunktion für die Berechnung des Mean Absolute Error (MAE)
# - obs sind die tatsächlich beobachteten Werte (aus dem Test-Holdout)  
# - pred sind die vorhergesagten Werte (über predict)
mae <- function(obs, pred) {
	# abs() bildet die absoluten Abweichungen, mean() deren Mittelwert und na.rm = TRUE ignoriert fehlende Werte
	mean(abs(obs - pred), na.rm = TRUE)
}
# Erstellt für eine beliebige Messspalte eine lineare Prognose für die beiden Folgejahre
# - df enthält Jahr und die zu prognostizierende Messspalte, value_col ist der Spaltenname als String
# - n_recent legt fest, wie viele der letzten verfügbaren Beobachtungen für die Regression verwendet werden
linear_forecast <- function(df, value_col, n_recent = 6) {
	# Sortiert die Beobachtungen chronologisch nach Jahr
	df <- df[order(df$Jahr), , drop = FALSE]
	# Beschränkt die Regression auf die letzten n_recent verfügbaren Beobachtungen
	df <- tail(df, n_recent)
	years_new <- max(df$Jahr, na.rm = TRUE) + 1:2 # Bestimmt die beiden auf das letzte vorhandene Jahr folgenden Prognosejahre
	# Erstellt dynamisch eine Regressionsformel der Form Messwert ~ Jahr und schätzt darauf ein lineares Modell
	formel <- as.formula(paste(value_col, "~ Jahr"))
	model <- lm(formel, data = df)
	# Berechnet mit dem Modell die prognostizierten Werte für die beiden Folgejahre
	values_new <- predict(model, newdata = data.frame(Jahr = years_new))
	out <- data.frame(Jahr = years_new, Wert = as.numeric(values_new))
	names(out)[2] <- value_col
	out # Gibt die prognostizierten Werte mit den zugehörigen Jahren zurück
}
# Vergleicht für den Internetindikator ein logistisches und ein lineares Modell.
# Die letzten zwei der verwendeten Beobachtungen dienen als Holdout, um die Prognosefehler beider Modelle zu vergleichen.
# - d_int enthält die Jahreswerte und gültigen Beobachtungen des Internetindikators eines Landes
# - n_recent legt fest, wie viele der letzten verfügbaren Beobachtungen für Modellvergleich und Prognose verwendet werden
forecast_internet <- function(d_int, n_recent = 6) {
	# Sortiert die Beobachtungen chronologisch nach Jahr
	d_int <- d_int[order(d_int$Jahr), , drop = FALSE]
	# Beschränkt die Modellierung auf die letzten n_recent verfügbaren Beobachtungen
	d_int <- tail(d_int, n_recent)
	# Verschiebt die Zeitachse auf einen Startwert von 0, um die Schätzung des logistischen Modells zu stabilisieren
	d_int$year0 <- d_int$Jahr - min(d_int$Jahr)
	# Bestimmt die beiden zukünftigen Prognosejahre und die dazugehörigen verschobenen Jahreswerte
	years_new <- max(d_int$Jahr, na.rm = TRUE) + 1:2
	years0_new <- years_new - min(d_int$Jahr)
	# Teilt die Zeitreihe in Trainings- und Holdout-Daten. Die letzten zwei Beobachtungen dienen dem Vergleich der Prognosefehler beider Modelle.
	n <- nrow(d_int)
	train <- d_int[1:(n - 2), , drop = FALSE] # Trainingsdaten
	test <- d_int[(n - 1):n, , drop = FALSE] # Holdout-Daten
	# 1.) Logistisches Modell mit SSlogis
	# try() verhindert einen Programmabbruch, falls die nichtlineare Schätzung fehlschlägt
	model_nls <- try(nls(Internetzugang ~ SSlogis(year0, Asym, xmid, scal), data = train), silent = TRUE)
	# Falls das Modell nicht geschätzt werden konnte, wird sein Fehler auf unendlich gesetzt, damit es bei der späteren Modellwahl nicht ausgewählt wird
	error_nls <- if (inherits(model_nls, "try-error")) {
		Inf
	} else {
		# Begrenzt die Vorhersagen auf den gültigen Prozentbereich von 0 bis 100 %, indem Werte unter 0 auf 0 und über 100 auf 100 gesetzt werden
		pred_nls <- pmin(100, pmax(0, predict(model_nls, newdata = test))) 
		# Berechnet den MAE anhand der beiden zurückgehaltenen Beobachtungen
		mae(test$Internetzugang, pred_nls)
	}
	# 2.) Lineares Modell mit anschließender Begrenzung auf 0 bis 100 %
	model_lm <- lm(Internetzugang ~ year0, data = train) # Schätzt das lineare Modell mit den Trainingsdaten
	pred_lm <- pmin(100, pmax(0, predict(model_lm, newdata = test))) # Begrenzt die Vorhersagen auf 0 bis 100 %
	error_lm <- mae(test$Internetzugang, pred_lm) # Berechnet den MAE anhand der Holdout-Daten
	errors <- c(nls_logis = error_nls, lm_cap100 = error_lm) # Speichert die MAE-Werte beider Modelle 	
	holdout_model <- names(errors)[which.min(errors)] # Wählt den Namen des Modells mit dem kleinsten MAE
	# Das tatsächlich verwendete Modell entspricht zunächst dem Modell mit dem geringeren Fehler
	used_model <- holdout_model
	# Schätzt das im Holdout-Vergleich ausgewählte Modell erneut mit allen verwendeten Beobachtungen
	if (holdout_model == "nls_logis") {
		fit_full <- try(nls(Internetzugang ~ SSlogis(year0, Asym, xmid, scal), data = d_int), silent = TRUE)
		# Verwendet bei fehlgeschlagener logistischer Schätzung mit allen Beobachtungen das lineare Modell als Fallback
		if (inherits(fit_full, "try-error")) {
			fit_full <- lm(Internetzugang ~ year0, data = d_int) # Schätzt das lineare Modell mit allen verfügbaren Beobachtungen
			used_model <- "lm_cap100" # Speichert das tatsächlich verwendete Modell für die spätere Beschriftung
		}
	} else {
		fit_full <- lm(Internetzugang ~ year0, data = d_int) # Schätzt das lineare Modell mit allen verfügbaren Beobachtungen
	}
	# Prognostiziert den Internetindikator für die Folgejahre und begrenzt die Werte auf 0 bis 100 %
	predictions <- predict(fit_full, newdata = data.frame(year0 = years0_new))
	predictions <- pmin(100, pmax(0, predictions))
	# Gibt Prognosewerte, MAE-Werte und Informationen zur Modellwahl zurück
	list(
		forecast = data.frame(Jahr = years_new, Internetzugang = as.numeric(predictions)),
		errors = errors,  # MAE-Werte beider getesteten Modelle
		holdout_model = holdout_model, # Modell mit dem kleineren MAE
		used_model = used_model  # Tatsächlich für die Prognose verwendetes Modell
	)
}
# Erstellt einen leeren Prognosedatensatz mit derselben Spaltenstruktur wie die später erzeugten Prognosewerte
empty_forecast_rows <- function() {
	data.frame(Land = character(), Jahr = numeric(), AusgabeProKopf = numeric(), Internetzugang = numeric(), 
		Bevoelkerungsanzahl = numeric(), Quelle = character())
}
# Erstellt für jedes Land Prognosen für Ausgaben, Internetindikator und Bevölkerung.
# Ausgaben und Bevölkerung werden linear prognostiziert. Für den Internetindikator wird zwischen linearem und logistischem Modell gewählt.
# Die Rückgabe ist eine Liste mit den Ergebnissen des Internet-Modellvergleichs (mae) und den erzeugten Prognosewerten (forecast_rows).
# - df enthält die länderspezifischen Zeitreihen, source_name bezeichnet die zugehörige Datenquelle
forecast_country_series <- function(df, source_name = "OECD") {
	# Für den Internetmodellvergleich werden mindestens sechs Beobachtungen benötigt (4 Trainingswerte und 2 Holdout-Beobachtungen)
	min_internet_rows <- 6
	# Ermittelt alle Länder und bereitet Listen für Fehlermaße und Prognosen vor
	countries <- sort(unique(df$Land))
	mae_list <- list()
	forecast_list <- list()
	# Beschriftungen der Internetmodelle
	label_map <- c(nls_logis = "Logistisch (SSlogis)", lm_cap100 = "Linear (0-100%)")
	# Durchläuft alle Länder einzeln
	for (country in countries) {
		# Bereitet die Länderzeitreihe vor und behält nur die benötigten Variablen
		d <- prepare_country_data(df, country)[, c("Land", "Jahr", "AusgabeProKopf", "Internetzugang", "Bevoelkerungsanzahl")]
		# Die drei Zeitreihen werden getrennt aufbereitet, da sie unterschiedliche fehlende Werte und letzte Beobachtungsjahre haben können
		d_spend <- d[!is.na(d$AusgabeProKopf), c("Jahr", "AusgabeProKopf"), drop = FALSE]
		d_int <- d[!is.na(d$Internetzugang), c("Jahr", "Internetzugang"), drop = FALSE]
		d_pop <- d[!is.na(d$Bevoelkerungsanzahl), c("Jahr", "Bevoelkerungsanzahl"), drop = FALSE]
		# Erstellt leere Prognosedatensätze für die drei Indikatoren
		fc_spend <- data.frame(Jahr = numeric(), AusgabeProKopf = numeric())
		fc_int <- data.frame(Jahr = numeric(), Internetzugang = numeric())
		fc_pop <- data.frame(Jahr = numeric(), Bevoelkerungsanzahl = numeric())
		fc_int_result <- NULL
		# Prognostiziert die drei Indikatoren, wenn jeweils mindestens sechs Beobachtungen vorliegen
		if (nrow(d_spend) >= 6) fc_spend <- linear_forecast(d_spend, "AusgabeProKopf")
		if (nrow(d_pop) >= 6) fc_pop <- linear_forecast(d_pop, "Bevoelkerungsanzahl")
		if (nrow(d_int) >= min_internet_rows) {
			fc_int_result <- forecast_internet(d_int)
			fc_int <- fc_int_result$forecast
		}
		# Überspringt das Land nur, wenn für keinen der drei Indikatoren eine Prognose möglich ist
		if (nrow(fc_spend) == 0 && nrow(fc_int) == 0 && nrow(fc_pop) == 0) next
		# Fasst die vorhandenen Prognosejahre zu einem gemeinsamen Jahresraster zusammen
		forecast_years <- sort(unique(c(fc_spend$Jahr, fc_int$Jahr, fc_pop$Jahr)))
		# Erstellt den gemeinsamen Prognosedatensatz, zunächst mit fehlenden Werten gefüllt
		new_df <- data.frame(Land = country, Jahr = forecast_years, AusgabeProKopf = NA_real_, Internetzugang = NA_real_,
				Bevoelkerungsanzahl = NA_real_, Quelle = source_name)
		# Ergänzt die vorhandenen Prognosewerte für die Indikatoren 
		# match() ordnet die Prognosewerte den gemeinsamen Prognosejahren zu
		if (nrow(fc_spend) > 0) new_df$AusgabeProKopf <- fc_spend$AusgabeProKopf[match(forecast_years, fc_spend$Jahr)]
		if (nrow(fc_int) > 0) new_df$Internetzugang <- fc_int$Internetzugang[match(forecast_years, fc_int$Jahr)]
		if (nrow(fc_pop) > 0) new_df$Bevoelkerungsanzahl <- fc_pop$Bevoelkerungsanzahl[match(forecast_years, fc_pop$Jahr)]
		# Speichert MAE, Holdout-Sieger, tatsächlich verwendetes Modell und kennzeichnet, ob ein Fallback verwendet wurde
		if (!is.null(fc_int_result)) {
			holdout_model <- fc_int_result$holdout_model # Modell mit dem kleineren Holdout-MAE
			used_model <- fc_int_result$used_model # Tatsächlich für die Prognose verwendetes Modell
			mae_list[[country]] <- data.frame(Land = country, Quelle = source_name, 
				Modell = unname(label_map[names(fc_int_result$errors)]), # Bezeichnungen der verglichenen Modelle 
				MAE = as.numeric(fc_int_result$errors), # MAE-Werte beider Modelle 
				HoldoutSieger = label_map[[holdout_model]], # Modell mit dem kleineren Holdout-MAE 
				VerwendetesModell = label_map[[used_model]], # Modell der endgültigen Prognose 
				FallbackVerwendet = holdout_model != used_model # TRUE, wenn vom Holdout-Sieger abgewichen wurde
			)
		}
		forecast_list[[country]] <- new_df # Speichert den Prognosedatensatz unter dem jeweiligen Ländernamen
	}
	# Führt die länderspezifischen Ergebnislisten zu gemeinsamen Datenrahmen zusammen.
	# Falls eine Ergebnisliste leer ist, wird für dieses Listenelement ein leerer Datenrahmen zurückgegeben.
	list(
		mae = if (length(mae_list)) { # Prüft, ob MAE-Ergebnisse vorhanden sind
			do.call(rbind, mae_list) # do.call(rbind, ...) verbindet die einzelnen Datenrahmen zeilenweise
		} else {
			data.frame() # Gibt einen leeren Datenrahmen zurück
		},
		forecast_rows = if (length(forecast_list)) {
			do.call(rbind, forecast_list)
		} else {
			empty_forecast_rows() # Gibt einen leeren Prognosedatensatz mit passender Spaltenstruktur zurück
		}
	)
}
# Verbindet beobachtete Werte und Prognosewerte zu einem gemeinsamen Datensatz für die Plot-Erstellung
# - original enthält die ursprünglichen Beobachtungen
# - forecast_rows enthält die erzeugten Prognosewerte
# - plot_cols legt fest, welche Spalten für die Darstellung übernommen werden
bind_observed_forecast <- function(original, forecast_rows, plot_cols) {
	observed <- original[plot_cols] # Übernimmt die benötigten Spalten der beobachteten Daten
	observed$IstPrognose <- FALSE # Kennzeichnet diese Zeilen als tatsächliche Beobachtungen
	predicted <- forecast_rows[plot_cols] # Übernimmt dieselben Spalten aus den Prognosedaten
	predicted$IstPrognose <- TRUE # Kennzeichnet diese Zeilen als Prognosewerte
	rbind(observed, predicted) # Verbindet Beobachtungen und Prognosen zeilenweise
}
# Wählt Prognosewerte für bestimmte Länder aus und weist ihnen eine einheitliche Datenquelle zu
# - forecast_rows enthält alle erzeugten Prognosewerte
# - countries enthält die auszuwählenden Länder
# - source_name gibt die zugehörige Datenquelle an
# - plot_cols legt fest, welche Spalten übernommen werden
select_source_forecasts <- function(forecast_rows, countries, source_name, plot_cols) {
	# Filtert auf die gewünschten Länder und übernimmt nur die für den Plot benötigten Spalten
	out <- forecast_rows[forecast_rows$Land %in% countries, plot_cols, drop = FALSE]
	out$Quelle <- source_name # Trägt für alle ausgewählten Prognosen die angegebene Datenquelle ein
	out$IstPrognose <- TRUE # Kennzeichnet alle ausgewählten Zeilen als Prognosewerte
	out # Gibt den aufbereiteten Prognosedatensatz zurück
}
# Die Prognosefunktion wird getrennt auf OECD- und Weltbank-Zeitreihen angewandt
res_oecd <- forecast_country_series(oecd_final, "OECD")
res_wb <- forecast_country_series(wb_final, "WorldBank")
# Speichert den Internet-Modellvergleich für OECD- und Weltbankdaten für das Debuggen
write.csv(rbind(res_oecd$mae, res_wb$mae), file = "modellvergleich_internet.csv", row.names = FALSE)
# Erstellt eine gemeinsame Spaltenstruktur für beobachtete und prognostizierte Daten
plot_cols <- c("Land", "Jahr", "AusgabeProKopf", "Internetzugang", "Bevoelkerungsanzahl")
oecd_with_fc <- bind_observed_forecast(oecd_final, res_oecd$forecast_rows, plot_cols)
wb_with_fc <- bind_observed_forecast(wb_final, res_wb$forecast_rows, plot_cols)
# Übernimmt nur Prognosen für die im kombinierten Datensatz tatsächlich verwendete Datenquelle
oecd_fc_selected <- select_source_forecasts(
	res_oecd$forecast_rows, unique(final_df_all$Land[final_df_all$Quelle == "OECD"]), "OECD", plot_cols
) # Länder mit OECD als verwendeter Datenquelle
wb_fc_selected <- select_source_forecasts(
	res_wb$forecast_rows, unique(final_df_all$Land[final_df_all$Quelle == "WorldBank"]), "WorldBank", plot_cols
) # Länder mit Weltbank als verwendeter Datenquelle
# Verbindet die ursprünglichen Beobachtungen mit den ausgewählten Prognosen zu einem gemeinsamen Datensatz
final_original <- final_df_all[c(plot_cols, "Quelle")]
final_original$IstPrognose <- FALSE
final_df_all_with_fc <- rbind(
	final_original, oecd_fc_selected[c(plot_cols, "Quelle", "IstPrognose")], wb_fc_selected[c(plot_cols, "Quelle", "IstPrognose")]
)
# Sortiert nach Land und innerhalb jedes Landes chronologisch nach Jahr
final_df_all_with_fc <- final_df_all_with_fc[order(final_df_all_with_fc$Land, final_df_all_with_fc$Jahr),] 
# Erstellt die normierten Vergleichsplots und die Einzelindikatorplots mit den Prognosen
plot_normalized_countries(oecd_with_fc, "OECD", TRUE)
plot_normalized_countries(wb_with_fc, "WorldBank", TRUE)
plot_country_indicators(final_df_all_with_fc, TRUE)
# Erstellt die Top-10-Plots mit den ergänzenden Prognosewerten
population_with_fc <- final_df_all_with_fc
population_with_fc$BevoelkerungMio <- population_with_fc$Bevoelkerungsanzahl / 1e6
# 1.) OECD: AIC pro Kopf
plot_top10_ts(
	df = oecd_with_fc, value_col = "AusgabeProKopf", out_file = "plots_ts/TS_Top10_AusgabeProKopf_mit_Prognose.png", 
	ylab = "US-$ pro Kopf (KKP-bereinigt)", main_title = "Entwicklung des AIC pro Kopf der Top-10-Länder einschließlich Prognose", forecast = TRUE
)
# 2.) OECD: Breitbandzugang
plot_top10_ts(
	df = oecd_with_fc, value_col = "Internetzugang", out_file = "plots_ts/TS_Top10_OECD_Internetzugang_mit_Prognose.png", ylab = "in % der Haushalte",
	main_title = "Entwicklung des Breitbandzugangs (% der Haushalte) der Top-10-Länder einschließlich Prognose", forecast = TRUE
)
# 3.) Weltbank: Haushalts- und NPISH-Konsumausgaben pro Kopf
plot_top10_ts(
	df = wb_with_fc, value_col = "AusgabeProKopf", out_file = "plots_ts/TS_Top10_WB_AusgabeProKopf_mit_Prognose.png", ylab = "Internationale $ pro Kopf (KKP-bereinigt)",
	main_title = "Entwicklung der Haushalts- und NPISH-Konsumausgaben pro Kopf der Top-10-Länder einschließlich Prognose", forecast = TRUE
)
# 4.) Weltbank: Internetnutzung
plot_top10_ts(
	df = wb_with_fc, value_col = "Internetzugang", out_file = "plots_ts/TS_Top10_WB_Internetzugang_mit_Prognose.png", ylab = "in % der Bevölkerung",
	main_title = "Entwicklung der Internetnutzung (% der Bevölkerung) der Top-10-Länder einschließlich Prognose", forecast = TRUE
)
# 5.) Bevölkerung
plot_top10_ts(
	df = population_with_fc, value_col = "BevoelkerungMio", out_file = "plots_ts/TS_Top10_Bevoelkerung_mit_Prognose.png", ylab = "Bevölkerung (in Mio.)",
	main_title = "Bevölkerungsentwicklung der Top-10-Länder einschließlich Prognose", forecast = TRUE
)

###### Korrelationsanalyse

### 15. Korrelationsanalyse

# Entfernt Zeilen, in denen mindestens eine der beiden Korrelationsvariablen fehlt
complete_correlation_data <- function(df) {
	df[complete.cases(df[c("AusgabeProKopf", "Internetzugang")]), , drop = FALSE]
}
# Berechnet Pearson- und Spearman-Korrelationen für Gruppen, z. B. je Jahr oder je Land
# - df enthält die auszuwertenden Daten
# - group_col ist der Name der Gruppierungsspalte als String
calculate_group_correlations <- function(df, group_col) {
	groups <- split(df, df[[group_col]]) # Teilt den Datenrahmen anhand der angegebenen Gruppierungsspalte auf
	# Berechnet für jede Gruppe die beiden Korrelationskoeffizienten
	results <- lapply(names(groups), function(group_name) {
		d <- complete_correlation_data(groups[[group_name]])
		# Für Gruppen mit weniger als drei vollständigen Beobachtungen wird keine Korrelation berechnet
		if (nrow(d) < 3) return(NULL)
		# Speichert Gruppenname, Pearson- und Spearman-Korrelation
		data.frame(Gruppe = group_name, Pearson = cor(d$AusgabeProKopf, d$Internetzugang, method = "pearson"), 
			Spearman = cor(d$AusgabeProKopf, d$Internetzugang, method = "spearman"))
	})
	out <- do.call(rbind, results) # Verbindet die einzelnen Gruppenergebnisse zeilenweise zu einem Datenrahmen
	# Falls keine Gruppe genügend Beobachtungen enthält, wird ein leerer Datenrahmen zurückgegeben
	if (is.null(out)) return(data.frame())
	names(out)[1] <- group_col # Benennt die Gruppenspalte entsprechend der verwendeten Gruppierung um
	if (group_col == "Jahr") out$Jahr <- as.numeric(out$Jahr)   # Wandelt die durch split() entstandenen Jahresbezeichnungen wieder in numerische Werte um
	rownames(out) <- NULL   # Entfernt die durch rbind() entstandenen Zeilennamen
	out # Gibt den Datenrahmen mit den gruppenweisen Korrelationen zurück
}
# Erstellt für jede Gruppe ein Streudiagramm von AIC pro Kopf und Breitbandzugang
# - df enthält die auszuwertenden Daten, group_col ist der Name der Gruppierungsspalte als String
# - out_directory gibt den Ausgabeordner an
# - file_name ist eine Funktion, die den Dateinamen für jede Gruppe erzeugt
# - title_text ist eine Funktion, die den Plot-Titel für jede Gruppe erzeugt
# - width und height bestimmen die Größe der PNG-Datei
# - point_col legt die Farbe der Datenpunkte fest
plot_group_correlations <- function(df, group_col, out_directory, file_name, title_text, width = 1000, height = 500, point_col = "black") {
	groups <- split(df, df[[group_col]]) # Teilt den Datenrahmen anhand der angegebenen Gruppierungsspalte auf
	for (group_name in names(groups)) {
		d <- complete_correlation_data(groups[[group_name]])
		if (nrow(d) < 3) next  # Überspringt Gruppen mit weniger als drei vollständigen Beobachtungen
		save_png(
			file.path(out_directory, file_name(group_name)),
			width, height,
			draw = function() {
				plot(
					d$AusgabeProKopf, d$Internetzugang, pch = 19, col = point_col, xlab = "US-$ pro Kopf (KKP-bereinigt)",
					ylab = "Breitbandzugang (% der Haushalte)", main = title_text(group_name, d), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.2
				)
				# Schätzt eine lineare Regression und zeichnet die Regressionsgerade ein
				abline(lm(Internetzugang ~ AusgabeProKopf, data = d), col = "red", lwd = 2)
			}
		)
	}
}
# a.) Momentaufnahme für 2021: Zusammenhang zwischen AIC pro Kopf und Breitbandzugang
corr_2021 <- complete_correlation_data(oecd_corr[oecd_corr$Jahr == 2021, ])
cor.test(corr_2021$AusgabeProKopf, corr_2021$Internetzugang, method = "pearson") # Berechnet Pearson-Korrelation einschließlich Signifikanztest
cor.test(corr_2021$AusgabeProKopf, corr_2021$Internetzugang, method = "spearman") # Berechnet Spearman-Korrelation einschließlich Signifikanztest
model_2021 <- lm(Internetzugang ~ AusgabeProKopf, data = corr_2021) # Schätzt die lineare Regressionsbeziehung zwischen AIC pro Kopf und Breitbandzugang
summary(model_2021) # Gibt unter anderem Regressionskoeffizienten, Standardfehler und Signifikanztests aus
# Erzeugt gleichmäßig verteilte AIC-Werte für die Darstellung der Regressionsgeraden
prog_frame <- data.frame(AusgabeProKopf = seq(min(corr_2021$AusgabeProKopf), max(corr_2021$AusgabeProKopf), length.out = 100)) 
pc <- predict(model_2021, interval = "confidence", newdata = prog_frame) # Berechnet die geschätzten Regressionswerte und das 95-%-Konfidenzintervall
save_png("plots_corr/streudiagramm_2021_aic_pro_kopf_vs_breitband.png", 1000, 800, draw = function() {
	plot(
		corr_2021$AusgabeProKopf, corr_2021$Internetzugang, 
		ylim = range(c(corr_2021$Internetzugang, pc), na.rm = TRUE), # Damit das Konfidenzband nicht abgeschnitten wird
		main = "Zusammenhang zwischen AIC pro Kopf und Breitbandzugang (% der Haushalte) im Jahr 2021",
		xlab = "US-$ pro Kopf (KKP-bereinigt)", ylab = "Breitbandzugang (% der Haushalte)", pch = 19, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.2
	)
	matlines(
		prog_frame$AusgabeProKopf, pc, lty = c(1, 2, 2), col = "red", lwd = c(2, 1, 1)
	) # Zeichnet die Regressionsgerade sowie die untere und obere Grenze des 95-%-Konfidenzintervalls
})
# b.) Länderübergreifende Korrelation je Jahr
# Für jedes Jahr wird die Korrelation über die in diesem Jahr verfügbaren OECD-Länder berechnet
cor_per_year <- calculate_group_correlations(oecd_corr, "Jahr")
# c.) Korrelation je Land über alle verfügbaren Jahre
# Für jedes Land wird die Korrelation über die verfügbaren Jahresbeobachtungen dieses Landes berechnet
cor_compare <- calculate_group_correlations(oecd_corr, "Land")
# Erstellt für jedes Land ein Streudiagramm
plot_group_correlations(
	oecd_corr, group_col = "Land", out_directory = "plots_corr/by_country",
	file_name = function(country) paste0(country, ".png"),
	title_text = function(country, d) {
		paste0("Zusammenhang zwischen AIC pro Kopf und Breitbandzugang (% der Haushalte) - ",country, " (", min(d$Jahr), "-", max(d$Jahr), ")")
	}
)
# Erstellt Streudiagramme je Jahr über alle Länder
plot_group_correlations(
	oecd_corr[oecd_corr$Jahr %in% YEARS, ], group_col = "Jahr", out_directory = "plots_corr/by_year",
	file_name = function(year) paste0("streudiagramm_", year, ".png"), 
	title_text = function(year, d) {
		paste("Zusammenhang zwischen AIC pro Kopf und Breitbandzugang (% der Haushalte) im Jahr", year)
	},
	width = 1000, height = 500, point_col = adjustcolor("black", alpha.f = 0.75)
)
# Ausgabe der Zusammenfassung zur Kontrolle/zum Debuggen und Export der Ergebnisse
summary(cor_compare$Pearson)
summary(cor_compare$Spearman)
summary(cor_per_year$Pearson)
summary(cor_per_year$Spearman)
write.csv(cor_compare, "cor_compare.csv", row.names = FALSE)
write.csv(cor_per_year, "cor_per_year.csv", row.names = FALSE)

###### Expansions- und Preisempfehlung

### 16. Scoring und regulatorische Anpassung

# Auswahl der Daten für den Score
scoring_2021 <- final_df_2021
n_all <- length(unique(scoring_2021$Land)) # Dokumentiert die Anzahl aller betrachteten Länder
print(paste("Betrachtete Länder: ", n_all)) # Gibt die Anzahl in der Konsole aus
# Für das Scoring werden nur Länder berücksichtigt, für die im Referenzjahr 2021 Internetindikator, Ausgaben pro Kopf und Bevölkerung (strukturelle Marktindikatoren) vorliegen
core_complete <- complete.cases(scoring_2021[c("Internetzugang", "AusgabeProKopf", "Bevoelkerungsanzahl")])
# Zeigt Länder mit unvollständigen strukturellen Marktindikatoren (für das Debuggen)
# scoring_2021[!core_complete,c("Land", "Quelle", "Internetzugang","AusgabeProKopf", "Bevoelkerungsanzahl")]
# Beschränkt das eigentliche Scoring auf Länder mit vollständigen strukturellen Marktindikatoren
scoring_2021 <- scoring_2021[core_complete, , drop = FALSE]
n_scoring <- length(unique(scoring_2021$Land)) # Dokumentiert die Zahl der tatsächlich bewertbaren Länder
print(paste("Im Scoring berücksichtigte Länder: ", n_scoring))
# Schätzt für jedes Land die lineare jährliche Steigung einer ausgewählten Messgröße
# - df ist der Datenrahmen, value_col ist der Name der Messspalte als String
# - n_recent ist die Anzahl der letzten verfügbaren Beobachtungen, die in die Berechnung einfließen
estimate_country_slope <- function(df, value_col, n_recent = 6) {
	groups <- split(df, df$Laendercode) # Teilt den Datenrahmen in einzelne Ländergruppen auf
	# Wendet die Funktion auf alle Ländergruppen an und erwartet pro Land genau einen numerischen Rückgabewert
	vapply(groups, function(d) {
		# Behält nur Jahr und Messwert und entfernt Zeilen mit fehlenden Messwerten
		d <- d[!is.na(d[[value_col]]), c("Jahr", value_col), drop = FALSE]
		d <- d[order(d$Jahr), , drop = FALSE]
		if (nrow(d) < n_recent) return(NA_real_) # Gibt NA zurück, wenn weniger Beobachtungen vorhanden sind, als benötigt werden
		d <- tail(d, n_recent) # Beschränkt die Analyse auf die letzten n_recent verfügbaren Beobachtungen
		# Erstellt dynamisch eine Regressionsformel der Form Messgröße ~ Jahr
		formel <- as.formula(paste(value_col, "~ Jahr"))
		# Schätzt für das jeweilige Land eine lineare Regression über die Zeit
		model <- lm(formel, data = d)
		# Gibt den Koeffizienten von "Jahr" als geschätzte jährliche lineare Veränderung zurück
		coef(model)["Jahr"]
	}, numeric(1)) # Erwartet genau einen numerischen Wert
}
# Schätzt für jedes Land den relativen jährlichen Wachstumstrend einer ausgewählten Messgröße anhand einer logarithmierten linearen Regression
# - df ist der Datenrahmen, value_col ist der Name der Messspalte als String
# - n_recent ist die Anzahl der letzten verfügbaren Beobachtungen, die in die Berechnung einfließen
estimate_country_growth_pct <- function(df, value_col, n_recent = 6) {
	groups <- split(df, df$Laendercode) # Teilt den Datenrahmen in einzelne Ländergruppen auf
	# Wendet die Funktion auf alle Ländergruppen an und erwartet pro Land genau einen numerischen Rückgabewert
	vapply(groups, function(d) {
		# Behält nur Jahr und Messwert, entfernt fehlende Werte und schließt Werte <= 0 aus, da diese nicht logarithmiert werden können
		d <- d[!is.na(d[[value_col]]) & d[[value_col]] > 0, c("Jahr", value_col), drop = FALSE]
		d <- d[order(d$Jahr), , drop = FALSE]
		if (nrow(d) < n_recent) return(NA_real_) # Gibt NA zurück, wenn weniger Beobachtungen vorhanden sind, als benötigt werden
		d <- tail(d, n_recent) # Beschränkt die Analyse auf die letzten n_recent verfügbaren Beobachtungen
		# Logarithmiert die Messwerte, damit relative Veränderungen über die Zeit geschätzt werden können
		d$LogWert <- log(d[[value_col]])
		# Schätzt für das jeweilige Land eine lineare Regression des logarithmierten Messwerts über die Zeit
		model <- lm(LogWert ~ Jahr, data = d)
		# Speichert den geschätzten Jahreskoeffizienten
		beta <- coef(model)["Jahr"]
		# Wandelt den Regressionskoeffizienten in eine geschätzte jährliche relative Veränderungsrate in Prozent um
		100 * (exp(beta) - 1)
	}, numeric(1))
}
# Berechnet die drei Wachstumsindikatoren 
codes <- unique(scoring_2021$Laendercode)
# Internetindikator: geschätzte Veränderung in Prozentpunkten pro Jahr
growth_internet <- estimate_country_slope(final_df_all, "Internetzugang")
# Ausgaben pro Kopf: relative jährliche Veränderung in %
# AIC pro Kopf für OECD-Datensatz, Haushalts- und NPISH-Konsumausgaben pro Kopf für Weltbank-Datensatz
growth_spending <- estimate_country_growth_pct(final_df_all, "AusgabeProKopf")
# Bevölkerung: relative jährliche Veränderung in %
growth_population <- estimate_country_growth_pct(final_df_all,"Bevoelkerungsanzahl")
# Führt die Wachstumsindikatoren zusammen
growth_df <- data.frame(Laendercode= codes, WachstumInternet = unname(growth_internet[codes]), WachstumAusgaben = unname(growth_spending[codes]),WachstumBevoelkerung = unname(growth_population[codes]))
# Ergänzt die Wachstumsindikatoren zu den Werten von 2021
scored <- merge(scoring_2021, growth_df, by = "Laendercode", all.x = TRUE)
# Die folgenden Quantile dienten zur Prüfung der Verteilungen aller Scoreindikatoren vor der Normalisierung bzw. Winsorisierung. Sie zeigen das Minimum, 
# 1%-, 2%-, 98%-, 98,5%- und 99%-Quantil und das Maximum an. Die Prüfaufrufe sind auskommentiert, um zusätzliche Konsolenausgaben zu vermeiden.
# quantile(scored$AusgabeProKopf, probs = c(0, 0.01, 0.02, 0.98, 0.985, 0.99, 1), na.rm = TRUE)
# quantile(scored$Internetzugang, probs = c(0, 0.01, 0.02, 0.98, 0.985, 0.99, 1), na.rm = TRUE)
# quantile(scored$Bevoelkerungsanzahl, probs = c(0, 0.01, 0.02, 0.98, 0.985, 0.99, 1), na.rm = TRUE)
# quantile(growth_df$WachstumInternet, probs = c(0, 0.01, 0.02, 0.98, 0.985, 0.99, 1), na.rm = TRUE)
# quantile(growth_df$WachstumAusgaben,probs = c(0, 0.01, 0.02, 0.98, 0.985, 0.99, 1), na.rm = TRUE)
# quantile(growth_df$WachstumBevoelkerung, probs = c(0, 0.01, 0.02, 0.98, 0.985, 0.99, 1), na.rm = TRUE)
# Hilfsfunktion zum Winsorisieren 
# - x ist der zu winsorisierende Vektor
# - low ist die untere Quantilsgrenze und up die obere Quantilsgrenze
winsorize <- function(x, low = 0, up = 1) {
	if (low < 0 || up > 1 || low >= up) { # Prüft die Gültigkeit der Quantilsgrenzen
		stop("low und up müssen zwischen 0 und 1 liegen und die untere Quantilsgrenze muss kleiner als die obere sein.")
	}
	lower <- quantile(x, probs = low, na.rm = TRUE)
	upper <- quantile(x, probs = up, na.rm = TRUE)
	pmax(lower, pmin(x, upper)) # Begrenzt Werte auf die untere und obere Quantilsgrenze
} 
# Normalisiert die Indikatoren und winsorisiert Bevölkerung und Ausgabenwachstum
scored$Internet_norm <- minmax(scored$Internetzugang)
scored$Ausgabe_norm <- minmax(scored$AusgabeProKopf)
scored$Bev_norm <- minmax(winsorize(scored$Bevoelkerungsanzahl, 0, 0.99))
scored$WachstumInternet_norm <- minmax(scored$WachstumInternet)
scored$WachstumAusgaben_norm <- minmax(winsorize(scored$WachstumAusgaben, 0.01, 1))
scored$WachstumBevoelkerung_norm <- minmax(scored$WachstumBevoelkerung)
# Zeigt Länder mit fehlenden Wachstumsindikatoren zur Kontrolle vor der Medianersetzung. 
# Der Aufruf ist auskommentiert, um zusätzliche Konsolenausgaben zu vermeiden.
# scored[is.na(scored$WachstumInternet) |is.na(scored$WachstumAusgaben) | is.na(scored$WachstumBevoelkerung), 
# c("Land", "Quelle", "WachstumInternet","WachstumAusgaben", "WachstumBevoelkerung")]
# Fehlende normalisierte Wachstumsindikatoren werden durch den Median der jeweiligen Datenquelle ersetzt
# - x enthält die zu ergänzenden Werte, source gibt die Datenquelle an, damit die Medianersetzung getrennt für OECD- und Weltbankdaten erfolgen kann
fill_source_median <- function(x, source) {
	out <- x
	for (s in unique(source)) { # Durchläuft die vorhandenen Datenquellen (OECD/WorldBank)
		idx_source <- source == s # Markiert Beobachtungen der aktuellen Datenquelle
		median_source <- median(x[idx_source], na.rm = TRUE) # Berechnet deren Median ohne fehlende Werte
		out[idx_source & is.na(out)] <- median_source # Ersetzt fehlende Werte durch den Median der Quelle
	}
	out # Gibt den Vektor mit ergänzten Werten zurück
}
scored$WachstumInternet_norm <- fill_source_median(scored$WachstumInternet_norm, scored$Quelle)
scored$WachstumAusgaben_norm <- fill_source_median(scored$WachstumAusgaben_norm, scored$Quelle)
scored$WachstumBevoelkerung_norm <- fill_source_median(scored$WachstumBevoelkerung_norm, scored$Quelle)
# Prüft, ob alle sechs Scorebestandteile vollständig sind
score_components <- c("Internet_norm","Ausgabe_norm","Bev_norm","WachstumInternet_norm","WachstumAusgaben_norm","WachstumBevoelkerung_norm")
if (anyNA(scored[score_components])) stop("Mindestens ein Scorebestandteil enthält noch einen fehlenden Wert.")
# Gewichtet die drei Wachstumsindikatoren zusammen mit insgesamt 10 %
scored$WachstumScore <- (0.10 / 3)*(scored$WachstumInternet_norm + scored$WachstumAusgaben_norm + scored$WachstumBevoelkerung_norm)
# Bildet den AppScore aus den drei strukturellen Indikatoren und dem WachstumScore
scored$AppScore <- 0.30 * scored$Bev_norm + 0.30 * scored$Internet_norm + 0.30 * scored$Ausgabe_norm + scored$WachstumScore
# 1. Sensitivitätsanalyse, wobei Internetindikator und Bevölkerung stärker gewichtet werden
scored$AppScore_weight_sens <- 0.35 * scored$Bev_norm + 0.35 * scored$Internet_norm + 0.20 * scored$Ausgabe_norm + scored$WachstumScore
# Sortiert die Länder absteigend nach dem AppScore 
scored <- scored[order(-scored$AppScore), ]
# head(scored[c("Land", "AppScore")], 50) # Zeigt die 50 höchsten Werte zur Kontrolle 
# Zuordnung der Länder laden. EU_EWR = 1 bedeutet Zugehörigkeit zum EU-/EWR-Raum, EU_EWR = 0 bedeutet außerhalb des EU-/EWR-Raums.
modifiers <- read.csv("Datensaetze/score_modifier.csv",sep = ";", check.names = FALSE)
# Sicherheitsprüfungen für Ländercodes und EU-/EWR-Zuordnung
if (anyDuplicated(modifiers$Laendercode)) stop("Die Modifier-Datei enthält doppelte Ländercodes.")
if (anyNA(modifiers$EU_EWR) || any(!modifiers$EU_EWR %in% c(0, 1))) stop("EU_EWR enthält fehlende oder ungültige Werte.")
# Ergänzt die EU-/EWR-Zuordnung länderweise zum Scoring-Datensatz
scored_ext <- merge(scored, modifiers[c("Laendercode", "EU_EWR")], by = "Laendercode", all.x = TRUE)
# Prüft, ob für alle Länder eine EU-/EWR-Zuordnung vorhanden ist
if (anyNA(scored_ext$EU_EWR)) stop("Für mindestens ein Land fehlt die EU-/EWR-Zuordnung.")
# Zunächst entspricht der FinalScore dem ursprünglichen AppScore
scored_ext$FinalScore <- scored_ext$AppScore
# Als Modellannahme wird für Länder außerhalb des EU-/EWR-Raums ein Abschlag von 15 % angewendet
scored_ext$FinalScore[scored_ext$EU_EWR == 0] <- scored_ext$FinalScore[scored_ext$EU_EWR == 0] * 0.85
# Wendet den gleichen regulatorischen Abschlag auf die 1. Sensitivitätsanalyse an
scored_ext$FinalScore_weight_sens <- scored_ext$AppScore_weight_sens
scored_ext$FinalScore_weight_sens[scored_ext$EU_EWR == 0] <- scored_ext$FinalScore_weight_sens[scored_ext$EU_EWR == 0] * 0.85
# 2. Sensitivitätsanalyse, wobei der angenommene Abschlag für Länder außerhalb des EU-/EWR-Raums von 15 % auf 5 % reduziert wird
scored_ext$FinalScore_reg_sens <- scored_ext$AppScore
scored_ext$FinalScore_reg_sens[scored_ext$EU_EWR == 0] <- scored_ext$FinalScore_reg_sens[scored_ext$EU_EWR == 0] * 0.95
# Sortiert Einträge absteigend nach dem FinalScore
scored_ext <- scored_ext[order(-scored_ext$FinalScore), ]
write.csv(scored_ext, "score_fertig.csv", row.names = FALSE)
# Erstellt Top-10, Minimum und Median für die Ergebnisanalyse
top10 <- head(scored_ext, 10)
# View(top10)
summary_view <- data.frame(Kennzahl = c("Minimum FinalScore (alle Länder)", "Median FinalScore (alle Länder)"),
			Wert = round(c(min(scored_ext$FinalScore, na.rm = TRUE),median(scored_ext$FinalScore, na.rm = TRUE)), 6))
# View(summary_view)
# Berechnet die Ergebnisse der beiden Sensitivitätsanalysen
top10_weight_sens <- head(scored_ext[order(-scored_ext$FinalScore_weight_sens),c("Land","FinalScore_weight_sens")],10)
# View(top10_weight_sens)
n_overlap_weight <- length(intersect(top10$Land, top10_weight_sens$Land)) # Anzahl der Länder, die in beiden Top-10-Listen enthalten sind
print(paste("Übereinstimmung der Top 10 bei anderer Gewichtung:", n_overlap_weight, "von 10 Ländern"))
top10_reg_sens <- head(scored_ext[order(-scored_ext$FinalScore_reg_sens),c("Land","FinalScore_reg_sens")],10)
# View(top10_reg_sens)
n_overlap_reg <- length(intersect(top10$Land, top10_reg_sens$Land)) # Anzahl der Länder, die in beiden Top-10-Listen enthalten sind
print(paste("Übereinstimmung der Top 10 bei 5 % regulatorischem Abschlag:", n_overlap_reg, "von 10 Ländern"))

### 17. Preisberechnung

# OECD-Kaufkraftparitäten laden und auf HFCE-KKP für 2021 beschränken. oecd_kkp ist bereits gefiltert, daher dient der Filter nur zur Sicherheit.
oecd_kkp <- read.csv("Datensaetze/oecd_kkp.csv")
kkp2021_hfce <- oecd_kkp[oecd_kkp$TIME_PERIOD == 2021 &oecd_kkp$TRANSACTION == "PPP_P31S14" &oecd_kkp$UNIT_MEASURE == "XDC_USD",]
# Vereinheitlicht die relevanten OECD-Spalten
kkp_df <- simplify_oecd(kkp2021_hfce, value_name = "KKP")
kkp_df <- kkp_df[c("Laendercode", "Land", "KKP")]
# Deutsche HFCE-KKP, Sicherheitsprüfung und deutscher Referenzpreis (12 €) für die Preisberechnung
kkp_de <- kkp_df$KKP[kkp_df$Laendercode == "DEU"]
if (length(kkp_de) != 1 || is.na(kkp_de)) stop("Für Deutschland wurde kein eindeutiger KKP-Referenzwert gefunden.")
price_de <- 12
# Wechselkurse 2025 für den ergänzenden Eurovergleich. oecd_exchange ist bereits gefiltert, daher dient der Filter nur zur Sicherheit.
oecd_exchange <- read.csv("Datensaetze/oecd_exchange.csv")
oecd_exchange <- oecd_exchange[oecd_exchange$MEASURE == "EXC_A" & oecd_exchange$UNIT_MEASURE == "XDC_USD",]
oecd_exchange_simple <- simplify_oecd(oecd_exchange, "Wert")
oecd_exchange_simple <- oecd_exchange_simple[oecd_exchange_simple$Jahr == 2025, c("Laendercode", "Land", "Wert")]
# Die Wechselkurse sind in Landeswährung je USD angegeben. Division durch den deutschen Wert ergibt Landeswährung je Euro.
ref_de <- oecd_exchange_simple$Wert[oecd_exchange_simple$Laendercode == "DEU"]
# Sicherheitsprüfung, ob genau ein deutscher Referenzwert vorliegt
if (length(ref_de) != 1 || is.na(ref_de)) stop("Für Deutschland wurde kein eindeutiger Wechselkurs-Referenzwert gefunden.")
oecd_exchange_simple$rel_factor <- oecd_exchange_simple$Wert / ref_de
# Verwendet die zehn Länder mit dem höchsten FinalScore
prices <- head(scored_ext[order(-scored_ext$FinalScore), ], 10)
# Ergänzt KKP und Wechselkurs, inklusive Sicherheitsabfrage
prices <- merge(prices, kkp_df[c("Laendercode", "KKP")], by = "Laendercode", all.x = TRUE)
prices <- merge(prices,  oecd_exchange_simple[c("Laendercode", "Wert", "rel_factor")], by = "Laendercode", all.x = TRUE)
if (anyNA(prices[c("KKP", "rel_factor")])) stop("Für mindestens ein Top-10-Land fehlen KKP- oder Wechselkursdaten.")
# KKP-basierter Preis in Landeswährung
prices$Preis_KKP_LW <- price_de * prices$KKP / kkp_de
# Rechnet den KKP-basierten Preis in Landeswährung mit den Wechselkursen von 2025 in einen ergänzenden Eurovergleich um
prices$Preis_KKP_EUR <- prices$Preis_KKP_LW / prices$rel_factor
# Preise auf zwei Nachkommastellen runden
prices$Preis_KKP_LW <- round(prices$Preis_KKP_LW, 2)
prices$Preis_KKP_EUR <- round(prices$Preis_KKP_EUR, 2)
# Stellt die für die Ausgabe benötigten Spalten zusammen
prices_out <- prices[c("Land", "FinalScore", "Preis_KKP_EUR", "Preis_KKP_LW")]
# Sortiert nach FinalScore und exportiert die Preisempfehlungen
prices_out <- prices_out[order(-prices_out$FinalScore), ]
write.csv(prices_out,"preisempfehlung.csv", row.names = FALSE)
