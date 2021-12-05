# Start up and Package requirements ===========================================
start_time <- Sys.time()
pkg_names <- c(
  "brio", "cowplot", "data.table", "forcats", "ggplot2", "ggpubr", "ggsci",
  "jpeg", "lubridate", "magick", "modelr", "patchwork", "qpcR", "RColorBrewer",
  "SnowballC", "stringi", "tidytext", "tidyverse", "tm", "wordcloud",
  "wordcloud2", "xts"
)
invisible(suppressMessages(lapply(pkg_names, require, character.only = TRUE)))
# Function to get v_df data bystate ======================================

get_state_data <- function(st) {
  v_df %>%
    arrange(state, vax_lot) %>%
    filter(state == st)
}
# Function for Color Palette for the Top 50 Plot ==============================
symptom_palette <- function(n, ...) {
  hcl.colors(n, "reds", rev = TRUE, ...)
}
# Function to find a word in the v_symptoms_df ================================
find_words <- function(wanted) {
  table(grepl(wanted, v_symptoms_df$text, ignore.case = TRUE))
}
# The VAERS Data Files ========================================================
data_2020 <- "/home/hd625b/Documents/04VAERS/Data/2020VAERSDATA.csv"
data_2021 <- "/home/hd625b/Documents/04VAERS/Data/2021VAERSDATA.csv"
symptoms_2020 <- "/home/hd625b/Documents/04VAERS/Data/2020VAERSSYMPTOMS.csv"
symptoms_2021 <- "/home/hd625b/Documents/04VAERS/Data/2021VAERSSYMPTOMS.csv"
vaccine_2020 <- "/home/hd625b/Documents/04VAERS/Data/2020VAERSVAX.csv"
vaccine_2021 <- "/home/hd625b/Documents/04VAERS/Data/2021VAERSVAX.csv"

v_data_2020 <- fread(data_2020,
                     header = TRUE, sep = ",", na.strings = "NA",
                     strip.white = TRUE, blank.lines.skip = FALSE, fill = TRUE
)
v_data_2021 <- fread(data_2021,
                     header = TRUE, sep = ",", na.strings = "NA",
                     strip.white = TRUE, blank.lines.skip = FALSE, fill = TRUE
)
v_symptoms_2020 <- fread(symptoms_2020,
                         header = TRUE, sep = ",",
                         na.strings = "NA", strip.white = TRUE,
                         blank.lines.skip = FALSE, fill = TRUE
)
v_symptoms_2021 <- fread(symptoms_2021,
                         header = TRUE, sep = ",",
                         na.strings = "NA", strip.white = TRUE,
                         blank.lines.skip = FALSE, fill = TRUE
)
v_vaccination_2020 <- fread(vaccine_2020,
                            header = TRUE, sep = ",",
                            na.strings = "NA", strip.white = TRUE,
                            blank.lines.skip = FALSE, fill = TRUE
)
v_vaccination_2021 <- fread(vaccine_2021,
                            header = TRUE, sep = ",",
                            na.strings = "NA", strip.white = TRUE,
                            blank.lines.skip = FALSE, fill = TRUE
)

v_data <- rbind(v_data_2020, v_data_2021)
v_symptoms <- rbind(v_symptoms_2020, v_symptoms_2021)
v_vaccination <- rbind(v_vaccination_2020, v_vaccination_2021)

v_df <- v_data %>%
  dplyr::left_join(v_symptoms, by = "VAERS_ID") %>%
  dplyr::left_join(v_vaccination, by = "VAERS_ID")

rm(
  v_data, v_data_2020, v_data_2021,
  v_symptoms, v_symptoms_2020, v_symptoms_2021,
  v_vaccination, v_vaccination_2020, v_vaccination_2021
)

names(v_df) <- tolower(names(v_df))

# format the date fields into what R understands ==============================
v_df[["recvdate"]] <- parse_date(v_df[["recvdate"]],
                                 format = "%m/%d/%Y",
                                 na = c("", "NA"),
                                 trim_ws = TRUE
)
v_df[["datedied"]] <- parse_date(v_df[["datedied"]],
                                 format = "%m/%d/%Y",
                                 na = c("", "NA"),
                                 trim_ws = TRUE
)
v_df[["vax_date"]] <- parse_date(v_df[["vax_date"]],
                                 format = "%m/%d/%Y",
                                 na = c("", "NA"),
                                 trim_ws = TRUE
)
v_df[["onset_date"]] <- parse_date(v_df[["onset_date"]],
                                   format = "%m/%d/%Y",
                                   na = c("", "NA"),
                                   trim_ws = TRUE
)
# dplyr::filter(!(word %in% my_words)) %>%
vax_dose <- c("1", "2")
u <- "??"
v_df <- v_df %>%
  dplyr::filter(vax_type == "COVID19" & died == "Y") %>%
  dplyr::mutate(vax_dose_series = replace(vax_dose_series, vax_dose_series == "N/A", "??")) %>%
  dplyr::mutate(vax_dose_series = replace(vax_dose_series, vax_dose_series == "UNK", "??")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "1920-01-18", "2021-01-18")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "1935-07-08", "2021-07-08")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "1968-02-10", "2021-02-10")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2001-01-28", "2021-01-28")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "1932-05-13", "2021-04-13")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2000-03-01", "2021-03-01")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2000-12-17", "2021-12-17")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2008-12-03", "2021-12-03")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2012-09-01", "2021-09-01")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2000-12-27", "2021-12-27")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2013-11-13", "2021-11-13")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2014-05-16", "2021-05-16")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2018-05-22", "2021-05-22")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2019-02-05", "2021-02-05")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2019-09-30", "2021-09-30")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2019-11-01", "2021-11-01")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2020-01-02", "2021-01-02")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2019-10-31", "2020-10-31")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2020-03-03", "2021-03-03")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2020-03-18", "2021-03-18")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2020-03-20", "2021-03-20")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2020-07-30", "2021-07-30")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2020-10-30", "2021-10-30")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2020-08-12", "2020-08-01")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2020-10-31", "2021-10-31")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2021-12-03", "2020-12-03")) %>%
  dplyr::mutate(vax_date = replace(vax_date, vax_date == "2021-12-27", "2020-12-27")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2000-03-15", "2021-03-15")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "1921-01-17", "2021-01-17")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2001-01-01", "2021-01-01")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2016-06-07", "2021-06-07")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2018-03-07", "2021-03-07")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2018-05-01", "2021-05-01")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2018-07-28", "2021-07-28")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2018-09-23", "2021-09-23")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2018-12-21", "2018-12-21")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2019-01-17", "2021-01-17")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2018-12-21", "2021-12-21")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2019-02-02", "2021-02-02")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2019-02-07", "2021-02-07")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2019-10-05", "2021-10-05")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2019-11-14", "2021-11-14")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2019-12-01", "2021-12-01")) %>%
  dplyr::mutate(onset_date = replace(onset_date, onset_date == "2020-01-15", "2021-01-15")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "1921-03-14", "2021-03-14")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2001-01-01", "2021-01-01")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2001-01-08", "2021-01-08")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2001-11-14", "2021-11-14")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2010-03-14", "2021-03-14")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2014-02-14", "2021-02-14")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2014-12-16", "2021-03-16")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2018-09-23", "2021-09-23")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2018-12-21", "2021-12-21")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2018-03-07", "2021-03-07")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2018-05-30", "2021-05-30")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2018-07-28", "2021-07-28")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2019-02-07", "2021-02-07")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2019-10-09", "2021-10-09")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2019-12-01", "2021-12-01")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2016-06-07", "2021-06-07")) %>%
  dplyr::mutate(datedied = replace(datedied, datedied == "2019-02-02", "2021-02-02")) %>%
  dplyr::mutate(state = replace(state, state == "", u)) %>%
  dplyr::mutate(state = replace(state, state == " ", u)) %>%
  dplyr::mutate(vax_manu = replace(vax_manu, vax_manu == "UNKNOWN MANUFACTURER", u)) %>%
  dplyr::mutate(vax_manu = replace(vax_manu, vax_manu == "PFIZER\\BIONTECH", "PFIZER")) %>%
  dplyr::mutate_at("vax_lot", str_replace, "#", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "(0 o6)", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "\\(", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "\\)", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "Pfizer", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "pfizer", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "PFIZER", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "PFIZER ", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "Moderna/", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "Moderna ", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "Moderna", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "MOD", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "MOD/", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "MOD ", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "Lot", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "LOT: ", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "LOT ", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "Lot ", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "JSN ", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "J&J ", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "O-", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "mRNA", "") %>%
  dplyr::mutate_at("vax_lot", str_replace, "  EW0196", "EW0196") %>%
  dplyr::mutate_at("vax_lot", str_replace, " 040C21A", "040C21A") %>%
  dplyr::mutate_at("vax_lot", str_replace, " EL9269", "EL9269") %>%
  dplyr::mutate_at("vax_lot", str_replace, " ER8734", "EL8734") %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "?", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Unsure", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "unsure", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "unknown1111", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "unknown, check", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Unknown", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "unknown", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "UNSURE", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Unk", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "unk", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "unavailable pl", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "unavailable", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "NO  AVAILA", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Mid-Michigan DH", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Janssen Zolazia", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Not provided", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Not Provided to", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "not listed", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "not known", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "not documented", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Not available", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "None given from", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "UNKNOWN/NA", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "UNKNOWN", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "UK", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "NO LOT AVAILA", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "NA - UNKNOWN", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "N/A unknown", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "N/A", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "n/a", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "n/A", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "lot not listed", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Given on 8/17/2", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "Given on 7/20/2", u)) %>%
  dplyr::mutate(vax_lot = replace(vax_lot, vax_lot == "/013L20A", "013L20A")) %>%
  dplyr::mutate(vaers_id = factor(vaers_id)) %>%
  dplyr::mutate(state = factor(state)) %>%
  dplyr::mutate(sex = factor(sex)) %>%
  dplyr::mutate(died = factor(died)) %>%
  dplyr::mutate(hospital = factor(hospital)) %>%
  dplyr::mutate(hospdays = as.numeric(hospdays)) %>%
  dplyr::mutate(numdays = as.numeric(numdays)) %>%
  dplyr::mutate(VaxToOnset = as.numeric(onset_date - vax_date)) %>%
  dplyr::mutate(OnsetToDied = as.numeric(datedied - onset_date)) %>%
  dplyr::mutate(VaxToDied = as.numeric(datedied - vax_date))

v_df$vax_lot <- toupper(v_df$vax_lot)

# v_symptoms_df-Select rows that are COVID19 and Columns that contain the =====
v_symptoms_df <- v_df %>%
  dplyr::select(9, 36, 38, 40, 42, 44)
# Stack the columns of the v_df ==============================================
v_symptoms_df <- stack(v_symptoms_df)
colnames(v_symptoms_df) <- c("text", "ind")

# Clean up words =============================================================
v_symptoms_df <- v_symptoms_df %>%
  dplyr::mutate_at("text", str_replace, "passed", "death") %>%
  dplyr::mutate_at("text", str_replace, "passed ", "death") %>%
  dplyr::mutate_at("text", str_replace, " passed ", "death") %>%
  dplyr::mutate_at("text", str_replace, "expired", "death") %>%
  dplyr::mutate_at("text", str_replace, "unresponsive", "death") %>%
  dplyr::mutate_at("text", str_replace, "deceased", "death") %>%
  dplyr::mutate_at("text", str_replace, "pronounced", "death") %>%
  dplyr::mutate_at("text", str_replace, "fever", "hypoxia") %>%
  dplyr::mutate_at("text", str_replace, "chills", "hypoxia") %>%
  dplyr::mutate_at("text", str_replace, "hypoxic", "hypoxia") %>%
  dplyr::mutate_at("text", str_replace, "arrest", "cardiac") %>%
  dplyr::mutate_at("text", str_replace, "cardiacs", "cardiac") %>%
  dplyr::mutate_at("text", str_replace, "cardiaced", "cardiac") %>%
  dplyr::mutate_at("text", str_replace, "recardiaced", "cardiac")

# Tidy the data ==============================================================
my_words <- c(
  "death", "patient", "covid", "vaccine", "reported", "after", "failure",
  "received", "hospital", "acute", "blood", "unknown", "positive",
  "started", "chest", "history", "cause", "admitted", "medical",
  "pfizer", "patient's", "found", "information", "family",
  "unspecified", "report", "vaccination", "oxygen", "increased",
  "breath", "event", "hospitalization", "continued", "normal",
  "negative", "shortness", "admission", "abnormal", "symptoms",
  "decreased", "moderna", "worsening", "status", "performed",
  "developed", "called", "prior", "autopsy", "disease", "fatigue",
  "severe", "treatment", "comfort", "shock", "medications",
  "weakness", "infection", "chronic", "intubated", "significant",
  "emergency", "nausea", "events", "noted", "transferred",
  "tested", "brain", "condition", "intubation", "pressure",
  "provided", "experienced", "injury", "batch", "vomiting",
  "spontaneous", "diagnosed", "breathing", "abdominal", "discharge",
  "bilateral", "required", "receiving", "treated", "hospice",
  "hospitalized", "included", "elevated", "concomitant", "secondary",
  "remdesivir", "vaccinated", "requiring", "weeks", "weeks",
  "feeling", "female", "morning", "hours", "multiple", "facility",
  "pulse", "route", "therapy", "follow", "resident", "health",
  "seriousness", "count", "appetite", "biontech", "diarrhea",
  "stated", "consumer", "reporter", "arrival", "unable", "bnt162b2",
  "discharged", "requested", "dexamethasone", "administered",
  "consulted", "comments", "reports", "ventilation",
  "administration", "related", "injection", "bipap", "outcome",
  "lower", "initially", "doctor", "antibiotics", "illness",
  "department", "product", "arrest", "night", "mental", "tomogram",
  "additional", "computerised", "initiated", "evaluation",
  "distress", "including", "saturation", "daughter", "support",
  "sender's", "decline", "approximately", "surgery", "resuscitaion",
  "syndrome", "mechanical", "fluid", "clinical", "metabolic",
  "resuscitation", "fever", "minutes", "endotracheal", "findings",
  "diagnosis", "vaccines", "subsequently", "march", "weight",
  "arrived", "intensive", "increasing", "patients", "suspected", "nasal",
  "husband", "abdomen", "relationship", "medically", "conditions", "response",
  "measures", "resulting", "april", "single", "criteria", "stimuli", "obtained",
  "bowel", "disorder", "upper", "revealed", "adverse", "improved", "start",
  "worsened", "physical", "details", "urine", "lungs", "stable", "bedside",
  "physician", "limited", "mother", "narrative", "onset", "temporal",
  "considered", "bleeding", "function", "ventilator", "imaging", "utimately",
  "completed", "action", "concern", "level", "lactic"
)

# Break the text into individual tokens =======================================
text_df <- v_symptoms_df %>%
  unnest_tokens(word, text)
# Count words? I Don't Know! ==================================================
text_df <- text_df %>%
  count(word, sort = TRUE)
# Filter the words ===========================================================
text_df <- text_df %>%
  dplyr::filter(!(word %in% my_words)) %>%
  dplyr::filter(nchar(word) > 4)
# Remove Stop-Words ===========================================================
text_df <- text_df %>%
  anti_join(stop_words)

# Top 50 Adverse Events Resulting in Death ====================================
top_50 <- text_df %>%
  mutate(word = reorder(word, n)) %>%
  slice(1:50) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "grey30") +
  scale_fill_manual(values = symptom_palette(50)) +
  ggtitle(paste(
    "Top 50 Adverse Events", "\n",
    "Excluting Death because all of These Reported Adverse Events Resulted in Death"
  )) +
  labs(y = "Number of Times Reported", x = "Symptom") +
  theme(
    plot.title = element_text(
      color = "darkblue", size = 14,
      face = "bold.italic"
    ),
    axis.title.x = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    ),
    axis.text = element_text(face = "bold", size = 14)
  ) +
  scale_y_continuous(
    breaks = seq(0, 8000, 500),
    limits = c(0, 8000)
  ) +
  coord_flip()

# Box Plot Vax to Died by Age =================================================
get_vtd_plot <- function(start_age = 0, end_age = 0) {
  v_df %>%
    dplyr::filter(VaxToOnset >= 0 & OnsetToDied >= 0 & VaxToDied >= 0) %>%
    dplyr::filter(age_yrs >= start_age & age_yrs <= end_age) %>%
    ggplot(aes(
      x = age_yrs, y = VaxToDied, group = age_yrs,
      fill = factor(age_yrs), alpha = 0.7
    )) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0, 300)) +
    scale_y_continuous(breaks = seq(0, 300, 10)) +
    scale_x_continuous(breaks = seq(start_age, end_age, 1)) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "blue") +
    stat_summary(
      fun = mean, geom = "text", fontface = "bold", size = 5,
      vjust = 1.5, aes(label = paste(
        "Mean:", "\n",
        round(..y.., digits = 0)
      ))
    ) +
    stat_summary(fun = median, geom = "point", shape = 20, size = 5, color = "red") +
    stat_summary(
      fun = median, geom = "text", fontface = "bold", size = 5,
      vjust = 1.5, aes(label = paste(
        "Median:", "\n",
        round(..y.., digits = 0)
      ))
    ) +
    theme(legend.position = "none") +
    theme(
      panel.background = element_rect(
        fill = "azure2",
        colour = "azure2",
        size = 0.5, linetype = "solid"
      ),
      panel.grid.major = element_line(
        size = 0.5, linetype = "solid",
        colour = "white"
      ),
      panel.grid.minor = element_line(
        size = 0.25, linetype = "solid",
        colour = "white"
      )
    ) +
    scale_color_manual(values = c("darkred", "darkblue")) +
    theme(
      axis.text = element_text(face = "bold.italic"),
      plot.title = element_text(
        color = "darkblue", size = 14,
        face = "bold.italic"
      ),
      axis.title.x = element_text(
        color = "darkblue", size = 14,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "darkblue", size = 14,
        face = "bold"
      )
    )
}
# Box Plot Vax to Died by Sex and Age =========================================
get_boxplot_by_sex <- function(start_age = 60) {
  v_df %>%
    dplyr::filter(VaxToOnset >= 0 & OnsetToDied >= 0 & VaxToDied >= 0) %>%
    dplyr::filter(age_yrs == start_age & sex != "U") %>%
    dplyr::mutate(Age = factor(age_yrs)) %>%
    ggplot(aes(x = Age, y = VaxToDied, group = sex, fill = sex)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0, 300)) +
    scale_y_continuous(breaks = seq(0, 300, 10)) +
    theme(legend.position = "top") +
    theme(
      panel.background = element_rect(fill = "khaki"),
      panel.grid.major = element_line(colour = "burlywood", size = 1.5),
      panel.grid.minor = element_line(
        colour = "tomato",
        size = .25,
        linetype = "dashed"
      ),
      panel.border = element_blank(),
      axis.line.x = element_line(
        colour = "darkorange",
        size = 1.5,
        lineend = "butt"
      ),
      axis.line.y = element_line(
        colour = "darkorange",
        size = 1.5
      )
    ) +
    theme(
      axis.text = element_text(face = "bold.italic", size = 12),
      plot.title = element_text(
        color = "darkblue", size = 14,
        face = "bold.italic"
      ),
      axis.title.x = element_text(color = "darkblue", size = 14, face = "bold"),
      axis.title.y = element_text(color = "darkblue", size = 14, face = "bold")
    )
}
# plot_data data frame used for the VTO, OTD & VTD plots =====================
plot_data <- v_df %>%
  dplyr::filter(state != "??") %>%
  group_by(state) %>%
  summarize(
    VTO = round(mean(VaxToOnset, na.rm = TRUE), 0),
    OTD = round(mean(OnsetToDied, na.rm = TRUE), 0),
    VTD = round(mean(VaxToDied, na.rm = TRUE), 0)
  )
# Vax to Onset of Symptoms Bar Plot ==========================================
vto_by_state <- plot_data %>%
  dplyr::mutate(state = fct_reorder(state, VTO)) %>%
  ggplot(aes(x = state, y = VTO)) +
  geom_bar(
    stat = "identity", fill = "darkblue",
    alpha = .9, color = "black", width = 0.5
  ) +
  coord_flip() +
  ylab("Days") +
  xlab("State or Territory") +
  ggtitle("Average Number of Days From Vaccination to Onset of Symptom") +
  scale_y_continuous(
    breaks = seq(0, 120, 15),
    limits = c(0, 120)
  ) +
  theme(
    panel.background = element_rect(
      fill = "azure2",
      colour = "azure2",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  ) +
  theme(axis.text = element_text(face = "bold"))
# Onset to Death Bar Plot =====================================================
otd_by_state <- plot_data %>%
  dplyr::filter(OTD > 0) %>%
  mutate(state = fct_reorder(state, OTD)) %>%
  ggplot(aes(x = state, y = OTD)) +
  geom_bar(
    stat = "identity", fill = "darkgreen",
    alpha = .9, color = "black", width = 0.5
  ) +
  coord_flip() +
  ylab("Days") +
  xlab("State or Territory") +
  ggtitle("Average Number of Days From Symptom Onset To Death") +
  scale_y_continuous(
    breaks = seq(0, 30, 1),
    limits = c(0, 30)
  ) +
  theme(
    panel.background = element_rect(
      fill = "azure2",
      colour = "azure2",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  ) +
  theme(axis.text = element_text(face = "bold"))
# Vax to Death Bystate Bar Plot ==============================================
vtd_by_state <- plot_data %>%
  dplyr::filter(VTD > 0) %>%
  mutate(state = fct_reorder(state, VTD)) %>%
  ggplot(aes(x = state, y = VTD)) +
  geom_bar(
    stat = "identity", fill = "darkred",
    alpha = .9, color = "black", width = .5
  ) +
  coord_flip() +
  ylab("Days") +
  xlab("State or Territory") +
  ggtitle("Average Number of Days From Vaccination to Death") +
  scale_y_continuous(
    breaks = seq(0, 130, 30),
    limits = c(0, 130)
  ) +
  theme(
    panel.background = element_rect(
      fill = "azure2",
      colour = "azure2",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  ) +
  theme(axis.text = element_text(face = "bold"))

# Line Plot bystate of VTD, VTO and OTD data =================================
# img <- readJPEG("/home/hd625b/Documents/04VAERS/Covid19.jpeg")

na_to_mean <- function(x) replace(x, is.na(x), round(mean(x, na.rm = TRUE), 1))
plot_data[] <- lapply(plot_data, na_to_mean)

plt_data <- plot_data %>%
  dplyr::filter(state != "??")

line_plot_by_state <- ggplot(plt_data) +
  # annotation_raster(img, xmin = 2, xmax = 16, ymin = 130, ymax = 160) +
  geom_line(aes(x = 1:56, y = VTO, colour = "darkblue", size = 0.3)) +
  geom_point(aes(
    x = 1:56, y = VTO, colour = "blue3", size = 0.5,
    alpha = 0.8
  )) +
  geom_line(aes(x = 1:56, y = OTD, colour = "darkgreen", size = 0.3)) +
  geom_point(aes(
    x = 1:56, y = OTD, colour = "green3", size = 0.5,
    alpha = 0.8
  )) +
  geom_line(aes(x = 1:56, y = VTD, colour = "darkred", size = 0.3)) +
  geom_point(aes(
    x = 1:56, y = VTD, colour = "red3", size = 0.5,
    alpha = 0.8
  )) +
  scale_colour_identity() +
  labs(
    title = "Outlierstates - Above Average Times for these Events",
    subtitle = "Vaccine Date to Death and Vaccine Date to Onset of Symptoms",
    caption = "Figure 1"
  ) +
  theme(legend.position = "none") +
  xlab("State or US Territory") +
  ylab("Number of Days") +
  ggplot2::annotate("text",
                    x = 35, y = 157.5, label = "Average Days from Vaccination to Symptoms Onset",
                    family = "Times", fontface = "bold", colour = "darkblue",
                    size = 7, alpha = 0.8
  ) +
  ggplot2::annotate("text",
                    x = 35, y = 152.5, label = "Average Days from Symptom Onset to Death",
                    family = "Times", fontface = "bold", colour = "darkgreen",
                    size = 7, alpha = 0.8
  ) +
  ggplot2::annotate("text",
                    x = 35, y = 147.5, label = "Average Days from Vaccination to Death",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 7, alpha = 0.8
  ) +
  ggplot2::annotate("text",
                    x = 3, y = 117.5, label = "Arizona",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 12, y = 127.5, label = "Georgia",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 20, y = 137.5, label = "Kentucky",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 22, y = 92.5, label = "Mass.",
                    family = "Times", fontface = "bold", colour = "darkblue",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 25, y = 107.5, label = "Michigan",
                    family = "Times", fontface = "bold", colour = "darkblue",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 30, y = 112.5, label = "Montana",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 41, y = 87.5, label = "South Dakota",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 45, y = 132.5, label = "Tennessee",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 49, y = 132.5, label = "Texas",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 6
  ) +
  ggplot2::annotate("text",
                    x = 53, y = 92.5, label = "Wisconsin",
                    family = "Times", fontface = "bold", colour = "darkred",
                    size = 6
  ) +
  scale_x_discrete(limits = plt_data[["state"]]) +
  geom_point(aes(x = 3, y = 93), size = 40, shape = 5, color = "gold4") +
  geom_point(aes(x = 12, y = 107), size = 40, shape = 5, color = "gold4") +
  geom_point(aes(x = 20, y = 118), size = 40, shape = 5, color = "gold4") +
  geom_point(aes(x = 22, y = 75), size = 40, shape = 5, color = "gold4") +
  geom_point(aes(x = 25, y = 87), size = 40, shape = 5, color = "gold4") +
  geom_point(aes(x = 30, y = 94), size = 40, shape = 5, color = "gold4") +
  geom_point(aes(x = 46, y = 82), size = 40, shape = 5, color = "gold4") +
  geom_point(aes(x = 47.6, y = 112), size = 45, shape = 5, color = "gold4") +
  geom_point(aes(x = 53, y = 73), size = 45, shape = 5, color = "gold4") +
  scale_y_continuous(breaks = seq(0, 160, 10), limits = c(0, 160))

# Data Frame for Bar Plot By Vax Lot =========================================
bar_plots_df <- v_df %>%
  dplyr::select(state, vax_lot) %>%
  dplyr::filter(vax_lot != "??") %>%
  dplyr::group_by(vax_lot) %>%
  summarise(n = n()) %>%
  dplyr::mutate(freq = paste0(round(100 * n / sum(n), 2), "%")) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::slice(1:30)
# Deaths by VAX LOT Bar Plot  =================================================
total_deaths <- format(nrow(v_df), big.mark = ",")
vax_lot_known <- format(length(which(v_df$vax_lot != "??")), big.mark = ",")
vax_lot_unknown <- format(length(which(v_df$vax_lot == "??")), big.mark = ",")

dbvl_note <- paste("There are", total_deaths, "deaths in the VAERS system associated", "\n",
                   "with the COVID19 vaccine as of November 17, 2021.", "\n",
                   "There are", vax_lot_known, "deaths where the vaccine lot number is known.", "\n",
                   "And,", vax_lot_unknown, "deaths where the vaccine lot number is unknown.",
                   sep = " "
)
deaths_by_vax_lot <- v_df %>%
  dplyr::filter(vax_lot != "??") %>%
  dplyr::select(vax_lot) %>%
  dplyr::group_by(vax_lot) %>%
  summarise(n = n()) %>%
  dplyr::mutate(freq = paste0(round(100 * n / sum(n), 2), "%")) %>%
  dplyr::mutate(vax_lot = fct_reorder(vax_lot, n)) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::slice(1:50) %>%
  ggplot(aes(x = vax_lot, y = n)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = .9, width = .3) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 225, 25),
    limits = c(0, 225)
  ) +
  ylab("Deaths") +
  xlab("Lot Number") +
  ggtitle(paste(
    " Deaths by Vaccine Lot Number", "\n",
    "Top 50 Vaccine Lot Numbers out of ~800 Unique Vaccine Lot Numbers"
  )) +
  theme(legend.position = "none") +
  ggplot2::annotate("text",
                    x = 20, y = 175, label = dbvl_note,
                    family = "serif", fontface = "bold", colour = "darkblue",
                    size = 7
  )
# Deaths bystate Bar Plot ====================================================
deaths_by_state <- v_df %>%
  dplyr::filter(state != "??") %>%
  dplyr::select(state) %>%
  dplyr::group_by(state) %>%
  summarise(n = n()) %>%
  dplyr::mutate(freq = paste0(round(100 * n / sum(n), 2), "%")) %>%
  dplyr::mutate(state = fct_reorder(state, n)) %>%
  ggplot(aes(x = state, y = n)) +
  geom_bar(
    stat = "identity", fill = "darkred", color = "grey30",
    alpha = .8, width = .7
  ) +
  coord_flip() +
  ylab("Deaths") +
  xlab("State or Territory") +
  ggtitle(paste(prettyNum(nrow(v_df), big.mark = ","), "Deaths ")) +
  theme(legend.position = "none") +
  theme(
    axis.text = element_text(face = "bold.italic"),
    plot.title = element_text(
      color = "darkblue", size = 14,
      face = "bold.italic"
    ),
    axis.title.x = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    )
  )

# Vax to Death bystate Bar Plot ===============================================
vax_to_death_by_state <- v_df %>%
  dplyr::select(state, VaxToDied) %>%
  dplyr::filter(VaxToDied > 0) %>%
  dplyr::group_by(state) %>%
  dplyr::summarize(m = mean(VaxToDied, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(state = fct_reorder(state, m)) %>%
  ggplot(aes(x = state, y = m)) +
  geom_bar(
    stat = "identity", fill = "darkred", color = "black",
    width = 0.7, alpha = 0.8
  ) +
  coord_flip() +
  ylab("Days") +
  xlab("State or Territory") +
  ggtitle(paste(
    "Average Number of Days from Vaccination to Death", "\n",
    "Sub Title goes Here"
  )) +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = seq(0, 140, 15),
    limits = c(0, 140)
  ) +
  theme(
    panel.background = element_rect(
      fill = "azure2",
      colour = "azure2",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  ) +
  theme(
    axis.text = element_text(face = "bold.italic"),
    plot.title = element_text(
      color = "darkblue", size = 14,
      face = "bold.italic"
    ),
    axis.title.x = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    )
  )

# vax_to_died mean and error bar plot male ====================================
sem <- function(x, na.rm = TRUE) {
  out <- sd(x, na.rm = na.rm) / sqrt(length(x))
  return(out)
}
vax_to_died_male <- v_df %>%
  dplyr::select(state, age_yrs, sex, VaxToOnset,
                OnsetToDied, VaxToDied, vax_manu, vax_dose_series) %>%
  dplyr::filter(VaxToDied > 0 & state != "??" & sex != "F" & vax_manu != "??" &
                  vax_dose_series %in% vax_dose) %>%
  dplyr::mutate(state = fct_reorder(state, VaxToDied, mean, na.rm = TRUE)) %>%
  dplyr::group_by(state, vax_dose_series) %>%
  dplyr::summarise(
    m = round(mean(VaxToDied, na.rm = TRUE), 0),
    s = sem(VaxToDied, na.rm = TRUE), .groups = "keep"
  ) %>%
  ggplot(aes(x = state, y = m, group = vax_dose_series, color = vax_dose_series)) +
  geom_point(size = 1, color = "darkblue", alpha = 0.7) +
  geom_errorbar(aes(
    ymin = m - s,
    ymax = m + s
  )) +
  geom_text(aes(x = state, y = m, label = m, vjust = 0.5, hjust = 1.2)) +
  ggtitle(paste(
    " Days from Vaccination Date to Date Died", "\n",
    "Mean and Spread - Male"
  )) +
  xlab("State or Territory") +
  ylab("Days") +
  labs(fill = "Sex -") +
  scale_y_continuous(
    breaks = seq(0, 200, 5),
    limits = c(0, 200)
  ) +
  theme(
    panel.background = element_rect(
      fill = "azure2",
      colour = "azure2",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  ) +
  # scale_color_manual(values = c("darkred", "darkblue", "darkgreen")) +
  theme(
    axis.text = element_text(face = "bold.italic"),
    plot.title = element_text(
      color = "darkblue", size = 14,
      face = "bold.italic"
    ),
    axis.title.x = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    )
  )
# vax_to_died mean and error bar plot female ==================================
vax_to_died_female <- v_df %>%
  dplyr::select(state, age_yrs, sex, VaxToOnset, OnsetToDied, VaxToDied, vax_manu, vax_dose_series) %>%
  dplyr::filter(VaxToDied > 0 & state != "??" & sex != "U" & sex != "M" & vax_manu != "??") %>%
  dplyr::mutate(state = fct_reorder(state, VaxToDied, mean, na.rm = TRUE)) %>%
  dplyr::group_by(state, vax_dose_series) %>%
  dplyr::summarise(
    m = round(mean(VaxToDied, na.rm = TRUE), 0),
    s = sem(VaxToDied, na.rm = TRUE), .groups = "keep"
  ) %>%
  ggplot(aes(x = state, y = m, group = vax_dose_series, color = vax_dose_series)) +
  geom_text(aes(x = state, y = m, label = m, vjust = 0.5, hjust = 1.2)) +
  geom_point(size = 1, color = "darkred", alpha = 0.7) +
  geom_errorbar(aes(
    ymin = m - s,
    ymax = m + s
  )) +
  ggtitle(paste(
    " Days from Vaccination Date to Date Died", "\n",
    "Mean and Spread - Females"
  )) +
  xlab("State or Territory") +
  ylab("Days") +
  labs(fill = "Sex -") +
  scale_y_continuous(
    breaks = seq(0, 200, 5),
    limits = c(0, 200)
  ) +
  theme(
    panel.background = element_rect(
      fill = "azure2",
      colour = "azure2",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  ) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  theme(
    axis.text = element_text(face = "bold.italic"),
    plot.title = element_text(
      color = "darkblue", size = 14,
      face = "bold.italic"
    ),
    axis.title.x = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "darkblue", size = 14,
      face = "bold"
    )
  )
# Deaths by Mfgr bar plot  ====================================================
deaths_by_mfgr <- v_df %>%
  dplyr::select(vax_manu) %>%
  dplyr::filter(vax_manu != "??") %>%
  dplyr::group_by(vax_manu) %>%
  summarise(n = n()) %>%
  dplyr::mutate(freq = paste0(round(100 * n / sum(n), 0), "%")) %>%
  dplyr::arrange(desc(n)) %>%
  mutate(vax_manu = fct_reorder(vax_manu, n)) %>%
  ggplot(aes(x = vax_manu, y = n, alpha = 0.7)) +
  geom_bar(
    stat = "identity", fill = "darkred",
    width = .3, alpha = 0.8
  ) +
  coord_flip() +
  ylab("Deaths") +
  xlab("Vaccine Mfgr") +
  ggtitle(paste(
    prettyNum(nrow(v_df),
              big.mark = ","
    ),
    "Deaths Associated with COVID19 Vaccines", "\n",
    "There were 57 deaths where no Manufacturer was Provided."
  )) +
  theme(legend.position = "none") +
  geom_label(aes(
    label = paste(prettyNum(n, big.mark = ","), "  Deaths = ",
                  prettyNum(freq, big.mark = ","),
                  sep = ""
    ),
    hjust = 1.2, family = "serif", fontface = "bold",
    size = 12 / .pt
  )) +
  scale_y_continuous(breaks = seq(0, 8600, 200), limits = c(0, 8600)) +
  theme(
    panel.background = element_rect(
      fill = "azure2",
      colour = "azure2",
      size = 0.5, linetype = "solid"
    ),
    panel.grid.major = element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  )

# Run The Plotslots ===========================================================
pdf("~/Documents/04VAERS/Plots/LinePlotByState.pdf",
    width = 14, height = 11, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)
    # paper = "A4")
plot(line_plot_by_state)
dev.off()

# vto_by_state
# otd_by_state
# vtd_by_state
#
# xv <- c(0, 21, 31, 41, 51, 61, 71, 81, 91)
# yv <- c(20, 30, 40, 50, 60, 70, 80, 90, 106)
# for (i in seq_len(9)) {
#   print(get_plot(xv[i], yv[i]))
# }
# get_boxplot_by_sex(44)
# top_50
# deaths_by_vax_lot
# deaths_by_state
# deaths_by_mfgr
# vax_to_died_male
# vax_to_died_female
# get_vtd_vs_age_state_n_mfgr("CA")
# get_vtd_plot(61, 70)

get_vtd_vs_age_state_n_mfgr <- function(st) {
  g <- v_df %>%
    dplyr::select(
      age_yrs, VaxToDied, state, vax_manu,
      sex, vax_dose_series, hospital
    ) %>%
    dplyr::filter(VaxToDied >= 0 & age_yrs > 0 & vax_manu != "??" & sex != "U" & state == st) %>%
    ggplot(aes(age_yrs, VaxToDied)) +
    geom_point(aes(color = vax_manu, size = 1)) +
    labs(title = "Age vs Vaccination to Death") +
    geom_smooth(method = "lm", se = FALSE) +
    theme_bw() +
    ggtitle("Vaccination Date vs Age by Vaccine Manfacturer") +
    xlab("Age at Death") +
    ylab("Days") +
    scale_y_continuous(breaks = seq(0, 300, 50),
                       limits = c(0, 300)) +
    facet_grid(state ~ vax_manu) +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "khaki"),
      panel.grid.major = element_line(colour = "burlywood", size = 1.5),
      panel.grid.minor = element_line(
        colour = "tomato",
        size = 0.25,
        linetype = "dashed"
      ),
      panel.border = element_blank(),
      axis.line.x = element_line(
        colour = "darkorange",
        size = 1.5,
        lineend = "butt"
      ),
      axis.line.y = element_line(
        colour = "darkorange",
        size = 1.5
      )
    )
  return(g)
}
round(Sys.time() - start_time, 1)

# Mean of columns by State - Not really usefull yet! =========================
# my_df <- new_df[, sapply(.SD, mean, na.rm = TRUE), by = state, .SDcols = 3:6]

# ggplot(my_df, aes(x = age_yrs)) +
# geom_line(aes(y = VaxToDied, color = "red"))
# geom_line(aes(y = VaxToOnset, color = "blue" )) +
# geom_line(aes(y = OnsetToDied, color = "green"))

# round(sapply(new_df[,3:6], sd, na.rm = TRUE), 0)

# lintr::lint("/home/hd625b/Documents/04VAERS/Code/VAERS.R",
#             with_defaults(line_length_linter = line_length_linter(99)))

lm_df <- v_df %>%
  dplyr::select(3, 7, 4, 14, 47, 48, 53, 54, 55) %>%
  dplyr::filter(
    # sex != "U" &
      # state != "??" &
      VaxToOnset > 0 &
      OnsetToDied > 0 &
      VaxToDied > 0
      # vax_manu != "??"
    )
summary(lm_df)

glm_model <- glm(VaxToDied ~ vax_manu,
                   data = lm_df)
summary(glm_model)

lm_model <- lm(VaxToDied ~ vax_manu,
              data = lm_df)
summary(lm_model)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_model, las = 1)

rr.huber <- rlm(VaxToDied ~ OnsetToDied + VaxToOnset,
                data = lm_df)
summary(rr.huber)

ggplot(lm_df, aes(x = vax_manu, y = VaxToDied)) +
  geom_jitter(alpha = .1) +
  geom_boxplot(alpha = .75) +
  scale_y_sqrt()

ggplot(lm_df) +
  geom_histogram(aes(age_yrs),binwidth = 1)

ggplot(lm_df) +
  geom_histogram(aes(VaxToOnset),binwidth = 1)

ggplot(lm_df) +
  geom_histogram(aes(OnsetToDied),binwidth = 1)

ggplot(lm_df) +
  geom_histogram(aes(VaxToDied),binwidth = 1)


  v_df %>%
    dplyr::filter(died == "Y" & vax_type == "COVID19")
