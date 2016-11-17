source("~/projects/music-informatics/etc/setup.r")
midi_directory <- "~/projects/music-informatics/midi/"

# Domenico Scarlatti's Keyboard Sonata in D major, K.119
# from http://imslp.org/wiki/File:PMLP335903-Scarlatti_Sonate_K.119.mid
midi_file <- "IMSLP295977-PMLP335903-Scarlatti_Sonate_K.119.mid"

midi <- readMidi(paste0(midi_directory, midi_file))
midi_notes <- getMidiNotes(midi)

#--------------------------------------------------------------------
# pitch exploration

# highest pitch
midi_notes[which.max(midi_notes$note), ]

# lowest pitch
midi_notes[which.min(midi_notes$note), ]

# table of pitch frequencies
(pitch_freq <- table(midi_notes$notename))

# table of pitch proportions
(pitch_prop <- round(prop.table(table(midi_notes$notename)), 4))

# most common pitch
pitch_freq[which.max(pitch_freq)]
pitch_prop[which.max(pitch_prop)]

# least common pitch (of those played)
pitch_freq_played <- pitch_freq[pitch_freq > 0]
pitch_prop_played <- pitch_prop[pitch_prop > 0]
pitch_freq_played[which.min(pitch_freq_played)]
pitch_prop_played[which.min(pitch_prop_played)]

# tables of pitch class frequencies and proportions
midi_notes$pitch_class <- gsub(",|'", "", toupper(midi_notes$notename))
(pitch_class_freq <- table(midi_notes$pitch_class))
(pitch_class_prop <- round(prop.table(table(midi_notes$pitch_class)), 4))


#--------------------------------------------------------------------
# tempo exploration

get_bpm <- function(usecs_per_quarter_note) {
  usecs_per_min <- 6e7
  usecs_per_min / usecs_per_quarter_note
}

midi$bpm <- NA
midi[midi$event == "Set Tempo", ] <- midi %>%
  filter(event == "Set Tempo") %>%
  mutate(bpm = get_bpm(as.numeric(parameterMetaSystem)))

# let's check the time signatures
# the last part should look like "8 1/32 notes / 24 clocks"
filter(midi, event == "Time Signature")[, c(1,2,7)]

# extract just the time signature data
midi_timing <- midi %>%
  filter(event == "Time Signature") %>%
  mutate(timing = strsplit(parameterMetaSystem, ","))

# messy, but not sure I even need a function for this
# unless it changes throughout a piece
clocks_per_quarter_note <- lapply(midi_timing$timing, function(t) {
  clocks <- strsplit(t[[3]], " / ")[[1]][2]
  n_clocks <- as.integer(gsub(" clocks", "", clocks))
  data.frame(event = "Time Signature",
             clocks_per_quarter_note = n_clocks,
             stringsAsFactors = FALSE)
}) %>% do.call("rbind", .)

# extract numerator and denominator of time signature
time_signatures <- lapply(midi_timing$timing, function(t) {
  values <- strsplit(t[[1]], "/")
  note_value <- as.integer(values[[1]][2])
  note_values_per_bar <- as.integer(values[[1]][1])
  data.frame(event = "Time Signature",
             note_value,
             note_values_per_bar,
             stringsAsFactors = FALSE)
}) %>% do.call("rbind", .)

midi <- midi %>%
  left_join(clocks_per_quarter_note, by = "event") %>%
  left_join(time_signatures, by = "event")

# need functions for:
# determining whether a note is quarter, half, whole, etc.
# binning notes into measures
