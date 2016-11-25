library(zoo)

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

# note: if there are no "Set Tempo" or "Time Signature" events,
# default the usecs_per_quarter_note to 500000 and the time signature to 4/4

# let's check the time signatures
# the last part should look like "8 1/32 notes / 24 clocks"
filter(midi, event == "Time Signature")[, c(1,2,7)]

# messy, but not sure I even need a function for this
# unless it changes throughout a piece
get_clocks_per_quarter_note <- function(t) {
  timing <- unlist(strsplit(t, ","))
  clocks <- strsplit(timing[3], " / ")[[1]][2]
  as.integer(gsub(" clocks", "", clocks))
}

# extract numerator and denominator of time signature
get_note_value <- function(t) {
  timing <- unlist(strsplit(t, ","))
  values <- strsplit(timing[1], "/")
  as.integer(values[[1]][2])
}

get_note_values_ber_bar <- function(t) {
  timing <- unlist(strsplit(t, ","))
  values <- strsplit(timing[1], "/")
  as.integer(values[[1]][1])
}

# extract just the time signature data
midi_timing <- midi %>%
  filter(event == "Time Signature") %>%
  group_by(time) %>%
  mutate(clocks_per_quarter_note = get_clocks_per_quarter_note(parameterMetaSystem),
         note_value = get_note_value(parameterMetaSystem),
         note_values_per_bar = get_note_values_ber_bar(parameterMetaSystem))

midi <- midi %>%
  left_join(midi_timing) %>%
  arrange(time)

midi$note_value <- na.locf(midi$note_value, na.rm = FALSE)
midi$note_values_per_bar <- na.locf(midi$note_values_per_bar, na.rm = FALSE)
midi$clocks_per_quarter_note <- na.locf(midi$clocks_per_quarter_note, na.rm = FALSE)

# bpm
# do I need to take into account note_value?
# see http://www.lastrayofhope.co.uk/2009/12/23/midi-delta-time-ticks-to-seconds/
get_bpm <- function(usecs_per_quarter_note, note_value = 4) {
  usecs_per_min <- 6e7
  (usecs_per_min / usecs_per_quarter_note) * (note_value / 4)
}

midi$bpm <- NA
midi[midi$event == "Set Tempo", ] <- midi %>%
  filter(event == "Set Tempo") %>%
  group_by(time) %>%
  mutate(bpm = get_bpm(as.numeric(parameterMetaSystem), note_value))

midi$bpm <- na.locf(midi$bpm, na.rm = FALSE)

# the first line of this midi file looks like this:
# 4d54 6864 0000 0006 0001 0002 0400 4d54
# the word 0400 refers to ticks per quater-note
# in this case, there are 1024 ticks per quarter-note
ticks_per_quarter_note <- 1024
ticks_per_eighth_note <- ticks_per_quarter_note / 2

midi$quater_notes_per_bar <- (midi$note_values_per_bar / midi$note_value) * 4
midi$ticks_per_bar <- midi$quater_notes_per_bar * ticks_per_quarter_note

m <- midi_notes %>%
  inner_join(midi, by = c("time", "channel")) %>%
  distinct(time, length, note, notename, velocity,
           pitch_class, bpm, ticks_per_bar)

# note: this currently only works if the time signature doesn't change
m$bar <- (m$time %/% m$ticks_per_bar) + 1
