source("~/projects/music-informatics/etc/setup.r")
midi_directory <- "~/projects/music-informatics/midi/"

# Domenico Scarlatti's Keyboard Sonata in D major, K.119
# from http://imslp.org/wiki/File:PMLP335903-Scarlatti_Sonate_K.119.mid
midi_file <- "IMSLP295977-PMLP335903-Scarlatti_Sonate_K.119.mid"

midi <- readMidi(paste0(midi_directory, midi_file))
midi_notes <- getMidiNotes(midi)

#--------------------------------------------------------------------
# exploration

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

# need functions for:
# determining whether a note is quarter, half, whole, etc.
# binning notes into measures
