source("~/projects/music-informatics/etc/setup.r")
midi_directory <- "~/projects/music-informatics/midi/"

# from http://imslp.org/wiki/File:PMLP335903-Scarlatti_Sonate_K.119.mid
midi_file <- "IMSLP295977-PMLP335903-Scarlatti_Sonate_K.119.mid"

midi <- readMidi(paste0(midi_directory, midi_file))
midi_notes <- getMidiNotes(midi)

# need functions for:
# determining whether a note is quarter, half, whole, etc.
# binning notes into measures
