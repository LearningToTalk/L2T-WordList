# Code dependencies:
# include ../Utilities/L2T-Utilities.praat
# include ../StartupForm/L2T-StartupForm.praat

# A procedure that creates a vector-like object for the names and numbers of
# columns in the Word List.
procedure wordlist_columns .experimental_task$
	call experimental_tasks
	if .experimental_task$ == experimental_tasks.nwr$
		# Define string constants for the columns of an NWR WordList.
		.trial_number = 1
		.trial_number$ = "TrialNumber"
		.trial_type = 2
		.trial_type$ = "TrialType"
		.orthography = 3
		.orthography$ = "Orthography"
		.worldbet = 4
		.worldbet$ = "WorldBet"
		.frame1 = 5
		.frame1$ = "Frame1"
		.target1 = 6
		.target1$ = "Target1"
		.target2 = 7
		.target2$ = "Target2"
		.frame2 = 8
		.frame2$ = "Frame2"
		.target_structure = 9
		.target_structure$ = "TargetStructure"
		.frequency = 10
		.frequency$ = "Frequency"
		.comparison_pair = 11
		.comparison_pair$ = "ComparisonPair"
		# Gather the string constants into a vector.
		.slot1$ = .trial_number$
		.slot2$ = .trial_type$
		.slot3$ = .orthography$
		.slot4$ = .worldbet$
		.slot5$ = .frame1$
		.slot6$ = .target1$
		.slot7$ = .target2$
		.slot8$ = .frame2$
		.slot9$ = .target_structure$
		.slot10$ = .frequency$
		.slot11$ = .comparison_pair$
		.length = 11
	elif .experimental_task$ == experimental_tasks.rwr$
		# Define string constants for the columns of an RWR WordList.
		.trial_number = 1
		.trial_number$ = "TrialNumber"
		.abbreviation = 2
		.abbreviation$ = "Abbreviation"
		.word = 3
		.word$ = "Word"
		.worldbet = 4
		.worldbet$ = "WorldBet"
		.target_c = 5
		.target_c$ = "TargetC"
		.target_v = 6
		.target_v$ = "TargetV"
		.frame = 7
		.frame$ = "Frame"
		.trial_type = 8
		.trial_type$ = "TrialType"
		.audio_prompt = 9
		.audio_prompt$ = "AudioPrompt"
		.picture_prompt = 10
		.picture_prompt$ = "PicturePrompt"
		# Gather the string constants into a vector.
		.slot1$ = .trial_number$
		.slot2$ = .abbreviation$
		.slot3$ = .word$
		.slot4$ = .worldbet$
		.slot5$ = .target_c$
		.slot6$ = .target_v$
		.slot7$ = .frame$
		.slot8$ = .trial_type$
		.slot9$ = .audio_prompt$
		.slot10$ = .picture_prompt$
		.length = 10
	elif .experimental_task$ == experimental_tasks.gfta$
		.word = 1
		.word$ = "word"
		.worldBet = 2
		.worldBet$ = "wb"
		.orthography = 3
		.orthography$ = "ortho"
		.stress	 = 4
		.stress$ = "stress"
		.targetC1 = 5
		.targetC1$ = "targetC1"
		.targetC2 = 6
		.targetC2$ = "targetC2"
		.targetC3 = 7
		.targetC3$ = "targetC3"
		.prosPos1 = 8
		.prosPos1$ = "prosPos1"
		.prosPos2 = 9
		.prosPos2$ = "prosPos2"
		.prosPos3 = 10
		.prosPos3$ = "prosPos3"
		# Gather the string constants into a vector.
		.slot1$  = .word$
		.slot2$  = .worldBet$
		.slot3$  = .orthography$
		.slot4$  = .stress$
		.slot5$  = .targetC1$
		.slot6$  = .targetC2$
		.slot7$  = .targetC3$
		.slot8$  = .prosPos1$
		.slot9$  = .prosPos2$
		.slot10$ = .prosPos3$
		.length  = 10
	endif
endproc

# A procedure that prints an error message to the Praat Info window if a 
# Word List was not successfully loaded.
procedure wordlist_error: .directory$, .participant_number$
  printline
  printline
  printline <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>>
  printline
  printline ERROR :: No WordList file was loaded
  printline
  printline Make sure the following directory exists on your computer:
  printline '.directory$'
  printline 
  printline Also, make sure that directory contains a WordList
        ... file for participant '.participant_number$'.
  printline
  printline <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>>
  printline
  printline 
endproc

procedure wordlist_for_turbulence_tagging
	# If the WordList exists as a Table in the Praat Objects list,
	# use the experimental task to determine how it should be subsetted.
	printline Extracting sibilant trials from 'wordlist.praat_obj$' ...
	if wordlist.experimental_task$ == experimental_tasks.rwr$
		select 'wordlist.praat_obj$'
		Extract rows where column (text)... 'wordlist_columns.target_c$'
			... "matches (regex)"
			... s|S
		.temp_obj1$ = selected$()
		Extract rows where column (text)... 'wordlist_columns.target_c$'
			... "is not equal to"
			... tS
		.temp_obj2$ = selected$()
		Extract rows where column (text)... 'wordlist_columns.trial_type$'
			... "is equal to"
			... Test
		.praat_obj$ = selected$()
		# Remove the original WordList Table, and the intermediary Tables.
		@remove: wordlist.praat_obj$
		@remove: .temp_obj1$
		@remove: .temp_obj2$
		# Rename the subsetted Table to the name of the original WordList.
		select '.praat_obj$'
		Rename... 'wordlist.table_obj$'
		.praat_obj$ = selected$()
		# Export a [.n_trials] variable to the [wordlist] namespace.
		wordlist.n_trials = Get number of rows
	endif
endproc

procedure wordlist_for_burst_tagging
	# If the WordList exists as a Table in the Praat Objects list,
	# use the [.experimental_task$] to determine how it should be subsetted.
	printline Extracting stop burst trials from 'wordlist.praat_obj$' ...
	if wordlist.experimental_task$ == experimental_tasks.rwr$
		select 'wordlist.praat_obj$'
		Extract rows where column (text)... 'wordlist_columns.target_c$'
			... "matches (regex)",
			... |t|k
		.temp_obj$ = selected$()
		Extract rows where column (text)... 'wordlist_columns.trial_type$'
			... "is equal to"
			... Test
		.praat_obj$ = selected$()
		# Remove the original WordList Table, and the intermediary Tables.
		@remove: wordlist.praat_obj$
		@remove: .temp_obj$
		# Rename the subsetted Table to the name of the original WordList.
		select '.praat_obj$'
		Rename... 'wordlist.table_obj$'
		.praat_obj$ = selected$()
		# Export a [.n_trials] variable to the [wordlist] namespace.
		wordlist.n_trials = Get number of rows
	endif
endproc

procedure wordlist
	# Import variables from the [session_parameters] namespace.
	.experiment_directory$ = session_parameters.experiment_directory$
	.experimental_task$ = session_parameters.experimental_task$
	.participant_number$ = session_parameters.participant_number$
	.activity$ = session_parameters.activity$

	# Call [wordlist_columns] so that this namespace is available later on.
	@wordlist_columns: .experimental_task$
	# The extension of a Word List is always .txt.
	.extension$ = ".txt"

	# Variables for the [.directory$] of the Word List and a regex-like
	# [.pattern$] for finding a Word List file in that directory.  But these can
	# only be set up if the [.experiment_directory$] is not an empty string.
	if .experiment_directory$ <> ""
		# If the [.experiment_directory$] exists, then set up the [.directory$] and
		# [.pattern$] variables...
		.directory$ = .experiment_directory$ + "/" + "WordLists"
		.pattern$   = .directory$ + "/" + 
			... .experimental_task$ + "_" +
			... .participant_number$ + "*" +
			... "WordList" + .extension$
		# Use the [.pattern$] to find a Word List file.
		@filename_from_pattern: .pattern$, "WordList"
		if filename_from_pattern.filename$ <> ""
			# Set up the path that the WordList will be [.read_from$].
			.read_from$ = .directory$ + "/" + filename_from_pattern.filename$
			# The [.write_to$] path is an empty string because modifications to the
			# WordList table should not be saved.
			.write_to$ = ""
			# Read in the Word List as a Praat Table.
			@parse_filepath: .read_from$
			printline Loading word list 'filename_from_pattern.filename$' from
				... '.directory$'
			Read Table from tab-separated file... '.read_from$'
			# Rename the Praat Table using the participant's full experimental ID.
			@participant: .read_from$, .participant_number$
			.table_obj$ = participant.id$ + "_WordList"
			Rename... '.table_obj$'
			.praat_obj$ = selected$()
			# Get the number of trials in the WordList Table.
			.n_trials = Get number of rows
			# If the user is current tagging turbulence events, the extract from
			# the WordList Table all and only those trials that contain a sibilant
			# as a target consonant.
			if .activity$ == praat_activities.tag_turbulence$ | .activity$ == praat_activities.add_place$
				@wordlist_for_turbulence_tagging
			elif .activity$ == praat_activities.tag_burst$
				@wordlist_for_burst_tagging
			endif
		elif .experimental_task$ == experimental_tasks.gfta$
			# If the task is GFTA, there is usually just one file for everyone.
			.read_from$ = session_parameters.analysis_directory$ + "/" + .experimental_task$ + "/GFTA_info.txt"
			# The [.write_to$] path is an empty string because modifications to the
			# WordList table should not be saved.
			.write_to$ = ""
			# Read in the Word List as a Praat Table.
			@parse_filepath: .read_from$
			printline Loading generic word list GFTA_info.txt from
				... '.experiment_directory$'
			Read Table from tab-separated file... '.read_from$'
			Rename... gfta_wordlist
			.praat_obj$ = selected$()
			# Get the number of trials in the WordList Table.
			.n_trials = Get number of rows
		else
			.read_from$ = ""
			.write_to$  = ""
			.praat_obj$ = ""
			# Print an error message if the [.directory$] does exist, but a Word List 
			# doesn't exist in that [.directory$].
			if .directory$ <> ""
				@wordlist_error: .directory$, .participant_number$
			endif
		endif
	else
		.read_from$ = ""
		.write_to$  = ""
		.praat_obj$ = ""
	endif
endproc