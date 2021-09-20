#####################################################
#### Example Using the MIMIC Data NOTEEVENTS.csv ####
#####################################################

# This data is available from the MIMIC project to verfified researchers after application
# Credentialed users can download the data from https://physionet.org/content/mimiciv/1.0/

# MIMIC was chosen as it is publicly available and can therefore be replicated by other researchers testing ToKSA
# The records from our institution cannot be shared digitally


# This example uses ToKSA to identify cases of gallstones from MIMIC ultrasound reports
# It excludes the indication and focuses on the findings section

# Load in packages
library(tidyverse)
library(tidytext)
library(fuzzyjoin)
library(stringdist)

# Create a directory in which to output files
dir.create("toksa_files")



#### PART 1 Split Reports ####

# Read in the data from the mimic project (requires application)
data = read_csv("mimic/1.4/NOTEEVENTS.csv")

# Limit to abdominal ultrasound scans and select only necessary columns
data = data %>% 
  filter(DESCRIPTION %in% c("US ABD LIMIT, SINGLE ORGAN",
                            "P LIVER OR GALLBLADDER US (SINGLE ORGAN) PORT")) %>% 
  select(ROW_ID, SUBJECT_ID, HADM_ID, CHARTDATE, TEXT)


# Filter out some reports not split into indications and report sections
# Ideally these should be manually updated so they can be split into indications and findings
# For the purposes of this example script they are dropped
data = data %>% 
  filter(str_detect(TEXT, "CLINICAL HISTORY:|HISTORY:|INDICATION.{0,1}:"))

# Use regex to filter out unnecessary text not related to findings at the start of each entry
# Also filter out the sentences containing the indication
# IMPORTANT: The following step will be bespoke to your data - work interactively with your data to ensure you get all of the findings sections
data = data %>% 
  mutate(FINDINGS = TEXT %>% 
           str_remove("(.|\\n)+?(?=FINAL REPORT)") %>% # Remove text about the final report section header
           str_remove("(CLINICAL HISTORY|HISTORY|INDICATION.{0,1}):(.)*\r?\n(.)*(?!\r?\n)") %>% # Remove information stored in request
           str_remove("(CLINICAL HISTORY|HISTORY|INDICATION.{0,1}):(.)*") %>% # Remove a few extra cases not covered by above
           str_replace_all("\\n (\\.|.)\\n", "\n\n") %>% # Remove lines left with just a full stop or a single character
           str_replace_all("\\n\\n( )?[a-z ]*\\n", "\n\n") %>% # Remove some of the additional lines of request left after newlines
           str_squish() %>% # This removes duplicated white space and newlines
           str_to_lower()) # This converts all to lower case to minimise variability



#### Part 2 Tokenize and Correct Words based on Spelling and Pattern Matching ####


# Create a new data frame with every single word as a row
all_findings_words = data %>% 
  unnest_tokens(word, FINDINGS) %>% 
  select(TEXT, ROW_ID, SUBJECT_ID, HADM_ID, word)


# Count them and create a term-frequency matrix
all_findings_word_count = all_findings_words %>% 
  count(word, sort = TRUE) %>% 
  filter(!is.na(word))


# Begin a basic spelling check
# Create a term of interest table and then use that to join
# IMPORTANT: The term of interest table should be updated for specific tasks
terms_of_interst_findings = tibble(term = c("gallstone", "gallstones", "stone", "calculus", "calculous", "calculi", 
                                            "sludge", "debris", "grit", 
                                            "cholecystectomy", "cholectomy", "cholecystostomy",
                                            "gallbladder", "gall bladder", "gall",
                                            "cholelithiasis", "cholecystolithiasis",
                                            "choledocholithiasis", "hepatolithiasis", "pancreatolithiasis",
                                            "mirizzi", "mirrizzi", "mirrizi",
                                            "bouveret", 
                                            "biliary", "hepatobiliary"))

# Join with the word count to see how many are detected by that
spell_check_findings = stringdist_left_join(terms_of_interst_findings,
                                            all_findings_word_count,
                                            by = c(term = "word"),
                                            max_dist = 2,
                                            method = "dl",
                                            distance_col = "edit_distance") %>% 
  mutate(char_count = str_count(term, ".")) %>% 
  group_by(word) %>% 
  filter(all(term != "gallstone") | # These filters are to ensure that double matching has not occurred
           term == "gallstone") %>% 
  filter(all(term != "calculus") |
           term == "calculus") %>% 
  filter(all(term != "cholecystectomy") |
           term == "cholecystectomy") %>% 
  filter(all(term != "gallbladder") |
           term == "gallbladder") %>% 
  filter(all(term != "mirizzi") |
           term == "mirizzi") %>% 
  filter(all(term != "mirrizzi") |
           term == "mirrizzi") %>% 
  ungroup() %>% 
  filter(!(char_count <=4 & edit_distance > 1)) %>% # If only 4 letters long then high risk of matching loads of nonsense
  filter(edit_distance != 0) %>% 
  filter(!(term == "cholecystostomy" &
             word == "cholecystectomy") &
           !(term == "cholecystectomy" &
               word == "cholecystostomy"))


# Check there are no further double matchings
spell_check_findings %>% 
  group_by(word) %>% 
  filter(n() > 1) # Should be none

# Now write out and check that matches are appropriate
# The simplest approach to marking incorrect matches is to leave a single . or x in a column titled "not_correct"
# This manual checking was conducted using Microsoft Excel before an updated CSV file was read back in
# When finished save the file as "findings_spellcheck_done.csv"
spell_check_findings %>% 
  write_csv("toksa_files/findings_spellcheck.csv")


# Aim to find any words which match a regex that aren't picked up by edit distance matching
# IMPORTANT: This regex will need updated for specific conditions
regex_matches = all_findings_word_count %>% 
  filter(str_detect(word, "gall|chole|lith|calc|debri|stone|mir(r)?iz|bouver|sludg|grit|biliary") &
           !word %in% spell_check_findings$word &
           !word %in% terms_of_interst_findings$term)

# The regex_matches object should be checked for any terms which do not appear in the spellcheck csv
# These can then be added in manually
# In the case of the MIMIC data set some replacements are apparent using the above regex:
# cholecysytitis, cholecystis, cholecysitis and cholecytitis -> cholecystitis


# Read in the approved data and sort some terms that aren't correctly spelled but used to detect typos
# In the MIMIC data there are a few misspellings of gallbladder, cholelithiasis, choledocholithiasis and biliary
# There are also several matches based on edit distance which should not be changed
# For example: "stent" should be marked as not appropriate to change to "stone"
# The final mutate() function here adds in useful regex to help with matching of specific words and terms
# Most of these replacements will not modify the data in the case of MIMIC but may be of use in other datasets
# When the function does not encounter a relevant pattern then no data is changed
spell_check_findings = read_csv("data/findings_spellcheck_done.csv") %>% 
  filter(is.na(not_correct)) %>% 
  select(-not_correct) %>% 
  mutate(word = paste0("\\b", word, "\\b") %>% 
           str_replace_all("\\.", "\\\\.") %>% 
           str_replace_all("\\$", "\\\\$") %>% 
           str_replace_all("\\^", "\\\\^") %>% 
           str_replace_all("\\<", "\\\\<") %>% 
           str_replace_all("\\>", "\\\\>") %>% 
           str_replace_all("\\[", "\\\\[") %>% 
           str_replace_all("\\]", "\\\\]") %>% 
           str_replace_all("\\{", "\\\\{") %>% 
           str_replace_all("\\}", "\\\\}") %>% 
           str_replace_all("\\(", "\\\\(") %>% 
           str_replace_all("\\)", "\\\\)") %>% 
           str_replace_all("\\?", "\\\\?") %>% 
           str_replace_all("\\*", "\\\\*") %>% 
           str_replace_all("\\+", "\\\\+") %>% 
           str_replace_all("\\|", "\\\\|"))


# Now use this corrected spelling to replace the terms in the findings_corrected variable
# This will search the FINDINGS variable and replace any matching terms with the correctly spelled word
# This step is computationally demanding and can take a few minutes
data = data %>% 
  mutate(findings_corrected = reduce2(c(spell_check_findings$word), c(spell_check_findings$term),  .init = FINDINGS,
                                      ~ str_replace(..1, ..2, ..3)))





# Matching patterns should be converted to a placeholder so that the original meaning of the sentence can still be understood
# This is particularly important if there are any incorrect substitutions
# E.g. convert all dates to [date]
# Convert all emails to [email] etc. etc.
# In the case of MIMIC most of this data sanitisation has been conducted already
# The regex here can be update to cover phone numbers, emails, dates, years, measurements, clinician ID numbers, patient ID numbers etc.
# The regex may also need updated for specific domains 
# e.g. the [0-3][0-9] and [0-1][0-9] within the date regex will need reversed if used for American dates with a MM-DD-YYYY format
data = data %>% 
  mutate(findings_corrected = findings_corrected %>% 
           str_replace_all("\\b([:alnum:]|\\.|-)+@([:alnum:]|\\.|-)+(\\.com|\\.uk|\\.net|\\.org|\\.us)\\b",
                           "[email]")%>% 
           str_replace_all("\\b[0-3][0-9](/|-|\\.)[0-1][0-9](/|-|\\.)[0-9][0-9]\\b",
                           "[date]") %>% 
           str_replace_all("\\b[0-3][0-9](/|-|\\.)[0-1][0-9](/|-|\\.)(19|20)[0-9][0-9]\\b",
                           "[date]") %>% 
           str_replace_all("\\b[1-9](/|-|\\.)[1-9](/|-|\\.)[0-9][0-9]\\b",
                           "[date]") %>% 
           str_replace_all("\\b[1-9](/|-|\\.)[1-9](/|-|\\.)(19|20)[0-9][0-9]\\b",
                           "[date]") %>% 
           str_replace_all(str_c(
             "\\b",
             paste0("19", 10:99, collapse = "|"),
             "|",
             paste0("200", 0:9, collapse = "|"),
             "|",
             paste0("201", 0:9, collapse = "|"),
             "\\b"
           ) %>% 
             str_replace_all("\\|", "\\\\b|\\\\b"),
           "[year]"
           ) %>% 
           str_replace_all("\\b[0-9][0-9](/|\\.|-)[0-9][0-9]\\b",
                           "[date]") %>% 
           str_replace_all("\\b[0-9]+(\\.[0-9]+)*()*(cm|mm)\\b",
                           "[measurement]") %>% 
           str_replace_all("\\b(07[:digit:]{9}\\b|\\b\\+44( |0)*7[:digit:]{9})\\b",
                           "[phone]"))



# Final str_squish to remove any extra whitespace added in with the substitutions
data = data %>% 
  mutate(findings_corrected = str_squish(findings_corrected))



#### Part 3 Redact Infrequent Words ####

# Create a one row per word data frame incorporating the corrections from above
all_findings_corrected_words = data %>% 
  unnest_tokens(word, findings_corrected) %>% 
  select(TEXT, ROW_ID, SUBJECT_ID, HADM_ID, word)


# Count them again in a new term-frequency matrix
all_findings_corrected_word_count = all_findings_corrected_words %>% 
  count(word, sort = TRUE) %>% 
  filter(!is.na(word))

# Now filter out low frequency terms to change to [redacted]
# Also update the words with characters that have special meaning in regex to assist with regex matching
findings_low_freq = all_findings_corrected_word_count %>% 
  filter(n <= 5) %>% 
  select(word) %>% 
  mutate(word = paste0("\\b", word, "\\b") %>% 
           str_replace_all("\\.", "\\\\.") %>% 
           str_replace_all("\\$", "\\\\$") %>% 
           str_replace_all("\\^", "\\\\^") %>% 
           str_replace_all("\\<", "\\\\<") %>% 
           str_replace_all("\\>", "\\\\>") %>% 
           str_replace_all("\\[", "\\\\[") %>% 
           str_replace_all("\\]", "\\\\]") %>% 
           str_replace_all("\\{", "\\\\{") %>% 
           str_replace_all("\\}", "\\\\}") %>% 
           str_replace_all("\\(", "\\\\(") %>% 
           str_replace_all("\\)", "\\\\)") %>% 
           str_replace_all("\\?", "\\\\?") %>% 
           str_replace_all("\\*", "\\\\*") %>% 
           str_replace_all("\\+", "\\\\+") %>% 
           str_replace_all("\\|", "\\\\|")
  )


# Overwrite everything with 5 or fewer occurrences as mostly will not be relevant
# For large datasets this process is computationally demanding so should be looped through
# To assist with this groupings based on 500 scans per group can be made
findings_low_freq = findings_low_freq %>% 
  mutate(n = row_number(),
         group = n %/% 500) %>% 
  mutate(group = group+1) %>% 
  select(-n)

# Get max number of groups to identify final loop index
max_findings_group = max(findings_low_freq$group)

# Get original vector as this will be updated
findings_vector = data$findings_corrected


# Update with each batch of the regex strings
# This step can take several minutes for even small datasets
for(i in 1:max_findings_group){
  regex_string = findings_low_freq %>% 
    filter(group == i) %>% 
    pull(word) %>% 
    paste0(collapse = "|")
  
  findings_vector = gsub(pattern = regex_string,
                         replacement = "[redacted]",
                         x = findings_vector,
                         perl = TRUE)
  
}

# Then overwrite the original data with infrequent words redacted (each overwritten with [redacted])
data = data %>% 
  mutate(findings_corrected = findings_vector)



#### Part 4 Split into Full sentence N-grams ####

# Splitting into sentences require a regex designed to find sentence boundaries
# Whilst a simple full stop match will identify most there are cases when this is inappropriate e.g. 2.2 or Dr. Smith etc.
# IMPORTANT: the regex for splitting sentences can be updated to match specific phrases that appear with a full stop in a given data set
# For example in the UK GMC no. xxxxxxx may be encountered (GMC number is a clinician registration number)
titles =  c("mr", "dr", "mrs", "ms", "prof", "gmc no")
regex = paste0("(?<!\\b(", paste(titles, collapse = "|"), "))\\. |(?<=[a-z]\\?) ")


# Now update the data to tokenize based on sentence
findings_data = data %>% 
  select(ROW_ID, SUBJECT_ID, HADM_ID, CHARTDATE, TEXT, FINDINGS, findings_corrected) %>% 
  unnest_tokens(output = sentence,
                input = findings_corrected,
                format = "text",
                to_lower = TRUE,
                drop = FALSE,
                collapse = NULL,
                token = "regex",
                pattern = regex
  )

# Create a count of sentences in a term-frequency matrix with most frequent sorted to top
findings_sentence_count = findings_data %>% 
  count(sentence) %>% 
  filter(!is.na(sentence)) %>% 
  arrange(desc(n))

# Tag the sentences with likely relevant findings
# IMPORTANT: This regex will need updated for specific classification tasks
# The regex should include names for the pathology, synonyms for the pathology, names for structures in which the pathology occurs
# and in some cases it should include other radiological terms that might be of note (e.g. opacification, dependent shadowing etc. etc.)
# When a pathology elsewhere is similar named an exclusion regex may be of benefit to prioritise reports (e.g. renal calculus and gallbladder calculus)
relevant_findings_regex = "stone|calcul.*|sludge|debris|grit|chole.*lith|cholecyst.*|gall.*|bile|duct|bili(a|o).*|mir(r)?iz(z)?i|bouv(e)?ret|lithiasis"
exclusions_regex = "(stone|calcul.*|lith.*).*(kidney|renal|ureter.*|prostat)|(kidney|renal|ureter.*|prostat).*(stone|calcul.*|lith.*)"

# Tag the data with these indications
findings_sentence_count = findings_sentence_count %>% 
  mutate(gallstone_terms = str_detect(sentence, relevant_findings_regex),
         kidney_terms = str_detect(sentence, exclusions_regex))



# Write out the most common sentences, 1 file for those with relevant terms and 1 without
findings_sentence_count %>% 
  filter(gallstone_terms & !kidney_terms) %>% 
  slice(1:1000) %>% 
  write_csv("toksa_files/findings_gallstone_terms_ready_to_annotate.csv")

# Write out the most common sentences, 1 file for those with relevant terms and 1 without
findings_sentence_count %>% 
  filter(!gallstone_terms) %>% 
  slice(1:1000) %>% 
  write_csv("toksa_files/findings_no_gallstone_terms_ready_to_annotate.csv")



#### Part 5 Annotate the findings ####
# This was conducted in a separate excel file
# Annotations for gallstone status were given 4 different values
# 1 represented definite gallstones
# 0 represented definitely no gallstones (statement sufficient to cover entire scan)
# 0.1 represented no gallstones in specific setting (statement not sufficient to fully exclude gallstones)
# 9 represented unknown (both complete lack of mention of biliary structures and statements stating unable to determine gallstone presence)

# An example sentence for classification 0 would be: "No biliary pathology demonstrated on scan" or "No gallstones identified on todays scan"
# An example sentence for classification 0.1 would be: "No gallstones seen in the gallbladder"
# The 0.1 example may be followed by a second sentence documenting stones in elsewhere in the biliary tree
# as such it is not sufficient to fully rule out gallstones


# Read in the files after they have been annotated
# Best practice is that the gallstones_ground_truth column has been agreed upon by two raters independently
# Disagreements can be resolved by a third rater
toksa_findings_data_part_1 = read_csv("toksa_files/findings_gallstone_terms_annotated.csv")
toksa_findings_data_part_2 = read_csv("toksa_files/findings_no_gallstone_terms_annotated.csv")


# Create combined gold standard for indication, gs status and cholecystectomy (sentences only here)
findings_gallstones_toksa_ground_truth = bind_rows(
  toksa_findings_data_part_1 %>% 
    select(sentence, gallstones_ground_truth),
  toksa_findings_data_part_2 %>% 
    select(sentence, gallstones_ground_truth)
)




# Check number of reports actually covered by the ToKSA ground truth
data %>% 
  data %>% 
  filter(!is.na(findings_corrected) &
           findings_corrected != "Nil") # In the case of empty reports it is a good idea to avoid counting them as they will inflate performance



#### Part 6 Aggregate the findings ####

# First check to make sure there are no conflicting definite yes versus definite no
# These should be checked to ensure that the ToKSA assumptions about what constitutes a definite case (or negative) are correct
# In some cases there may be genuinely conflicting statements within the same report
# With conflicting statements a decision must be made about what takes priority
conflicts = findings_data %>% 
  left_join(findings_sentence_count) %>%  # Join with the sentence count
  left_join(findings_gallstones_toksa_ground_truth) %>% # Join with ToKSA-derived ground truth
  group_by(ROW_ID) %>% # Group by ROW_ID again
  summarise(conflicting_annotation = ifelse(
    any(gallstones_ground_truth == 1) & any(gallstones_ground_truth == 0),
    "Conflict",
    NA 
  )) %>% 
  ungroup() %>% 
  filter(!is.na(conflicting_annotation)) # This data frame should be inspected and specific reports hard coded

# Aggregate the ToKSA-derived ground truth values
findings_gallstones_toksa_gold_standard = findings_data %>% 
  left_join(findings_sentence_count) %>%  # Join with the sentence count
  left_join(findings_gallstones_toksa_ground_truth) %>% # Join with ToKSA-derived ground truth
  group_by(ROW_ID) %>% # Group by ROW ID such that all sentences for each scan are considered in a group
  mutate(gallstone_sentences_missed = ifelse(
    any(is.na(gallstones_ground_truth) & # Check to see if all of the tagged sentences containing a term of interest have been tagged
          gallstone_terms == TRUE),
    "Yes-missed", "No-covered"
  )) %>% 
  ungroup() %>%
  filter(!is.na(gallstones_ground_truth)) %>% # Drop sentences in which there are no ground truths generated
  group_by(ROW_ID) %>% # Group by ROW_ID again
  summarise(gallstone_ground_truth_aggregated = case_when(
    any(gallstones_ground_truth == 1) ~ "Gallstones", # Assign any cases in which gallstones are seen
    any(gallstones_ground_truth %in% c(0, 0.1) &
          gallstone_sentences_missed == "No-covered") ~ "No Gallstones", # If all sentences with a term of interest are negative then mark negative
    any(gs_gold_standard == 0) ~ "No Gallstones", # If any sentences have a definite negation then mark as negative
    any(gs_gold_standard %in% c(9)) & 
      all(!gs_gold_standard %in% c(0, 0.1, 1)) &
      all(gallstone_sentences_missed == "No-covered") ~ "Unknown", # If all relevant sentences are covered and are marked as Unknown then mark as unknown
    TRUE ~ NA_character_ # Anything else leave out as cannot be certain
  )) %>% 
  ungroup()



#### Part 7 Validate results ####

# This final step requires validation of the ToKSA output against a ground truth derived from the annotation of full reports