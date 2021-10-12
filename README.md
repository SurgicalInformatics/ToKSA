# ToKSA



## Overview

This GitHub repo includes a tutorial and R script covering Tokenized Key Sentence Annotation (ToKSA). This repo is based on the paper: ToKSA - Tokenized Key Sentence Annotation - a Novel Method for Rapid Approximation of Ground Truth for Natural Language Processing. 


## ToKSA Methods

The script `toksa.R` is designed to conduct the basic aspects of the ToKSA methodology. The script has been adapted to work with the MIMIC dataset which can be requested by researchers (NHS Lothian electronic health record data cannot be made publicly available and as such a script based on this could not be as easily investigated by other researchers). MIMIC is available to credentialled researchers from https://physionet.org/content/mimiciv/1.0/.

### Packages

The script relies on the R packages `tidyverse`, `tidytext`, `fuzzyjoin` and `stringdist`. The versions used for these packages during development were `1.3.0`, `0.2.5`, `0.1.6` and `0.9.6`. The R version used was `3.6.3`. If using other versions, the ToKSA script is likely to function normally but may occasionally require small edits if functions change between package updates.


### Steps

The following steps form the ToKSA pipeline:

1. *Prepare and split the data*. Using regular expressions to identify and extract the "findings" section of the abdominal ultrasounds within the MIMIC dataset. This can be adapted to handle other datasets or to extract the "indications" section.
1. *Tokenize the words and correct important spelling mistakes*. A disease-specific (or phenotype-specific) glossary is created to cover words related to the research question. In the example script words relate to gallstone pathology terms and hepatobiliary structures typically affected by gallstones. This glossary of terms is then combined with relevant words using a fuzzy join based on edit distance. Each of the identified words is then checked to identify genuine spelling mistakes. For example the words "gllstones" and "galstones" are both an edit distance of 1 from "gallstones" and should be updated. "done" is an edit distance of 2 from "stone" but should not be updated. Common but irrelevant patterns such as patient ID numbers, phone numbers or emails are overwritten.
1. *Infrequent words are redacted*. Words which occur very infrequently across the cohort are unlikely to help with classification and can be redacted. These are overwritten with the term "[redacted]" so that potentially informative redactions can be identified during manual annotation.
1. *Reports are split into full sentence N-grams*. Each report is split into full sentences. The sentences are then grouped based on frequency across the entire corpus of reports. The top 1,000 sentences containing one of the terms from the term-specific glossary are identified and stored in a `.csv` file. The top 1,000 sentences that do not contain any of the terms from the glossary are also extracted and stored in a separate `.csv` file.
1. *Reports are annotated*. The two `.csv` files are annotated based on the classification task. In the example script the task is to annotate based on gallstone status. The `.csv` file in which sentences did not contain a term from the term-specific glossary is inspected primarily to ensure that no common terms have been missed.
1. *Labels generated from annotating sentences are aggregated*. Using domain-specific knowledge the labels from the sentences are aggregated to the full reports. Care is needed to ensure that sentences which only rule out the disease / phenotype in one area do not create an overall negative label if another sentence documents the disease. For example "Gallstones are not seen in the bile duct." may be seen in the same report as "Gallstones seen within thin-walled gallbladder." In this case the positive result takes precedence over the negative if the desired classification is a simple "Yes"/"No" for gallstone status.
1. *Results are validated*. Results generated from ToKSA should be validated (typically by analysing interobserver agreement) against a small subset of data from which the full reports have been annotated.
