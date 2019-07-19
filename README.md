# hadaca_bundle
A template for data challenges on CodaLab for HADACA project

To create a new challenge:

[1] COPY a challenge template

[2] EDIT the following files
- competition_head.yaml
    add title, description, dates and admin names (the leader board will be handled in the scoring.r)
- overview.Rmd (EDIT and KNIT -> will be in the bundle)
- evaluation.Rmd (EDIT and KNIT -> will be in the bundle)
- get_starting_kit.Rmd (EDIT and KNIT -> will be in the bundle)
- data.Rmd (EDIT and KNIT -> will be in the bundle)
- submission_script.Rmd (EDIT and KNIT -> will be in the bundle)
- submission_script.R (same code as in submission_script.Rmd, it will be placed in the starting kit file)
- socring_program/scoring.r
   -> be careful to update the leader board

[3] GENERATE the dataset
data.rds -> the data that you will give to participant
data_full -> the ground truth used by the scoring.R script to evaluate participant
Save them in the main directory

[4] CHOOSE your vignette (a nce picture) and save it as 'vignette.png'

[5] KNIT the starting kit and generate the CodaLab bundle zip file
- Edit the starting_kit.Rmd file and knit it

[6] Check that you have the proper file in the starting kit:
- overview.Rmd
- data.Rmd
- get_starting_kit.Rmd
- evaluation.Rmd
- submission_script.Rmd and submission_script.R



