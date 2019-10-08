Assignment 1, Language development in Autism Spectrum Disorder (ASD) - Brushing up your code skills
===================================================================================================

In this first part of the assignment we will brush up your programming
skills, and make you familiar with the data sets you will be analysing
for the next parts of the assignment.

In this warm-up assignment you will: 1) Create a Github (or gitlab)
account, link it to your RStudio, and create a new repository/project 2)
Use small nifty lines of code to transform several data sets into just
one. The final data set will contain only the variables that are needed
for the analysis in the next parts of the assignment 3) Warm up your
tidyverse skills (especially the sub-packages stringr and dplyr), which
you will find handy for later assignments.

N.B: Usually you'll also have to doc/pdf with a text. Not for Assignment
1.

Learning objectives:
--------------------

-   Become comfortable with tidyverse (and R in general)
-   Test out the git integration with RStudio
-   Build expertise in data wrangling (which will be used in future
    assignments)

0. First an introduction on the data
------------------------------------

Language development in Autism Spectrum Disorder (ASD)
======================================================

Reference to the study: <https://www.ncbi.nlm.nih.gov/pubmed/30396129>

Background: Autism Spectrum Disorder (ASD) is often related to language
impairment, and language impairment strongly affects the patients
ability to function socially (maintaining a social network, thriving at
work, etc.). It is therefore crucial to understand how language
abilities develop in children with ASD, and which factors affect them
(to figure out e.g. how a child will develop in the future and whether
there is a need for language therapy). However, language impairment is
always quantified by relying on the parent, teacher or clinician
subjective judgment of the child, and measured very sparcely (e.g. at 3
years of age and again at 6).

In this study we videotaped circa 30 kids with ASD and circa 30
comparison kids (matched by linguistic performance at visit 1) for ca.
30 minutes of naturalistic interactions with a parent. We repeated the
data collection 6 times per kid, with 4 months between each visit. We
transcribed the data and counted: i) the amount of words that each kid
uses in each video. Same for the parent. ii) the amount of unique words
that each kid uses in each video. Same for the parent. iii) the amount
of morphemes per utterance (Mean Length of Utterance) displayed by each
child in each video. Same for the parent.

Different researchers involved in the project provide you with different
datasets: 1) demographic and clinical data about the children (recorded
by a clinical psychologist) 2) length of utterance data (calculated by a
linguist) 3) amount of unique and total words used (calculated by a
fumbling jack-of-all-trade, let's call him RF)

Your job in this assignment is to double check the data and make sure
that it is ready for the analysis proper (Assignment 2), in which we
will try to understand how the children's language develops as they grow
as a function of cognitive and social factors and which are the "cues"
suggesting a likely future language impairment.

1. Let's get started on GitHub
------------------------------

In the assignments you will be asked to upload your code on Github and
the GitHub repositories will be part of the portfolio, therefore all
students must make an account and link it to their RStudio (you'll thank
us later for this!).

Follow the link to one of the tutorials indicated in the syllabus: \*
Recommended: <https://happygitwithr.com/> \* Alternative (if the
previous doesn't work):
<https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN>
\* Alternative (if the previous doesn't work):
<https://docs.google.com/document/d/1WvApy4ayQcZaLRpD6bvAqhWncUaPmmRimT016-PrLBk/mobilebasic>

N.B. Create a GitHub repository for the Assignment 1 and link it to a
project on your RStudio.

2. Now let's take dirty dirty data sets and make them into a tidy one
---------------------------------------------------------------------

If you're not in a project in Rstudio, make sure to set your working
directory here. If you created an RStudio project, then your working
directory (the directory with your data and code for these assignments)
is the project directory.

    pacman::p_load(tidyverse,janitor)

Load the three data sets, after downloading them from dropbox and saving
them in your working directory: \* Demographic data for the
participants:
<https://www.dropbox.com/s/w15pou9wstgc8fe/demo_train.csv?dl=0> \*
Length of utterance data:
<https://www.dropbox.com/s/usyauqm37a76of6/LU_train.csv?dl=0> \* Word
data: <https://www.dropbox.com/s/8ng1civpl2aux58/token_train.csv?dl=0>

    # Demographic data
    demo_data <- read.csv("demo_train.csv")

    # length of utterance data
    lu_data <- read.csv("LU_train.csv")

    # Word data
    word_data <- read.csv("token_train.csv")

Explore the 3 datasets (e.g. visualize them, summarize them, etc.). You
will see that the data is messy, since the psychologist collected the
demographic data, the linguist analyzed the length of utterance in May
2014 and the fumbling jack-of-all-trades analyzed the words several
months later. In particular: - the same variables might have different
names (e.g. participant and visit identifiers) - the same variables
might report the values in different ways (e.g. participant and visit
IDs) Welcome to real world of messy data :-)

Before being able to combine the data sets we need to make sure the
relevant variables have the same names and the same kind of values.

So:

2a. Identify which variable names do not match (that is are spelled
differently) and find a way to transform variable names. Pay particular
attention to the variables indicating participant and visit.

Tip: look through the chapter on data transformation in R for data
science (<http://r4ds.had.co.nz>). Alternatively you can look into the
package dplyr (part of tidyverse), or google "how to rename variables in
R". Or check the janitor R package. There are always multiple ways of
solving any problem and no absolute best method.

    # Overview of colum names in the dataframe
    names(demo_data)

    ##  [1] "Child.ID"                   "Visit"                     
    ##  [3] "Ethnicity"                  "Diagnosis"                 
    ##  [5] "ASD_check"                  "ASD2"                      
    ##  [7] "Gender"                     "Birthdate"                 
    ##  [9] "Age"                        "Total..Understands...Says."
    ## [11] "Total..Understands."        "Total.of.Both"             
    ## [13] "Age2"                       "ADOS"                      
    ## [15] "CARS"                       "CDI1"                      
    ## [17] "VinelandStandardScore"      "VinelandReceptive"         
    ## [19] "VinelandExpressive"         "VinelandWritten"           
    ## [21] "DailyLivingSkills"          "Socialization"             
    ## [23] "MotorSkills"                "MullenRaw"                 
    ## [25] "MullenTScore"               "MullenAge"                 
    ## [27] "FineMotorRaw"               "FineMotorTScore"           
    ## [29] "FIneMotorAge"               "ReceptiveLanguageRaw"      
    ## [31] "ReceptiveLanguageTScore"    "ReceptiveLanguageAge"      
    ## [33] "ExpressiveLangRaw"          "ExpressiveLangTScore"      
    ## [35] "ExpressiveLangAge"          "EarlyLearningComposite"

    names(lu_data)

    ##  [1] "SUBJ"      "VISIT"     "MOT_MLU"   "MOT_LUstd" "MOT_LU_q1"
    ##  [6] "MOT_LU_q2" "MOT_LU_q3" "CHI_MLU"   "CHI_LUstd" "CHI_LU_q1"
    ## [11] "CHI_LU_q2" "CHI_LU_q3"

    names(word_data)

    ## [1] "SUBJ"         "VISIT"        "types_MOT"    "types_CHI"   
    ## [5] "types_shared" "tokens_MOT"   "tokens_CHI"   "X"

    # Visit and subject names are the columns that do not fit


    summary(demo_data)

    ##     Child.ID       Visit                  Ethnicity   Diagnosis
    ##  A.D.   :  6   Min.   :1.000   White           :320   A:176    
    ##  A.H.   :  6   1st Qu.:2.000   African American: 12   B:196    
    ##  A.R.   :  6   Median :3.000   White/Latino    : 12            
    ##  A.S.   :  6   Mean   :3.438   Asian           :  6            
    ##  A.Z.   :  6   3rd Qu.:5.000   Lebanese        :  6            
    ##  Adam   :  6   Max.   :6.000   White/Asian     :  6            
    ##  (Other):336                   (Other)         : 10            
    ##    ASD_check           ASD2            Gender         Birthdate  
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :1.000   25/03/04: 12  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1.000   6/2/03  : 12  
    ##  Median :0.0000   Median :0.0000   Median :1.000   20/04/07:  8  
    ##  Mean   :0.4731   Mean   :0.7378   Mean   :1.167   1/3/10  :  6  
    ##  3rd Qu.:1.0000   3rd Qu.:2.0000   3rd Qu.:1.000   10/12/04:  6  
    ##  Max.   :1.0000   Max.   :2.0000   Max.   :2.000   10/2/04 :  6  
    ##                   NA's   :2                        (Other) :322  
    ##       Age        Total..Understands...Says. Total..Understands.
    ##  Min.   :18.07   Min.   :  0.00             Min.   :  0.0      
    ##  1st Qu.:28.61   1st Qu.: 15.00             1st Qu.: 52.0      
    ##  Median :36.10   Median : 50.00             Median :104.0      
    ##  Mean   :36.57   Mean   : 99.85             Mean   :116.9      
    ##  3rd Qu.:42.99   3rd Qu.:152.00             3rd Qu.:174.0      
    ##  Max.   :62.40   Max.   :345.00             Max.   :281.0      
    ##  NA's   :10      NA's   :307                NA's   :307        
    ##  Total.of.Both        Age2            ADOS             CARS      
    ##  Min.   :  0.0   Min.   :17.77   Min.   : 0.000   Min.   :15.00  
    ##  1st Qu.: 56.0   1st Qu.:28.62   1st Qu.: 0.000   1st Qu.:15.00  
    ##  Median : 98.5   Median :36.10   Median : 5.000   Median :19.00  
    ##  Mean   :183.9   Mean   :36.46   Mean   : 7.184   Mean   :25.42  
    ##  3rd Qu.:287.0   3rd Qu.:42.78   3rd Qu.:14.000   3rd Qu.:35.00  
    ##  Max.   :679.0   Max.   :62.40   Max.   :25.000   Max.   :52.00  
    ##  NA's   :22      NA's   :8       NA's   :247      NA's   :247    
    ##       CDI1       VinelandStandardScore VinelandReceptive
    ##  Min.   :  1.0   Min.   :  8.00        Min.   : 0.0     
    ##  1st Qu.:155.0   1st Qu.: 83.00        1st Qu.:22.0     
    ##  Median :218.0   Median :100.00        Median :26.0     
    ##  Mean   :213.7   Mean   : 92.33        Mean   :25.2     
    ##  3rd Qu.:296.0   3rd Qu.:107.00        3rd Qu.:30.0     
    ##  Max.   :390.0   Max.   :150.00        Max.   :67.0     
    ##  NA's   :307     NA's   :3             NA's   :15       
    ##  VinelandExpressive VinelandWritten DailyLivingSkills Socialization   
    ##  Min.   :  5.00     Min.   : 0.00   Min.   : 25.00    Min.   : 38.00  
    ##  1st Qu.: 31.00     1st Qu.: 3.00   1st Qu.: 77.00    1st Qu.: 75.00  
    ##  Median : 57.00     Median : 6.00   Median : 96.00    Median : 94.00  
    ##  Mean   : 53.84     Mean   :10.74   Mean   : 90.93    Mean   : 90.22  
    ##  3rd Qu.: 76.00     3rd Qu.:10.00   3rd Qu.:105.00    3rd Qu.:103.00  
    ##  Max.   :104.00     Max.   :98.00   Max.   :132.00    Max.   :125.00  
    ##  NA's   :18         NA's   :140     NA's   :4         NA's   :3       
    ##   MotorSkills       MullenRaw      MullenTScore     MullenAge    
    ##  Min.   : 17.00   Min.   :13.00   Min.   :20.00   Min.   :10.00  
    ##  1st Qu.: 85.00   1st Qu.:27.00   1st Qu.:35.00   1st Qu.:25.00  
    ##  Median : 96.00   Median :33.00   Median :55.50   Median :33.00  
    ##  Mean   : 92.69   Mean   :34.53   Mean   :50.63   Mean   :36.47  
    ##  3rd Qu.:105.00   3rd Qu.:43.00   3rd Qu.:65.00   3rd Qu.:48.00  
    ##  Max.   :124.00   Max.   :50.00   Max.   :80.00   Max.   :69.00  
    ##  NA's   :3        NA's   :190     NA's   :190     NA's   :190    
    ##   FineMotorRaw  FineMotorTScore  FIneMotorAge   ReceptiveLanguageRaw
    ##  Min.   :16.0   Min.   :20.00   Min.   :14.00   Min.   : 2.0        
    ##  1st Qu.:22.0   1st Qu.:23.00   1st Qu.:21.00   1st Qu.:21.0        
    ##  Median :27.0   Median :44.00   Median :26.00   Median :27.0        
    ##  Mean   :28.9   Mean   :42.17   Mean   :30.45   Mean   :28.8        
    ##  3rd Qu.:35.0   3rd Qu.:52.00   3rd Qu.:38.00   3rd Qu.:37.0        
    ##  Max.   :49.0   Max.   :80.00   Max.   :68.00   Max.   :48.0        
    ##  NA's   :249    NA's   :249     NA's   :249     NA's   :249         
    ##  ReceptiveLanguageTScore ReceptiveLanguageAge ExpressiveLangRaw
    ##  Min.   :20.00           Min.   : 1.00        Min.   : 8.00    
    ##  1st Qu.:23.00           1st Qu.:20.00        1st Qu.:16.00    
    ##  Median :51.00           Median :28.00        Median :22.00    
    ##  Mean   :46.83           Mean   :32.67        Mean   :26.03    
    ##  3rd Qu.:63.00           3rd Qu.:44.00        3rd Qu.:36.00    
    ##  Max.   :80.00           Max.   :69.00        Max.   :50.00    
    ##  NA's   :249             NA's   :249          NA's   :249      
    ##  ExpressiveLangTScore ExpressiveLangAge EarlyLearningComposite
    ##  Min.   :20.0         Min.   : 7.0      Min.   : 49.00        
    ##  1st Qu.:20.0         1st Qu.:16.0      1st Qu.: 56.00        
    ##  Median :45.0         Median :23.0      Median : 98.00        
    ##  Mean   :43.6         Mean   :29.8      Mean   : 89.48        
    ##  3rd Qu.:58.0         3rd Qu.:42.0      3rd Qu.:113.00        
    ##  Max.   :80.0         Max.   :70.0      Max.   :147.00        
    ##  NA's   :249          NA's   :249       NA's   :261

    summary(lu_data)

    ##        SUBJ         VISIT        MOT_MLU        MOT_LUstd    
    ##  A.D.    :  6   visit3.: 44   Min.   :1.856   Min.   :1.328  
    ##  A.H.    :  6   visit4.: 42   1st Qu.:3.513   1st Qu.:2.269  
    ##  A.Z.    :  6   visit5.: 41   Median :3.971   Median :2.466  
    ##  Albert. :  6   Visit2.: 33   Mean   :3.918   Mean   :2.431  
    ##  Alfie.  :  6   Visit6.: 32   3rd Qu.:4.300   3rd Qu.:2.619  
    ##  Allison.:  6   Visit1.: 31   Max.   :5.744   Max.   :3.069  
    ##  (Other) :316   (Other):129                                  
    ##    MOT_LU_q1       MOT_LU_q2       MOT_LU_q3        CHI_MLU     
    ##  Min.   :1.000   Min.   :1.000   Min.   :2.000   Min.   :0.000  
    ##  1st Qu.:1.000   1st Qu.:3.000   1st Qu.:5.000   1st Qu.:1.204  
    ##  Median :2.000   Median :4.000   Median :6.000   Median :1.857  
    ##  Mean   :1.747   Mean   :3.626   Mean   :5.646   Mean   :1.993  
    ##  3rd Qu.:2.000   3rd Qu.:4.000   3rd Qu.:6.000   3rd Qu.:2.758  
    ##  Max.   :4.000   Max.   :6.000   Max.   :8.000   Max.   :4.365  
    ##                                                                 
    ##    CHI_LUstd        CHI_LU_q1        CHI_LU_q2       CHI_LU_q3    
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0.000  
    ##  1st Qu.:0.6121   1st Qu.:1.0000   1st Qu.:1.000   1st Qu.:1.000  
    ##  Median :1.2875   Median :1.0000   Median :1.000   Median :2.000  
    ##  Mean   :1.3130   Mean   :0.9553   Mean   :1.545   Mean   :2.672  
    ##  3rd Qu.:2.0343   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:4.000  
    ##  Max.   :2.8951   Max.   :2.0000   Max.   :4.000   Max.   :6.000  
    ## 

    summary(word_data)

    ##        SUBJ         VISIT       types_MOT       types_CHI    
    ##  A.D.    :  6   visit3.: 44   Min.   : 74.0   Min.   :  0.0  
    ##  A.H.    :  6   visit4.: 42   1st Qu.:298.0   1st Qu.: 29.0  
    ##  A.Z.    :  6   visit5.: 41   Median :352.5   Median :100.0  
    ##  Albert. :  6   Visit2.: 33   Mean   :354.5   Mean   :104.7  
    ##  Alfie.  :  6   Visit6.: 32   3rd Qu.:410.0   3rd Qu.:163.2  
    ##  Allison.:  6   Visit1.: 31   Max.   :601.0   Max.   :307.0  
    ##  (Other) :316   (Other):129                                  
    ##   types_shared      tokens_MOT     tokens_CHI        X          
    ##  Min.   :  0.00   Min.   : 209   Min.   :   0.0   Mode:logical  
    ##  1st Qu.: 22.75   1st Qu.:1441   1st Qu.: 138.5   NA's:352      
    ##  Median : 84.50   Median :1839   Median : 353.0                 
    ##  Mean   : 84.26   Mean   :1832   Mean   : 389.8                 
    ##  3rd Qu.:133.25   3rd Qu.:2260   3rd Qu.: 586.5                 
    ##  Max.   :232.00   Max.   :3182   Max.   :1294.0                 
    ## 

    # Change column names 
    demo_data <- plyr::rename(demo_data, c("Child.ID" = "SUBJ", "Visit" = "VISIT"))

    # Otherwise I could have used colnames
    colnames(demo_data)[1] <- "SUBJ"
    colnames(demo_data)[2] <- "VISIT"

2b. Find a way to homogeneize the way "visit" is reported (visit1 vs.
1).

Tip: The stringr package is what you need. str\_extract () will allow
you to extract only the digit (number) from a string, by using the
regular expression \\d.

    # Removing all irrelevant stuff in the VISIT column making them the same
    lu_data$VISIT <- gsub("[^[:digit:]., ]", "", lu_data$VISIT)
    lu_data$VISIT <- str_replace_all(lu_data$VISIT,"[.]","" )

    # Otherwise I could have extracted numbers like this:
    lu_data$VISIT <- str_extract(lu_data$VISIT,"\\d" )

    word_data$VISIT <- gsub("[^[:digit:]., ]", "", word_data$VISIT)
    word_data$VISIT <- str_replace_all(word_data$VISIT,"[:punct:]","" )

    # Now, The visit column only contains numbers 
    # I make sure they are all numeric before merging
    demo_data$VISIT <- as.numeric(demo_data$VISIT)
    lu_data$VISIT <- as.numeric(lu_data$VISIT)
    word_data$VISIT <- as.numeric(word_data$VISIT)

2c. We also need to make a small adjustment to the content of the
Child.ID coloumn in the demographic data. Within this column, names that
are not abbreviations do not end with "." (i.e. Adam), which is the case
in the other two data sets (i.e. Adam.). If The content of the two
variables isn't identical the rows will not be merged. A neat way to
solve the problem is simply to remove all "." in all datasets.

Tip: stringr is helpful again. Look up str\_replace\_all Tip: You can
either have one line of code for each child name that is to be changed
(easier, more typing) or specify the pattern that you want to match
(more complicated: look up "regular expressions", but less typing)

    # Removing all "." from the column named SUBJ in all data sets
    word_data$SUBJ <- str_replace_all(word_data$SUBJ,"[.]","" )
    lu_data$SUBJ <- str_replace_all(lu_data$SUBJ,"[.]","" )
    demo_data$SUBJ <- str_replace_all(demo_data$SUBJ,"[.]","" )

2d. Now that the nitty gritty details of the different data sets are
fixed, we want to make a subset of each data set only containig the
variables that we wish to use in the final data set. For this we use the
tidyverse package dplyr, which contains the function select().

The variables we need are: \* Child.ID, \* Visit, \* Diagnosis, \*
Ethnicity, \* Gender, \* Age, \* ADOS,  
\* MullenRaw, \* ExpressiveLangRaw, \* Socialization \* MOT\_MLU, \*
CHI\_MLU, \* types\_MOT, \* types\_CHI, \* tokens\_MOT, \* tokens\_CHI.

    # Select relevant columns from demo_data
    demo_sel <- select(demo_data, SUBJ, VISIT, Diagnosis, Ethnicity,Gender,Age,ADOS,MullenRaw,ExpressiveLangRaw, Socialization)

    # Select relevant columns from lu_data
    lu_sel <- select(lu_data, SUBJ, VISIT, MOT_MLU, CHI_MLU)

    # Select relevant columns from word_data
    word_sel <- select(word_data, SUBJ, VISIT, types_MOT, types_CHI, tokens_MOT, tokens_CHI)

Most variables should make sense, here the less intuitive ones. \* ADOS
(Autism Diagnostic Observation Schedule) indicates the severity of the
autistic symptoms (the higher the score, the worse the symptoms). Ref:
<https://link.springer.com/article/10.1023/A:1005592401947> \* MLU
stands for mean length of utterance (usually a proxy for syntactic
complexity) \* types stands for unique words (e.g. even if "doggie" is
used 100 times it only counts for 1) \* tokens stands for overall amount
of words (if "doggie" is used 100 times it counts for 100) \* MullenRaw
indicates non verbal IQ, as measured by Mullen Scales of Early Learning
(MSEL
<https://link.springer.com/referenceworkentry/10.1007%2F978-1-4419-1698-3_596>)
\* ExpressiveLangRaw indicates verbal IQ, as measured by MSEL \*
Socialization indicates social interaction skills and social
responsiveness, as measured by Vineland
(<https://cloudfront.ualberta.ca/-/media/ualberta/faculties-and-programs/centres-institutes/community-university-partnership/resources/tools---assessment/vinelandjune-2012.pdf>)

Feel free to rename the variables into something you can remember (i.e.
nonVerbalIQ, verbalIQ)

    # renaming MullenRaw
    colnames(demo_sel)[8] <- "nonverbalIQ"
    colnames(demo_sel)[9] <- "verbalIQ"
    colnames(demo_sel)[7] <- "severity"

    # Overwise, it is possible to use rename from dplyr: demo_sel <- plyr::rename(demo_sel, c("before" = "after", "before" = "after" ...))

2e. Finally we are ready to merge all the data sets into just one.

Some things to pay attention to: \* make sure to check that the merge
has included all relevant data (e.g. by comparing the number of rows) \*
make sure to understand whether (and if so why) there are NAs in the
dataset (e.g. some measures were not taken at all visits, some
recordings were lost or permission to use was withdrawn)

    # Checking number of rows 
    nrow(demo_sel)

    ## [1] 372

    nrow(lu_sel)

    ## [1] 352

    nrow(word_sel)

    ## [1] 352

    # NA information
    summary(is.na(demo_sel))

    ##     SUBJ           VISIT         Diagnosis       Ethnicity      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:372       FALSE:372       FALSE:372       FALSE:372      
    ##                                                                 
    ##    Gender           Age           severity       nonverbalIQ    
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:372       FALSE:362       FALSE:125       FALSE:182      
    ##                  TRUE :10        TRUE :247       TRUE :190      
    ##   verbalIQ       Socialization  
    ##  Mode :logical   Mode :logical  
    ##  FALSE:123       FALSE:369      
    ##  TRUE :249       TRUE :3

    summary(is.na(lu_sel))

    ##     SUBJ           VISIT          MOT_MLU         CHI_MLU       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:352       FALSE:352       FALSE:352       FALSE:352

    summary(is.na(word_sel))

    ##     SUBJ           VISIT         types_MOT       types_CHI      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:352       FALSE:352       FALSE:352       FALSE:352      
    ##  tokens_MOT      tokens_CHI     
    ##  Mode :logical   Mode :logical  
    ##  FALSE:352       FALSE:352

    # Merging the datasets
    total <- merge(word_sel,lu_sel,by=c("SUBJ","VISIT"), all = TRUE)
    total <- merge(total,demo_sel, by=c("SUBJ","VISIT"), all = TRUE)
    nrow(total)

    ## [1] 372

    summary(is.na(total))

    ##     SUBJ           VISIT         types_MOT       types_CHI      
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:372       FALSE:372       FALSE:352       FALSE:352      
    ##                                  TRUE :20        TRUE :20       
    ##  tokens_MOT      tokens_CHI       MOT_MLU         CHI_MLU       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:352       FALSE:352       FALSE:352       FALSE:352      
    ##  TRUE :20        TRUE :20        TRUE :20        TRUE :20       
    ##  Diagnosis       Ethnicity         Gender           Age         
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:372       FALSE:372       FALSE:372       FALSE:362      
    ##                                                  TRUE :10       
    ##   severity       nonverbalIQ      verbalIQ       Socialization  
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:125       FALSE:182       FALSE:123       FALSE:369      
    ##  TRUE :247       TRUE :190       TRUE :249       TRUE :3

2f. Only using clinical measures from Visit 1 In order for our models to
be useful, we want to minimize the need to actually test children as
they develop. In other words, we would like to be able to understand and
predict the children's linguistic development after only having tested
them once. Therefore we need to make sure that our ADOS, MullenRaw,
ExpressiveLangRaw and Socialization variables are reporting (for all
visits) only the scores from visit 1.

A possible way to do so: \* create a new dataset with only visit 1,
child id and the 4 relevant clinical variables to be merged with the old
dataset \* rename the clinical variables (e.g. ADOS to ADOS1) and remove
the visit (so that the new clinical variables are reported for all 6
visits) \* merge the new dataset with the old

    newdata <- subset(total, VISIT == 1) %>% select(SUBJ, severity, Socialization, nonverbalIQ, verbalIQ) %>% plyr::rename(c("severity" = "severity1", "Socialization" = "Socialization1", "nonverbalIQ"="nonverbalIQ1", "verbalIQ"="verbalIQ1"))

    # Otherwise, I could have used total[which(total$VISIT == 1),] instead of subset()

    # Merging with the original
    newdata <- merge(total,newdata, by ="SUBJ", all = TRUE)

2g. Final touches

Now we want to \* anonymize our participants (they are real children!).
\* make sure the variables have sensible values. E.g. right now gender
is marked 1 and 2, but in two weeks you will not be able to remember,
which gender were connected to which number, so change the values from 1
and 2 to F and M in the gender variable. For the same reason, you should
also change the values of Diagnosis from A and B to ASD (autism spectrum
disorder) and TD (typically developing). Tip: Try taking a look at
ifelse(), or google "how to rename levels in R". \* Save the data set
using into a csv file. Hint: look into write.csv()

    # Change Gender number to gender letter
    newdata$Gender[newdata$Gender == 1]<-"M"
    newdata$Gender[newdata$Gender == 2]<-"F"

    # Change diagnosis
    newdata$Diagnosis <- as.character(newdata$Diagnosis)

    newdata$Diagnosis[newdata$Diagnosis == "A"]<-"ASD"
    newdata$Diagnosis[newdata$Diagnosis == "B"]<-"TD"

    # Rename race
    newdata$Ethnicity <- as.character(newdata$Ethnicity)
    newdata$Ethnicity[newdata$Ethnicity == "Bangledeshi"]<-"Bangladeshi"

    ?write.csv()

    ## starting httpd help server ... done

    newdata$SUBJ <- as.numeric(as.factor(newdata$SUBJ))
    newdata$SUBJ <- sub("^", "subject", newdata$SUBJ )

    # Write to CSV
    write.csv(newdata, file = "clean_data.csv")

1.  BONUS QUESTIONS The aim of this last section is to make sure you are
    fully fluent in the tidyverse. Here's the link to a very helpful
    book, which explains each function:
    <http://r4ds.had.co.nz/index.html>

2.  USING FILTER List all kids who:

<!-- -->

1.  have a mean length of utterance (across all visits) of more than 2.7
    morphemes.
2.  have a mean length of utterance of less than 1.5 morphemes at the
    first visit
3.  have not completed all trials. Tip: Use pipes to solve this

USING ARRANGE

1.  Sort kids to find the kid who produced the most words on the 6th
    visit
2.  Sort kids to find the kid who produced the least amount of words on
    the 1st visit.

USING SELECT

1.  Make a subset of the data including only kids with ASD, mlu and word
    tokens
2.  What happens if you include the name of a variable multiple times in
    a select() call?

USING MUTATE, SUMMARISE and PIPES 1. Add a column to the data set that
represents the mean number of words spoken during all visits. 2. Use the
summarise function and pipes to add an column in the data set containing
the mean amount of words produced by each trial across all visits. HINT:
group by Child.ID 3. The solution to task above enables us to assess the
average amount of words produced by each child. Why don't we just use
these average values to describe the language production of the
children? What is the advantage of keeping all the data?
