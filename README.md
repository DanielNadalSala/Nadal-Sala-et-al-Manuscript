Nadal-Sala, Grote, (...), Ruehr. (2020). Assessing model performance via the most limiting environmental driver (MLED) in two differently stressed pine stands. Ecological Applications

###### Folders contained:

###Data folder:

Contains all the data required to run the analysis performed in the experiment, identified by the two forest stands analyzed (i.e. Yatir and Hyytiala forests)

##Files:


###Code folder:
Contains all the R scripts used to analyze the data, also identified by the two forest stands.

##Files:


Hyytiala_CombinedLimitations.rda
Add files via upload
3 minutes ago
Hyytiala_LDNDC.rda
Add files via upload
3 minutes ago
Hyytiala_Modeled_Limitations.rda
Add files via upload
3 minutes ago
Hyytiala_Observed.rda
Add files via upload
3 minutes ago
Hyytiala_Observed_Limitations.rda
Add files via upload
3 minutes ago
Readme.txt
Update Readme.txt
10 seconds ago
Yatir_CombinedLimitations.rda
Add files via upload
3 minutes ago
Yatir_LDNDC.rda
Add files via upload
3 minutes ago
Yatir_Modeled_Limitations.rda
Add files via upload
3 minutes ago
Yatir_Observed.rda
Add files via upload
3 minutes ago
Yatir_Observed_Limitations.rda
Add files via upload
3 minutes ago 


#Main code analysis:

Stepwise, these files are called to analyze the data for Hyytiälä and Yatir forests. There are two files per forest, one analyzing the observations and one analyzing the LDNDC outputs (Identified always as LDNDC).

First step: Merges the environmental data with the flux data.

- Hyytiala First step_ Merging the data.R  ####(Observations)
- Hyytiala First step LDNDC_ Merging the data.R ####(LDNDC simulations)

- Yatir First step_ Merging the data.R ####(Observations)
- Yatir First step LDNDC_ Merging the data.R ####(LDNDC simulations)

Second step: To obtain the sensitivities of GPP the different environmental drivers by running the Random Forest algorithm.

- Hyytiala Second step Observed_RandomForest in observed data.R ####(Observations)
- Hyytiala Second step LDNDC_RandomForest in observed data.R ####(LDNDC simulations)

- Yatir Second step Observed_RandomForest in observed data.R ####(Observations)
- Yatir Second step LDNDC_RandomForest in observed data.R ####(LDNDC simulations)

Third step: This step compares the Observed and LDNDC modeled sensitivities of the different environmental drivers.

- Hyytiala Third step_Plotting Merged Observed and Modeled Sensitivities.R
- Yatir Third step_Plotting Merged Observed and Modeled Sensitivities.R

Fourth step: This step performs the optimization of the alpha coefficients, and determines the MLED for each day.

- Hyytiala Fourth step_Optimization + MLED.R
- Yatir Fourth step_Optimization + MLED.R

#Supplementary: 

These files are the ones that analyze the data and generate the figures for the Supplementary Notes.

- To plot monthly climate variables (Supplementary 1).R
- To plot GPP against D limitations in Yatir (Supplementary 2).R
- Analyze the Importance for Random Forest (Supplementary 3).R











