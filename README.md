Nadal-Sala, Grote, (...), Ruehr. (2020). Assessing model performance via the most limiting environmental driver (MLED) in two differently stressed pine stands. Ecological Applications

###### Folders contained:

###Data folder:

Contains all the data required to run the analysis performed in the experiment, identified by the two forest stands analyzed (i.e. Yatir and Hyytiala forests)

##Files:


###Code folder:
Contains all the R scripts used to analyze the data, also identified by the two forest stands.

##Files:

Observed data for both stands:

- Hyytiala_Observed.rda
- Yatir_Observed.rda

Simulated data for both stands:

- Hyytiala_LDNDC.rda
- Yatir_LDNDC.rda

Observed and modeled limitations (ILSi) for both stands:

- Hyytiala_Observed_Limitations.rda
- Hyytiala_Modeled_Limitations.rda

- Yatir_Observed_Limitations.rda
- Yatir_Modeled_Limitations.rda

Combined limitations after alpha coefficient optimization (LSi):

- Hyytiala_CombinedLimitations.rda
- Yatir_CombinedLimitations.rda


#Main code analysis:

Stepwise, these files are called to analyze the data for Hyytiälä and Yatir forests. There are two files per forest, one analyzing the observations and one analyzing the LDNDC outputs (Identified always as LDNDC).

First step: To obtain the sensitivities of GPP the different environmental drivers by running the Random Forest algorithm.

- Hyytiala First Observed_RandomForest in observed data.R ####(Observations)
- Hyytiala First LDNDC_RandomForest in observed data.R ####(LDNDC simulations)

- Yatir First Observed_RandomForest in observed data.R ####(Observations)
- Yatir First LDNDC_RandomForest in observed data.R ####(LDNDC simulations)

Second step: This step compares the Observed and LDNDC modeled sensitivities of the different environmental drivers.

- Hyytiala Second_Plotting Merged Observed and Modeled Sensitivities.R
- Yatir Second_Plotting Merged Observed and Modeled Sensitivities.R

Third step: This step performs the optimization of the alpha coefficients, and determines the MLED for each day.

- Hyytiala Third_Optimization + MLED.R
- Yatir Third_Optimization + MLED.R

#Supplementary: 

These files are the ones that analyze the data and generate the figures for the Supplementary Notes.

- To plot monthly climate variables (Supplementary 1).R
- To plot GPP against D limitations in Yatir (Supplementary 2).R
- Analyze the Importance for Random Forest (Supplementary 3).R
