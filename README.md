This repository contains R code to perform single-trials predictions of
experimental variables from EEG recordings and provides sample data to run this
code. Below we describe:

1. Code installation

2. Input data format,

3. Scripts provided to peform analysis,

4. Plotting functions.

# 1. Code installation

The code is distributed in the R package singleTrialEEGPredictions. To run
this code first load the suggested packages: boot, bootstrap, Rwave, reshape,
lattice, ggplot2, WRS. All but the last packages are available from CRAN. To
install WRS follow the installation
instructions in https://github.com/nicebread/WRS/blob/master/README.md

Download or clone the code and data available at
https://github.com/joacorapela/singleTrialEEGPredictions. Open a terminal, go
to the directory where you installed the code and data, invoke R, and from the
R command line run install.packages("singleTrialEEGPredictions_1.0.tar.gz").

# 2. Input data

The input data should be epoched activations of Independent Components (ICs)
from an Independent Component Analysis. Epochs should be built around the
presentation of standards of a given modality (e.g., standardModality in
{Auditory or Visual}) under a given attentional condition (e.g.,
attentionCondition in {switchvision, switchaudition}).  Components should be
grouped in clusters (e.g., clusterID in 1:19). In the examples below
`<sortvar>` should be any text string identifying your analysis.

In the directory data we provide an example dataset from clusterID 4, Visual
standardModality and switchvision attended condition.

For each `<clusterID>`, `<attentionCondition>`, and `<standardModality>` one
should provide six files, named as follows:


1. inputClusterID`<clusterID>`Condition`<attentionCondition>`Sortvars`<sortvar><standardModality>`Stim_metaData.dat

This should be a file in double format with 3 values: 

    1. nSamplesPerTrial (i.e., number of samples per epoch),

    2.  nTrialsAllSubjects (i.e., the sum of the number of trials of all subjects for `<attentionCondition>` and `<standardModality>`),

    3. sampleRate

2. inputClusterID`<clusterID>`Condition`<attentionCondition>`Sortvars`<sortvar><standardModality>`Stim_data.dat

This should be a file in double format with nSamplesPerTrial x
nTrialsAllSubjects values. This file should contain the activations of all ICs
from all subjects for `<attentionCondition>` and `<standardModality>`).

3. inputClusterID`<clusterID>`Condition`<attentionCondition>`Sortvars<sortvar><standardModality>`Stim_epochEventIDs.dat

This should be a file in double format with nTrialsAllSubjects values. This file should contain the identification numbers of all trials (i.e., epochs) from all independent components (ICs) from all subjects for `<attentionCondition>` and `<standardModality>`).

4. inputClusterID`<clusterID>`Condition`<attentionCondition>`Sortvars`<sortvar><standardModality>`Stim_times.dat

This should be a file in double format with nSamplesPerTrial values. This file should contain the times corresponding to the samples of every trial from all independent components (ICs) from all subjects for `<attentionCondition>` and `<standardModality>`).

5. inputClusterID`<clusterID>`Condition`<attentionCondition>`Sortvars`<sortvar><standardModality>`Stim_components.dat

This should be a file in double format with nTrialsAllSubjects values. This file should contain the IC number corresponding to each trial (i.e., epoch) from all subjects for `<attentionCondition>` and `<standardModality>`).  

6.  inputClusterID`<clusterID>`Condition`<attentionCondition>`Sortvars`<sortvar><standardModality>`Stim_subjectNames.dat

This should be a file in plain text format with nTrialsAllSubjects lines.  The ith line should contain the subject name correspoding to the ith trial (i.e., epoch).

The previous six files appear under data/erpImages.

In addition one should provide the following files:

1.  sfpds`<subjectName><standardModality><attentionCondition>`.dat

This should be a file in double format. The first value of this file should contain the length of the file and the remainder values should be the vectorized form of a matrix with number of rows equal to the number of trials for `<subjectName>, `<sandardModality>` and `<attentionCondition>`. This matrix should contain two columns; the first one should contain identification numbers of trials and the second one the corresponding standard foreperiod durations.  
These files appear under data/sfpds.

2. subjectsAndComponentsClusterID`<clusterID>`.tsv

This should be a tab separated text file with as many rows as components in `<clusterID>`. It should contain two columns; the first one should give the name of a subject and the second one a component number of this subject that has been grouped in `<clusterID>`.

These files appear under data/subjectsAndComponents.

3. errorRates`<condition>`.tsv
 
This should be a tab separated text file with as many rows as subjects in the experiment. It should contain four columns; the first one should give the name of a subject, and the second, third, and fourth columns should give the mean error rate, and the lower and higher bounds of the 95% confidence interval of the mean error rate, respectively.

These files appear under data/behavioralAnalysis/errorRates.

# 2. Data analysis

One should run the following three scripts:

1. doSaveSFPDsInfo.R

This script groups all Standard Foreperiod Durations (SFPDs) into a single R file and saves this file as results/sfpds/sfpdsInfo.RData.

2. doProcessAll.R

This script will read the input data and:

    1. Compute Inter-Trial Coherence and extract peak ITC frequencies,
    2. Compute phases,
    3. Compute Deviation from the Mean Phase,
    4. Perform regression analysis.

Note that running this script will take several hours, since the regression
analysis is perform for many maximum SFPDs.

3. doSelectMinAndMaxSFPDsOfBestPredictions.R

For each subject, standard modality and attended condition, this script selects the optimal maximum SFPD.

# 3. Plots

The following scripts plot results from the data analysis.

1. doPlotPredictionsCorCoefsVsErrorRatesAtBestMinMaxSFPDs.R

This script plots predictive power of models versus subjects error rates, as in Figure 5 in Rapela et al. 2017.

