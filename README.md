# CLIPi

This repository contains the R scripts developed for the Final Degree Project:

**Design and development of a tool for the computation of the CLIPi prognostic index for patient stratification in Mycosis Fungoides using data extracted from the Electronic Health Record.**

## Project Overview

The project aims to reconstruct the Cutaneous Lymphoma International Prognostic Index (CLIPi) longitudinally using retrospective Electronic Health Record (EHR) data obtained from Hospital Universitario 12 de Octubre.

The workflow integrates demographic information, laboratory results, dermatology follow-up records, and anatomical pathology reports to generate longitudinal patient trajectories and compute dynamic CLIPi scores over time.

## Repository Structure

### 1. Data extraction and cohort identification
`1 Data extraction and cohort identification.R`

Patient selection, dataset integration, cohort construction, and variable definition.

### 2. Data quality and exploratory analysis
`2 Data quality and exploratory analysis.R`

Exploratory assessment of data availability, demographic characteristics, laboratory information, and follow-up coverage.

### 3. Rule-based NLP
`3 Rule based NLP.R`

Identification of prognostic variables from unstructured pathology reports using rule-based Natural Language Processing techniques.

### 4. NLP manual validation
`4 NLP manual validation.R`

Manual validation of NLP outputs and performance assessment.

### 5. Longitudinal event table construction
`5 Longitudinal event table construction.R`

Integration of clinical events into a unified longitudinal event-based structure.

### 6. Event-based CLIPi computation
`6 Event based CLIPi computation.R`

Longitudinal computation of CLIPi variables and dynami scores throughout patient follow-up.

### 7. Survival analysis and CLIPi validation
`7 Survival analysis CLIPi validation.R`

Kaplan-Meier and Cox proportional hazards analyses used to evaluate the prognostic performance of the reconstructed CLIPi scores.

## Data Availability

Clinical datasets are not included in this repository due to patient confidentiality and data protection regulations.

## Software

The analysis was performed in R using packages for data manipulation, text processing, visualization, and survival analysis.
