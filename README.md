# Automatic Assessment of Species Commonness and Prevalence for Ecological Niche Modelling
This repository contains a sequence of scripts to estimate species commonness among a set of predefined classes, high/medium/low, corresponding to very common, fairly common, and rare assessments. These levels are meant to be associated with species prevalence probabilities for Ecological Niche Models, corresponding to 0.8, 0.5, and 0.2, respectively.

The scripts are general enough to be applicable to any collection of species in large data collections such as [GBIF](https://www.gbif.org/), [OBIS](https://obis.org/), and others. The methodology uses GBIF as the default data collection.

The methodology internally uses three model: Multi K-means (from [risk assessment models](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment)), X-means (clustering based on BIC), and a Variational Autoencoder (from the implementation in [this repository](https://github.com/cybprojects65/VariationalAutoencoder)). 

The repository is organised as follows:
1 - [Script](https://github.com/cybprojects65/AutomaticSpeciesPrevalenceAssessment/tree/main/Scripts) contains the entire sequence of scripts constituting the methodology  (from step 1 to 12)
2 - [Input](https://github.com/cybprojects65/AutomaticSpeciesPrevalenceAssessment/tree/main/Input) contains an example of list of species from Italian wetlands (from the Ramsar Convention) for use in the models and the descriptions of the Italian wetlands as geospatial polygons.
3 - [Features](https://github.com/cybprojects65/AutomaticSpeciesPrevalenceAssessment/tree/main/Features) contains  examples of aggregative features extracted for the species list, calculated from GBIF.
4 - [Output](https://github.com/cybprojects65/AutomaticSpeciesPrevalenceAssessment/tree/main/Output) contains output examples from the models involved.
5 - [Model Assessments](https://github.com/cybprojects65/AutomaticSpeciesPrevalenceAssessment/tree/main/Model%20Assessments) contains species commonness assessments by the three models, aligned in one comparison table.
6 - [Expert Assessments](https://github.com/cybprojects65/AutomaticSpeciesPrevalenceAssessment/tree/main/Expert%20Assessments) contains species commonness assessments by two experts of the Massaciuccoli Lake basin, in Tuscany, who evaluated the assessments by the models.