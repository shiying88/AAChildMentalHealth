## Applied Machine Learning to Identify Differential Risk Groups Underlying Externalizing and Internalizing Problem Behaviors Trajectories

Code, data, and fitted models for the paper "Applied Machine Learning to Identify Differential Risk Groups Underlying Externalizing and Internalizing Problem Behaviors Trajectories: A Case Study using a Cohort of Asian American Children" by Samrachana Adhikari, Shiying You, Alan Chen, Sabrina Cheng, and Keng-Yen Huang.


Organization of the repository is as follow:


  - **code**: A subfolder consists of .R scripts for data cleaning (1_prep), clustering differential externalizing and internalizing trajectories (2_lcmm), fitting machine learning algorithms (3_train), and summarizing fitted algorithms (4_summary_evaluation). 
  - **data**: A subfolder consists of the original dataset (eclsk2011_dem.rds) and interim datasets for identifying risk behavioral trajectories and fitting machine learning algorithms
  - **models**: A subfolder consists of risk behavioral trajectories [lcmm](https://github.com/shiying88/AAChildMentalHealth/tree/master/models/lcmm) and fitted models (rf, superlearner, and univariate)
