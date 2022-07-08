## Applied Machine Learning to Identify Differential Risk Groups Underlying Externalizing and Internalizing Problem Behaviors Trajectories
###

Code, data, and fitted models for the paper "Applied Machine Learning to Identify Differential Risk Groups Underlying Externalizing and Internalizing Problem Behaviors Trajectories: A Case Study using a Cohort of Asian American Children" by Samrachana Adhikari, Shiying You, Alan Chen, Sabrina Cheng, and Keng-Yen Huang.


Organization of the repository is as follows:


- **code**: A subfolder consists of .R scripts for data cleaning ([1_prep](https://github.com/shiying88/AAChildMentalHealth/tree/master/code/1_prep)), externalizing and internalizing problem behavior trajectory clustering ([2_lcmm](https://github.com/shiying88/AAChildMentalHealth/tree/master/code/2_lcmm)), machine learning model fitting ([3_train](https://github.com/shiying88/AAChildMentalHealth/tree/master/code/3_train)), and model performance evaluation ([4_summary_evaluation](https://github.com/shiying88/AAChildMentalHealth/tree/master/code/4_summary_evaluation)). 
- **data**: A subfolder consists of the original dataset ([eclsk2011_dem](https://github.com/shiying88/AAChildMentalHealth/blob/master/data/eclsk2011_dem.rds)) and interim datasets for identifying differential problem behavior trajectories and fitting machine learning algorithms.
- **models**: A subfolder consists of problem behavior trajectory models ([lcmm](https://github.com/shiying88/AAChildMentalHealth/tree/master/models/lcmm)) and machine learning fitted models (including [random forest](https://github.com/shiying88/AAChildMentalHealth/tree/master/models/rf), [Superlearner](https://github.com/shiying88/AAChildMentalHealth/tree/master/models/superlearner), and [univariate analyses](https://github.com/shiying88/AAChildMentalHealth/tree/master/models/univariate)).
