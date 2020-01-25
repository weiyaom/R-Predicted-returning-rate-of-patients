# R-Predicted-returning-rate-of-patients

1.1 Our Goal
Project Final Report
Our project is aiming to understand more about the patients who will return to the hospital within 30 days after being discharged and to predict whether the patient will return or not. By analyzing the patients’ records, we hope to find some patterns and build a statistic model to accurately predict if the patient will return to the hospital or not.

1.2 Business Value
By analyzing and identifying the patients who are going to return within 30 days, the hospital could improve their quality of service, and better assist the patients who will return. If the hospital could know in advance, the hospital could review the patient’s profile before the patient was discharged, conducting some extra check and treating procedures. Hopefully, by doing so, the hospital could reduce the percentage of repeating patients, which could help the patients save a significant amount of money and save valuable time for the doctors.

1.3 Research Methodologies
By visualizing the patient info dataset, we performed a preliminary feature selection on the variables and formed some insights. We plan to build a model to predict whether a patient will return to the hospital after discharged. Since mistakenly classify return patients into no-return patients or no-return patients into return patients are both costly for the patients and the doctors (money/time), we decided to evaluate our candidate models by their prediction accuracy, which is ​neutral and fair. After tried​ logistic, LDA, decision tree, Random Forest, and boosting models, we’ve found that a combination of Random Forest and PCA gives us the best prediction accuracy on the test dataset. Although the prediction results from the previous models were not very satisfying, we still could get some inference and useful information from them.
