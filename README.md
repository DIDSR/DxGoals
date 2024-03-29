# DxGoals
Diagnostic tests classify the binary status of a target condition (e.g., absent, present). Classification accuracy measures include sensitivity (Se), specificity (Sp), positive likelihood ratio PLR = Se / (1 – Sp), negative likelihood ratio NLR = (1 – Se)/Sp, and receiver operating characteristic curve (ROC). Classification accuracy measures are popular because they do not depend on the prevalence (i.e., pre-test probability) of the target condition and thus for low prevalence conditions can be estimated without bias in a moderately-sized, well-conducted study enriched with subjects having the condition.

Unfortunately, performance goals (PGs) for classification accuracy are often chosen with only a vague understanding of whether they confer that the test would be clinically useful in practice. Clinicians often think the PGs should be set by the statisticians. Statisticians think the PGs should be set by the clinicians!

A framework is needed for how to set clinically meaningful PGs for classification accuracy. Such a framework would improve conversations among stakeholders (Patients / FDA / Industry / Payers) on appropriate acceptance criteria for validating a diagnostic test for its intended use.

Clinically meaningful PGs for classification accuracy may be determined based on desired risk stratification. DxGoals aims to establish performance goals (PGS) based on input risk stratification and perform data analysis to assess the test’s alignment with PGs. By utilizing input risk stratification, the results include thresholds for PGs in terms of likelihood ratio, accompanied by graphs that provide visual support for conclusions.
# Code 
Code to set up performance goals from input stratification and analyze data is available in code directory. Examples and dataset could be found at (Gene Pennello, 2021) in citation. 
# Link 
https://fda-cdrh-osel-didsr-rst.shinyapps.io/DxGoals/ 
# Citation 
1. <a href="https://www.tandfonline.com/doi/full/10.1080/24709360.2021.1878406" target="_blank">Gene Pennello (2021) Classification accuracy goals for diagnostic tests based on risk stratification, Biostatistics & Epidemiology, 5:2, 149-168!</a>
 
# Disclaimer

This software and documentation (the “Software”) were developed at the Food and Drug Administration (FDA) by employees of the Federal Government in the course of their official duties. Pursuant to Title 17, Section 105 of the United States Code, this work is not subject to copyright protection and is in the public domain. Permission is hereby granted, free of charge, to any person obtaining a copy of the Software, to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, or sell copies of the Software or derivatives, and to permit persons to whom the Software is furnished to do so. FDA assumes no responsibility whatsoever for use by other parties of the Software, its source code, documentation or compiled executables, and makes no guarantees, expressed or implied, about its quality, reliability, or any other characteristic. Further, use of this code in no way implies endorsement by the FDA or confers any advantage in regulatory decisions. Although this software can be redistributed and/or modified freely, we ask that any derivative works bear some notice that they are derived from it, and any modified versions bear some notice that they have been modified.
