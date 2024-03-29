# CRLM-SPA

 Code for submission of our article: 'Deep learning-derived spatial organization features on histopathology images predicts prognosis in colorectal liver metastasis patients after hepatectomy'

 Authors: Lin Qi<sup>†</sup>, Jie-ying Liang<sup>†</sup>, Zhong-wu Li<sup>†</sup>, Shao-yan Xi<sup>†</sup>, Yu-ni Lai, Feng Gao, Xian-rui Zhang, De-shen Wang, Ming-tao Hu, Yi Cao, Li-jian Xu, Ronald CK Chan, Bao-cai Xing\*, Xin Wang\*, Yu-hong Li\*

# Reproducing of the results

This vignette including the details of all the downstream analysis to ensure the reproducibility, including the cox regression analysis, cox proportional hazards analysis, risk-scoring model selection, cutoff selection, independent validation and survival analysis. 

# The training of CNN model

To train a model for tissue classification, we downloaded two public datasets, NCT-CRC-HE-100k and CRC-VAL-HE-7K, with annotated tissue patches tessellated from primary CRC WSIs from http://dx.doi.org/10.5281/zenodo.1214456. All tissue patches were obtained from pathologists annotated contiguous pure tissue areas with 224 × 224 pixels (px) at a magnification of 20×, and were color-normalized using Macenko's method. The tissue patches were classified into nine classes: normal (NORM), adipose tissue (ADI), background (BACK), debris (DEB), lymphocytes (LYM), mucus (MUC), smooth muscle (MUS), cancer-associated stroma (STR) and colorectal adenocarcinoma epithelium (TUM). These two cohorts were used to train a CRC tissue classifier as described in our previous work [https://www.sciencedirect.com/science/article/pii/S2590124921000043]. 

Considering the difference in the tissue architecture between CRLM and CRC, we adjusted the classes of CRLM tissue to: background (BACK), debris (DEB), hepatocyte (HEP), lymphocytes (LYM), mucus (MUC), cancer-associated stroma (STR) and cancer epithelium (TUM). An experienced pathologist manually annotated 59 H&E slides of CRLM tissue in the SYSUCC cohort following the same criteria of the two public datasets. The hand-annotated tissue areas were tessellated into non-overlapping tissue patches followed by image augmentation to create a training set of 143,718 tissue patches from 38 H&E slides and a testing set of 17,653 tissue patches from 21 H&E slides 

© 2022 · Copyright© 2022 LIN QI. All rights reserved.
