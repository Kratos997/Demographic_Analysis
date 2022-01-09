# Demographic Analysis tool for CU and CI subject data



<img src="https://n.neurology.org/content/neurology/69/13/1331/F1.large.jpg" alt="fMRI memory activation in AD" style="height: 200px; width:620px;"/>


**Figure 1** Coronal and left lateral views of correlations between performance on standardized clinical trial measures of verbal memory and novel-vs-repeated (NvR) activation during an associative-encoding task.  [DOI here](https://doi.org/10.1212/01.wnl.0000277292.37292.69) 

This a statistical tool meant to perform and model correlations of _Locus Coeruleus_ (LC) DTI metrics, Cognition test scores and Network measures between them originating from a dataset of subjects for research purposes that is imported from the user. This dataset needs to be preprocessed before uploading it. The subjects of the study can be separated in **Cognitively Unimpaired** (**CU**) or **Cognitively Impaired** (**CI**) groups.

The DTI (Diffusion Tensor Imaging) metrics of the LC that are used in the present tool are :
- Fractional Anisotropy (FA)
- Mean Diffusivity (MD)
- Axonal Diffusivity (AD)
- Radial Diffusivity (RD)

The Cognition tests that the subjects underwent are as follows :
- Mini-Mental State Exam (MMSE)
- FSCRT Immediate Recall (IR) 
- FSCRT Delayed Recall (DR)

The different Networks that were assessed for all subjects are respectively :
- The Dorsal Attention Network (DAN)
- The Dorsal Motor Network (DMN)
- The Frontal-Parietal Network (FPN)
- The Limbic Network (LIM)
- The Salience Network (SAL)
- The Sensorimotor Network (SMN)
- The Visual Network (VIS)
- The Locus Coeruleus Network (LC)
- The YeLC network 

Further on the DTI metrics mentioned above can be also measured seprately in the Rostral and the Caudal Part of the brain.

The models for which the correlations were tested for are the linear, quadratic and simple logarithmic models. For each of these models, were plotted only the significant correlations (with p-value < 0,05) for the coefficients used with their corresponding confidence interval.
The global p-values as well as the R2 and MSE (Minimal Squared Error) values of the models were extracted and attached to the plots.

_**Done by Konstantin Toussas**_