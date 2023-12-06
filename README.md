# iEEG
Repository for performing signal processing and relating neural and behavioral data using simple linear and mixed effects regression for one exemplar intracranial electrophysiology dataset. This tutorial is relevant for Saez laboratory members/collaborators performing preliminary analyses with iEEG data:

**Signal processing scripts**:
 * Drop noisy channels, notch filter, white matter re-reference all gray matter channels:`preprocessing.ipynb`
 * Sync behavioral and neural data, remove IEDs, create Wavelet TFRs:`analysis.ipynb`

All signal processing conducted using Saez Laboratory's [LFPAnalysis](https://github.com/seqasim/LFPAnalysis) pipeline. 

**Regression scripts**:
* Conduct simple linear and mixed effects regression using behavioral and neural data:`regression.R`

**Visualization scripts**:
* Create visualizations (bargraph and lineplots):`visualize_regression_results.ipynb`
