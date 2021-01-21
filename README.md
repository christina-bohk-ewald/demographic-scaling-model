# demographic-scaling-model

The `demographic-scaling-model` is designed to estimate in a straightforward manner the total number and prevalence of COVID-19 infections for countries worldwide.  

Publication: 

Bohk-Ewald, C., Dudel, C., and M. Myrskylä (2020). A demographic scaling model for estimating the total number of COVID-19 infections. This article has been accepted for publication in the International Journal of Epidemiology, dyaa198, [https://doi.org/10.1093/ije/dyaa198](https://doi.org/10.1093/ije/dyaa198), published by Oxford University Press. A preprint is available on [medRxiv](https://www.medrxiv.org/content/10.1101/2020.04.23.20077719v2), [arXiv.org](https://arxiv.org/abs/2004.12836), and [OSF](https://dx.doi.org/10.17605/OSF.IO/GQYJA).

We provide here the **[Latest results](#latest-results)** as well as **[How to run](#how-to-run)** the R source code to generate them.

## Latest results

For the ten countries with most reported deaths as of July 23, 2020:

<p align="center"><img width=100% src="https://github.com/christina-bohk-ewald/demographic-scaling-model/blob/master/Figure-1.png"></p>
<p align="center"><img width=80% src="https://github.com/christina-bohk-ewald/demographic-scaling-model/blob/master/Figure-2.png"></p>

## How to run

The code needs to be executed in four steps, as defined by `step-*.R` files in the root directory.

### Prerequisites

- The `info-input-data.txt` file contains information about required input data.
  
- Due to copyrights you will need to download and save input data yourself. Original file names and URLs where to download them are given in the `info-input-data.txt` file. 

- Input data include confirmed cases and reported deaths attributable to COVID-19 of JHU CSSE (2020), population counts and abridged life tables of UNWPP 2019, infection fatality rates by 10-year age groups as, for example, published in Verity et al. (2020), and global age distribution of COVID-19 deaths as, for example, calculated based on data of Dudel et al. (2020).

- Make sure you have set the correct working directories before you start. 

### Execution

Run the `step-*.R` scripts in the root directory in the prescribed order.

## How to cite

If you use this code for academic research, please cite this GitHub repository as well as the paper noted above. 

## How to contribute

Please note that this source code is an academic project. We welcome any issues and pull requests.

## License

The source code of `demographic-scaling-model` is published under the [GNU General Public License version 3](https://www.gnu.org/licenses/gpl-3.0.en.html). 
