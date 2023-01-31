# Modelling Farm Size Distributions

Code for data preperation:

- Linking household survey data to administrative data
- Linking survey data to raster data

## Data preperation

Data comes from multiple sources. First we have
household survey data which comes from the Rural 
Household Multi-Indicator Survey:

* The [Rural Household Multi Indicator Survey](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TFXQJN):
* [Google Earth Engine](https://code.earthengine.google.com/)
* The FAO's [GAEZ](https://gaez.fao.org/pages/data-viewer) Portal

 

# Configuration

Make sure to load all the module (if running on HPC) from `data-prep.sh`.

In addition, create a file `~/.R/Makevars` with the following contents:

```
MAKEFLAGS = -j8

## C++ flags
CXX=g++
CXX11=g++
CXX14=g++
CXX17=g++

CXXFLAGS=-O3 -march=native -Wno-ignored-attributes
CXX11FLAGS=-O3 -march=native -Wno-ignored-attributes
CXX14FLAGS=-O3 -march=native -Wno-ignored-attributes
CXX17FLAGS=-O3 -march=native -Wno-ignored-attributes

CXXPICFLAGS=-fPIC
CXX11PICFLAGS=-fPIC
CXX14PICFLAGS=-fPIC
CXX17PICFLAGS=-fPIC

CXX11STD=-std=c++11
CXX14STD=-std=c++14
CXX17STD=-std=c++17

## C flags
CC=gcc
CFLAGS=-O3 -march=native

## Fortran flags
FC=gfortran
F77=gfortran
FFLAGS=-O3 -march=native
FCFLAGS=-O3 -march=native


```

See [here](https://github.com/stan-dev/rstan/issues/892)
for more details


<!-- For running on BC4, you can run:

Run `sbatch --export=d='testing_14_10_2022' run-model.sh`
sbatch --export=d='simulation_different_samples_14_10_2022' run-model.sh

Check job status with this sacct -j <jobid> -->

