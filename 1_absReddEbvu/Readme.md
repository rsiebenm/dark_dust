
# 1-Parameter Estimator of A<sub>V</sub> (1_absReddEbvu):

Code for a 1-parameter dust model fitting the absolute reddening curve
using V-band photometry, E(B-V), and the star's absolute magnitude
M<sub>V</sub>.  The trigonometric distance D<sub.Gaia<\sub> and the
visual extinction A<sub>V</sub> are unknowns.  The visual extinction
is derived from the reddening at infinite wavelengths of the best dust
model fit Av=-E(oo). The C abundance in dust of the amorphous carbon
grains is treated as a single-free parameter under the assumption that
50% of the dust mass is in dark dust.




##  📌  Description


- _mp_absredEbvu.pro_:

IDL/GDL program for fitting absolute reddening curves with
A<sub>V</sub> computed for given reddening E(B−V), V-band, absolute
brightness M<sub>V</sub>, and the trigonometric distance
D<sub.Gaia<\sub> of the star. Chi2 fitter mpfit is used with the C
abundance in dust of the amorphous carbon grains as a single-free
parameter of the dust model by [Siebenmorgen
(2023)](https://doi.org/10.48550/arXiv.2311.03310)

- _crossec_Ebv.f_:

Fortran code for computing cross-sections and optical depth using the
dust model by [Siebenmorgen(2023)](https://doi.org/10.48550/arXiv.2311.03310) for fixed amount of
dark dust and given E(B-V). This file must be compiled
first. Compilation instructions and additional details are provided in
the file's header.

- _./Data_Redd2023_AA676_132_Tab4_:
Includes reddening curves for the high-quality far UV selected
reddening curve sample by [Siebenmorgen (2023)](https://doi.org/10.48550/arXiv.2311.03310)


- _RunAll.pro_: 
An IDL script to run the reddening curve fit for the sample.


##  📌  Requirements:
gfortran compiler and IDL 8.8.2 or install GDL

## 🚀  Quickstart

1) Compile:
```bash
> gfortran -ffixed-line-length-132 -O5 crossec_AvEbv.f sigtDark_AvEbv.f -o a.crossec_Dgaia
```

2) Start IDL using:
```bash
> idl istart  
```

Alternatively, add the necessary libraries to your PATH:

```bash
!PATH = './AAstron/:' + !PATH  
!PATH = './Coyote/:'  + !PATH  
!PATH = './MPFIT/:'   + !PATH
```

## 🚀  Example 

1) Fit a Single Star: 
For a specific target initialize in idl the input parameters and run for that star _mp_absredEbvu_

```bash
mp_absreDEbvu, 'HD315023' , 0.361, Vmag=10.03, Mv=-2.28,  /ps
```

2) Run the reddening curve fit for all 33 stars in the sample type in idl:

```bash
.r RunAll
```


3) Results
The results will be saved in the ./Result/ directory.


## 📌  Additional Info
Further descriptions are included in the headers of the Files.



## 📌  Reading:

[The distance method](https://doi.org/10.48550/arXiv.2311.03310)

[The dust model](https://doi.org/10.1051/0004-6361/202243860) 


## 📌  Contact: 

Ralf.Siebenmorgen@eso.org

https://www.eso.org/~rsiebenm/

