
# Full-Model Estimator of A<sub>V</sub> (AbsreDgaia):

Code for a full-parameter dust model fitting the absolute reddening
curve using V-band photometry, the star's absolute magnitude
M<sub>V</sub>, and the visual extinction A<sub>V</sub> derived from
the the trigonometric distance D<sub>Gaia</sub>



##  📌  Description


- _mp_absreDgaia.pro_:

IDL/GDL program for fitting absolute reddening curves with Av computed
for given reddening E(B−V), V-band, absolute brightness Mv, and the
trigonometric distance Dgaia of the star. Chi2 fitter mpfit is used
with 7 free parameters of the dust model by [Siebenmorgen
(2023)](https://doi.org/10.48550/arXiv.2311.03310)

- _crossec_AvEbv.f_:

Fortran code for computing cross-sections and optical depth using the
dust model by [Siebenmorgen et al
(2024)](https://doi.org/10.48550/arXiv.2311.03310). This file must be
compiled first. Compilation instructions and additional details are
provided in the file's header.

- _./Data_Redd2023_AA676_132_Tab4_:

Includes reddening curves for the high-quality far UV selected
reddening curve sample by [Siebenmorgen et al
(2023)](https://doi.org/10.1051/0004-6361/202244594)



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
For a specific target (e.g., HD027778), initialize in idl the input parameters and run for that star _mp_absreDgaia_

```bash
target = 'HD027778'  
Vmag    = 6.327  
Ebv_ref = 0.391  
Mv      = -1.52  
Dgaia   = 210.3  

mp_absreDgaia, target, Vmag, Ebv_ref, Mv, Dgaia, /ps  
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

