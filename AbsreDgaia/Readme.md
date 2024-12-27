# 1-Parameter Estimator of A<sub>V</sub> (1_absReddEbvu):

Code for a 1-parameter dust model fitting the absolute reddening curve
using V-band photometry, the star's absolute magnitude M<sub>V</sub>,
and the visual extinction A<sub>V</sub> derived without knowldege of
the trigonometric distance, under the assumption that 50% of the dust
mass is in dark dust. 



##  📌  Description


```bash
mp_absreDgaia.pro:
```
IDL/GDL program for fitting absolute reddening curves with Av computed
for given reddening E(B−V), V-band, absolute brightness Mv, and the
trigonometric distance Dgaia of the star. Chi2 fitter mpfit is used
with 7 free parameters of the dust model by Siebenmorgen (2023).


crossec_AvEbv.f:
Fortran code for computing cross-sections and optical depth using the
dust model by Siebenmorgen R. (2023, A&A, 670A, 115).  This file must
be compiled first. Compilation instructions and additional details are
provided in the file's header.



./Data_Redd2023_AA676_132_Tab4:

Includes reddening curves for the high-qualityfar UV selected
reddening curve sample by Siebenmorgen et al 2023, A&A, 676, A132, 292



RunAll.pro: 
An IDL script to run the reddening curve fit for the sample.

./Data_Redd2023_AA676_132_Tab4
Includes reddening curves for each star.

Requirements:
IDL 8.8.2 (darwin x86_64 m64) or install GDL.

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
For a specific target (e.g., HD027778), initialize in idl the input parameters and run for that star mp_absreDgaia

```bash
target = 'HD027778'  
Vmag    = 6.327  
Ebv_ref = 0.391  
Mv      = -1.52  
Dgaia   = 210.3  

mp_absreDgaia, target, Vmag, Ebv_ref, Mv, Dgaia, /ps  
```

2) Run for all Stars
To run the reddening curve fit for all 33 stars in the sample type in idl:

```bash
.r RunAll
```


3) Results
The results will be saved in the ./Result/ directory.


## 📌  Additional Info
Further descriptions are included in the headers of the Files.


