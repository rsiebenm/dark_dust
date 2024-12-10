Device, Retain=2, Decomposed=0, true_color=24
!PATH = './AAstron/:' + !PATH
!PATH = './Coyote/:'  + !PATH

!PATH = './MPFIT/:'  + !PATH

.r alias
.r mpfit

.r mp_absreDD_Ebv

.r DLumMd.pro
.r pl_DL2DGcorMd

mp_absreDD_Ebv, /ps
DLumMd.pro
pl_DL2DGcorMd









