PRO oploterr, x, y, xerr, yerr, NOHAT=hat, HATLENGTH=hln, ERRTHICK=eth, $
      ERRSTYLE=est, THICK = thick, NOCLIP=noclip, ERRCOLOR = ecol, $
      NSKIP = nskip, _EXTRA = pkey, ANONYMOUS_ = Dummy_
;+
; NAME:
;	OPLOTERR
; PURPOSE:
;	Over-plot data points with accompanying X or Y error bars.
; EXPLANATION:
;	For use instead of PLOTERR when the plotting system has already been
;	defined. 
;
; CALLING SEQUENCE:
;	oploterr, [ x,]  y, [xerr], yerr  [,/NOHAT, HATLENGTH= ,
;		 ERRTHICK= , ERRSTYLE=, ERRCOLOR = ]
; INPUTS:
;	X = array of abcissae, any datatype except string
;	Y = array of Y values, any datatype except string
;	XERR = array of error bar values (along X)
;       YERR = array of error bar values (along Y)
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;	NOHAT     = if specified and non-zero, the error bars are drawn
;	            without hats.
;	HATLENGTH = the length of the hat lines used to cap the error bars.
;	            Defaults to !D.X_VSIZE / 100).
;	ERRTHICK  = the thickness of the error bar lines.  Defaults to the
;	            THICK plotting keyword.
;	ERRSTYLE  = the line style to use when drawing the error bars.  Uses
;	            the same codes as LINESTYLE.
;	ERRCOLOR =  scalar integer (0 - !D.N_TABLE) specifying the color to
;			use for the error bars
;	NSKIP = Positive Integer specifying the error bars to be plotted.   
;		For example, if NSKIP = 2 then every other error bar is 
;		plotted; if NSKIP=3 then every third error bar is plotted.   
;		Default is to plot every error bar (NSKIP = 1)
;
; NOTES:
;       If only two parameters are input, they are taken as Y and YERR
;       If only three parameters are input, they will be taken as X, Y and
;       YERR respectively.
;
; EXAMPLE:
;       Suppose one has X and Y vectors with associated errors XERR and YERR
;	and that a plotting system has already been defined:
;
;       (1) Overplot Y vs. X with both X and Y errors and no lines connecting
;           the points
;                  IDL> oploterr, x, y, xerr, yerr, psym=3
;       (2) Like (1) but overplot only the Y errors bars and omits "hats"
;                  IDL> oploterr, x, y, yerr, psym=3, /NOHAT
;
; PROCEDURE:
;	A plot of X versus Y with error bars drawn from Y - YERR to Y + YERR
;	and optionally from X - XERR to X + XERR is written to the output device
;
; WARNING:
;	This an enhanced version of a procedure that already exists in the
;	standard IDL V4.0 distribution.   Any call to the standard IDL version
;	should also work with this version, but the reverse is not true.
;
; MODIFICATION HISTORY:
;	Adapted from the most recent version of PLOTERR.  M. R. Greason,
;		Hughes STX, 11 August 1992.
;       Removed spurious keywords for IDL V3.0.0  W. Landsman Jan. 1993 
;	Added ability to plot a single point W. Landsman   July 1993
;	Added COLOR keyword option to error bars W. Landsman   November 1993
;	Remove CHANNEL call for V4.0 compatibility W. Landsman June 1995
;	Add ERRCOLOR, use _EXTRA keyword,           W. Landsman, July 1995
;	Remove spurious call to PLOT_KEYWORDS     W. Landsman, August 1995
;	OPLOT more than 32767 error bars          W. Landsman, Feb 1996
;	Added NSKIP keyword                       W. Landsman, Dec 1996
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;			Check the parameters.
;
 On_error, 2
 np = N_params()
 IF (np LT 2) THEN BEGIN
	print, "OPLOTERR must be called with at least two parameters."
	print, "Syntax: oploterr, [x,] y, [xerr], yerr, [..oplot keywords... "
	print,'     /NOHAT, HATLENGTH = , ERRTHICK=, ERRSTLYE=, ERRCOLOR=]'
	RETURN
 ENDIF

; Error bar keywords (except for HATLENGTH; this one will be taken care of 
; later, when it is time to deal with the error bar hats).

 IF (keyword_set(hat)) THEN hat = 0 ELSE hat = 1
 if not keyword_set(THICK) then thick = !P.THICK
 IF (n_elements(eth) EQ 0) THEN eth = thick
 IF (n_elements(est) EQ 0) THEN est = 0
 IF (n_elements(ecol) EQ 0) THEN ecol = !P.COLOR
 if N_elements( NOCLIP ) EQ 0 THEN noclip = 0
 if not keyword_set(NSKIP) then nskip = 1
;
; If no X array has been supplied, create one.  Make sure the rest of the 
; procedure can know which parameter is which.
;
 IF np EQ 2 THEN BEGIN			; Only Y and YERR passed.
	yerr = abs(y)
	yy = x
	xx = indgen(n_elements(yy))
        xerr = make_array(size=size(xx))

 ENDIF ELSE IF np EQ 3 THEN BEGIN 	; X, Y, and YERR passed.
        yerr = abs(xerr)
        yy = y
        xx = x

 ENDIF ELSE BEGIN                        ; X, Y, XERR and YERR passed.
	yerr = abs(yerr)
	yy = y
        xerr = abs(xerr)
	xx = x
 ENDELSE
;
;			Determine the number of points being plotted.  This
;			is the size of the smallest of the three arrays
;			passed to the procedure.  Truncate any overlong arrays.
;

 n = N_elements(xx) < N_elements(yy)

 IF np GT 2 then n = n < N_elements(yerr)   
 IF np EQ 4 then n = n < N_elements(xerr)

 xx = xx[0:n-1]
 yy = yy[0:n-1]
 yerr = yerr[0:n-1]
 IF np EQ 4 then xerr = xerr[0:n-1]

 ylo = yy - yerr
 yhi = yy + yerr

 if Np EQ 4 then begin
     xlo = xx - xerr
     xhi = xx + xerr
 endif
;
;			Plot the positions.
;
 if n NE 1 then begin
     oplot, xx, yy, NOCLIP=noclip,THICK = thick,_EXTRA = pkey 
 endif else begin 
     plots, xx, yy, NOCLIP=noclip,THICK = thick,_EXTRA = pkey
 endelse
;
;	Plot the error bars.   Compute the hat length in device coordinates
;       so that it remains fixed even when doing logarithmic plots.
;
    data_low = convert_coord(xx,ylo,/TO_DEVICE)
    data_hi = convert_coord(xx,yhi,/TO_DEVICE)
    if NP EQ 4 then begin
       x_low = convert_coord(xlo,yy,/TO_DEVICE)
       x_hi = convert_coord(xhi,yy,/TO_DEVICE)
    endif
    ycrange = !Y.CRANGE   &  xcrange = !X.CRANGE
    
FOR i = 0L, (n-1), Nskip DO BEGIN

    plots, [xx[i],xx[i]], [ylo[i],yhi[i]], LINESTYLE=est,THICK=eth,  $
		NOCLIP = noclip, COLOR = ecol
;                                                         Plot X-error bars 
;
    if np EQ 4 then plots, [xlo[i],xhi[i]],[yy[i],yy[i]],LINESTYLE=est, $
		THICK=eth, COLOR = ecol, NOCLIP = noclip
	IF (hat NE 0) THEN BEGIN
		IF (N_elements(hln) EQ 0) THEN hln = !D.X_VSIZE/100. 
		exx1 = data_low[0,i] - hln/2.
		exx2 = exx1 + hln
		plots, [exx1,exx2], [data_low[1,i],data_low[1,i]],COLOR=ecol, $
                      LINESTYLE=est,THICK=eth,/DEVICE, noclip = noclip
		plots, [exx1,exx2], [data_hi[1,i],data_hi[1,i]], COLOR = ecol,$
                       LINESTYLE=est,THICK=eth,/DEVICE, noclip = noclip
;                                          
                IF np EQ 4 THEN BEGIN
                   IF (N_elements(hln) EQ 0) THEN hln = !D.Y_VSIZE/100.
                   eyy1 = x_low[1,i] - hln/2.
                   eyy2 = eyy1 + hln
                   plots, [x_low[0,i],x_low[0,i]], [eyy1,eyy2],COLOR = ecol, $
                         LINESTYLE=est,THICK=eth,/DEVICE, NOCLIP = noclip
                   plots, [x_hi[0,i],x_hi[0,i]], [eyy1,eyy2],COLOR = ecol, $
                         LINESTYLE=est,THICK=eth,/DEVICE, NOCLIP = noclip
                ENDIF
	ENDIF
    NOPLOT:
ENDFOR
;
RETURN
END
