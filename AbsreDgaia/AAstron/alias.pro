

;;;----------------------------------------------------------------------------
;;; Get out of IDL by all possible means.
;;;----------------------------------------------------------------------------

;------------------ ATV --------------
;.run  /home/rsiebenm/IDL/IMA/cmps_form
;.run  /home/rsiebenm/IDL/IMA/atv

;  .r fsc_color

pro colors, red, green, violet, blue, magenta, orange
  red     = fsc_color('red')
  green   = fsc_color('green')
  violet  = fsc_color('violet') 
  blue    = fsc_color('sky blue')
  magenta = fsc_color('magenta')
  orange  = fsc_color('orange')
end

;------------------ QUIT --------------


pro ls
case !version.os of
    'vms': cmd = 'dir'
    'windows': cmd = 'dir'
    'MacOS': goto, notsup
    else: cmd = 'ls'
    endcase
if !version.os NE 'vms' then spawn, cmd, /noshell   $
else  spawn, cmd
goto, DONE
NOTSUP: print, "This operation is not supported on the Macintosh'
DONE:
end

;--------------- HISTORY----------------

pro h
help, /RECALL_COMMANDS
end

pro hs, x
help, /struct, x
end

;---------------- PWD ------------------

pro pwd
case !version.os of
    'vms': cmd = 'show def'
    'windows': cmd = 'pwd'
    'MacOS': goto, notsup
    else: cmd = 'pwd'
    endcase
if !version.os NE 'vms' then spawn, cmd, /noshell $
else spawn, cmd
goto, DONE
NOTSUP: print, "This operation is not supported on the Macintosh'
DONE:
end


function mean, array
  ;; Return the average of a 1D or 2D data array.
  npix = size(array)
  return, total(array) /  N_ELEMENTS(array)
end

pro info, Tab
help, Tab
print, "   Min    = ", min(Tab)
print, "   Max    = ", max(Tab)
print, "   median = ", median(Tab)
print, "   mean   = ", mean(Tab)
print, "   sigma  = ", sigma(Tab)
print, "   total  = ", total(Tab)
end

;==============================

function mygauss, idim, jdim, sigma, pos=pos

;-----------------
; parameters check
;-----------------
 IF N_PARAMS() LT 1 THEN BEGIN
   PRINT, $
   'CALLING SEQUENCE: GAUSS= GAUSS( idim, jdim, sigma, pos=pos)'
   GOTO, CLOSING
 ENDIF
if n_elements( pos) eq 0 then begin
  pos= make_array( 2, /float)
  pos(0)= FLOAT(IDIM)/2.0
  pos(1)= FLOAT(JDIM)/2.0
endif
if n_elements( pos) eq 1 then begin
  print, 'Give 2 numbers for POS'
  goto, closing
endif

GAUSS= 1./2.d0/!pi/ SIGMA^2. * $
       EXP(-(( FINDGEN( IDIM)#REPLICATE( 1.0, IDIM)- POS(0))^2+$
          (REPLICATE( 1.0, JDIM)#FINDGEN( JDIM)- POS(1))^2)/(2.*SIGMA^2))
return, gauss
closing:
end

;==============================

pro setps, file=file
if not keyword_set(file) then filename='idl.ps'

pageInfo = cgPSWINDOW()
set_plot, 'PS'
DEVICE, _Extra=pageInfo, decomposed=0, filename=file, BITS_PER_PIXEL=8, /color
end


pro seteps, file=file
if not keyword_set(file) then filename='idl.ps'

device, decomposed=0
set_plot, 'PS'
device, filename='idl.eps', /portrait, /encapsulated, /color
end

pro endps
device, /close
set_plot, 'X'
device, decomposed=1
!p.color=fsc_color('white')

end

 colors, red, green, violet, blue, magenta, orange

end
;==============================
