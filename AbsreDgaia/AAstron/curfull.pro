pro curfull, x, y, WaitFlag, DATA=Data, DEVICE=Device, NORMAL=Normal, $
   NOWAIT=NoWait, WAIT=Wait, DOWN=Down, CHANGE=Change, $
   LINESTYLE=Linestyle, THICK=Thick, NOCLIP=NoClip
;*******************************************************************************
;+
; NAME:
;	CURFULL
;
; PURPOSE:
;	Like intrinsic CURSOR procedure but with a full-screen cursor
; EXPLANATION:
;	This program is designed to essentially mimic the IDL CURSOR command,
;	but using a full screen cursor rather than a small cross cursor.
;	Uses OPLOT and X-windows graphics masking to emulate the cursor.
;
; CALLING SEQUENCE:
;	curfull, [X, Y, WaitFlag], [/DATA, /DEVICE, /NORMAL, /NOWAIT, /WAIT,
;		/DOWN, /CHANGE, LINESTYLE=, THICK=, /NOCLIP]
;
; REQUIRED INPUTS:
;	None.
;
; OPTIONAL INPUTS: 
;	WAITFLAG = if equal to zero it sets the NOWAIT keyword {see below} 
;
; OPTIONAL KEYWORD PARAMETERS:
;	DATA = Data coordinates are returned.
;	DEVICE = device coordinates are returned.
;	NORMAL = normal coordinates are returned.
;	NOWAIT = if non-zero the routine will immediately return the cursor's
;		present position.
;       WAIT = if non-zero will wait for a mouse key click before returning.
;       DOWN = equivalent to WAIT
;       CHANGE = returns when the mouse is moved OR if a key is clicked.
;       LINESTYLE = style of line that makes the cursor.
;       THICK = thickness of the line that makes the cursor.
;       NOCLIP = if non-zero will make a full-screen cursor, otherwise it will
;		default to the value in !P.NOCLIP.
;
; NOTES:
;	Note that this procedure does not allow the "UP" keyword/flag...which 
;	doesn't seem to work too well in the origianl CURSOR version anyway.
;
;       If a data coordinate system has not been established, then CURFULL
; 	will create one identical to the device coordinate system.   Note
;	that this kluge is required even if the user specified /NORMAL
;	coordinates, since CURFULL makes use of the OPLOT procedure
;
;	Only tested on X-windows systems.    If this program is interrupted,
;	the graphics function might be left in a non-standard state.   Type
;	DEVICE,SET_GRAPHICS=3 to return the standard graphics function.
;
; PROCEDURE:
;	The default cursor is blanked out and full-screen (or full plot window, 
; 	depending on the value of NOCLIP) intersecting lines are drawn centerd
;	on the cursor's position. 
;
; MODIFICATION HISTORY:
;	Written by J. Parker  22 Nov 93
;	Create data coordinates if not already present, W. Landsman Nov. 93
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;*******************************************************************************
On_error,2
if (!D.FLAGS AND 256) NE 256 then message, $
  'ERROR - Current graphics device ' + !D.NAME + ' does not support windows'
;
;   Check to see if the user does not want to wait.
;
if (N_Params() eq 3) then NoWait = (WaitFlag eq 0)

; Define plotting coordinates if they are not already established

if ( (!X.CRANGE[0] EQ 0) and (!X.CRANGE[1] EQ 0) ) then $
	plot, [0,!D.X_SIZE-1], [0,!D.Y_SIZE-1], /NODATA, XSTYLE=5, YSTYLE=5, $
              XMAR = [0,0], YMAR = [0,0]

if keyword_set(NoWait) then begin
   cursor, X, Y, /NOWAIT, DATA=Data, DEVICE=Device, NORMAL=Normal
   return
endif

;
;   Set up the graphics to be XOR with the cursor, and define the default cursor
; to be blank.
;
device, GET_GRAPHICS=OldGraphics, SET_GRAPHICS=6
device, CURSOR_IMAGE=intarr(16)

;
;   Determine the data range for the full screen.
;
Yfull = convert_coord([0,1], [0,1], /NORMAL, /TO_DATA)
Xfull = Yfull[0,*]
Yfull = Yfull[1,*]

;
;   Set up the linestyle, thickness, and clipping parameters for the oplot commands.
;
if not(keyword_set(Linestyle)) then Linestyle = 0
if not(keyword_set(Thick)) then Thick = 1
NoClip = keyword_set(NoClip)

;
;   Begin the loop that will repeat until a button is clicked (or a change if
; that is what the user wanted).  First read in the cursor's position, then
; overplot two full-screen lines intersecting at that position.
;   Wait for a change (movement or key click).  Delete the old lines, and
; if we don't exit the loop, repeat and draw new lines.
;   If we do exit the loop, we need to quickly read the cursor one more time
; to be sure we are in the coordinate system the user selected.
;
!Err = 0
repeat begin
   cursor, X, Y, /nowait
   oplot, [X,X], Yfull, LINESTYLE=linestyle, THICK=Thick, NOCLIP=NoClip
   oplot, Xfull, [Y,Y], LINESTYLE=linestyle, THICK=Thick, NOCLIP=NoClip

   cursor, XX, YY, /change
   oplot, [X,X], Yfull, LINESTYLE=linestyle, THICK=Thick, NOCLIP=NoClip
   oplot, Xfull, [Y,Y], LINESTYLE=linestyle, THICK=Thick, NOCLIP=NoClip
endrep until (keyword_set(Change) or (!Err ne 0))


cursor, X, Y, /NOWAIT, DATA=Data, DEVICE=Device, NORMAL=Normal

;
;   Go back to the default TV and cursor setups.
;
device, /CURSOR_CROSSHAIR
device, SET_GRAPHICS=OldGraphics

;
return
end   ;   CURFULL   by   Joel Parker   22 Nov 93
