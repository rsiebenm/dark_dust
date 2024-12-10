pro fitswrite,unit,file,keyword
;+
; NAME:
;	FITSWRITE
; PURPOSE:
;	Procedure will write an internal SDAS file to a FITS tape on the 
;	specified tape unit.    Called by FITSWRT
;
; CALLING SEQUENCE:
;	FITSWRITE, UNIT, FILE, [ KEYWORD ]
;
; INPUTS:
;	file - internal FITS file name without extension (extension
;		is assumed to be .HHH and .HHD)
;	unit - IDL tape unit number
;
; OPTIONAL INPUT:
;	keyword - keyword to place file name into.  if not supplied
;		or equals '' then the file name is not put into the
;		header before writing it to tape.
;
; SIDE EFFECT:
;	A FITS file is written to tape.
;
; PROCEDURE CALLS:
;	remchar, sxhread, sxpar, fdecomp, sxaddpar, fitstape
;
; HISTORY:
;	version 3  D. Lindler   Nov. 1986                
;	version 4  W. Landsman  Oct. 1988 (Changed st*.pro calls to sx*.pro)
;	converted to IDL Version 2.  M. Greason, STX, June 1990.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
On_error,2              ;Return to caller

remchar,file,' '
print,file              ;Name of file being processed
sxhread,file,h		;Get FITS header
;                    
; add file name to supplied keyword
;
if N_params() LT 3 then keyword=''
if keyword NE '' then begin
	fdecomp,file,disk,dir,name,exten,vers
	sxaddpar,h,keyword,name
end
;
; extract required keywords from header
;
bitpix=sxpar(h,'BITPIX')
naxis=sxpar(h,'NAXIS')
psize=sxpar(h,'PSIZE')
if !ERR LT 0 then psize=sxpar(h,'pcount')*bitpix
gcount=sxpar(h,'gcount')
if !ERR LT 0 then gcount=1
number=1L			;number of elements in data
if naxis EQ 0 then number=0 $
	else begin 
        openr,1,file+'.hhd',/block     ;Open data file
        for i=1,naxis do number=number*sxpar(h,'naxis'+strtrim(i,2))
endelse
;
; compute data size
;
ndata=number*bitpix/8		;number of data bytes
npar=psize/8			;number of param. bytes
nbytes=npar+ndata			;bytes per group
;
; write FITS header to tape
;
nlines=1		;count of lines in header
while strmid(h[nlines-1],0,8) ne 'END     ' do nlines=nlines+1
nrecs=(nlines+35)/36	;number of 2880 byte records required
nwrite = 0
for i=0,nrecs-1 do begin
	hbuf=bytarr(2880)+32B	;blank header
	for j=0,35 do begin
		line=i*36+j
 		if line lt nlines then hbuf[j*80] = byte(h[line])
	end
	status=fitstape('write',unit,8,hbuf)
        nwrite = nwrite+1
	if status lt 0 then retall
end
;
; set up data buffers
;
if (nbytes EQ 0) or (gcount EQ 0) then goto,done	;any data?
outbuf=bytarr(2880)
data=bytarr(nbytes)
outpos=0			;position in output tape buffer
;
; loop on groups
;
for ig=0,gcount-1 do begin
;
; read data and swap para. block and data
;
	nleft=nbytes		;number of bytes left to ext.
	sbyte=nbytes*ig		;number of bytes to skip in file
	dpos=0			;position in data array
	srec=sbyte/512		;starting record in file
	sbyte=sbyte-srec*512	;starting byte in the record
	while nleft gt 0 do begin
		nget=nleft<(512-sbyte) 	;num. of bytes to get in record
		last=sbyte+nget-1	;last byte to get from record
		inrec=assoc(1,bytarr(nget,/nozero),srec*512+sbyte)
                data[dpos] = inrec[0]
		dpos=dpos+nget		;increment pointer in data
		srec=srec+1		;move to next block
		sbyte=0			;start with first byte of
					; next record
		nleft=nleft-nget
	end;while
	if (npar GT 0) and (ndata GT 0) then $
		data=[data[ndata:nbytes-1],data[0:ndata-1]]
;
; write data to output buffer
;
	nleft=nbytes		;number of bytes left to write
	inpos=0			;position in input array
	while nleft GT 0 do begin
		num=nleft<(2880-outpos)	;number that con be written
                outbuf[outpos] = data[inpos:inpos+num-1]
		outpos=outpos+num
		nleft=nleft-num
		inpos=inpos+num
		if outpos eq 2880 then begin	;output buffer full?
			status=fitstape('write',unit,bitpix,outbuf)
                        nwrite = nwrite+1 
			if status LT 0 then retall
                        outpos = 0
		end
	end; while
end; for ig
;
; empty last buffer
;
	if outpos GT 0 then begin
		status=fitstape('write',unit,bitpix,outbuf)
               nwrite = nwrite+1 
		if status LT 0 then retall
	endif
;
; done
;
done:
close,1
return
end
