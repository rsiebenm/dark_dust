pro fitswrt,dummy_argument	;FITS_tape_writer
;+
; NAME:
;	FITSWRT
; PURPOSE:
;	Interactive procedure to write internal SDAS file(s) to a FITS tape
;
; CALLING SEQUENCE: 
;	FITSWRT
;
; INTERACTIVE INPUT:
;	User will be prompted for the following
;	(1)  tape unit number 
;	(2)  blocking factor (1 - 10) = # of 2880 byte records per block 
;	(3)  Name of a FITS keyword to put file names into.  This will simplify
;		subsequent reading of the FITS tape, since individual filenames
;		will not have to be specified.  If you don't want to put the 
;		file names into the FITS header, then just hit  [RETURN].
;	(4)  Whether FITS extension files (e.g FITS tables) should be auto-
;		matically searched for.  If so, these should be of the form 
;		name_extnumber
;	(5)  file names - these may either be specified individually, or a
;		tapename may be specified, and all files in the
;		form tapename<number> will be written to tape.
;
; SIDE EFFECTS:
;	Tape is not rewound before files are written.  Tape should be positioned
;	with REWIND or SKIPF before calling FITSWRT.  If you want to append
;	new FITS files to a tape, then call TINIT (tape init) to position tape 
;	between final double EOF.
;
; PROCEDURE CALLS:
;	fitstape, getfiles, gettok, fitswrite
;
; HISTORY:
;	version 3  D. Lindler  Nov 86
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
; get tape unit number
;
unit=0
read,'Enter output tape unit number: ',unit
;
; get tape block size
;
nb=0
read,'Enter blocking factor for tape records (<10): ',nb
;
; initialize tape buffers
;
status=fitstape('init',unit,nb)
;
; get keyword for file names
;
select = ''
keyword=''
print,'Do you want to put filename into FITS header? (Y/N)'
read,select
if strupcase(strmid(select,0,1)) eq 'Y' then $
	read,'Enter header keyword in which to put file name: ',keyword 
;
; get automatic extension search parameters
;
autoext=0
read,'Do you want to automatically search for FITS extension files?' + $
'(Y/N) ',select
if strupcase(strmid(select,0,1)) eq 'Y' then autoext=1
;
; get file names- menu
;
menu:
select = 0
print,'How do you want to select files to be written to tape?'
print,'Choose one of the following: '
print,'(1)  Specify tape name- Files are in form tapename<number>.
print,'(2)  Specify file names individually
read,select
tapename=''
;
case select of
1:  begin
print,'Files are in form tapename<number>.'
read,'Enter tape name. ',tapename
if tapename ne '' then begin
	getfiles,list
	nfiles=n_elements(list)
endif
end
2:  begin
	filebuf=strarr(100)
	if autoext eq 0 then begin
     print,'If you have FITS Table extension files, they may be specified by:'
     print,'		filename/extname1/extname2...'
	end
	print,'Enter file names, one per line.  '
	print,'Enter blank line to quit'
	st=''
	for i=0,99 do begin
		read,st
		if st eq '' then goto,fini
		filebuf[i]=st

	end;for i
;	
fini:
	filebuf=filebuf[0:i-1]
	nfiles=i
end
else:  begin
	print,'ERROR- Invalid choice.'
	goto,menu
       end
endcase
;
;
; loop on files
;
for i=0,nfiles-1 do begin
;
; get filename
;
	if tapename eq '' then begin
		st=strtrim(filebuf[i])
		fname=gettok(st,'/')
	   end else begin
		fname=tapename+strtrim(list[i],2)
	end
;
; write file to tape
;
	fitswrite,unit,fname,keyword
;
; get names of extension files
;
	if autoext then begin
		extnames=findfile(fname+'_*.hhh',count = next)

		if next gt 0 then begin
		  for iext=0,next-1 do begin
			fdecomp,extnames[iext],disk,dir,name,ext,ver
			extnames[iext]=disk+dir+name ;get rid of ext/ver
		  end
		end
	    end else begin
		next=0
		if tapename eq '' then begin
			extnames=strarr(20)
			while st ne '' do begin
				extnames[next]=gettok(st,'/')
				next=next+1
			end
		end
	end
;
; write extension files
;
	if next gt 0 then begin
		for iext=0,next-1 do begin
			fitswrite,unit,strtrim(extnames[iext]),keyword
		end
	end
;
; write eof
;
	status=fitstape('weof',unit)
end
;
; write second eof, and position in front of second eof.
;
status=fitstape('weof',unit)
skipf,unit,-1
return
end
