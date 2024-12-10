;+
; NAME:
;   SELECTIMAGE
;
; PURPOSE:
;
;   The purpose of this program is to allow the user to select
;   an image file for reading. The image data is returned as the
;   result of the function. The best feature of this program is
;   the opportunity to browse the image before reading it.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;   General programming.
;
; CALLING SEQUENCE:
;
;   image = SelectImage()
;
; INPUT PARAMETERS:
;
;   None. All input is via keywords.
;
; INPUT KEYWORDS:
;
;   BMP -- Set this keyword to select BMP files.
;
;   DICOM -- Set this keyword to select DICOM files.
;
;   DIRECTORY -- The initial input directory name. The current directory by default.
;
;   FILENAME -- The initial filename. If the initial directory has image files of the
;               correct type, the default is to display the first of these files. Otherwise, blank.
;
;   FLIPIMAGE -- Set this keyword if you wish to flip the image from its current orientation. Setting
;                this keyword reverses the Y dimension of the image.
;
;   _EXTRA -- This keyword is used to collect and pass keywords on to the FSC_FILESELECT object. See
;             the code for FSC_FILESELECT for details.
;   GIF -- Set this keyword to select GIF files. This capability is not available in IDL 5.4 and higher.
;
;   GROUP_LEADER -- Set this keyword to a widget identifier group leader. This keyword MUST be
;                   set when calling this program from another widget program to guarantee modal operation.
;
;   JPEG -- Set this keyword to select JPEG files.
;
;   ONLY2D -- Set this keyword if you only want the user to be able to select 2D images. Note
;             that the user will be able to browse all images, but the Accept button will only
;             be sensitive for 2D images.
;
;   ONLY3D -- Set this keyword if you only want the user to be able to select 3D or true-color images.
;             Note that the user will be able to browse all images, but the Accept button will only
;             be sensitive for 3D or true-color images.
;
;   PICT -- Set this keyword to select PICT files.
;
;   PGM -- Set this keyword to select PGM files.
;
;   PPM -- Set this keyword to select PPM files.
;
;   PNG -- Set this keyword to select PNG files.
;
;   PREVIEWSIZE -- Set this keyword to the maximum size (in pixels) of the preview window. Default is 150.
;
;   TIFF -- Set this keyword to select TIFF files. (This is the default filter selection.)
;
;   TITLE -- Set this keyword to the text to display as the title of the main image selection window.
;
; OUTPUT KEYWORDS:
;
;   CANCEL -- This keyword is set to 1 if the user exits the program in any way except hitting the ACCEPT button.
;             The ACCEPT button will set this keyword to 0.
;
;   FILEINFO -- This keyword returns information about the selected file. Obtained from the QUERY_**** functions.
;
;   OUTDIRECTORY -- The directory where the selected file is found.
;
;   OUTFILENAME -- The short filename of the selected file.
;
;   PALETTE -- The current color table palette returned as a 256-by-3 byte array.
;
; COMMON BLOCKS:
;
;   None.
;
; RESTRICTIONS:
;
;   Probably doesn't work correctly on VMS systems :-( If you can help, please
;   contact me. I don't have a VMS system to test on.
;
; OTHER COYOTE LIBRARY FILES REQUIRED:
;
;  http://www.dfanning.com/programs/error_message.pro
;  http://www.dfanning.com/programs/fsc_fileselect.pro
;  http://www.dfanning.com/programs/tvimage.pro
;
; EXAMPLE:
;
;   To read JPEG files from the directory:
;
;      IDL> image = SelectImage(/JPEG)
;
; MODIFICATION HISTORY:
;
;   Written by: David W. Fanning, 18 Jan 2001.
;   Added modification to read both 8-bit and 24-bit BMP files. 27 Jan 2001. DWF.
;   Fixed a problem with calculating the new size of the draw widget. 5 April 2002. DWF.
;   Fixed a problem with List Widgets not sizing correctly on UNIX machines. 10 Aug 2002. DWF.
;   Fixed a problem with the initial file not being selected correctly when you changed
;     the file type. 10 Aug 2002. DWF.
;   Added a FLIPIMAGE keyword 10 Aug 2002. DWF.
;   When user chooses to Flip Image, I now reverse the Y dimension of the image,
;     rather than set the !Order system variable. 10 Aug 2002. DWF.
;   Added OUTDIRECTORY and OUTFILENAME keywords. 18 Aug 2002. DWF.
;   Fairly extensive changes in the way this program works and selects images.
;     A new version of FSC_FileSelect is also required. Because of interactions
;     with the operating system with image filters, the program has probably
;     become more Windows-centric. The default is now to display all image
;     files the program is capable of reading. 31 October 2002. DWF.
;   Added ONLY2D keyword to allow the acceptance of 2D images only. 3 Nov 2002. DWF.
;   Added ability to center itself on the display. 8 Nov 2002. DWF.
;   Fixed a problem caused by reading old images with short color table vectors. 26 Nov 2002. DWF.
;   Fixed a problem with specifying a fully-qualified filename. 26 Nov 2002. DWF.
;   Now highlights the selected file in the directory. 26 Nov 2002. DWF.
;   Improved error handling. 9 Dec 2002. DWF.
;   Added PALETTE keyword and improved color operation on 8-bit displays. If the image file
;     contains a color palette, that palette is now loaded when the image is read from the file.
;     The current color palette can be obtained with the PALETTE keyword. 4 April 2003. DWF.
;   Added ONLY3D keyword. 19 April 2003. DWF.
;   Added ability to read PPM and PGM files. 24 November 2003. DWF.
;   Added TITLE keyword. 1 December 2003. DWF.
;
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000-2002 Fanning Software Consulting
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################


PRO SelectImage_CenterTLB, tlb, x, y, NoCenter=nocenter

IF N_Elements(x) EQ 0 THEN xc = 0.5 ELSE xc = Float(x[0])
IF N_Elements(y) EQ 0 THEN yc = 0.5 ELSE yc = 1.0 - Float(y[0])
center = 1 - Keyword_Set(nocenter)

screenSize = Get_Screen_Size()
IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
xCenter = screenSize[0] * xc
yCenter = screenSize[1] * yc

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2 * center
yHalfSize = geom.Scr_YSize / 2 * center

XOffset = 0 > (xCenter - xHalfSize) < (screenSize[0] - geom.Scr_Xsize)
YOffset = 0 > (yCenter - yHalfSize) < (screenSize[1] - geom.Scr_Ysize)
Widget_Control, tlb, XOffset=XOffset, YOffset=YOffset

END


FUNCTION SelectImage_BSort, Array, Asort, INFO=info, REVERSE = rev
;
; NAME:
;       SelectImage_BSort
; PURPOSE:
;       Function to sort data into ascending order, like a simple bubble sort.
; EXPLANATION:
;       Original subscript order is maintained when values are equal (FIFO).
;       (This differs from the IDL SORT routine alone, which may rearrange
;       order for equal values)
;
; CALLING SEQUENCE:
;       result = SelectImage_BSort( array, [ asort, /INFO, /REVERSE ] )
;
; INPUT:
;       Array - array to be sorted
;
; OUTPUT:
;       result - sort subscripts are returned as function value
;
; OPTIONAL OUTPUT:
;       Asort - sorted array
;
; OPTIONAL KEYWORD INPUTS:
;       /REVERSE - if this keyword is set, and non-zero, then data is sorted
;                 in descending order instead of ascending order.
;       /INFO = optional keyword to cause brief message about # equal values.
;
; HISTORY
;       written by F. Varosi Oct.90:
;       uses WHERE to find equal clumps, instead of looping with IF ( EQ ).
;       compatible with string arrays, test for degenerate array
;       20-MAY-1991     JKF/ACC via T AKE- return indexes if the array to
;                       be sorted has all equal values.
;       Aug - 91  Added  REVERSE keyword   W. Landsman
;       Always return type LONG    W. Landsman     August 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;
        N = N_elements( Array )
        if N lt 1 then begin
                print,'Input to SelectImage_BSort must be an array'
                return, [0L]
           endif

        if N lt 2 then begin
            asort = array       ;MDM added 24-Sep-91
            return,[0L]    ;Only 1 element
        end
;
; sort array (in descending order if REVERSE keyword specified )
;
        subs = sort( Array )
        if keyword_set( REV ) then subs = rotate(subs,5)
        Asort = Array[subs]
;
; now sort subscripts into ascending order
; when more than one Asort has same value
;
             weq = where( (shift( Asort, -1 ) eq Asort) , Neq )

        if keyword_set( info ) then $
                message, strtrim( Neq, 2 ) + " equal values Located",/CON,/INF

        if (Neq EQ n) then return,lindgen(n) ;Array is degenerate equal values

        if (Neq GT 0) then begin

                if (Neq GT 1) then begin              ;find clumps of equality

                        wclump = where( (shift( weq, -1 ) - weq) GT 1, Nclump )
                        Nclump = Nclump + 1

                  endif else Nclump = 1

                if (Nclump LE 1) then begin
                        Clump_Beg = 0
                        Clump_End = Neq-1
                  endif else begin
                        Clump_Beg = [0,wclump+1]
                        Clump_End = [wclump,Neq-1]
                   endelse

                weq_Beg = weq[ Clump_Beg ]              ;subscript ranges
                weq_End = weq[ Clump_End ] + 1          ; of Asort equalities.

                if keyword_set( info ) then message, strtrim( Nclump, 2 ) + $
                                " clumps of equal values Located",/CON,/INF

                for ic = 0L, Nclump-1 do begin          ;sort each clump.

                        subic = subs[ weq_Beg[ic] : weq_End[ic] ]
                        subs[ weq_Beg[ic] ] = subic[ sort( subic ) ]
                  endfor

                if N_params() GE 2 then Asort = Array[subs]     ;resort array.
           endif

return, subs
end


FUNCTION SelectImage_FileExtension, filename

; Function finds the file extension of the filename by
; searching for the last ".".

parts = StrSplit(filename, ".", /Extract)
IF N_Elements(parts) EQ 1 THEN extension = "*" ELSE $
   extension = parts[N_Elements(parts)-1]

RETURN, StrUpCase(extension)
END



FUNCTION SelectImage_Dimensions, image, $

; This function returns the dimensions of the image, and also
; extracts relevant information via output keywords. Works only
; with 2D and 3D (24-bit) images.

   XSize=xsize, $          ; Output keyword. The X size of the image.
   YSize=ysize, $          ; Output keyword. The Y size of the image.
   TrueIndex=trueindex, $  ; Output keyword. The position of the "true color" index. -1 for 2D images.
   XIndex=xindex, $        ; Output keyword. The position or index of the X image size.
   YIndex=yindex           ; Output keyword. The position or index of the Y image size.

   ; Get the number of dimensions and the size of those dimensions.

ndims = Size(image, /N_Dimensions)
dims =  Size(image, /Dimensions)

   ; Is this a 2D or 3D image?

IF ndims EQ 2 THEN BEGIN
   xsize = dims[0]
   ysize = dims[1]
   trueindex = -1
   xindex = 0
   yindex = 1
ENDIF ELSE BEGIN
   IF ndims NE 3 THEN Message, /NoName, 'Unknown image dimensions. Returning.'
   true = Where(dims EQ 3, count)
   trueindex = true[0]
   IF count EQ 0 THEN Message, /NoName, 'Unknown image type. Returning.'
   CASE true[0] OF
      0: BEGIN
         xsize = dims[1]
         ysize = dims[2]
         xindex = 1
         yindex = 2
         ENDCASE
      1: BEGIN
         xsize = dims[0]
         ysize = dims[2]
         xindex = 0
         yindex = 2
         ENDCASE
      2: BEGIN
         xsize = dims[0]
         ysize = dims[1]
         xindex = 0
         yindex = 1
         ENDCASE
   ENDCASE
ENDELSE
RETURN, dims
END; ----------------------------------------------------------------------------------------


PRO SelectImage_FlipImage, event

; This event handler reverses the Y dimension of the image and re-displays it.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(/Traceback)
   IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

Widget_Control, event.top, Get_UValue=info, /No_Copy

dims = SelectImage_Dimensions(*(*(info.storagePtr)).image, YIndex=yindex)
*(*(info.storagePtr)).image = Reverse(*(*(info.storagePtr)).image, yindex + 1)
WSet, info.previewWID
TVLCT, info.r, info.g, info.b
IF (Min(*(*(info.storagePtr)).image) LT 0) OR (Max(*(*(info.storagePtr)).image) GT (!D.Table_Size-1)) THEN $
   TVImage, BytScl(*(*(info.storagePtr)).image, Top=!D.Table_Size-1), /Keep_Aspect, /NoInterpolation, /Erase ELSE $
   TVImage, *(*(info.storagePtr)).image, /Keep_Aspect, /NoInterpolation, /Erase

Widget_Control, event.top, Set_UValue=info, /No_Copy
END; ----------------------------------------------------------------------------------------



PRO SelectImage_SetFilter, event

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(/Traceback)
   IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

; This event handler sets the filter for image data files.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; The filter is in the User Value of the button. Store it.

Widget_Control, event.id, Get_UValue=theFilter
*info.filter = theFilter

   ; Get the current filename.

Widget_Control, info.filenameID, Get_Value=filename

   ; Set the new filter in the Filename compound widget.

info.filenameObj->SetProperty, Filter=theFilter

   ; Look in the data directory for the files.

CD, info.dataDirectory, Current=thisDirectory

   ; Locate appropriate files.

FOR j=0, N_Elements(*info.filter)-1 DO BEGIN

   specificFiles = Findfile((*info.filter)[j], Count=fileCount)
   IF fileCount GT 0 THEN IF N_Elements(theFiles) EQ 0 THEN $
      theFiles = specificFiles[SelectImage_BSort(StrLowCase(specificFiles))] ELSE $
      theFiles = [theFiles, specificFiles[SelectImage_BSort(StrLowCase(specificFiles))]]
ENDFOR
fileCount = N_Elements(theFiles)
IF fileCount EQ 0 THEN BEGIN
   theFiles = ""
   filename = ""
ENDIF ELSE BEGIN
   filename = theFiles[0]
ENDELSE

   ; Update the widget interface according to what you found.

Widget_Control, info.filenameID, Set_Value=filename
Widget_Control, info.fileListID, Set_Value=theFiles
IF fileCount GT 0 THEN Widget_Control, info.fileListID, Set_List_Select=0
*info.theFiles = theFiles

   ; Is this a valid image file name. If so, go get the image.

image = BytArr(info.previewsize, info.previewsize)
fileInfo = {channels:0, dimensions:[info.previewsize, info.previewsize]}

IF filename NE "" THEN BEGIN

   thisExtension = SelectImage_FileExtension(filename)

   CASE thisExtension OF

      "BMP": BEGIN
         ok = Query_BMP(filename, fileInfo)
         IF ok THEN IF fileInfo.channels EQ 3 THEN image = Read_BMP(filename, /RGB) ELSE $
                                                   image = Read_BMP(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "DCM": BEGIN
         ok = Query_DICOM(filename, fileInfo)
         IF ok THEN image = Read_Dicom(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "GIF": BEGIN
         ok = Query_GIF(filename, fileInfo)
         IF ok THEN Read_GIF, filename, image, r, g, b
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PICT": BEGIN
         ok = Query_PICT(filename, fileInfo)
         IF ok THEN Read_PICT, filename, image, r, g, b
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PGM": BEGIN
         ok = Query_PPM(filename, fileInfo)
         IF ok THEN Read_PPM, filename, image
         TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PPM": BEGIN
         ok = Query_PPM(filename, fileInfo)
         IF ok THEN Read_PPM, filename, image
         TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PNG": BEGIN
         ok = Query_PNG(filename, fileInfo)
         IF ok THEN image = Read_PNG(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "JPEG": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
         ENDIF
         IF fileInfo.has_palette EQ 1 THEN $
         BEGIN
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
            TVLCT, r, g, b
         ENDIF
         ENDCASE

      "JPG": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
         ENDIF
         IF fileInfo.has_palette EQ 1 THEN $
         BEGIN
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
            TVLCT, r, g, b
         ENDIF
         ENDCASE

      "TIF": BEGIN
         ok = Query_TIFF(filename, fileInfo)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(filename)
               ELSE: image = Read_TIFF(filename, r, g, b)
            ENDCASE
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         ENDCASE

      "TIFF": BEGIN
         ok = Query_TIFF(filename, fileInfo)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(filename)
               ELSE: image = Read_TIFF(filename, r, g, b)
            ENDCASE
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         ENDCASE

      ELSE:

   ENDCASE

ENDIF

   ; Store RGB vectors if they got set.


IF N_Elements(r) NE 0 THEN info.r = r ELSE info.r = Bindgen(!D.Table_Size)
IF N_Elements(g) NE 0 THEN info.g = g ELSE info.g = Bindgen(!D.Table_Size)
IF N_Elements(b) NE 0 THEN info.b = b ELSE info.b = Bindgen(!D.Table_Size)

   ; What kind of image is this?

CASE fileinfo.channels OF
   3: imageType = "True-Color Image"
   0: imageType = "No Image"
   ELSE: imageType = '8-Bit Image'
ENDCASE

   ; Get the file sizes. Dicom images can report incorrect sizes,
   ; which is what we are trying to fix in the ysize line.

xsize = fileInfo.dimensions[0]
ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)

   ; Get the file sizes.

dimensions = SelectImage_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

   ; Flip the image if required.

IF info.flipimage THEN image = Reverse(image, yindex+1)

   ; Calculate a window size for the image preview.

aspect = Float(xsize) / ysize
IF aspect GT 1 THEN BEGIN
   wxsize = Fix(info.previewSize)
   wysize = Fix(info.previewSize / aspect) < info.previewSize
ENDIF ELSE BEGIN
   wysize = Fix(info.previewSize)
   wxsize = Fix(info.previewSize / aspect) < info.previewSize
ENDELSE

   ; If you don't have an image, then get sensible numbers for the labels.

IF imageType EQ 'No Image' THEN BEGIN
   xsize = 0
   ysize = 0
   minval = 0
   maxval = 0
ENDIF

   ; Update the display with what you have.

IF imageType EQ 'No Image' THEN imageDataType = 'NONE' ELSE imageDataType = Size(image, /TNAME)
Widget_Control, info.labelTypeID, Set_Value=imageType
Widget_Control, info.labelXSizeID, Set_Value="X Size: " + StrTrim(xsize, 2)
Widget_Control, info.labelYSizeID, Set_Value="Y Size: " + StrTrim(ysize, 2)
Widget_Control, info.labelDataTypeID, Set_Value="Type: " + imageDataType
Widget_Control, info.labelminvalID, Set_Value="Min Value: " + StrTrim(Fix(Min(image)), 2)
Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + StrTrim(Fix(Max(image)), 2)
;Widget_Control, info.previewID, Draw_XSize=wxsize, Draw_YSize=wysize

   ; Draw the preview image.

WSet, info.previewWID
TVLCT, info.r, info.g, info.b
IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
   TVImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect, /NoInterpolation, /Erase ELSE $
   TVImage, image, /Keep_Aspect, /NoInterpolation, /Erase
IF imageDataType EQ 'NONE' THEN image = 0

   ; Save the image data for later retrieval.

*(*(info.storagePtr)).image = image
*(*(info.storagePtr)).fileInfo = fileInfo
(*info.storagePtr).r = info.r
(*info.storagePtr).g = info.g
(*info.storagePtr).b = info.b

   ; Clean up.

CD, thisDirectory
Widget_Control, event.top, Set_UValue=info, /No_Copy
END; ----------------------------------------------------------------------------------------



PRO SelectImage_FilenameEvents, event

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(/Traceback)
   IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the name of the file.

filename = event.basename
CD, event.directory, Current=thisDirectory

   ; Locate appropriate files.

Ptr_Free, info.theFiles
info.theFiles = Ptr_New(/Allocate_Heap)

FOR j=0, N_Elements(*info.filter)-1 DO BEGIN

   specificFiles = Findfile((*info.filter)[j], Count=fileCount)
   IF fileCount GT 0 THEN IF N_Elements(*(info.theFiles)) EQ 0 THEN $
      *info.theFiles = specificFiles[SelectImage_BSort(specificFiles)] ELSE $
      *info.theFiles = [*info.theFiles, specificFiles[SelectImage_BSort(specificFiles)]]
ENDFOR
fileCount = N_Elements(*info.theFiles)
IF fileCount EQ 0 THEN *info.theFiles = "" ELSE BEGIN
   IF filename EQ "" THEN filename = (*info.theFiles)[0]
ENDELSE
info.dataDirectory = event.directory

   ; Is the filename amoung the list of files? If not,
   ; chose another filename.

index = Where(StrLowCase(*info.theFiles) EQ StrLowCase(filename), count)
IF count EQ 0 THEN BEGIN
   filename = (*info.theFiles)[0]
   Widget_Control, info.filenameID, Set_Value=filename
ENDIF

Widget_Control, info.fileListID, Set_Value=*info.theFiles

   ; Can you find the filename in the list of files? If so,
   ; highlight it in the list.

i = Where(StrUpCase(*info.theFiles) EQ StrUpCase(filename), count)
IF count GT 0 THEN Widget_Control, info.filelistID, Set_List_Select=i

   ; Set the file extension.

thisExtension = SelectImage_FileExtension(filename)

   ; Is this a valid image file name. If so, go get the image.

image = BytArr(info.previewsize, info.previewsize)
fileInfo = {channels:0, dimensions:[info.previewsize, info.previewsize]}

IF filename NE "" THEN BEGIN

   CASE thisExtension OF

      "BMP": BEGIN
         ok = Query_BMP(filename, fileInfo)
         IF ok THEN IF fileInfo.channels EQ 3 THEN image = Read_BMP(filename, /RGB) ELSE $
                                                   image = Read_BMP(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "DCM": BEGIN
         ok = Query_DICOM(filename, fileInfo)
         IF ok THEN image = Read_Dicom(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "GIF": BEGIN
         ok = Query_GIF(filename, fileInfo)
         IF ok THEN Read_GIF, filename, image, r, g, b
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PICT": BEGIN
         ok = Query_PICT(filename, fileInfo)
         IF ok THEN Read_PICT, filename, image, r, g, b
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PGM": BEGIN
         ok = Query_PPM(filename, fileInfo)
         IF ok THEN Read_PPM, filename, image
         TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PPM": BEGIN
         ok = Query_PPM(filename, fileInfo)
         IF ok THEN Read_PPM, filename, image
         TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PNG": BEGIN
         ok = Query_PNG(filename, fileInfo)
         IF ok THEN image = Read_PNG(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "JPEG": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
         ENDIF
         IF fileInfo.has_palette EQ 1 THEN $
         BEGIN
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
            TVLCT, r, g, b
         ENDIF
         ENDCASE

      "JPG": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
         ENDIF
         IF fileInfo.has_palette EQ 1 THEN $
         BEGIN
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
            TVLCT, r, g, b
         ENDIF
         ENDCASE

      "TIF": BEGIN
         ok = Query_TIFF(filename, fileInfo)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(filename)
               ELSE: image = Read_TIFF(filename, r, g, b)
            ENDCASE
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         ENDCASE

      "TIFF": BEGIN
         ok = Query_TIFF(filename, fileInfo)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(filename)
               ELSE: image = Read_TIFF(filename, r, g, b)
            ENDCASE
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         ENDCASE

      ELSE:

   ENDCASE

ENDIF

IF info.only2d THEN IF fileinfo.channels NE 1 THEN $
   Widget_Control, info.acceptID, Sensitive=0 ELSE $
   Widget_Control, info.acceptID, Sensitive=1

IF info.only3d THEN IF fileinfo.channels NE 3 THEN $
   Widget_Control, info.acceptID, Sensitive=0 ELSE $
   Widget_Control, info.acceptID, Sensitive=1

   ; Store RGB vectors if they got set.

IF N_Elements(r) NE 0 THEN info.r = r ELSE info.r = Bindgen(!D.Table_Size)
IF N_Elements(g) NE 0 THEN info.g = g ELSE info.g = Bindgen(!D.Table_Size)
IF N_Elements(b) NE 0 THEN info.b = b ELSE info.b = Bindgen(!D.Table_Size)

   ; What kind of image is this?

CASE fileinfo.channels OF
   3: imageType = "True-Color Image"
   0: imageType = "No Image"
   ELSE: imageType = '8-Bit Image'
ENDCASE

   ; Get the file sizes. Dicom images can report incorrect sizes,
   ; which is what we are trying to fix in the ysize line.

xsize = fileInfo.dimensions[0]
ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)

   ; Get the file sizes.

dimensions = SelectImage_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

   ; Flip the image if required.

IF info.flipimage THEN image = Reverse(image, yindex+1)

   ; Calculate a window size for the image preview.

aspect = Float(xsize) / ysize
IF aspect GT 1 THEN BEGIN
   wxsize = Fix(info.previewSize)
   wysize = Fix(info.previewSize / aspect) < info.previewSize
ENDIF ELSE BEGIN
   wysize = Fix(info.previewSize)
   wxsize = Fix(info.previewSize / aspect) < info.previewSize
ENDELSE

   ; If you don't have an image, then get sensible numbers for the labels.

IF imageType EQ 'No Image' THEN BEGIN
   xsize = 0
   ysize = 0
   minval = 0
   maxval = 0
ENDIF

   ; Update the display with what you have.

IF imageType EQ 'No Image' THEN imageDataType = 'NONE' ELSE imageDataType = Size(image, /TNAME)
Widget_Control, info.labelTypeID, Set_Value=imageType
Widget_Control, info.labelXSizeID, Set_Value="X Size: " + StrTrim(xsize, 2)
Widget_Control, info.labelYSizeID, Set_Value="Y Size: " + StrTrim(ysize, 2)
Widget_Control, info.labelDataTypeID, Set_Value="Type: " + imageDataType
Widget_Control, info.labelminvalID, Set_Value="Min Value: " + StrTrim(Fix(Min(image)), 2)
Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + StrTrim(Fix(Max(image)), 2)

   ; Draw the preview image.

WSet, info.previewWID
TVLCT, info.r, info.g, info.b
IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
   TVImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect, /NoInterpolation, /Erase ELSE $
   TVImage, image, /Keep_Aspect, /NoInterpolation, /Erase
IF imageDataType EQ 'NONE' THEN image = 0

   ; Store the image data for later retrieval.

*(*(info.storagePtr)).image = image
*(*(info.storagePtr)).fileInfo = fileInfo
(*info.storagePtr).r = info.r
(*info.storagePtr).g = info.g
(*info.storagePtr).b = info.b

   ; Clean up.

CD, thisDirectory
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;---------------------------------------------------------------------------------



PRO SelectImage_ListEvents, event

   ; Only handle single click events.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(/Traceback)
   IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

IF event.clicks NE 1 THEN RETURN

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the name of the file.

filename = (*info.theFiles)[event.index]
CD, info.dataDirectory, Current=thisDirectory

   ; Set it in the Filename widget.

Widget_Control, info.filenameID, Set_Value=filename

   ; Is this a valid image file name. If so, go get the image.

image = BytArr(info.previewsize, info.previewsize)
fileInfo = {channels:2, dimensions:[info.previewsize, info.previewsize]}

IF filename NE "" THEN BEGIN
   extension = SelectImage_FileExtension(filename)

   CASE extension OF

      "BMP": BEGIN
         ok = Query_BMP(filename, fileInfo)
         IF ok THEN IF fileInfo.channels EQ 3 THEN image = Read_BMP(filename, /RGB) ELSE $
                                                   image = Read_BMP(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "DCM": BEGIN
         ok = Query_DICOM(filename, fileInfo)
         IF ok THEN image = Read_Dicom(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "GIF": BEGIN
         ok = Query_GIF(filename, fileInfo)
         IF ok THEN Read_GIF, filename, image, r, g, b
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PICT": BEGIN
         ok = Query_PICT(filename, fileInfo)
         IF ok THEN Read_PICT, filename, image, r, g, b
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PGM": BEGIN
         ok = Query_PPM(filename, fileInfo)
         IF ok THEN Read_PPM, filename, image
         TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PPM": BEGIN
         ok = Query_PPM(filename, fileInfo)
         IF ok THEN Read_PPM, filename, image
         TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PNG": BEGIN
         ok = Query_PNG(filename, fileInfo)
         IF ok THEN image = Read_PNG(filename, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "JPEG": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
         ENDIF
         IF fileInfo.has_palette EQ 1 THEN $
         BEGIN
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
            TVLCT, r, g, b
         ENDIF
         ENDCASE

      "JPG": BEGIN
         ok = Query_JPEG(filename, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, filename, image, True=1
            ENDIF ELSE BEGIN
               Read_JPEG, filename, image, colortable, Dither=1, Colors=!D.Table_Size
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
         ENDIF
         IF fileInfo.has_palette EQ 1 THEN $
         BEGIN
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
            TVLCT, r, g, b
         ENDIF
         ENDCASE

      "TIF": BEGIN
         ok = Query_TIFF(filename, fileInfo)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(filename)
               ELSE: image = Read_TIFF(filename, r, g, b)
            ENDCASE
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         ENDCASE

      "TIFF": BEGIN
         ok = Query_TIFF(filename, fileInfo)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(filename)
               ELSE: image = Read_TIFF(filename, r, g, b)
            ENDCASE
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         ENDCASE

     ELSE: BEGIN
         Message, 'File type unrecognized', /Informational
         ENDCASE

   ENDCASE

ENDIF

IF info.only2d THEN IF fileinfo.channels NE 1 THEN $
   Widget_Control, info.acceptID, Sensitive=0 ELSE $
   Widget_Control, info.acceptID, Sensitive=1

IF info.only3d THEN IF fileinfo.channels NE 3 THEN $
   Widget_Control, info.acceptID, Sensitive=0 ELSE $
   Widget_Control, info.acceptID, Sensitive=1

   ; Store RGB vectors if they got set.

IF N_Elements(r) NE 0 THEN info.r = r ELSE info.r = Bindgen(!D.Table_Size)
IF N_Elements(g) NE 0 THEN info.g = g ELSE info.g = Bindgen(!D.Table_Size)
IF N_Elements(b) NE 0 THEN info.b = b ELSE info.b = Bindgen(!D.Table_Size)

   ; What kind of image is this?

CASE fileinfo.channels OF
   3: imageType = "True-Color Image"
   0: imageType = "No Image"
   ELSE: imageType = '2D Image'
ENDCASE

   ; Get the file sizes. Dicom images can report incorrect sizes,
   ; which is what we are trying to fix in the ysize line.

xsize = fileInfo.dimensions[0]
ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)

   ; Get the file sizes.

dimensions = SelectImage_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

   ; Flip the image if required.

IF info.flipimage THEN image = Reverse(image, yindex+1)

   ; Calculate a window size for the image preview.

aspect = Float(xsize) / ysize
IF aspect GT 1 THEN BEGIN
   wxsize = Fix(info.previewSize)
   wysize = Fix(info.previewSize / aspect) < info.previewSize
ENDIF ELSE BEGIN
   wysize = Fix(info.previewSize)
   wxsize = Fix(info.previewSize / aspect) < info.previewSize
ENDELSE

   ; If you don't have an image, then get sensible numbers for the labels.

IF imageType EQ 'No Image' THEN BEGIN
   xsize = 0
   ysize = 0
   minval = 0
   maxval = 0
ENDIF

   ; Update the display with what you have.

IF imageType EQ 'No Image' THEN imageDataType = 'NONE' ELSE imageDataType = Size(image, /TNAME)
Widget_Control, info.labelTypeID, Set_Value=imageType
Widget_Control, info.labelXSizeID, Set_Value="X Size: " + StrTrim(xsize, 2)
Widget_Control, info.labelYSizeID, Set_Value="Y Size: " + StrTrim(ysize, 2)
Widget_Control, info.labelDataTypeID, Set_Value="Type: " + imageDataType
Widget_Control, info.labelminvalID, Set_Value="Min Value: " + StrTrim(Fix(Min(image)), 2)
Widget_Control, info.labelmaxvalID, Set_Value="Max Value: " + StrTrim(Fix(Max(image)), 2)

   ; Draw the preview image.

WSet, info.previewWID
TVLCT, info.r, info.g, info.b
IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
   TVImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect, /NoInterpolation, /Erase ELSE $
   TVImage, image, /Keep_Aspect, /NoInterpolation, /Erase
IF imageDataType EQ 'NONE' THEN image = 0

   ; Store the image data for later retrieval.

*(*(info.storagePtr)).image = image
*(*(info.storagePtr)).fileInfo = fileInfo
(*info.storagePtr).r = info.r
(*info.storagePtr).g = info.g
(*info.storagePtr).b = info.b

   ; Clean up.

CD, thisDirectory
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;---------------------------------------------------------------------------------



PRO SelectImage_Action, event

; This event handler responds to CANCEL and ACCEPT buttons.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(/Traceback)
   IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_Value=buttonValue
IF buttonValue EQ 'Accept' THEN (*info.storagePtr).cancel = 0
info.filenameObj->GetProperty, Directory=outdirectory, Filename=outfilename
(*info.storagePtr).outdirectory = outdirectory
(*info.storagePtr).outfilename = outfilename
Widget_Control, event.top, Set_UValue=info, /No_Copy
Widget_Control, event.top, /Destroy
END ;---------------------------------------------------------------------------------



PRO SelectImage_Cleanup, tlb

; Program pointers are cleaned up here.

Widget_Control, tlb, Get_UValue=info, /No_Copy
IF N_Elements(info) EQ 0 THEN RETURN
Ptr_Free, info.theFiles
Ptr_Free, info.filter
END ;---------------------------------------------------------------------------------



FUNCTION SelectImage, $
   BMP=bmp, $                      ; Set this keyword to select BMP files.
   Cancel=cancel, $                ; An output keyword. Returns 0 if the ACCEPT button is used, 1 otherwise.
   Dicom=dicom, $                  ; Set this keyword to select DICOM files
   Directory=directory, $          ; Initial directory to search for files.
   FileInfo=fileInfo, $            ; An output keyword containing file information from the Query_*** routine.
   Filename=filename, $            ; Initial file name of image file.
   Flipimage=flipimage, $          ; Set this keyword to flip the Y indices of the image. Set to 0 by default.
   _Extra=extra, $                 ; This is used to pass keywords on to FSC_FILESELECT. See that documentation for details.
   GIF=gif, $                      ; Set this keyword to select GIF files
   Group_Leader=group_leader, $    ; The group leader ID of this widget program.
   JPEG=jpeg, $                    ; Set this keyword to select JPEG files
   ONLY2D=only2d, $                ; Set this keyword so that only 2D images can be accepted.
   ONLY3D=only3d, $                ; Set this keyword so that only 3D or true-color images can be accepted.
   OutDirectory=outdirectory, $    ; The directory name of the selected image file.
   OutFilename=outfilename, $      ; The short filename (without directory) of the selected image file.
   Palette=palette, $              ; The color palette associated with the file.
   PICT=pict, $                    ; Set this keyword to select PICT files
   PGM=pgm, $                      ; Set this keyword to read PGM files.
   PPM=ppm, $                      ; Set this keyword to read PPM files.
   PNG=png, $                      ; Set this keyword to select PNG files.
   TIFF=tiff, $                    ; Set this keyword to select TIFF files.
   TITLE=title, $                  ; The title of the main image selection window.
   PreviewSize=previewsize         ; The maximum size of the image preview window. 150 pixels by default.


   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   Cancel = 1
   ok = Error_Message(/Traceback)
   RETURN, 0
ENDIF

   ; Check for availability of GIF files.

thisVersion = Float(!Version.Release)
IF thisVersion LT 5.3 THEN haveGif = 1 ELSE haveGIF = 0

   ; Set up the filter.

IF Keyword_Set(bmp) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.bmp"] ELSE filter = [filter, "*.bmp"]
IF Keyword_Set(dicom) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.dcm"] ELSE filter = [filter, "*.dcm"]
flipimage = Keyword_Set(flipimage)
IF Keyword_Set(gif) THEN BEGIN
   IF havegif THEN filter = "*.gif" ELSE $
      ok = Dialog_Message('GIF files not supported in this IDL version. Replacing with TIFF.')
ENDIF
IF Keyword_Set(pict) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.pict"] ELSE filter = [filter, "*.pict"]
IF Keyword_Set(tiff) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.tif"] ELSE filter = [filter, "*.tif"]
IF Keyword_Set(jpeg) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.jpg"] ELSE filter = [filter, "*.jpg"]
IF Keyword_Set(png) THEN IF N_Elements(filter) EQ 0 THEN filter = ["*.png"] ELSE filter = [filter, "*.png"]
IF N_Elements(filter) EQ 0 THEN filter = ['*.bmp', '*.dcm', '*.jpg', '*.pict', '*.ppm', '*.pgm', '*.png', '*.tif']
only2D = Keyword_Set(only2d)
only3D = Keyword_Set(only3d)
IF N_Elements(title) EQ 0 THEN title = 'Select Image File'

   ; Get the current directory. Some processing involved.

CD, Current=startDirectory
IF N_Elements(directory) EQ 0 THEN directory = startDirectory ELSE BEGIN
   IF StrMid(directory, 0, 2) EQ ".." THEN BEGIN
      CASE StrUpCase(!Version.OS_Family) OF
      'MACOS': BEGIN
         CD, '..'
         CD, Current=basename
         directory = basename + StrMid(directory, 3)
         END
      'VMS': BEGIN
         CD, '..'
         CD, Current=basename
         directory = basename + StrMid(directory, 3)
         END
      ELSE: BEGIN
         CD, '..'
         CD, Current=basename
         directory = basename + StrMid(directory, 2)
         END
      ENDCASE
   ENDIF
   IF StrMid(directory, 0, 1) EQ "." THEN BEGIN
      CASE StrUpCase(!Version.OS_Family) OF
      'MACOS': BEGIN
         CD, Current=basename
         directory = basename + StrMid(directory, 2)
         END
      'VMS': BEGIN
         CD, Current=basename
         directory = basename + StrMid(directory, 2)
         END
      ELSE: BEGIN
         CD, Current=basename
         directory = basename + StrMid(directory, 1)
      END
      ENDCASE
   ENDIF
ENDELSE
CD, directory

   ; Check other keyword values.

IF N_Elements(filename) EQ 0 THEN file = "" ELSE BEGIN
   dir=StrMid(filename, 0, StrPos(filename, Path_Sep(), /REVERSE_SEARCH))
   IF dir NE "" THEN BEGIN
      directory = dir
      CD, directory
      file = StrMid(filename, StrLen(directory)+1)
   ENDIF ELSE file = filename
ENDELSE
IF N_Elements(previewSize) EQ 0 THEN previewSize = 150

   ; Locate appropriate files.

FOR j=0, N_Elements(filter)-1 DO BEGIN

   specificFiles = Findfile(filter[j], Count=fileCount)
   IF fileCount GT 0 THEN IF N_Elements(theFiles) EQ 0 THEN $
      theFiles = specificFiles[SelectImage_BSort(StrLowCase(specificFiles))] ELSE $
      theFiles = [theFiles, specificFiles[SelectImage_BSort(StrLowCase(specificFiles))]]
ENDFOR
fileCount = N_Elements(theFiles)
IF fileCount EQ 0 THEN theFiles = "" ELSE BEGIN
   IF file EQ "" THEN file = theFiles[0]
ENDELSE

   ; Is this a valid image file name. If so, go get the image.

image = BytArr(previewsize, previewsize)
fileInfo = {channels:2, dimensions:[previewsize, previewsize], has_palette:0}

IF file NE "" THEN BEGIN
   extension = SelectImage_FileExtension(file)

   CASE extension OF

      "BMP": BEGIN
         ok = Query_BMP(file, fileInfo)
         IF ok THEN IF fileInfo.channels EQ 3 THEN image = Read_BMP(file, /RGB) ELSE $
                                                   image = Read_BMP(file, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "DCM": BEGIN
         ok = Query_DICOM(file, fileInfo)
         IF ok THEN image = Read_Dicom(file, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "GIF": BEGIN
         ok = Query_GIF(file, fileInfo)
         IF ok THEN Read_GIF, file, image, r, g, b
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PICT": BEGIN
         ok = Query_PICT(file, fileInfo)
         IF ok THEN Read_PICT, file, image, r, g, b
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PGM": BEGIN
         ok = Query_PPM(file, fileInfo)
         IF ok THEN Read_PPM, file, image
         TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PPM": BEGIN
         ok = Query_PPM(file, fileInfo)
         IF ok THEN Read_PPM, file, image
         TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "PNG": BEGIN
         ok = Query_PNG(file, fileInfo)
         IF ok THEN image = Read_PNG(file, r, g, b)
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDCASE

      "JPEG": BEGIN
         ok = Query_JPEG(file, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, file, image, True=1
            ENDIF ELSE BEGIN
               Read_JPEG, file, image, colortable, Dither=1, Colors=!D.Table_Size
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
         ENDIF
         IF fileInfo.has_palette EQ 1 THEN $
         BEGIN
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
            TVLCT, r, g, b
         ENDIF
         ENDCASE

      "JPG": BEGIN
         ok = Query_JPEG(file, fileInfo)
         IF ok THEN $
         BEGIN
            Device, Get_Visual_Depth = theDepth
            IF theDepth GT 8 THEN BEGIN
               Read_JPEG, file, image, True=1
            ENDIF ELSE BEGIN
               Read_JPEG, file, image, colortable, Dither=1, Colors=!D.Table_Size
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
               TVLCT, r, g, b
            ENDELSE
         ENDIF
         IF fileInfo.has_palette EQ 1 THEN $
         BEGIN
               r = colortable[*,0]
               g = colortable[*,1]
               b = colortable[*,2]
            TVLCT, r, g, b
         ENDIF
         ENDCASE

      "TIF": BEGIN
         ok = Query_TIFF(file, fileInfo)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(file)
               ELSE: image = Read_TIFF(file, r, g, b)
            ENDCASE
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         ENDCASE

      "TIFF": BEGIN
         ok = Query_TIFF(file, fileInfo)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: image = Read_TIFF(file)
               ELSE: image = Read_TIFF(file, r, g, b)
            ENDCASE
         IF fileInfo.has_palette EQ 1 THEN TVLCT, r, g, b ELSE TVLCT, Bindgen(256), Bindgen(256), Bindgen(256)
         ENDIF
         ENDCASE

      ELSE: BEGIN
         Message, 'File type unrecognized', /Informational
         ENDCASE

   ENDCASE

ENDIF

   ; Get the file sizes.

dimensions = SelectImage_Dimensions(image, XSize=xsize, YSize=ysize, YIndex=yindex)

   ; Flip the image if required.

IF flipimage THEN image = Reverse(image, yindex+1)

   ; Create the widgets.

IF N_Elements(group_leader) NE 0 THEN BEGIN
   tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center, $
      /Modal, Group_Leader=group_leader)
ENDIF ELSE BEGIN
   tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center)
ENDELSE

fileSelectBase = Widget_Base(tlb, Column=1, Frame=1)
buttonBase = Widget_Base(tlb, Row=1)

   ; Define file selection widgets.

filenameID = FSC_FileSelect(fileSelectBase, Filename=file, ObjectRef=filenameObj,$
   Directory=directory, Event_Pro='SelectImage_FilenameEvents', Filter=filter, _Extra=extra)
fsrowbaseID = Widget_Base(fileSelectBase, Row=1, XPad=10)
xsize = Max(StrLen(theFiles)) + 0.1*Max(StrLen(theFiles)) > 20
filelistID = Widget_List(fsrowbaseID, Value=theFiles, YSize = 10, XSize=xsize, $
   Event_Pro='SelectImage_ListEvents')
spacer = Widget_Label(fsrowbaseID, Value="     ")
previewID = Widget_Draw(fsrowbaseID, XSize=previewSize, YSize=previewSize)
spacer = Widget_Label(fsrowbaseID, Value="     ")
labelBaseID = Widget_Base(fsrowbaseID, Column=1, /Base_Align_Left)
IF fileInfo.channels EQ 3 THEN imageType = "True-Color Image" ELSE imageType = '2D Image'
xsize = fileInfo.dimensions[0]
ysize = fileInfo.dimensions[1] > Fix(xsize * 0.5)
imageDataType = Size(image, /TNAME)
labeltypeID = Widget_Label(labelBaseID, Value=imageType, /Dynamic_Resize)
labelxsizeID = Widget_Label(labelBaseID, Value="X Size: " + StrTrim(xsize, 2), /Dynamic_Resize)
labelysizeID = Widget_Label(labelBaseID, Value="Y Size: " + StrTrim(ysize, 2), /Dynamic_Resize)
labeldataTypeID = Widget_Label(labelBaseID, Value="Type: " + imageDataType, /Dynamic_Resize)
labelminvalID = Widget_Label(labelBaseID, Value="Min Value: " + StrTrim(Fix(Min(image)),2), /Dynamic_Resize)
labelmaxvalID = Widget_Label(labelBaseID, Value="Max Value: " + StrTrim(Fix(Max(image)),2), /Dynamic_Resize)

   ; Size the draw widget appropriately.
   ; Calculate a window size for the image preview.

IF xsize NE ysize THEN BEGIN
   aspect = Float(ysize) / xsize
   IF aspect LT 1 THEN BEGIN
      wxsize = previewSize
      wysize = (previewSize * aspect) < previewSize
   ENDIF ELSE BEGIN
      wysize = previewSize
      wxsize = (previewSize / aspect) < previewSize
   ENDELSE
ENDIF

   ; Can you find the filename in the list of files? If so,
   ; highlight it in the list.

index = Where(StrUpCase(theFiles) EQ StrUpCase(file), count)
IF count GT 0 THEN Widget_Control, filelistID, Set_List_Select=index

   ; Define buttons widgets.

button = Widget_Button(buttonBase, Value='Cancel', Event_Pro='SelectImage_Action')
filterID = Widget_Button(buttonBase, Value='Image Type', /Menu, Event_Pro='SelectImage_SetFilter')
button = Widget_Button(filterID, Value='BMP Files', UValue=['*.bmp'])
button = Widget_Button(filterID, Value='DICOM Files', UValue=['*.dcm'])
IF havegif THEN button = Widget_Button(filterID, Value='GIF Files', UValue=['*.gif'])
button = Widget_Button(filterID, Value='PICT Files', UValue=['*.pict'])
button = Widget_Button(filterID, Value='PPM Files', UValue=['*.pgm', '*.ppm'])
button = Widget_Button(filterID, Value='PGM Files', UValue=['*.pgm', '*.ppm'])
button = Widget_Button(filterID, Value='PNG Files', UValue=['*.png'])
button = Widget_Button(filterID, Value='JPEG Files', UValue=['*.jpg'])
button = Widget_Button(filterID, Value='TIFF Files', UValue=['*.tif'])
button = Widget_Button(filterID, Value='All Types', $
   UValue=['*.bmp', '*.dcm', '*.jpg', '*.pict', '*.ppm', '*.pgm', '*.png', '*.tif'])
button = Widget_Button(buttonBase, Value='Flip Image', Event_Pro='SelectImage_FlipImage')
acceptID = Widget_Button(buttonBase, Value='Accept', Event_Pro='SelectImage_Action')
IF only2d THEN BEGIN
   IF fileinfo.channels NE 1 THEN Widget_Control, acceptID, Sensitive=0
   Widget_Control, tlb, TLB_Set_Title=title + ' (2D Images Only)'
ENDIF

IF only3d THEN BEGIN
   IF fileinfo.channels NE 3 THEN Widget_Control, acceptID, Sensitive=0
   Widget_Control, tlb, TLB_Set_Title=title + ' (True-Color Images Only)'
ENDIF

SelectImage_CenterTLB, tlb
Widget_Control, tlb, /Realize
Widget_Control, previewID, Get_Value=previewWID

   ; Set up RGB color vectors.

IF N_Elements(r) EQ 0 THEN r = Bindgen(!D.Table_Size)
IF N_Elements(g) EQ 0 THEN g = Bindgen(!D.Table_Size)
IF N_Elements(b) EQ 0 THEN b = Bindgen(!D.Table_Size)
WSet, previewWID
TVLCT, r, g, b

   ; In some old bitmap files, the RGB vectors can be
   ; less than 256 in length. That will cause problems,
   ; as I have learned today. :-(

IF N_Elements(r) LT 256 THEN BEGIN
   rr = BIndgen(256)
   gg = rr
   bb = rr
   rr[0] = r
   gg[0] = g
   bb[0] = b
   r = rr
   g = gg
   b = bb
ENDIF

   ; Display the image.

IF (Min(image) LT 0) OR (Max(image) GT (!D.Table_Size-1)) THEN $
   TVImage, BytScl(image, Top=!D.Table_Size-1), /Keep_Aspect, /NoInterpolation ELSE $
   TVImage, image, /Keep_Aspect, /NoInterpolation

   ; Set up information to run the program.

storagePtr = Ptr_New({cancel:1, image:Ptr_New(image), fileInfo:Ptr_New(fileInfo), $
   outdirectory:"", outfilename:"", r:r, g:g, b:b})

info = { storagePtr: storagePtr, $           ; The "outside the program" storage pointer.
         previewID: previewID, $             ; The ID of the preview draw widget.
         previewWID: previewWID, $           ; The window index number of the preview draw widget.
         r:r, $                              ; The R color vector.
         g:g, $                              ; The G color vector.
         b:b, $                              ; The B color vector.
         theFiles: Ptr_New(theFiles), $      ; The current list of files in the directory.
         filenameID: filenameID, $           ; The identifier of the FileSelect compound widget.
         fileListID: fileListID, $           ; The identifier of the file list widget.
         flipimage:flipimage, $              ; A flag to flip the image Y order.
         previewSize: previewSize, $         ; The default size of the preview window.
         acceptID: acceptID, $               ; The idenfier of the Accept button widget.
         only2d: only2d, $                   ; A flag that permits only the acceptance of 2D images.
         only3d: only3d, $                   ; A flag that permits only the acceptance of true-color images.
         filter: Ptr_New(filter), $          ; The file filter.
         filenameObj: filenameObj, $         ; The FileSelect compound widget object reference.
         dataDirectory: directory, $         ; The current data directory.
         labelmaxvalID: labelmaxvalID, $     ; The ID of the Max Value label.
         labelminvalID: labelminvalID, $     ; The ID of the Max Value label.
         labelTypeID: labelTypeID, $         ; The ID of the Image Type label.
         labelXSizeID: labelXSizeID, $       ; The ID of the X Sizee label.
         labelYSizeID: labelYSizeID, $       ; The ID of the Y Size label.
         labelDataTypeID: labelDataTypeID $  ; The ID of the Data Type label.
       }
Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Blocking or modal widget mode, depending upon presence of GROUP_LEADER.

XManager, "selectimage", tlb, Cleanup='SelectImage_Cleanup'

   ; Return collected information.

cancel = (*storagePtr).cancel
fileInfo = *(*storagePtr).fileInfo
image = *((*storagePtr).image)
outDirectory = (*storagePtr).outDirectory
outFilename = (*storagePtr).outFilename
Ptr_Free, (*storagePtr).image
Ptr_Free, (*storagePtr).fileInfo
IF Arg_Present(palette) THEN $
BEGIN
   palette = BytArr(256,3)
   palette[*,0] = (*storagePtr).r
   palette[*,1] = (*storagePtr).g
   palette[*,2] = (*storagePtr).b
ENDIF
Ptr_Free, storagePtr

   ; Restore start directory.

CD, startDirectory
IF cancel EQ 1 THEN RETURN, 0 ELSE RETURN, image
END