PRO MPI_Axis_Widget_Events, Event

; The main event handler for the compound widget. It should react only
; to the button events "Accept" or "Dismiss"


   Catch, theError
   IF theError NE 0 THEN BEGIN
      print, '%% Error diagnostics: widget event = '
      help,event,/stru
      ok = Dialog_Message('Error handling widget event!')
      RETURN
   ENDIF

   Widget_Control, event.ID, Get_UValue=theMessage
   IF theMessage.Button EQ 'Close' THEN  $
      Obj_Destroy, theMessage.KillObject  $
   ELSE $
      theMessage.Object->UpdateObject

   Widget_Control, event.top, /Destroy

END ;-----------------------------------------------------------------------------------------------------------------------------


FUNCTION MPI_Axis,  $
            Group_Leader=group_leader,  $
            XAxis=xaxis,  $
            YAxis=yaxis,  $
            Short=short,  $
            Only_Style=only_style,  $
            Only_Tick=only_tick,    $
            _Extra=e


   ON_Error, 2
   IF Keyword_Set(XAxis) EQ 0 AND Keyword_Set(YAxis) EQ 0 THEN BEGIN
      Message, 'You must specify either X axis or y axis!'
   ENDIF

   block = 1  ; always block this widget

   ; Set widget label font name.

   thisOS = StrUpCase(!Version.os_family)
   CASE thisOS OF
      'WINDOWS': BEGIN
         labelfont = 'Times*Bold'
         defaultfont = 'MS Sans Serif*10'
      END
      'MACOS': BEGIN
         labelfont = 'Times*Bold'
         defaultfont = 'Times*10'
      END
      ELSE: BEGIN
;;++mgs
;;   self.labelfont = '-adobe-times-bold-r-normal--10-100-75-75-p-57-iso8859-1'
         labelfont = '-*-times-bold-r-*-*-12-*'
;;   self.defaultfont = '8x13'
         defaultfont = '-*-times-medium-r-*-*-12-*'
;;--mgs
      END
   ENDCASE


   Widget_Control, Default_Font=defaultfont

   IF N_Elements(group_leader) EQ 0 THEN BEGIN
      tlb = Widget_Base(Column=1, Title='Axis Options...', Base_Align_Center=1)
   ENDIF ELSE BEGIN
      tlb = Widget_Base(Column=1, Title='Axis Options...', Base_Align_Center=1, $
                        Group_Leader=group_leader)
   ENDELSE

   mainbase = Widget_Base(tlb, Base_Align_Center=1, Column=1, Frame=1)


   IF Keyword_Set(Xaxis) THEN axis = Obj_New("MPI_Axis", /Xaxis, _Extra=e)
   IF Keyword_Set(Yaxis) THEN axis = Obj_New("MPI_Axis", /YAxis, _Extra=e)

   IF NOT Obj_Valid(axis) THEN BEGIN
      Message, 'Error initialising axis object!'
   ENDIF

   axisID = axis->GUI(tlb, short=short, only_style=only_style, only_tick=only_tick)

   buttonbase = Widget_Base(tlb, Row=1)
   button = Widget_Button(buttonbase, Value='Close', $
                          UValue={Button:'Close', KillObject:axis})
   IF Keyword_Set(block) THEN BEGIN
      button = Widget_Button(buttonbase, Value='Accept', $
                             UValue={Button:'Accept', Object:axis})
   ENDIF ELSE BEGIN
      button = Widget_Button(buttonbase, Value='Apply', $
                             UValue={Button:'Apply', object:axis})
   ENDELSE

   CenterTLB, tlb
   Widget_Control, tlb, /Realize

   XManager, 'mpi_axis', tlb, Event_Handler='MPI_Axis_Widget_Events', $
      No_Block = 1 - Keyword_Set(block)

   ;; If widget was cancelled, it will destroy the axis object. Hence,
   ;; a valid object reference means, the choices were accepted
   IF Obj_Valid(axis) THEN BEGIN
      keywords = axis->GetKeywords()
      Obj_Destroy, axis
   ENDIF ELSE BEGIN
      keywords = { dummy:''}
   ENDELSE

   RETURN, keywords
END
