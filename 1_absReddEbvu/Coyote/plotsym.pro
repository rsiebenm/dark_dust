function plotsym, circle=circle, triangle=triangle, diamond=diamond, $
                  box=box, line=line, star = star, superstar = superstar, $
                  triangle_down = triangle_down , plus  = plus, cross = cross, $
                  left_arrow = left_arrow, right_arrow=right_arrow, $
                  upper_limit  = upper_limit, lower_limit  = lower_limit, $
                  scale=scale,angle=angle,  _extra=extra
;+
; NAME:
;  plotsym
; PURPOSE:
;  function to make plotting symbols much easier.
; Usage:
;   plot,x,y,psym=plotsym(/circle,scale=2,/fill)
;
; CATEGORY:
;   Graphics.
;
; Keywords:  --  Symbols --
;  circle        = 
;  diamond       = 
;  box           = 
;  triangle      = 
;  line          = line symbol
;  triangle_down = 
;  plus          = 
;  cross         = 
;  star          = 
;  superstar     =
;  lower_limit   = 
;  upper_limit   = 
;   scale         = scales the symbol
;  angle         = angle the symbol should be rotated
;  _extra        = extra keywords for usersym.  These are thick, color and fill
;
; Written by:
; Ronn Kling
; Ronn Kling Consulting
; 7038 Westmoreland Dr.
; Warrenton, VA 20187
; klingrl@juno.com
; copyright 1999, all rights reserved
;-

  if not (keyword_set(circle)   or keyword_set(triangle) or keyword_set(diamond)     or keyword_set(box) $
          or  keyword_set(line) or keyword_set(star)     or keyword_set(superstar)   or keyword_set(triangle_down) $
          or  keyword_set(plus) or keyword_set(cross)    or keyword_set(upper_limit) or keyword_set(lower_limit)   $
          or  keyword_set(left_arrow)   or  keyword_set(right_arrow)) then begin
      print, ' *** Error: symbol unknown. 
      print, ' Set of symbols are:'
      print, '   circle, triangle, diamond,  box, line, star, superstar , triangle_down '
      print, '   plus, cross,  upper_limit, lower_limit'
      return, 8
 endif


if not keyword_set(scale) then scale=1.0
if not keyword_set(angle) then angle=0.0

if keyword_set(circle) then begin
  theta = findgen(30)/29.*360.
endif else if keyword_set(triangle) then begin
  theta = [-30.,90.,210., -30.]
endif else if keyword_set(diamond) then begin
  theta = [0.,90.,180.,270.,0.]
endif else if keyword_set(box) then begin
  theta = [315.,45.,135.,225.,315.]
endif else if keyword_set(line) then begin
  theta = [-180.,0.]
endif

if keyword_set(circle)  or keyword_set(triangle) or $
   keyword_set(diamond) or keyword_set(box) or  keyword_set(line) then begin
   theta = theta + angle
   x = cos(theta * !dtor)
   y = sin(theta * !dtor)
endif

;;;;;;;;;;;;;;
; More symbols
;;;;;;;;;;;;;;;

s05 = 1.0 / sqrt(2.0)


if keyword_set(star) then begin
 x = [0.0, -s05, s05, 0.0,  s05, -s05, 0.0, -1.0, 1.0, 0.0,  0.0, 0.0, 0.0] 
 y = [0.0, -s05, s05, 0.0, -s05,  s05, 0.0,  0.0, 0.0, 0.0, -1.0, 1.0, 0.0]
endif



if keyword_set(superstar) then begin
 star_x = [0.0, 0.294/1.5, 0.951,  0.476/1.5,  0.588,  0.0/1.5, -0.588, -0.476/1.5, -0.951, -0.294/1.5, 0.0, 0.294/1.5]
 star_y = [1.0, 0.405/1.5, 0.309, -0.155/1.5, -0.809, -0.5/1.5, -0.809, -0.155/1.5,  0.309,  0.405/1.5, 1.0, 0.405/1.5]
 x = star_x * 1.5
 y = star_y * 1.5
endif


if keyword_set(cross) then begin
  x = [0.0, -s05, s05, 0.0,  s05, -s05, 0.0] 
  y = [0.0, -s05, s05, 0.0, -s05,  s05, 0.0]
endif


if keyword_set(plus) then begin
  x = [0.0, -1.0, 1.0, 0.0,  0.0, 0.0, 0.0]
  y = [0.0,  0.0, 0.0, 0.0, -1.0, 1.0, 0.0] 
endif


if keyword_set(triangle_down) then begin
  tri_center = 1.0/sqrt(2.0)
  x = [0.0, 1.0, -1.0, 0.0, 1.0]
  y = [-sqrt(2)+tri_center, tri_center, tri_center, -sqrt(2.0)+tri_center, tri_center]
endif


if keyword_set(left_arrow) then begin
 x = [ 0.0, 0.0, 0.0, -2.5, -1.75, -2.5, -1.75]
 y = [-0.5, 0.5, 0.0,  0.0, -0.75,  0.0,  0.75]
endif

if keyword_set(right_arrow) then begin
 x = [ 0.0, 0.0, 0.0, 2.5, 1.75, 2.5, 1.75]
 y = [0.5, -0.5, 0.0,  0.0, 0.75,  0.0,  -0.75]
endif

if keyword_set(upper_limit) then begin
 x = [-0.5, 0.5, 0.0,  0.0, -0.75,  0.0,  0.75]
 y = [ 0.0, 0.0, 0.0, -2.5, -1.75, -2.5, -1.75]
endif


if keyword_set(lower_limit) then begin
 x = [0.5, -0.5, 0.0,  0.0, 0.75,  0.0,  -0.75]
 y = [ 0.0, 0.0, 0.0, 2.5, 1.75, 2.5, 1.75]
endif

;;;;;;;;;;;;;;


x = x * scale
y = y * scale

usersym, x, y, _extra=extra
return,8
end

