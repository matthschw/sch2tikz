/*=============================================================================*
*
*   sch2tikz
*
*   Purpose:    schematic to tikzpicture converter
*
*   Author:     Matthias Schweikardt
*   Email:      matthias.schweikardt@reutlingen-university.de
*
*   Lint: IQ score is 100 (best is 100) 
*         {sklint ?file "PATH")}
*
*   Revision:   2021-08-25 Created
*
*   Copyright 2022 Reutlingen University, Electronics & Drives
*
*   Permission is hereby granted, free of charge, to any person obtaining a 
*   copy of this software and associated documentation files (the "Software"), 
*   to deal in the Software without restriction, including without limitation 
*   the rights to use, copy, modify, merge, publish, distribute, sublicense, 
*   and/or sell copies of the Software, and to permit persons to whom the 
*   Software is furnished to do so, subject to the following conditions:
*
*   The above copyright notice and this permission notice shall be included in 
*   all copies or substantial portions of the Software.
*
*   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
*   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
*   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
*   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
*   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
*   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
*   DEALINGS IN THE SOFTWARE.
*
*=============================================================================*/

;FUNCTION
; EDsch2tikz
;
;Description:
; This function exports a schematic in Cadence Virtuoso
; to a tikzpicture
;
;Parameters:
; cv           cellView, schematic cell view to be exported
; mag          number, magnification
; file         string, path to resulting tikzpicture
;
;Return:
;  path        string, path to resulting tikzpicture
;
(defun EDsch2tikz (cv mag file "gfS")
  (let (port)

    (if
      (and
        (dbIsId cv)
        (equal cv~>objType      "cellView" )
        (equal cv~>cellViewType "schematic")
      );and
    then

      (setq port (outfile file))

      (fprintf port "\\begin{tikzpicture}\n")

      (EDsch2tikzExportShapes
        port
        cv~>shapes
        (list (list 0:0 "R0" mag))
      );EDsch2tikzExportShapes

      (foreach inst cv~>instances
        (EDsch2tikzExportShapes
          port
          inst~>master~>shapes
          (list inst~>transform (list 0:0 "R0" mag))
          ?inst inst
        );EDsch2tikzExportShapes
      );foreach

      (fprintf port "\\end{tikzpicture}")
      (close port)

      (simplifyFilename file)
    else
      (error "[ED-SCH2TikZ] Provided Cellview is not a schematic")
      nil
    );if
  );let
);defun


;FUNCTION
; EDsch2tikzExportShapes
;
;Description:
; This functions converts all elements of a list of shapes and
; exports them to a port.
;
;Parameters:
; port             port, where resulting TikZ code is exported
; shapes           list, list of shapes to be exported
; transformations  number, list of transformations
; ?inst            instance, instance that contains the shapes
;
;Return:
;  t               bool, always true
;
(defun EDsch2tikzExportShapes (port shapes transformations @key (inst nil))

  inst

  (foreach shape shapes

    (case shape~>objType
      ("line"
        (EDsch2tikzExportLine
          port
          shape
          transformations
        );EDsch2tikzExportLine
      )
      ("ellipse"
        (EDsch2tikzExportEllipse
          port
          shape
          transformations
        );EDsch2tikzExportLine
      )  
      ("rect"
        (EDsch2tikzExportRect
          port
          shape
          transformations
        );EDsch2tikzExportLine
      ) 
      ("polygon"
        (EDsch2tikzExportPolygon
          port
          shape
          transformations
        );EDsch2tikzExportLine
      )     
      ("arc"
        (EDsch2tikzExportArc
          port
          shape
          transformations
        );EDsch2tikzExportLine
      )
      ("label"
        ;(EDsch2tikzExportLabel
        ;  port
        ;  shape
        ;  transformations
        ;  ?inst inst
        ;);EDsch2tikzExportLine
      )
      (t
        nil
      )
    );case
  );foreach

  t
);defun EDsch2tikzExportShapes


;FUNCTION
; EDsch2tikzExportLine
;
;Description:
; This functions converts a line in a schematic to a TikZ representation
; and writes the code to a port.
;
;Parameters:
; port             port, where resulting TikZ code is exported
; line             line, line to be exported
; transformations  list, list of transformations
;
;Return:
;  t               bool, always true
;
(defun EDsch2tikzExportLine (port line transformations)
  (let ((ignore nil))

    (unless (member line~>lpp ignore)
      (fprintf
        port
        "\\draw[line cap=rect] %s;\n" 
        (buildString
          (foreach mapcar point (EDsch2tikzTransformPoints line~>points transformations)
            (lsprintf "(%L,%L)" (xCoord point) (yCoord point))
          );foreach
          " -- "
        );buildString
      );fprintf
    );unless

    t
  );let
);defun EDsch2tikzExportLine


;FUNCTION
; EDsch2tikzExportEllipse
;
;Description:
; This functions converts an ellipse in a schematic to a TikZ representation
; and writes the code to a port.
;
;Parameters:
; port             port, where resulting TikZ code is exported
; ellipse          ellipse, ellipse to be exported
; transformations  list, list of transformations
;
;Return:
;  t               bool, always true
;
(defun EDsch2tikzExportEllipse (port ellipse transformations)
  (let (bBox center (ignore nil) (fill '(("wire" "drawing"))))

    (unless (member ellipse~>lpp ignore)
      (setq bBox (EDsch2tikzTransformBbox ellipse~>bBox transformations))
      (setq center (centerBox bBox))

      (fprintf
        port
        "\\%s[] (%L,%L) ellipse (%L and %L);\n" 
        (if (member ellipse~>lpp fill)
         "fill"   
          "draw"
        );if
        (xCoord center)
        (yCoord center)
        (quotient (difference (rightEdge bBox) (leftEdge bBox)) 2.0)
        (quotient (difference (topEdge bBox) (bottomEdge bBox)) 2.0)
      );fprintf
    );unless 
  );let
);defun


;FUNCTION
; EDsch2tikzExportArc
;
;Description:
; This functions converts an arc in a schematic to a TikZ representation
; and writes the code to a port.
;
;Parameters:
; port             port, where resulting TikZ code is exported
; ellipse          arc, arc to be exported
; transformations  list, list of transformations
;
;Return:
;  t               bool, always true
;
(defun EDsch2tikzExportArc (port arc transformations)
  (let (bBox center (ignore nil) (fill nil))

    (unless (member arc~>lpp ignore)

      (setq bBox (EDsch2tikzTransformBbox arc~>ellipseBBox transformations))
      (setq center (centerBox bBox))

      (fprintf
        port
        "\\%s[domain=%L:%L] plot ({%L+%L*cos(deg(\\x))}, {%L+%L*sin(deg(\\x))});\n" 
        (if (member arc~>lpp fill) "fill" "draw")
        arc~>startAngle
        arc~>stopAngle
        (xCoord center)
        (quotient (difference (rightEdge bBox) (leftEdge bBox)) 2.0)
        (yCoord center)
        (quotient (difference (topEdge bBox) (bottomEdge bBox)) 2.0)
      );fprintf
    );unless 
  );let
);defun EDsch2tikzExportArc


;FUNCTION
; EDsch2tikzExportRect
;
;Description:
; This functions converts a rectangle in a schematic to a TikZ representation
; and writes the code to a port.
;
;Parameters:
; port             port, where resulting TikZ code is exported
; rect             rectangle, rectangle to be exported
; transformations  list, list of transformations
;
;Return:
;  t               bool, always true
;
(defun EDsch2tikzExportRect (port rect transformations)
  (let (bBox (ignore '(("instance" "drawing"))) (fill '(("pin" "drawing"))))

    (unless (member rect~>lpp ignore)

      (setq bBox (EDsch2tikzTransformBbox rect~>bBox transformations))

      (fprintf
        port
        "\\%s[] (%L,%L) rectangle (%L,%L);\n" 
        (if (member rect~>lpp fill) "fill" "draw")
        (leftEdge bBox)  (bottomEdge bBox)
        (rightEdge bBox) (topEdge bBox)
      );fprintf
    );unless 
  );let
);defun EDsch2tikzExportRect


;FUNCTION
; EDsch2tikzExportPolygon
;
;Description:
; This functions converts a polygon in a schematic to a TikZ representation
; and writes the code to a port.
;
;Parameters:
; port             port, where resulting TikZ code is exported
; polygon          polygon, polygon to be exported
; transformations  list, list of transformations
;
;Return:
;  t               bool, always true
;
(defun EDsch2tikzExportPolygon (port polygon transformations)
  (let ((ignore nil))

    (unless (member polygon~>lpp ignore)
      (fprintf
        port
        "\\draw[line cap=rect] %s -- cycle;\n" 
        (buildString
          (foreach mapcar
            point
            (EDsch2tikzTransformPoints polygon~>points transformations)

            (lsprintf "(%L,%L)" (xCoord point) (yCoord point))
          );foreach
          " -- "
        );buildString
      );fprintf
    );unless 
  );let
);defun EDsch2tikzExportPolygon

(defun EDsch2tikzExportLabel (port label transformations @key (inst nil))

  (let ((ignore nil) point)

    inst

    (unless (member label~>lpp ignore)

      (setq point (EDsch2tikzTransformPoint label~>xy transformations))

      (fprintf
        port
        "\\node[inner sep=0pt,font=\\tiny\\tt,rotate=%L,anchor=%s] () at (%L,%L) {%s};\n" 
        (case label~>orient
          ("R0"     0)
          ("R90"   90)
          ("R180" 180)
          ("R270" 270)
          (nil      0)
        );case
        (case label~>justify
          ("upperLeft"   "north east")
          ("centerLeft"  "west")
          ("lowerLeft"   "south west")
          ("upperCenter" "north")
          ("lowerCenter" "south")
          ("upperRight" "north west")
          ("centerRight" "east")
          ("lowerRight" "south east")
          (nil          "center")
        );case
        (xCoord point)
        (yCoord point)
        (if label~>theLabel
          label~>theLabel  
          label~>associate~>name 
        );if
      );fprintf
    );unless 
  );let
);defun EDsch2tikzExportLabel


;FUNCTION
; EDsch2tikzTransformPoint
;
;Description:
; Transform a point wrt. a list of transformation
;
;Parameters:
; point            list, point
; transformations  list, list of transformations
;
;Return:
; point            list, transformed point
;
(defun EDsch2tikzTransformPoint (point transformations)
  (foreach transformation transformations
    (setq point (dbTransformPoint point transformation))   
  );foreach
  point
);defun EDsch2tikzTransformPoint

;FUNCTION
; EDsch2tikzTransformPoints
;
;Description:
; Transform a list of points wrt. a list of transformation
;
;Parameters:
; point            list, list of points
; transformations  list, list of transformations
;
;Return:
; list             list, list of transformed points
;
(defun EDsch2tikzTransformPoints (points transformations)
  (foreach transformation transformations
    (setq points (dbTransformPointList points transformation))   
  );foreach
  points
);defun EDsch2tikzTransformPoints

;FUNCTION
; EDsch2tikzTransformBbox
;
;Description:
; Transform a bounding box  wrt. a list of transformation
;
;Parameters:
; bBox             list, bounding box
; transformations  list, list of transformations
;
;Return:
; bBox             list, transformed bounding box
;
(defun EDsch2tikzTransformBbox (bBox transformations)
  (foreach transformation transformations
    (setq bBox (dbTransformBBox bBox transformation))
  );foreach
  bBox
);defun EDsch2tikzTransformBbox