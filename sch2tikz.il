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
*   Copyright 2021 Reutlingen University, Electronics & Drives
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

        ;(fprintf port "%% %s\n" inst~>name)

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
    );if
  );let
);defun

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
);defun

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
  );let
);defun

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

(defun EDsch2tikzExportArc (port arc transformations)
  (let (bBox center (ignore nil) (fill nil))

    (unless (member arc~>lpp ignore)

      (setq bBox (EDsch2tikzTransformBbox arc~>ellipseBBox transformations))
      (setq center (centerBox bBox))

      (fprintf
        port
        "\\%s[domain=%L:%L] plot ({%L+%L*cos(\\x)}, {%L+%L*sin(\\x)});\n" 
        (if (member arc~>lpp fill)
         "fill"   
          "draw"
        );if
        arc~>startAngle/3.141592*180
        arc~>stopAngle/3.141592*180
        (xCoord center)
        (quotient (difference (rightEdge bBox) (leftEdge bBox)) 2.0)
        (yCoord center)
        (quotient (difference (topEdge bBox) (bottomEdge bBox)) 2.0)
      );fprintf
    );unless 
  );let
);defun

(defun EDsch2tikzExportRect (port rect transformations)
  (let (bBox (ignore '(("instance" "drawing"))) (fill '(("pin" "drawing"))))

    (unless (member rect~>lpp ignore)

      (setq bBox (EDsch2tikzTransformBbox rect~>bBox transformations))

      (fprintf
        port
        "\\%s[] (%L,%L) rectangle (%L,%L);\n" 
        (if (member rect~>lpp fill)
         "fill"   
          "draw"
        );if
        (leftEdge bBox)  (bottomEdge bBox)
        (rightEdge bBox) (topEdge bBox)
      );fprintf
    );unless 
  );let
);defun

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

(defun EDsch2tikzTransformPoint (point transformations)
  (foreach transformation transformations
    (setq point (dbTransformPoint point transformation))   
  );foreach
  point
);defun

(defun EDsch2tikzTransformPoints (points transformations)
  (foreach transformation transformations
    (setq points (dbTransformPointList points transformation))   
  );foreach
  points
);defun

(defun EDsch2tikzTransformBbox (bBox transformations)
  (foreach transformation transformations
    (setq bBox (dbTransformBBox bBox transformation))
  );foreach
  bBox
);defun