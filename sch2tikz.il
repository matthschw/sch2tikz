(defun EDsch2tikz (cv mag file)
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

        (fprintf port "%% %s\n" inst~>name)

        (EDsch2tikzExportShapes
          port
          inst~>master~>shapes
          (list inst~>transform (list 0:0 "R0" mag))
        );EDsch2tikzExportShapes
      );foreach

      /*
      (fprintf
        port
        "\\node[anchor=south,inner sep=0pt,font=\\tiny\\tt,rotate=%L,anchor=%s] () at (%L,%L) {%s};\n" 
        (case shape~>orient
          ("R0"     0)
          ("R90"   90)
          ("R180" 180)
          ("R270" 270)
          (nil      0)
        );case
        (case shape~>justify
          ("upperLeft"   "north east")
          ("centerLeft"  "west")
          ("lowerLeft"   "south west")
          ("upperCenter" "north")
          ("lowerCenter" "south")
          ("upperRight" "north west")
          ("centerRight" "east")
          ("lowerRight" "south east")
          (nil      "center")
        );case
        (xCoord point)
        (yCoord point)
        (if shape~>theLabel
          shape~>theLabel  
          shape~>associate~>name 
        );if
      );fprintf
      */

      (fprintf port "\\end{tikzpicture}")

      (close port)
    else
      (error "[ED-SCH2TikZ] Provided Cellview is not a schematic")
    );if
  );let
);defun

(defun EDsch2tikzExportShapes (port shapes transformations)

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
        ;(EDsch2tikzExportArc
        ;  port
        ;  shape
        ;  transformations
        ;);EDsch2tikzExportLine
        nil
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
  (let (bBox center (ignore nil) (fill '(("wire" "drawing"))))

    (unless (member arc~>lpp ignore)

      (setq bBox (EDsch2tikzTransformBbox arc~>ellipseBBox transformations))
      (setq center (centerBox bBox))

      (fprintf
        port
        "\\%s[] (%L,%L) arc (%L:%L:%L and %L);\n" 
        (if (member arc~>lpp fill)
         "fill"   
          "draw"
        );if
        (xCoord center)
        (yCoord center)
        ;arc~>startAngle/3.141592*180
        ;arc~>stopAngle/3.141592*180
        0 350
        (quotient (difference (rightEdge bBox) (leftEdge bBox)) 2.0)
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

(EDsch2tikz (geGetWindowCellView) 2.0 "fig.tex")