(defun EDsch2tikz (cv mag)
  (let (lLines tabPoints port tabShapes)

    (if
      (and
        (dbIsId cv)
        (equal cv~>objType      "cellView" )
        (equal cv~>cellViewType "schematic")
      );and
    then

      (setq tabShapes (makeTable "tabShapes" nil))

      (setq port (outfile "./fig.tex"))

      (fprintf port "\\begin{tikzpicture}\n")

      (foreach net cv~>nets

        (setq
          lLines
          (setof fig net~>figs (equal fig~>objType "line"))
        );setq

        (setq tabPoints (makeTable "tabPoints" nil))

        (foreach line lLines

          (setarray tabShapes line t)

          (if (arrayref tabPoints (car line~>points)) then
            (tconc (arrayref tabPoints (car line~>points)) line)
          else
            (setarray tabPoints (car line~>points) (tconc nil line))
          );if

          (if (arrayref tabPoints (cadr line~>points)) then
            (tconc (arrayref tabPoints (cadr line~>points)) line)
          else
            (setarray tabPoints (cadr line~>points) (tconc nil line))
          );if
        );foreach

        (foreach point tabPoints~>?

          (setarray
            tabPoints 
            point
            (car (arrayref tabPoints point))
          );setarray
        );foreach

        (setq lEndPoints (setof x tabPoints~>? (oddp (length (arrayref tabPoints x)))))

        (setq tabVisitedLines (makeTable "tabLines" nil))

        (foreach endPoint lEndPoints

          (setq lLine (tconc nil endPoint))
          (setq nextPoint endPoint)

          (while nextPoint

            (setq nextPoint nil)

            (foreach line (arrayref tabPoints (cadr lLine))

              (when (and (not (arrayref tabVisitedLines line)) (not nextPoint))
                (setq
                  nextPoint
                  (if (equal (car line~>points) (cadr lLine)) then
                    (cadr line~>points)
                  else
                    (car line~>points)
                  );if
                );setq
                (setarray tabVisitedLines line t)
              );when
            );foreach

            (when nextPoint
              (tconc lLine nextPoint)
            );when  
          );while

          (setq lLine (car lLine))

          (when (greaterp (length lLine) 1)
            (fprintf
              port
              "\\draw[] %s;\n" 
              (buildString
                (foreach mapcar point lLine
                  (setq point (dbTransformPoint point (list 0:0 "R0" mag)))
                  (lsprintf 
                    "(%L,%L)"
                    (xCoord point)
                    (yCoord point)
                  );lsprintf
                );foreach
                " -- "
              );buildString
            );fprintf
          );when
        );foreach
      );foreach

      (foreach shape cv~>shapes

        (unless (arrayref tabShapes shape)

          (when (equal shape~>objType "ellipse")

            (setq bBox (dbTransformBBox shape~>bBox (list 0:0 "R0" mag)))
            (setq point (centerBox bBox))
            (fprintf
              port
              "\\fill[] (%L,%L) ellipse (%L and %L);\n" 
              (xCoord point)
              (yCoord point)
              (quotient (difference (rightEdge bBox) (leftEdge bBox)) 2.0)
              (quotient (difference (topEdge bBox) (bottomEdge bBox)) 2.0)
            );fprintf
          );when

          (when (or (equal shape~>objType "label") (equal shape~>objType "textDisplay"))

            (setq point (dbTransformPoint shape~>xy (list 0:0 "R0" mag)))

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
          );when
        );unless 
      );foreach

      (foreach inst cv~>instances

        (setq master inst~>master)

        (foreach shape master~>shapes

          (when (equal shape~>objType "line")

            (setq points (dbTransformPointList shape~>points inst~>transform))
            (setq points (dbTransformPointList points (list 0:0 "R0" mag)))

            (fprintf
              port
              "\\draw[] %s;\n" 
              (buildString
                (foreach mapcar point points
                  (lsprintf 
                    "(%L,%L)"
                    (xCoord point)
                    (yCoord point)
                  );lsprintf
                );foreach
                " -- "
              );buildString
            );fprintf
          );when  

          (when (and (equal shape~>objType "rect") (nequal shape~>lpp (list "instance" "drawing")))

            (setq bBox (dbTransformBBox shape~>bBox inst~>transform))
            (setq bBox (dbTransformBBox bBox (list 0:0 "R0" mag)))
            (fprintf
              port
              "\\fill[] (%L,%L) rectangle (%L,%L);\n" 
              (leftEdge bBox)
              (bottomEdge bBox)
              (rightEdge bBox)
              (topEdge bBox)
            );fprintf
          );when 


          (when (and (equal shape~>objType "polygon") (nequal shape~>lpp (list "instance" "drawing")))

            (setq points (dbTransformPointList shape~>points inst~>transform))
            (setq points (dbTransformPointList points (list 0:0 "R0" mag)))

            (fprintf
              port
              "\\fill[] %s -- cycle;\n" 
              (buildString
                (foreach mapcar point points
                  (lsprintf 
                    "(%L,%L)"
                    (xCoord point)
                    (yCoord point)
                  );lsprintf
                );foreach
                " -- "
              );buildString
            );fprintf
          );when    
        );foreach  
      );foreach

      (fprintf port "\\end{tikzpicture}")
      (close port)
    else
      (error "[ED-SCH2TikZ] Provided Cellview is not a schematic")
    );if
  );let
);defun

(EDsch2tikz (geGetWindowCellView) 2.0)