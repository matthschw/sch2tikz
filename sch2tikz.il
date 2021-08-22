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

          (when (equal shape~>objType "label")

            (setq point (dbTransformPoint shape~>xy (list 0:0 "R0" mag)))



            (fprintf
              port
              "\\node[anchor=south,inner sep=0pt,font=\\tiny\\tt] () at (%L,%L) {%s};\n" 
              (xCoord point)
              (yCoord point)
              shape~>theLabel
            );fprintf
          );when
        );unless 

        (foreach item cv~>instances
            
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