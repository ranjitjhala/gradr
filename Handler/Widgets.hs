module Handler.Widgets where

import           Import

classEdit classId clsEnc clsWidget
  = $(widgetFile "classEdit")

classInstructors classId instructor teachers insEnc insWidget
  = $(widgetFile "classInstructors")

classAssignments classId asgns asgnEnc asgnWidget
  = $(widgetFile "classAssignments")

classStudents classId students stdEnc stdWidget stdCsvEnc stdCsvWidget
  = $(widgetFile "classStudents")

classExport classId
  = $(widgetFile "classExport")

classImport classId scoEnc scoWidget
  = $(widgetFile "classImport")
