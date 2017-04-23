module Handler.Class where

import           Import
import           DB
import           Utils
import qualified Text.Blaze   as TB
import qualified Auth.Account as Auth
import           Text.Read (readMaybe)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as M
-- import qualified Data.Vector  as V
-- import qualified Data.Csv     as Csv
-- import qualified Util as Util
-- import qualified Data.ByteString.Lazy as L
-- import Data.Conduit.Binary

-- import Yesod.Form.MassInput
-- import qualified Yesod.Form.Bootstrap3 as BS3 -- (BootstrapSubmit, BootstrapFormLayout (..), renderBootstrap3)
-- import Yesod.Form.Jquery (jqueryAutocompleteField)

scoresStudent :: (Eq k, Hashable k) => [k] -> [(Text, [b])] -> [(k, [(Text, b)])]
stringScores  :: Int -> String -> Either String (Text, [Int])

--------------------------------------------------------------------------------
-- | Viewing/Changing User Settings --------------------------------------------
--------------------------------------------------------------------------------
getEditUserR :: Handler Html
getEditUserR = do
  (_, user)           <- requireAuthPair
  (setWidget, setEnc) <- generateFormPost (settingsForm user)
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s Settings page"
    $(widgetFile "settings")

postEditUserR :: Handler Html
postEditUserR = do
  (uid, u)       <- requireAuthPair
  ((result,_),_) <- runFormPost (settingsForm u)
  case result of
    FormSuccess o -> DB.updUser uid (setName o)
    _             -> setMessage "Something went wrong!"
  redirect ProfileR

data SettingsForm = SettingsForm
  { setName :: Text }
  deriving (Show)

settingsForm :: User -> Form SettingsForm
settingsForm u = renderForm $ SettingsForm
  <$> areq textField  "Name"  (Just (userIdent u))
  <*  submitButton "Update"

--------------------------------------------------------------------------------
-- | Import Class Scores by .csv -----------------------------------------------
--------------------------------------------------------------------------------
postClassImportR :: ClassId -> Handler Html
postClassImportR classId =
  extendClassFormR
    "updating scores by .csv"
    fileForm
    (addScoresR classId)
    classId
    (ClassInsR classId)

-- | Add multiple scores via CSV upload
addScores1R :: ClassId -> AssignmentId -> FileForm -> Handler ()
addScores1R classId assignId scoresForm = do
  res            <- liftIO $ fileAsgnScores (fFile scoresForm)
  case res of
    Left err     -> setMessage $ "Error updating scores: " ++ TB.text (fromString err)
    Right scores -> addScoresR' classId assignId scores

-- | Add scores for multiple assignments via CSV upload
addScoresR :: ClassId -> FileForm -> Handler ()
addScoresR classId scoresForm = do
  res          <- fileScores classId (fFile scoresForm)
  case res of
    Left err   -> setMessage $ "Error updating scores: " ++ TB.text (fromString err)
    Right aScs -> forM_ aScs (uncurry (addScoresR' classId))

addScoresR' :: ClassId -> AssignmentId -> [(Text, Int)] -> Handler ()
addScoresR' classId assignId scores = do
  oldScores <- getAssignmentScores classId assignId
  updAssignmentScores assignId oldScores scores

-- YUCK
fileScores :: ClassId -> FileInfo
           -> Handler (Either String [(AssignmentId, [(Text, Int)])])
fileScores classId file = do
  ls <- liftIO $ fileLines file
  case ls of
    []       -> return (Left "Empty .csv file!")
    [_,_]    -> return (Left "Empty .csv file!")
    (l:_:ls) -> case fromString <$> stringFields' l of
                  []     -> return (Left "No assignments in .csv file!")
                  (_:as) -> do aIds'   <- fileAsgns classId as
                               case aIds' of
                                 Left e -> return (Left e)
                                 Right aIds -> case sequenceA (stringScores (length aIds) <$> ls) of
                                                 Left e  -> return (Left e)
                                                 Right z -> return (Right $ scoresStudent aIds z)

fileAsgns :: ClassId -> [Text] -> Handler (Either String [AssignmentId])
fileAsgns classId as = do
  asgns <- getAssignmentsByClass classId
  return $ sequenceA (getAsgnId asgns <$> as)

-- HEREHEREHEREHEREHERE
getAsgnId :: [Entity Assignment] -> Text -> Either String AssignmentId
getAsgnId asgns a = case find f asgns of
                      Nothing   -> Left ("Unknown assignment" ++ show a)
                      Just asgn -> Right (entityKey asgn)
  where
    f             = (a ==) . assignmentName . entityVal

{-@ scoresStudent :: (Eq k, Hashable k)
                  => aIds:[k] -> [(Text, ListX b aIds)] -> [(k, [(Text, b)])]
  @-}
scoresStudent as xnss = groupList [ (a, (x, n)) | (x, ns) <- xnss
                                                , (a, n)  <- zip as ns ]

fileAsgnScores :: FileInfo -> IO (Either String [(Text, Int)])
fileAsgnScores file = sequenceA . map stringScore <$> fileLines file

stringScore :: String -> Either String (Text, Int)
stringScore s = case stringFields 2 s of
                  Left err       -> Left err
                  Right [em, s2] -> case readMaybe s2 of
                                      Nothing -> Left ("Malformed score: " ++ s)
                                      Just n  -> Right (fromString em, n)

{-@ stringScores :: n:Nat -> String -> Either String (Text, ListN Int n) @-}
stringScores n s = case stringFields (n + 1) s of
                     Left err      -> Left err
                     Right (em:ss) -> case sequence (readMaybe <$> ss) of
                                        Nothing -> Left ("Malformed score: " ++ s)
                                        Just ns -> Right (fromString em, ns)

--------------------------------------------------------------------------------
-- | Export Class Scores to .csv -----------------------------------------------
--------------------------------------------------------------------------------

getClassExportR :: ClassId -> Handler TypedContent
getClassExportR classId = do
  bytes <- csvBytes <$> classCsv classId
  addHeader "Content-Disposition" "attachment; filename=\"scores.csv\""
  sendResponse (T.encodeUtf8 "text/csv", toContent bytes)

data ClassCsv = ClassCsv
  { csvAsgns  :: Int
  , csvNames  :: [Text]           -- List Text csvAsgns
  , csvPoints :: [Int]            -- List Int  csvAsgns
  , csvScores :: [(Text, [Int])]  -- [(Text, List Int csvAsgns)]
  }
  deriving (Show)

type Scores = [(Entity Assignment, [(Text, Int)])]

classCsv :: ClassId -> Handler ClassCsv
classCsv classId = scoresCsv <$> classScores classId

classScores :: ClassId -> Handler Scores
classScores classId = do
  asgns    <- getAssignmentsByClass classId
  forM asgns $ \a -> do
    scores <- getRawScores (entityKey a)
    return (a, scores)

scoresCsv :: Scores -> ClassCsv
scoresCsv sc  = ClassCsv n ns pts (studentScores sc)
  where
    n         = length sc
    (ns, pts) = unzip [(n, pt) | (Entity _ (Assignment n pt _), _) <- sc]

studentScores    :: Scores -> [(Text, [Int])]
studentScores sc = [(e, eScore e <$> aTables) | e <- emails]
  where
    eScore e     = M.lookupDefault 0 e
    aTables      :: [M.HashMap Text Int]
    aTables      = [M.fromList m | m <- hscores]
    emails       :: [Text]
    emails       = M.keys . M.fromList $ concat hscores
    hscores      :: [[(Text, Int)]]
    hscores      = snd <$> sc


csvBytes :: ClassCsv -> ByteString
csvBytes c = B.unlines $ namesBS  (csvNames  c)
                       : pointsBS (csvPoints c)
                       : (scoresBS <$> csvScores c)

namesBS :: [Text] -> ByteString
namesBS asgns = commaCat ("names" : (T.encodeUtf8 <$> asgns))

pointsBS :: [Int] -> ByteString
pointsBS pts = commaCat ("points" : (intBS <$> pts))

scoresBS :: (Text, [Int]) -> ByteString
scoresBS (u, ns) = commaCat (T.encodeUtf8 u : (intBS <$> ns))

intBS :: Int -> ByteString
intBS = fromString . show

commaCat :: [ByteString] -> ByteString
commaCat = intercalate ","


--------------------------------------------------------------------------------
-- | Viewing Existing Classes --------------------------------------------------
--------------------------------------------------------------------------------
getClassInsR :: ClassId -> Handler Html
getClassInsR classId = do
  klass                 <- getClassById classId
  instructor            <- getUserById (classInstructor klass)
  asgns                 <- getAssignmentsByClass classId
  students              <- getStudentsByClass    classId
  teachers              <- getInstructorsByClass classId
  (asgnWidget, asgnEnc) <- generateFormPost assignForm
  (stdWidget,  stdEnc)  <- generateFormPost addUserForm
  (csvWidget,  csvEnc)  <- generateFormPost fileForm      -- upload students
  (insWidget,  insEnc)  <- generateFormPost addUserForm
  (clsWidget,  clsEnc)  <- generateFormPost classForm
  (scoWidget,  scoEnc)  <- generateFormPost fileForm      -- upload scores
  defaultLayout $
    $(widgetFile "viewClassInstructor")


getClassStdR :: ClassId -> Handler Html
getClassStdR classId = do
  (userId, _) <- requireAuthPair
  klass       <- getClassById classId
  scores      <- getScoresByUser userId classId
  defaultLayout $
    $(widgetFile "viewClassStudent")

getAssignmentR :: ClassId -> AssignmentId -> Handler Html
getAssignmentR classId assignId = do
  klass               <- getClassById classId
  instr               <- getUserById  (classInstructor klass)
  asgn                <- getAssignmentById assignId
  scores              <- getAssignmentScores classId assignId
  (stdWidget, stdEnc) <- generateFormPost (scoreForm scores)
  (asgWidget, asgEnc) <- generateFormPost assignForm
  (scoWidget, scoEnc) <- generateFormPost fileForm
  defaultLayout $
    $(widgetFile "viewAssignment")

--------------------------------------------------------------------------------
-- | Edit Name/Term for Class --------------------------------------------------
--------------------------------------------------------------------------------
postEditClassR :: ClassId -> Handler Html
postEditClassR classId =
  extendClassFormR
    "edit class name"
    classForm
    (\cf -> DB.updClass classId (cName cf) (cTerm cf))
    classId
    (ClassInsR classId)

--------------------------------------------------------------------------------
-- | Edit Name/Points for Assignment -------------------------------------------
--------------------------------------------------------------------------------
postEditAssignmentR :: ClassId -> AssignmentId -> Handler Html
postEditAssignmentR classId asgnId =
  extendClassFormR
    "edit assignment"
    assignForm
    (\af -> DB.updAssign asgnId (asgnName af) (asgnPoints af))
    classId
    (AssignmentR classId asgnId)

getDelAssignmentR :: ClassId -> AssignmentId -> Handler Html
getDelAssignmentR classId asgnId =
  extendClassHandleR
    "delete assignment"
    (return (Just asgnId))
    (DB.delAssign classId)
    classId
    (ClassInsR classId)

--------------------------------------------------------------------------------
-- | Update Scores for Assignment ----------------------------------------------
--------------------------------------------------------------------------------
postScoreR :: ClassId -> AssignmentId -> Handler Html
postScoreR classId assignId = do
  oldScores <- getAssignmentScores classId assignId
  extendClassFormR
    "update scores"
    (scoreForm oldScores)
    (updAssignmentScores assignId oldScores)
    classId
    (AssignmentR classId assignId)



--------------------------------------------------------------------------------
-- | Deleting Instructors/Students from a Class --------------------------------
--------------------------------------------------------------------------------
getDelInsR :: ClassId -> UserId -> Handler Html
getDelInsR classId userId =
  extendClassHandleR
    "remove teacher"
    (return (Just userId))
    (DB.delTeacher classId)
    classId
    (ClassInsR classId)

getDelStdR :: ClassId -> UserId -> Handler Html
getDelStdR classId userId =
  extendClassHandleR
    "remove student"
    (return (Just userId))
    (DB.delStudent classId)
    classId
    (ClassInsR classId)

--------------------------------------------------------------------------------
-- | Adding New Instructors ----------------------------------------------------
--------------------------------------------------------------------------------
postNewInstructorR :: ClassId -> Handler Html
postNewInstructorR classId =
  extendClassFormR
    "add instructor"
    addUserForm
    (addInstructorR classId)
    classId
    (ClassInsR classId)

addInstructorR :: ClassId -> AddUserForm -> Handler ()
addInstructorR classId userForm = do
  mbStd <- addUserR classId userForm
  case mbStd of
    Nothing ->    setMessage $ "Error adding instructor: " ++ TB.text (auEmail userForm)
    Just e  -> do setMessage $ "Added instructor: "        ++ TB.text (auEmail userForm)
                  void $ updTeacher classId (entityKey e)
                  -- runDB (insert (Teacher (entityKey e) classId))

--------------------------------------------------------------------------------
-- | Creating New Assignments --------------------------------------------------
--------------------------------------------------------------------------------
postNewAssignR :: ClassId -> Handler Html
postNewAssignR classId =
  extendClassFormR
    "create assignment"
    assignForm
    (addAssignR classId)
    classId
    (ClassInsR classId)

addAssignR :: ClassId -> AssignForm -> Handler ()
addAssignR classId (AssignForm aName aPts) = do
  _ <- runDB $ insert $ Assignment aName aPts classId
  setMessage $ "Added new assignment: " ++ TB.text aName

scoreForm :: [(Entity User, Int)] -> Form [(Text, Int)]
scoreForm scores = renderForm (sequenceA (userForm <$> scores) <* submitButton "Submit")
  -- BS3.bootstrapSubmit (BS3.BootstrapSubmit ("Submit" :: Text) "btn-default" [("attribute-name","attribute-value")])
  where
    userForm (user, score) = (,) <$> pure email
                                 <*> areq intField (textString email) (Just score)
      where
        email = userEmailAddress (entityVal user)

dummyScores :: [(Text, Int)]
dummyScores = [ ("Michael", 26)
              , ("Alice"  , 10)
              , ("Robert" , 19)
              ]

data AssignForm = AssignForm
  { asgnName   :: Text
  , asgnPoints :: Int
  }
  deriving (Show)

assignForm :: Form AssignForm
assignForm = renderForm $ AssignForm
  <$> areq textField "Name"   Nothing
  <*> areq intField  "Points" Nothing
  <*  submitButton   "Submit"

--------------------------------------------------------------------------------
-- | Enrolling New Students ----------------------------------------------------
--------------------------------------------------------------------------------
data AddUserForm = AddUserForm
  { auEmail :: Text }
  deriving (Show)

addUserForm :: Form AddUserForm
addUserForm = renderForm $ AddUserForm
  <$> areq textField "Email" Nothing
  <*  submitButton   "Enroll"

data FileForm = FileForm
  { fFile :: FileInfo }

fileForm :: Form FileForm
fileForm = renderForm $ FileForm
  <$> areq fileField "" Nothing
  <*  submitButton   "Upload"

postNewStudentsR :: ClassId -> Handler Html
postNewStudentsR classId =
  extendClassFormR
    "enroll students by .csv"
    fileForm
    (addStudentsR classId)
    classId
    (ClassInsR classId)

postNewStudentR :: ClassId -> Handler Html
postNewStudentR classId =
  extendClassFormR
    "enroll student"
    addUserForm
    (addStudentR classId)
    classId
    (ClassInsR classId)


-- | Enroll multiple students via CSV upload
addStudentsR :: ClassId -> FileForm -> Handler ()
addStudentsR classId usersForm = do
  res <- liftIO $ fileUsers (fFile usersForm)
  case res of
    Left err     -> setMessage $ "Error enrolling students: " ++ TB.text (fromString err)
    Right emails -> mapM_ (addStudentR classId) emails

fileUsers :: FileInfo -> IO (Either String [AddUserForm])
fileUsers file = Right . map stringUser <$> fileLines file
  where
    stringUser :: String -> AddUserForm
    stringUser = AddUserForm . fromString . snipSpaces



-- | Enroll a single student to the class
addStudentR :: ClassId -> AddUserForm -> Handler ()
addStudentR classId userForm = do
  mbStd <- addUserR classId userForm
  case mbStd of
    Nothing ->    setMessage $ "Error enrolling student: " ++ TB.text (auEmail userForm)
    Just e  -> do setMessage $ "Enrolled student: "        ++ TB.text (auEmail userForm)
                  void $ runDB (insert (Student (entityKey e) classId))

addUserR :: ClassId -> AddUserForm -> Handler (Maybe (Entity User))
addUserR classId (AddUserForm sEmail) = do
  mbU <- getUserByEmail sEmail
  case mbU of
    Just _  -> return mbU
    Nothing -> do _ <- Auth.createNewCustomAccount
                         (Auth.CustomNewAccountData sEmail ("?" ++ sEmail ++ "?") sEmail sEmail)
                         (const (ClassInsR classId))
                  getUserByEmail sEmail

--------------------------------------------------------------------------------
-- | Generic Class Extension ---------------------------------------------------
--------------------------------------------------------------------------------
extendClassFormR :: Html
                 -> Form a
                 -> (a -> Handler ())
                 -> ClassId
                 -> Route App
                 -> Handler Html
extendClassFormR msg form =
  extendClassHandleR msg (runFormHandler form)

runFormHandler :: Form a -> Handler (Maybe a)
runFormHandler form = do
  ((result,_),_) <- runFormPost form
  case result of
    FormSuccess o -> return (Just o)
    _             -> return Nothing

extendClassHandleR
  :: Html -> Handler (Maybe a) -> (a -> Handler ()) -> ClassId -> Route App -> Handler Html
extendClassHandleR msg h extR classId r = do
  instrId      <- classInstructor <$> getClassById classId
  (uid    , _) <- requireAuthPair
  if uid /= instrId
    then setMessage ("Sorry, can only " ++ msg ++ "for your own class!")
    else do
      result <- h
      case result of
        Just o -> extR o
        _      -> setMessage "Something went wrong!"
  redirect r


--------------------------------------------------------------------------------
-- | Creating New Classes ------------------------------------------------------
--------------------------------------------------------------------------------
data ClassForm = ClassForm
    { cName :: Text
    , cTerm :: Text
    }
    deriving (Show)

postNewClassR :: Handler Html
postNewClassR = do
  (uid    , _)     <- requireAuthPair
  ((result, _), _) <- runFormPost classForm
  case result of
    FormSuccess (ClassForm cName cTerm) -> do
      _ <- runDB $ insert $ Class cName cTerm uid
      setMessage $ "Added new class! " ++ TB.text cName ++ " in term " ++ TB.text cTerm
      redirect ProfileR
    _ -> do
      setMessage "Yikes! Something went wrong"
      redirect NewClassR

getNewClassR :: Handler Html
getNewClassR = do
  (formWidget, formEnctype) <- generateFormPost classForm
  defaultLayout $
    $(widgetFile "newclass")

classForm :: Form ClassForm
classForm = renderForm $ ClassForm
    <$> areq textField "Name" Nothing
    <*> areq textField "Term" Nothing
    <*  submitButton "Submit"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

classEditWidget classId clsEnc clsWidget
  = $(widgetFile "classEdit")

classInstructors classId instructor teachers insEnc insWidget
  = $(widgetFile "classInstructors")

classAssignments classId asgns asgnEnc asgnWidget
  = $(widgetFile "classAssignments")

classStudents classId students stdEnc stdWidget stdCsvEnc stdCsvWidget
  = $(widgetFile "classStudents")
