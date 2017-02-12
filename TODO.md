# TODO


HEREHEREHERE:
  - add "Class" to the config/model
  - allow a user to create a class

## Model

1. Users
    - Admin
    - Students
    - Instructors

2. Classes
    - Instructors :: [User]
    - Students    :: [User]
    - Assignments :: [Assignment]

----- config/model


User
  ident            Text
  password         ByteString
  emailAddress     Text
  UniqueUser       emailAddress
  verified         Bool
  verifyKey        Text
  resetPasswordKey Text
  deriving         Show
  deriving         Eq
  deriving         Typeable

Class
  name             Text
  term             Text
  instructor       UserId
  deriving         Show
  deriving         Eq
  deriving         Typeable

Teacher
  name             UserId          
  class            ClassId

Student
  name             UserId
  class            ClassId

Assignment
  name             Text
  points           Int
  class            ClassId



## Routes

  /static StaticR Static appStatic

  /auth   AuthR   Auth   getAuth

  /favicon.ico FaviconR GET
  /robots.txt RobotsR GET

  /         HomeR    GET POST

  /comments CommentR POST

  /profile ProfileR GET


  /admin

    - Just shows list of instructors

  /create

  /auth/login







## View

### Login

### Instructor

- Home
  - List of Classes

- Class
  - List of Evals
  - Add Eval
  - View

### Student


1. New  Instructor
1. View Instructors


## Controller
