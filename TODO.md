# TODO

Policies

* Only an instructor can add assignments
* Only an instructor can add students

## Set Class as Current

- [ ] Class-link in "profile" issues route

    /class/#ClassId ClassR GET

- [ ] Handler for the above route

    Handler/Class.hs
    getClassR :: Handler Html

  The handler should:

    1. [ ] Set the class as the current class

    2. [ ] Print out some information about the class
           - list of assignments and points

    3. [ ] Have a form for adding new assignments

Instructor

- [ ] Set "Class" as "current"
- [ ] Add Assignments to "current" class
- [ ] View "current" assignments

- [ ] Add Students to "current" class
- [ ] View "current" students
- [ ] Upload scores for Assignment (by CSV)
- [ ] Download scores for class (by CSV)

Student

- [ ] View classes
- [ ] Set "class" as "current"
- [ ] View scores

## Model

See `config/models`

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
