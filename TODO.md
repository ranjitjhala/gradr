# TODO

Policies

* Only an instructor can add assignments
* Only an instructor can add students
* Only a student in a class can see scores
* Student can only see their own scores

## Set Class as Current

- [x] Class-link in "profile" issues route

    /class/#ClassId ClassR GET

- [x] Handler for the above route

    getClassR :: Handler Html

- [x] Handler should print information about the class
    - [x] list of assignments and points
    - [x] form for adding new assignments

Instructor

- [ ] Add Students to class         (by CSV)
- [ ] View students in class
- [ ] Upload scores for Assignment  (by CSV)
- [ ] Download scores for class     (by CSV)

Student

- [ ] View classes
- [ ] Select class
- [ ] View assignment scores

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
