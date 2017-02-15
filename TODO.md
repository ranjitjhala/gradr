# TODO

## Policies

* Only an instructor can add assignments
* Only an instructor can add students
* Only an instructor can view the students in a class
* Only an instructor can view the assignments in a class
* Only an instructor can view the scores in a class
* Only a student in a class can see the assignments
* Only a student in a class can see their own scores

## Profile View

- [ ] Allow user to change their name (not their email)

## Instructor Views

- [x] Class-link in "profile" issues route

    /class/#ClassId ClassR GET

- [x] Handler for the above route

    getClassR :: Handler Html

- [x] Handler should print information about the class
    - [x] list of assignments and points
    - [x] form for adding new assignments

- [x] Add students to class     

- [x] View students in class

- [x] Add scores for Assignment

- [x] Add other instructors for class

- [ ] Add scores for Assignment  by CSV

- [ ] Add Students to class      by CSV
    - See [upload csv](http://stackoverflow.com/questions/23377137/how-to-read-contents-of-an-uploaded-file)

- [ ] Download scores for class  by CSV

## Student View

- [ ] View classes (profile) HEREHEREHERE
      * /class/student/#ClassId  ClassStdR GET

- [ ] Select class
      * Click on above link

- [ ] View assignment scores
