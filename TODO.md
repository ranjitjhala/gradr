# TODO

## Policies

* Only an instructor can add assignments
* Only an instructor can add students
* Only an instructor can view the students in a class
* Only an instructor can view the assignments in a class
* Only an instructor can view the scores in a class
* Only a student in a class can see the assignments
* Only a student in a class can see their own scores

## HEREHERE: Adding Students to Class

+ Upload a CSV file with:

    email, name, sid

+ Parse file and create accounts


## View-Class-as-Instructor

- [x] Class-link in "profile" issues route

    /class/#ClassId ClassR GET

- [x] Handler for the above route

    getClassR :: Handler Html

- [x] Handler should print information about the class
    - [x] list of assignments and points
    - [x] form for adding new assignments

- [x] Add Students to class         (by FORM)

- [ ] Add Students to class         (by CSV)
    - See [upload csv](http://stackoverflow.com/questions/23377137/how-to-read-contents-of-an-uploaded-file)

- [ ] View students in class

- [ ] Upload scores for Assignment  (by CSV)

- [ ] Download scores for class     (by CSV)



## View-Class-as-Instructor

- [ ] View classes
- [ ] Select class
- [ ] View assignment scores
