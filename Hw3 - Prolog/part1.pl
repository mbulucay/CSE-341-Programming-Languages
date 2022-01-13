/* % trace. */
/* % notrace. */

/*Facts*/

classroom(z23, 10, projector).
classroom(z23, 10, hcapped).

classroom(z02, 11, hcapped).
classroom(z02, 11, smartboard).

classroom(z11, 12, hcapped).
classroom(z11, 12, projector).

%course(id, instructor, capacity, lecturehour, needroom) 
course(102, genc,    10,4, z23).
course(231, turker,  6, 3, z23).
course(241, bayrakci,5, 3, z23).
course(101, gozupek, 10,10,z02).

%schedule(classroom, hours, courseid)

/*
schedule(z23, 8,  241).
schedule(z23, 9,  241).
schedule(z23, 10, 241).
schedule(z23, 11, 241).
% schedule(z23, 11, 102).
schedule(z23, 12, _).
schedule(z23, 13, 101).
schedule(z23, 14, 101).
schedule(z23, 15, 101).
schedule(z23, 16, _).
 
schedule(z11, 8,2 102).
schedule(z11, 9,2 102).
schedule(z11, 10, 102).
schedule(z11, 11, 102).
schedule(z11, 12, _).
schedule(z11, 13, _).
schedule(z11, 14, _).
schedule(z11, 15, 101).
schedule(z11, 16, 101).
*/

schedule(z23, 8,  102).
schedule(z23, 9,  102).
schedule(z23, 10, 102).
schedule(z23, 11, 102).
schedule(z23, 12, _).
schedule(z23, 13, 241).
schedule(z23, 14, 241).
schedule(z23, 15, 241).
schedule(z23, 16, 241).

schedule(z02, 8,  231).
schedule(z02, 9,  231).
schedule(z02, 10, 231).
schedule(z02, 11, 231).
schedule(z02, 12, _).
schedule(z02, 13, _).
schedule(z02, 14, 101).
schedule(z02, 15, 101).
schedule(z02, 16, 101).

schedule(z11, 8,  101).
schedule(z11, 9,  101).
schedule(z11, 10, 241).
schedule(z11, 11, 241).
schedule(z11, 12, _).
schedule(z11, 13, _).
schedule(z11, 14, 231).
schedule(z11, 15, 231).
schedule(z11, 16, _).

%student(id , class, handicapped)
student(11, 102, no).
student(11, 231, no).
student(11, 241, no).

student(22, 102, no).
student(22, 231, no).

student(33, 102, yes).
student(33, 241, yes).

student(44, 102, yes).

student(55, 241, no).
student(55, 231, no).

student(66, 102, no).
student(66, 231, no).
student(66, 241, no).

student(77, 102, no).
student(77, 231, no).

student(88, 102, no).
student(88, 241, no).

student(99,102,yes).

%instructor(id, courseid, need)
instructor(yakup, 102, projector).
instructor(erkan, 231, smartboard).
instructor(yusuf, 241, _).
instructor(mehmet,101, smartboard).

/*End of Facts*/

%Check whether there is any scheduling conflict.
conflicts(Course_1,Course_2) :- 
    schedule(Class_1,Time_1,Course_1), 
    schedule(Class_2,Time_2,Course_2),
    Course_1 \= Course_2,
    Class_1 == Class_2 ,Time_1 =:= Time_2,
    (format('There is no conflict between ~w ~w at in ', [Course_1, Course_2, Time1, Class_1])).

%Check which room can be assigned to a given class.
assigment(ClassId, CourseId) :- 
    classroom(ClassId, Class_Cap, Class_need),
    instructor(_, CourseId, Instructor_need),
    course(CourseId, _, Course_cap, _, Course_need),
    Course_cap =< Class_Cap ,
    Course_need == ClassId,
    Instructor_need == Class_need,
    write('You can assign') .

%Check which room can be assigned to which classes.
assigment() :-
    classroom(Room, Class_Cap, Class_need),
    course(Course, Inst, Course_cap, _,Course_need),
    instructor(Inst, _, Instructor_need),
    Course_need == Class_need,
    Instructor_need == Class_need,
    Class_Cap >= Course_cap,
    (format(" Class ~w can be assign to ~w ",[Room, Course])).

%Check whether a student can be enrolled to a given class.
enroll(StudentId,CourseId):- 
    course(CourseId, _, Capacity, _, _),
    studentCourse(StudentId),
    CountStart is 0,
    getCap(0, CourseId, CountStart, Capacity),
	CountStart >= Capacity, 
    (format(' ~w is full ' ,[Capacity])),
    (format( 'the ~w course ' ,[CourseId])); 
	(format(' ~w can get the ~w course ' ,[StudentId,CourseId])). 

studentCourse(StudentId) :- 
    student(StudentId, Course, _),
    format('~w is getting ~w ',[StudentId, Course]), nl.

/*
isHandicapped(S, C) :-
    classroom(C,_, CH),
    (S == no ; (S == yes, CH == hcapped)).
*/

/*End recursive rules for capacity*/
getCap(_ , 102 , 10, 10) :- 
    write(" 102 has full capacity :( ").
getCap(_, 231, 6, 6) :- 
    write(" 231 has full capacity :( ").
getCap(_, 241, 5, 5) :- 
    write(" 241 has full capacity :( ").
getCap(_, 101, 10, 10) :- 
    write(" 101 has full capacity :( ").

/* Get the current capacity of the course*/
getCap(COUNTER, CourseId, X, Capacity) :- 
    COUNTER is COUNTER+1,
    student(COUNTER, CourseId, _),
    Next is Prev + 1,
    CourseId is CourseId+1,
    getCap(COUNTER, CourseId, Next, Capacity) ;
    COUNTER is COUNTER+1, 
    getCap(COUNTER, CourseId, Prev, Capacity).

