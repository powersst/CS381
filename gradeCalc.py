class CourseWork(object):
    def __init__(self, name=None, earned=0.00, possible=0.00, adjustment=0.00):
        self.name = name
        self.earned = earned
        self.possible = possible

class WorkType(object):
    def __init__(self, name=None, weight=0.00, entries=[]):
        self.name = name
        self.weight = weight
        self.entries = entries

hw1 = CourseWork("HW1", 32.0, 32.0)
hw2 = CourseWork("HW2", 18.0, 20.0)
hw3 = CourseWork("HW3", 15.0, 10.0)

qz1 = CourseWork("QZ1", 5.0, 10.0)
qz2 = CourseWork("QZ1", 6.0, 14.0, 2.5)

mt1 = CourseWork("MT1", 28.0, 44.0, 3.5)
mt2 = CourseWork("MT2", 26.0, 40.0, 3.0)

fin = CourseWork("FIN", 0.0, 0.0, 0.0)

homework = WorkType("Homework", 0.25, [hw1, hw2, hw3])
quizzes = WorkType("Quizzes", 0.15, [qz1, qz2])
midterms = WorkType("Midterms", 0.30, [mt1,mt2])
final = WorkType("Final", 0.30, [fin])

workTypes = [homework,quizzes,midterms,final]

for i in workTypes:
    print(i.name + " = " + str(i.weight))
