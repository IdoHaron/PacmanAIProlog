from pyswip import Prolog
prolog = Prolog()
prolog.consult("MainPl.pl")
prolog.assertz("father(michael,john)")
for res in prolog.query("father(X, Y)."):
    print(res);