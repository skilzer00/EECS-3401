 :- op(100,xfy,on).
makekid(0,[]).
makekid(N,[kid(_Firstname,_Lastname,_Age,_Gender)| List])
 :- N>0, N1 is N-1, makekid(N1,List).
X on List :- member(X,List).

find(List):- %facts
    makekid(3,List),
    kid('Angela',_, _,female) on List,
    kid('David',_,_,male) on List,
    kid('Mary',_,_,_) on List,
    kid(_,_,5,_) on List,
    kid(_,_,7,_) on List,
    kid(_,_,8,_) on List,
    kid(_,'Diamond',_,_) on List,
    kid(_,'Grant',_age1,_) on List,
    kid(_,'Leung',_age2,_) on List,
    _age1 is _age2+3.
