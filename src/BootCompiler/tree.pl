:- use_module(ocall).
'tree#export'("I6'em'U'tree*tree'1_'<>':k't'F2Lk't'Lk't'Lk't''nd':k'T'C3U'tree*tree'1k'T'k'T'U'tree*tree'1k'T'U'tree*tree'1k'T''walk':k't'F1U'tree*tree'1k't'Lk't''t'U'tree*tree'1S'u'U'tree*tree'1S").
'tree#types'("I1'tree'T2:k'T'YU'tree*tree'1k'T't'lo.std*thing':k'T'YU'tree*tree'1k'T'I1'leaves'F0Lk'T'").
'tree@assert'() :- 'tree@u'(XX669),
    ocall('leaves%1'(XX668),XX669,XX669),
    'tree@t'(XX671),
    ocall('leaves%1'(XX670),XX671,XX671),
    XX668 = XX670.
'tree#em'('leaves%1'(XV31), XLbl31, XThis31) :- !,
    'tree#em@leaves'(XV31, XLbl31, XThis31).
'tree#em@leaves'([], XLbV21, XThV21) :- XLbV21 = 'tree#em',
    !.
'tree#em@leaves'(_, _, _) :- abort.
'tree@<>'([], XX, XX) :- !.
'tree@<>'([XE | XX], XY, [XE | XX623]) :- !,
    'tree@<>'(XX, XY, XX623).
'tree@<>'(_, _, _) :- abort.
'tree#nd'('label%1'(XV32), XLbl32, XThis32) :- !,
    'tree#nd@label'(XV32, XLbl32, XThis32).
'tree#nd'('leaves%1'(XV33), XLbl33, XThis33) :- !,
    'tree#nd@leaves'(XV33, XLbl33, XThis33).
'tree#nd@label'(XB, XLbV22, XThV22) :- XLbV22 = 'tree#nd'(XL, XB, XR),
    !.
'tree#nd@label'(_, _, _) :- abort.
'tree#nd@leaves'(XX638, XLbV22, XThV22) :- XLbV22 = 'tree#nd'(XL, XB, XR),
    !,
    ocall('leaves%1'(XX630),XL,XL),
    ocall('leaves%1'(XX635),XR,XR),
    'tree@<>'([XB], XX635, XX637),
    'tree@<>'(XX630, XX637, XX638).
'tree#nd@leaves'(_, _, _) :- abort.
'tree@walk'('tree#nd'(XL, XB, XR), XX651) :- !,
    'tree@walk'(XL, XX644),
    'tree@walk'(XR, XX649),
    'tree@<>'([XB], XX649, XX650),
    'tree@<>'(XX644, XX650, XX651).
'tree@walk'('tree#em', []) :- !.
'tree@walk'(_, _) :- abort.
'tree@t'('tree#nd'('tree#nd'('tree#em', "a", 'tree#em'), "b", 'tree#nd'('tree#em', "c", 'tree#em'))) :- !.
'tree@u'('tree#nd'('tree#em', "a", 'tree#nd'('tree#em', "b", 'tree#nd'('tree#em', "c", 'tree#em')))) :- !.
