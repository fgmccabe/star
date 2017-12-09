component is package {
  import ports;

/***********************************************
	import component;
	myComponent is component{ 
		 portI has type respond { 
		    IN has type stream of (string, %t) 
		 }; 
		 
		 portQ has type respond { 
		    QQ has type relation of (string, %t) 
		 }; 
		 
		 portO is originate { 
		    Alerts has type stream of (string, level, %t) 
		 };
		 
		 attribute count has type int;
		 count default is 5;
	}
************************************************/
  #prefix((attribute), 999);
  #force(prefix((import),999)); 
  
  #?I is component{?Specs} :: Package :- Specs;*componentBit ## {
     #?P has type respond {?channel} :: componentBit :- channel ;* typeAnnotation;
     #?P is originate {?channel} :: componentBit :- channel ;* typeAnnotation;
     #?A has type attributes {?attrib} :: componentBit :- attrib ;* typeAnnotation;
     
     # import ?I :: componentBit :- I::expression;
     # identifier default is ?E :: componentBit :- E::expression;
     # attribute identifier has type ?T :: componentBit :- T::typeExpression;
     #?anything :: componentBit  :- anything :: statement;
  
  };

  #?N is component{?statements} ==> write(N,scan(statements)) ## {
  

	-- Scan all the statements, if more than one, split, scan and join the results
    #scan(#(?L;?R)#) ==> join(scan(L), scan(R));

  	-- Parameterized imports are kept for inside the package
   	#scan(#(import ?I #@ ?P)#) ==> (((),importStmtParams,(I,P)), ());
  	-- Import statements have to be kept for inclusion at the top of the package
   	#scan(#(import ?I)#) ==> (((),importStmt,I), ());
    -- Pick out respond and originate type declarations and store in Dictionary
   	#scan(#(?P has type respond { ?C })#) ==> ((P,respond,C),());
   	#scan(#(?P is originate { ?C })#) ==> ((P,originate,C),());
   	-- Pick out respond port declarations. Store in Dictionary and Archive
   	#scan(#(?P is respond { ?C })#?L) ==> ((P,respondDeclaration,C),L);
   	-- store any recognized functions
   	#scan(#(startActors() {?X} )#?L) ==> ((startActors,function,()), L);
   	#scan(#(startActors() do ?X )#?L) ==> ((startActors,function,()), L);
   	#scan(#(stopActors() {?X} )#?L) ==> ((stopActors,function,()), L);
   	#scan(#(stopActors() do ?X )#?L) ==> ((stopActors,function,()), L);
   	#scan(#(pauseActors() {?X} )#?L) ==> ((pauseActors,function,()), L);
   	#scan(#(pauseActors() do ?X )#?L) ==> ((pauseActors,function,()), L);
   	-- Attributes
  	#scan(#(attribute ?A has type ?T)#) ==> ((A,attr,T),());
  	#scan(#(?A default is ?V)#) ==> ((A,attrDefault,V),());
   	-- Other statements go in the Archive
   	#scan(?S) ==> ((),S);
   	
   	-- Join the Dictionaries and Archives
   	#join((?D1,?A1),(?D2,?A2)) ==> (cat(D1, D2), cat(A1, A2));
   	
   	-- Catenate two tuples
   	#cat(?A, ()) ==> A;
   	#cat((), ?B) ==> B;
   	#cat(?A, ?B) ==> (A, B);
   	
   	-- Write out the new file
   	#write(?PackageName, (?D, ?Code)) ==> #( 
   		PackageName is package { 

   			emit(D,imports);
   			emit(D,portDecs);

			type attributeValues is alias of braces(D,attrTypes);
   			type originatePorts is alias of braces(D,portTypes);   			
 			
   			PackageName(#(attributes has type attributeValues)#, #(ports)#) is let {

   				emit(D,importsParams);
   				
   				emit(D,assignAttrs);
   				emit(D,assignPorts);
   				emitCode(D,Code);
  			} in {
   				emit(D,returnValues); 
   			};
  			
   			StarComponentVersion() is 1;
   			
   			StarOriginatingPortNames() is list of braces(D,oportNames);
   			StarRespondingPortNames() is list of braces(D,rportNames);
   			StarPortTypes() is braces(D,portStringTypes,dummy);
   			StarAttributeNames() is list of braces(D,attrNames);
   			StarAttributeTypes() is braces(D,attrStringTypes,dummy);
   			StarAttributeDefaultNames() is list of braces(D,attrDefaultNames);
   			StarAttributeDefaultValues() is braces(D,attrDefaultValues,dummy);

   		} 
   	)#;

	-- Supporting macros to build stuff for the resultant file
	-- use as: getData(D, macroName);
	-- macro(name,respond|originate|function|attr,channel info|value) 
	
   	#imports((),importStmt,?I) ==> #(import I)#;
   	#imports(?P,?X,?C) ==> ()

   	#importsParams((),importStmtParams,(?I,?P)) ==> #(import I @ P)#;
   	#importsParams(?P,?X,?C) ==> ()

	#portDecs(?P,respond,?C) ==> #(
   		type P#+_respondRecordType is alias of { C };
    	type P#+_respondType is alias of port of P#+_respondRecordType;
 		P has type P#+_respondType
  	)#; 
   	#portDecs(?P,originate,?C) ==> #(
   		type P#+_originateRecordType is alias of { C };
    	type P#+_originateType is alias of port of P#+_originateRecordType
    )#;
   	#portDecs(?P,?X,?C) ==> ();
   	
   	#portTypes(?P,originate,?C) ==> #(
 		P has type P#+_originateType
   	)#; 
   	#portTypes(?P,?X,?C) ==> ()
		
   	#oportNames(?P,originate,?C) ==> $$P;
   	#oportNames(?P,?X,?C) ==> ()
   	
   	#rportNames(?P,respondDeclaration,?C) ==> $$P;
   	#rportNames(?P,?X,?C) ==> ()
   	
   	#attrNames(?A,attr,?T) ==> $$A;
   	#attrNames(?P,?X,?C) ==> ()

   	#attrName(?A,attr,?T) ==> A;
   	#attrName(?P,?X,?C) ==> ()

   	#matchAttrName(?A,(?D,?Code),?V) ==> matchAttrName1(A,D,V);
   	#matchAttrName1(?A,(?D),?V) ==> matchAttrName1(A,D,V);
   	#matchAttrName1(?A,(?A,attr,?T),?V) ==> matchAttrName2(A,V);
   	#matchAttrName1(?A,((?A,attr,?T),?D),?V) ==> matchAttrName2(A,V);
   	#matchAttrName1(?A,(?E,?D),?V) ==> matchAttrName1(A,D,V);
   	#matchAttrName1(?A,?X,?V) ==> ();
   	
   	#matchAttrName2(?A,()) ==> $$A;
   	#matchAttrName2(?A,?V) ==> A=asString(V);
   	
   	#asString(string?S) ==> S;
   	#asString(?S) ==> $$S;
   	
   	-- Maybe this is better
   	-- #matchAttrName(?A,(?D,?Code),?V) ==> matchAttrName(A, getData(D, attrName),?V);
   	-- #matchAttrName(?A,#(?A;?X;?Y;?Z)#,()) ==> $$A;
   	-- #matchAttrName(?A,#(?A;?X;?Y;?Z)#,?V) ==> A=asString(V);
   	-- #matchAttrName(?A,#(?B;?X;?Y;?Z)#,?V) ==> ();
   	-- Why doesn't the following line work?
   	-- #matchAttrName(?A,?X,?V) ==> ()
   	
   	
   	#attrTypes(?A,attr,?T) ==> #(A has type T)#; 
   	#attrTypes(?P,?X,?C) ==> ()

   	#attrStringTypes(?A,attr,?T) ==> A=$$T;
   	#attrStringTypes(?P,?X,?C) ==> ()
   	 
   	#attrDefaultNames(?A,attrDefault,?V) ==> matchAttrName(A,scan(statements),());
   	#attrDefaultNames(?P,?X,?C) ==> ()
   	
   	#attrDefaultValues(?A,attrDefault,?V) ==> matchAttrName(A,scan(statements),V);
   	-- #attrDefaultValues(?A,attrDefault,?V) ==> A=$$V;
   	#attrDefaultValues(?P,?X,?C) ==> ()
   	
   	#assignAttrs(?A,attr,?T) ==> #(A is attributes.A)#; 
   	#assignAttrs(?P,?X,?C) ==> ()
   	
   	#portStringTypes(?P,respond,?C) ==> P=$$C; 
   	#portStringTypes(?P,originate,?C) ==> P=$$C; 
   	#portStringTypes(?P,?X,?C) ==> (); 
   	
   	#assignPorts(?P,originate,?C) ==> #(P is ports.P)#; 
   	#assignPorts(?P,?X,?C) ==> ()

   	#returnValues(?P,respondDeclaration,?C) ==> #(P = P)#; 
   	#returnValues(?P,function,?C) ==> #(P = P)#; 
   	#returnValues(?P,?X,?C) ==> ()
   	
   	-- Make sure we don't get a null
   	-- If M(D) is null emit a valid statement
   	emit(?D,?M) ==> emit1(#*getData(D,M),M);
   	emit1(?X,?M) ==> ifNull(X, #(M is $$#(no#+M)#)#)
   	
   	-- Make sure we don't get a null
   	-- If D(M) is null emit Y
   	-- If X is null emit Y
   	ifNull(?D,?M,?Y) ==> ifNull(#*getData(D,M),Y);
   	ifNull(?X,?Y) ==> ifNull1(#*clean(X),Y);
   	ifNull1((),?Y) ==> Y;
   	ifNull1(?X,?Y) ==> X;
   	
	-- Remove blank entries from ';' separated list
	-- Args: List, Continuation
	#clean(?X) ==> rmNull(X,());
   	#rmNull(#(?X;)#, ?Cont) ==> rmNull(X,Cont);
   	#rmNull(#(();?R)#, ?Cont) ==> rmNull(R,Cont);
   	#rmNull(#(?L;?R)#, ()) ==> rmNull(R,L);
   	#rmNull(#(?L;?R)#, ?Cont) ==> rmNull(R,#(Cont;L)#);
	#rmNull(?X,()) ==> X;
   	#rmNull((),?Cont) ==> Cont;
	#rmNull(?X,?Cont) ==> #(Cont;X)#;
	
	-- return something in braces or default record
	braces(?D,?M,?T) ==> dBraces(#*braces(D,M),T);
	dBraces(#({})#,?T) ==> { T=$$T };
	dBraces(?B,?T) ==> B; 
	-- return something in braces or empty braces
	braces(?D,?M) ==> braces(#*getData(D,M)); 
	braces(?X) ==> cbraces(#*clean(X));
	cbraces((?X)) ==> cbraces(X);
	cbraces(()) ==> {}
	cbraces(?X) ==> { X }
   	   	
   	-- get data from the dictionary
   	-- Args: Dictionary, respond|originate|function, macro
   	#getData((?D),?M) ==> getData(D,M); 
   	#getData((?D,?DR),?M) ==> #(getData(D,M); getData(DR,M))#; 
   	#getData((?P,?T,?C),?M) ==> M(P,T,C);
   	
   	-- get the user code and munge any port code used
   	-- Args: Dictionary, statements
   	#emitCode(?D, ?Code) ==> ifNull( #*emitCode1(D, Code), #(UserCode is "NoUserCode")#);
   	#emitCode1(?D, (?Code,?CodeR)) ==> #( emitCode1(D,Code); emitCode1(D,CodeR) )#;
   	#emitCode1(?D, #(?P is respond { ?C })#) ==> #(P is p0rt { channel(D,P,respond,()); C})# 
   	#emitCode1(?D, ?Code) ==> Code; 
   	
   	-- Get named channel definitions from the dictionary
   	#channel((?P,?T,?C),?P,?T,?D) ==> C;
   	#channel((?D,?DR),?P,?T,?R) ==> channel(D,P,T,(DR,R));
   	#channel(?X,?P,?T,(?D,?DR)) ==> channel(D,P,T,DR);

  };
  

}