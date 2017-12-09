sbug is package {
  type personalName is polynym{
    	surname has type string;
    	givenName has type string;
    } or mononym{
    	givenName has type string;
    };
    
  name2string has type (personalName) => string;
  name2string(polynym{surname = S; givenName = G}) is "#G #S";
  name2string(mononym{givenName = G}) is G;
    
  type emailAddress is alias of string; 
         
  -- A function for generating emailAddress values                                       
   makeEmailAddress has type (string, string) => string;                                       
   makeEmailAddress(L, D) is L ++ "@" ++ D;
    
   -- A pattern abstraction for generating patterns that match emailAddress values                                  
   emailAddress has type (string, string) <= string;
   emailAddress(L, D) from (_ matching `(.*:L)@(.*:D)`);
    
   -- Nodes will respond to these update events
   type updateEvent is newName(personalName) or newEmail(emailAddress);
    
   type socialNetNode is alias of actor of {
     getName has type () => personalName;
     getEmail has type () => emailAddress;
     updateChannel has type stream of updateEvent;
   };
    
    -- A function for printing nodes readably
    node2string(A) is let {
    	N is query A's getName with getName();
    	E has type emailAddress;  -- Needed in 2.1.2
    	E is query A's getEmail with getEmail();
    } in "#(name2string(N)) [#E]"; 
    
    -- Links between person-nodes are constructed using message
    type socialNetLink is message{
    	fromAddress has type emailAddress;
    	toAddress has type emailAddress;
    	content has type string;
    	time has type long;
    };
    
    -- A function for printing links readably
    link2string has type (socialNetLink) => string;
    link2string(message{fromAddress = fromA; toAddress = toA; content = C; time = T}) 
      is "Email from #fromA to #toA (at $T): \"#(firstNChars(12, C))...\"";
    
    -- A utility for selecting the first N characters of string S
    -- Note: Generally useful, so not made local to link2string
    firstNChars has type (integer, string) => string;
    firstNChars(positive(N), S) where S matches`(.:C)(.*:restS)` is C ++ firstNChars(N - 1, restS);
    firstNChars(_, _) default is "";
        
    -- A utility for restricting integer matches to positive values
    -- Note: Generally useful, so not made local to firstNChars
    positive has type (integer) <= integer;
    positive(N) from (N where N > 0);
      
    -- Top-level type definition: A social network is a table of nodes plus a table of links
    type socialNetworkType is alias of {
      nodeTable has type relation of socialNetNode;
      linkTable has type relation of socialNetLink;
    };
    
    -- socialNetwork is "wrapped" with an actor
   /*
 activeModel has type actor of {
    	newPersonChannel has type stream of socialNetNode;
    	newMessageChannel has type stream of socialNetLink;
    };
*/
    activeModel is actor{
    	private unsafe var socialNetwork := {
    		nodeTable = relation{}; 
    		linkTable = relation{};
    	};
    	
    	on P on newPersonChannel do {
    		-- Next line is temporary (for tracking progress)
    		logMsg(info, "activeModel notified of new person: #(node2string(P))");
    		extend socialNetwork.nodeTable with P;
    	};
    	
    	on M on newMessageChannel do {
    		-- Next line is temporary (for tracking progress)
    		logMsg(info, "activeModel notified of new email message: #(link2string(M))");
    		extend socialNetwork.linkTable with M;
    	};
    	
    };
}
