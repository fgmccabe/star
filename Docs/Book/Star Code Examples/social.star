social is package {
  -- Notifying dataSource actor using goSignal starts the transmission of data to the activeModel actor
  type signal is goSignal;

  main() do {
        notify dataSource with goSignal on signalChannel;
    };

  -- Names are a person-representation building block
  -- Note: Assume that every name can be treated as a monomym or a "standard" polynym
  type personalName is polynym{
    	surname has type string;
    	givenName has type string;
    } or mononym{
    	givenName has type string;
    };
    
  -- A function for printing names readably
  -- (Note: Defining an implementation of the pPrint contract is an alternative)
  name2string has type (personalName) => string;
  name2string(polynym{surname = S; givenName = G}) is "#G #S";
  name2string(mononym{givenName = G}) is G;
    
  -- Email addresses are both a person-recond and a message-record building block
  -- Note: Parsing email addresses is so easy -- see the pattern abstraction emailAddress below --
  --       that making them reconds is unnecessary
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
    
    -- A makePerson routine is required to build nodes, as not everything in the aggregate is the
    --   value of an attribute
    makePerson has type (personalName, emailAddress) => socialNetNode;
    makePerson(N, E) is actor{
        private var name := N;
        getName() is name;
    	on newName(newN) on updateChannel do {
    		name := newN;
    	};
    	
    	private var email := E;
    	getEmail() is email;
    	on newEmail(newE) on updateChannel do {
    		email := newE;
    	};	
    };
    
    -- A function for printing nodes readably
    node2string(A) is let {
    	N is query A's getName with getName();
--    	E has type emailAddress;  -- Needed in 2.1.2
    	E is query A's getEmail with getEmail();
    } in "#(name2string(N)) [#(query A's getEmail with getEmail())]"; 
    
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
    activeModel has type actor of {
    	newPersonChannel has type stream of socialNetNode;
    	newMessageChannel has type stream of socialNetLink;
    };
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
    
    dataSource is actor{
      on goSignal on signalChannel do {
    		-- DELTA
    		for P in persons do notify activeModel with P on newPersonChannel;
    		for M in messages do notify activeModel with M on newMessageChannel;
      } using {  
    		persons is relation{
    		  makePerson(polynym{givenName = "Ann"; surname = "Archer"}, "ann@aaa.com");
    		  makePerson(polynym{givenName = "Brian"; surname = "Bohm"}, "bb@bbb.com");
    		  makePerson(mononym{givenName = "Cher"}, "cher@cher.com");
    		  makePerson(polynym{givenName = "Daniel"; surname = "Davis"}, "dan@ddd.com");
    		  makePerson(polynym{givenName = "Emma"; surname = "Englund"}, "emma_englund@eee.net");
    		  makePerson(polynym{givenName = "Frederic"; surname = "Ford"}, "freddie@fff.com");
    		  makePerson(polynym{givenName = "Grace"; surname = "Goto"}, "grace_g@ggg.net");
    		  makePerson(polynym{givenName = "Harvey"; surname = "Harris"}, "hharris@hhh.com");
    		  makePerson(polynym{givenName = "Inga"; surname = "Ivers"}, "ivers@iii.com");
    		  makePerson(polynym{givenName = "Joe"; surname = "Jones"}, "joe@jjj.net");
	   		  makePerson(polynym{givenName = "Kathy"; surname = "Kramer"}, "kk@kkk.com");
	   		  makePerson(polynym{givenName = "Louis"; surname = "Li"}, "LouLi@lll.net");
	   		  makePerson(polynym{givenName = "Martha"; surname = "Myers"}, "mm@mmm.com");
	   		  makePerson(polynym{givenName = "Nathan"; surname = "Nelson"}, "nathan@nnn.net");
	   		  makePerson(polynym{givenName = "Olivia"; surname = "O'Connell"}, "oliviaoc@ooo.com");
	   		  makePerson(polynym{givenName = "Peter"; surname = "Potts"}, "potts@ppp.com");
	   		  makePerson(polynym{givenName = "Quinn"; surname = "Quine"}, "qq@qqq.net");
	   		  makePerson(polynym{givenName = "Richard Raymond"; surname = "Roberts"}, "richard@rrr.com");
	   		  makePerson(polynym{givenName = "Sally"; surname = "Spencer"}, "sally_spencer@sss.net");
	   		  makePerson(polynym{givenName = "Ted"; surname = "Tomita"}, "tomita@ttt.net");
	   		  makePerson(polynym{givenName = "Ursula"; surname = "Ulrich"}, "ursula@uuu.net");
	   		  makePerson(polynym{givenName = "Vincent"; surname = "Vandermeer"}, "vince@vvv.co.uk");
	    	  makePerson(polynym{givenName = "Wendy"; surname = "Wilson"}, "wwilson@www.net");
	    	  makePerson(polynym{givenName = "Xavier"; surname = "Xiao"}, "xx@xxx.org");
	    	  makePerson(polynym{givenName = "Yumi"; surname = "Young"}, "yyoung@yyy.com");
	    	  makePerson(polynym{givenName = "Zhi"; surname = "Zhang"}, "zhang@zzz.net");
	    	};
    	
	    	-- Some auxiliaries used in message generation
	    	startTime is 1319000000000L;                -- The beginning of the universe
	    	-- Used in initialization (on the order of 10 messages, sent at least mostly prior to the Cher special)
	    	sendTime is let {
	    		var lastSentTime := startTime;
	    		nextSendTime() is valof {
	    			lastSentTime := lastSentTime + random(100000000L);  -- 1/10 the time to the Cher special
	    			valis lastSentTime;
	    		}
	    	} in nextSendTime;
	    	eventTime0 is 1320000000000L;               -- 30 Oct 2011 (Cher TV special end time)
	    	sendTime0() is random(600000000L);          -- within a week
	    	replyTime0 is eventTime0 + 600000000L;      -- a week after (bulk email response time)
	    	eventTime1 is 1321000000000L;               -- early on 11 Nov 2011 (initial email time)
	    	maxRelayTime1 is 180000L;                   -- three minutes after 
	    	relayTime1() is 60000L + random(120000L);   -- between one and three minutes after 
        
	    	messages is relation{
	    		-- 
	    		-- Some meaningless initial noise, just to make the graph more interesting
	    		--
	    		message{
	    			fromAddress = "ann@aaa.com";
	    			toAddress = "bb@bbb.com";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "dan@ddd.com";
	    			toAddress = "cher@cher.com";
	    			content = "Hi!";
	    			time = sendTime();
    		    };
    		 	message{
    		 		fromAddress = "emma_englund@eee.net";
    		 		toAddress = "freddie@fff.com";
    		 		content = "Hi!";
    		 		time = sendTime();
    		    };
    		    message{
    		    	fromAddress = "hharris@hhh.com";
    		    	toAddress = "grace_g@ggg.net";
    		    	content = "Hi!";
    		    	time = sendTime();
    		    };
    		    message{
    		    	fromAddress = "ivers@iii.com";
	    			toAddress = "joe@jjj.net";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "LouLi@lll.net";
	    			toAddress = "kk@kkk.com";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "mm@mmm.com";
	    			toAddress = "nathan@nnn.net";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "potts@ppp.com";
	    			toAddress = "oliviaoc@ooo.com";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "qq@qqq.net";
	    			toAddress = "richard@rrr.com";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "tomita@ttt.net";
	    			toAddress = "sally_spencer@sss.net";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "ursula@uuu.net";
	    			toAddress = "vince@vvv.co.uk";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "xx@xxx.org";
	    			toAddress = "wwilson@www.net";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		message{
	    			fromAddress = "yyoung@yyy.com";
	    			toAddress = "zhang@zzz.net";
	    			content = "Hi!";
	    			time = sendTime();
	    		};
	    		-- Cher becomes a star: She receives mail from and sends mail to half the population 
	    		--
	    		message{
	    			fromAddress = "ann@aaa.com";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "ann@aaa.com";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "emma_englund@eee.net";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "emma_englund@eee.net";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "grace_g@ggg.net";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "grace_g@ggg.net";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "ivers@iii.com";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "ivers@iii.com";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "kk@kkk.com";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "kk@kkk.com";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "mm@mmm.com";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "mm@mmm.com";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "oliviaoc@ooo.com";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "oliviaoc@ooo.com";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "qq@qqq.net";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "qq@qqq.net";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "sally_spencer@sss.net";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "sally_spencer@sss.net";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "ursula@uuu.net";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan."
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "ursula@uuu.net";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "wwilson@www.net";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan.";
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "wwilson@www.net";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		message{
	    			fromAddress = "yyoung@yyy.com";
	    			toAddress = "cher@cher.com";
	    			content = "Just thought I'd write to say that I'm a big fan."
	    			time = sendTime0();
	    		};
	    		message{
	    			fromAddress = "cher@cher.com";
	    			toAddress = "yyoung@yyy.com";
	    			content = "Thanks!  I always enjoy hearing from a fan.";
	    			time = replyTime0;
	    		};
	    		-- 
	    		-- Brian is root: His email triggers email that triggers email that ...
	    		-- 
	    		message{
	    		    fromAddress = "bb@bbb.com";
	                toAddress = "dan@ddd.com";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1;
	            };
	    	    message{
	    		    fromAddress = "bb@bbb.com";
	                toAddress = "freddie@fff.com";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1;
	    	    };
	    	    message{
	    		    fromAddress = "dan@ddd.com";
	                toAddress = "hharris@hhh.com";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + relayTime1();
	    	    };
	    	    message{
	    		    fromAddress = "dan@ddd.com"; 
	                toAddress = "joe@jjj.net";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + relayTime1();
	    	    };
	   	        message{
	   		        fromAddress = "freddie@fff.com";
	                toAddress = "LouLi@lll.net";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + relayTime1();
	   	        };
	   	        message{
	   	        	fromAddress = "freddie@fff.com"; 
	                toAddress = "nathan@nnn.net";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + relayTime1();
	   	        };
	   	        -- Adding maxRelayTime1 ensures that no relays occur prior to the message receptions that "trigger" them
	   	        message{
	   		        fromAddress = "hharris@hhh.com";
	                toAddress = "potts@ppp.com";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + maxRelayTime1 + relayTime1();
	   	        };
	   	        message{
	   		        fromAddress = "hharris@hhh.com";
	                toAddress = "richard@rrr.com";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + maxRelayTime1 + relayTime1();
	   	        };
	   	        message{
	   		        fromAddress = "joe@jjj.net";
	                toAddress = "tomita@ttt.net";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + maxRelayTime1 + relayTime1();
	   	        };
	            message{
	   		        fromAddress = "joe@jjj.net";
	                toAddress = "vince@vvv.co.uk";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + maxRelayTime1 + relayTime1();
	   		    };
	    	    message{
	    		    fromAddress = "LouLi@lll.net";
	                toAddress = "xx@xxx.org";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + maxRelayTime1 + relayTime1();
	    	    };
	    	    message{
	    		    fromAddress = "LouLi@lll.net";
	                toAddress = "zhang@zzz.net";
	                content = "Meet at Rick's at 8 p.m.";
	                time = eventTime1 + maxRelayTime1 + relayTime1();
	    	    };
	       };
	    };
	};

}
