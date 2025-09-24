/+  pile-rule=language-server-parser, dprint
|%
++  parse-pile
  ~/  %parse-pile
  |=  [pax=path tex=tape]
  ^-  pile:clay
  =/  [=hair res=(unit [=pile:clay =nail])]
    %-  road  |.
    ((pile-rule pax) [1 1] tex)
  ?^  res  pile.u.res
  %-  mean
  =/  lyn  p.hair
  =/  col  q.hair
  ^-  (list tank)
  :~  leaf+"syntax error at [{<lyn>} {<col>}] in {<pax>}"
    ::
      =/  =wain  (to-wain:format (crip tex))
      ?:  (gth lyn (lent wain))
        '<<end of file>>'
      (snag (dec lyn) wain)
    ::
      leaf+(runt [(dec col) '-'] "^")
  ==
++  print-hover
  |=  [id=term typ=type spt=spot]
  =/  to-display  %-  mule  |.((find-item-in-type:dprint ~[id] typ))
  :: ~&  >  todis=to-display
  :: =/  output=(list sole-effect)
  =/  output
    ?:  ?=(%| -.to-display)
        [%tan [%leaf "Could not find help A"] p.to-display]~
    ?~  p.to-display
        [%tan [%leaf "Could not find help B"] p.to-display]~
    =/  item  (mule |.((print-item:dprint u.p.to-display)))
    ?:  ?=(%| -.item)
      [%tan [%leaf "Could not find help C"] p.item]~
    p.item
  output
++  find-spot
  |=  [id=term typ=type]  ^-  (unit spot)
  =/  to-display  %-  mule  |.((find-item-in-type:dprint ~[id] typ))
    ?:  ?=(%| -.to-display)  ~
    =/  spt  (find-spot-in-item:dprint p.to-display id)
    spt

++  analyze-type
:: NOTE there's an arm with this very name at xray.hoon
|=  =type
  =/  slo  (sloe type)
  ~&  sloe=slo
?@  type  ~&  noun-type=type
~
          ~&  cell-type=-.type
?-  -.type
  %atom  ~&  atom=+.type  ~
  %cell  ~&  [$(type p.type) $(type q.type)]  ~
  %core  ~&  garb=p.q.type  ~&  keys=~(key by q.r.q.type)  ~
  %face  ~&  face=p.type  ~
  %fork  ~&  ~(wyt in p.type)  ~
  %hint  ~&  note=q.p.type  ~
  %hold  ~&  hoon=q.type  ~
==
++  hoon-spot  =|  retries=@
  |=  hon=hoon  ^-  (unit spot)
  ?:  (gth retries 5)  ~
  ?+  hon
    =/  doz  ~(open ap hon)
    ?:  =(doz hon)  $(hon doz, retries +(retries))  $(hon doz)
    ::
    [%dbug *]  (some p.hon)
:: [^ *]  $(hon p.hon)  :: TODO replace-both
  :: [%base p=base]     
  :: [%bust p=base]   
  :: [%hand p=type q=nock]
  :: [%note p=note q=hoon]  $(hon q.hon)
  :: [%fits p=hoon q=wing]  $(hon p.hon)
  :: [%knit p=(list woof)]  
  :: [%leaf p=(pair term @)]   
  :: [%limb p=term]    
::   [%lost p=hoon]      $(hon p.hon)
::   [%rock p=term q=*]                                  
::   [%sand p=term q=*]                                  
::   [%tell p=(list hoon)]                               
::   [%tune p=$@(term tune)]                             
::   [%wing p=wing]                                      
::   [%yell p=(list hoon)]                               
::   [%xray p=manx:hoot]                                 
:: ::                                            ::::::  
::   [%brbc sample=(lest term) body=spec]                
::   [%brcb p=spec q=alas r=(map term tome)]             
::   [%brcl p=hoon q=hoon]                               
::   [%brcn p=(unit term) q=(map term tome)]             
::   [%brdt p=hoon]                                      
::   [%brkt p=hoon q=(map term tome)]                    
::   [%brhp p=hoon]                                      
::   [%brsg p=spec q=hoon]                               
::   [%brtr p=spec q=hoon]                               
::   [%brts p=spec q=hoon]                               
::   [%brpt p=(unit term) q=(map term tome)]             
::   [%brwt p=hoon]                                      
:: ::                                            ::::::  
::   [%clcb p=hoon q=hoon]                               
::   [%clkt p=hoon q=hoon r=hoon s=hoon]                 
::   [%clhp p=hoon q=hoon]                               
::   [%clls p=hoon q=hoon r=hoon]                        
::   [%clsg p=(list hoon)]                               
::   [%cltr p=(list hoon)]                               
:: ::                                            ::::::  
::   [%cncb p=wing q=(list (pair wing hoon))]            
::   [%cndt p=hoon q=hoon]                               
::   [%cnhp p=hoon q=hoon]                               
::   [%cncl p=hoon q=(list hoon)]                        
::   [%cntr p=wing q=hoon r=(list (pair wing hoon))]     
::   [%cnkt p=hoon q=hoon r=hoon s=hoon]                 
::   [%cnls p=hoon q=hoon r=hoon]                        
::   [%cnsg p=wing q=hoon r=(list hoon)]                 
::   [%cnts p=wing q=(list (pair wing hoon))]            
:: ::                                            ::::::  
::   [%dtkt p=spec q=hoon]                               
::   [%dtls p=hoon]                                      
::   [%dttr p=hoon q=hoon]                               
::   [%dtts p=hoon q=hoon]                               
::   [%dtwt p=hoon]                                      
:: ::                                            ::::::  
::   [%ktbr p=hoon]                                      
::   [%ktdt p=hoon q=hoon]                               
::   [%ktls p=hoon q=hoon]                               
::   [%kthp p=spec q=hoon]                               
::   [%ktpm p=hoon]                                      
::   [%ktsg p=hoon]                                      
::   [%ktts p=skin q=hoon]                               
::   [%ktwt p=hoon]                                      
::   [%kttr p=spec]                                      
::   [%ktcl p=spec]                                      
:: ::                                            ::::::  
::   [%sgbr p=hoon q=hoon]                               
::   [%sgcb p=hoon q=hoon]                               
::   [%sgcn p=chum q=hoon r=tyre s=hoon]                 
::   [%sgfs p=chum q=hoon]                               
::   [%sggl p=$@(term [p=term q=hoon]) q=hoon]           
::   [%sggr p=$@(term [p=term q=hoon]) q=hoon]           
::   [%sgbc p=term q=hoon]                               
::   [%sgls p=@ q=hoon]                                  
::   [%sgpm p=@ud q=hoon r=hoon]                         
::   [%sgts p=hoon q=hoon]                               
::   [%sgwt p=@ud q=hoon r=hoon s=hoon]                  
::   [%sgzp p=hoon q=hoon]                               
:: ::                                            ::::::  
::   [%mcts p=marl:hoot]                                 
::   [%mccl p=hoon q=(list hoon)]                        
::   [%mcfs p=hoon]                                      
::   [%mcgl p=spec q=hoon r=hoon s=hoon]                 
::   [%mcsg p=hoon q=(list hoon)]                        
::   [%mcmc p=spec q=hoon]                               
:: ::                                            ::::::  
::   [%tsbr p=spec q=hoon]                               
::   [%tscl p=(list (pair wing hoon)) q=hoon]            
::   [%tsfs p=skin q=hoon r=hoon]                        
::   [%tsmc p=skin q=hoon r=hoon]                        
::   [%tsdt p=wing q=hoon r=hoon]                        
::   [%tswt p=wing q=hoon r=hoon s=hoon]                 
::   [%tsgl p=hoon q=hoon]                               
::   [%tshp p=hoon q=hoon]                               
::   [%tsgr p=hoon q=hoon]                               
::   [%tskt p=skin q=wing r=hoon s=hoon]                 
::   [%tsls p=hoon q=hoon]                               
::   [%tssg p=(list hoon)]                               
::   [%tstr p=(pair term (unit spec)) q=hoon r=hoon]     
::   [%tscm p=hoon q=hoon]                               
:: ::                                            ::::::  
::   [%wtbr p=(list hoon)]                               
::   [%wthp p=wing q=(list (pair spec hoon))]            
::   [%wtcl p=hoon q=hoon r=hoon]                        
::   [%wtdt p=hoon q=hoon r=hoon]                        
::   [%wtkt p=wing q=hoon r=hoon]                        
::   [%wtgl p=hoon q=hoon]                               
::   [%wtgr p=hoon q=hoon]                               
::   [%wtls p=wing q=hoon r=(list (pair spec hoon))]     
::   [%wtpm p=(list hoon)]                               
::   [%wtpt p=wing q=hoon r=hoon]                        
::   [%wtsg p=wing q=hoon r=hoon]                        
::   [%wthx p=skin q=wing]                               
::   [%wtts p=spec q=wing]                               
::   [%wtzp p=hoon]                                      
:: ::                                            ::::::  
::   [%zpcm p=hoon q=hoon]                               
::   [%zpgr p=hoon]                                      
::   [%zpgl p=spec q=hoon]                               
::   [%zpmc p=hoon q=hoon]                               
::   [%zpts p=hoon]                                      
::   [%zppt p=(list wing) q=hoon r=hoon]                 
::   [%zpwt p=$@(p=@ [p=@ q=@]) q=hoon]                  
::   [%zpzp ~]                                           
==
--
