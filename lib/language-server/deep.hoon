|%
++  spec-spot  |=  s=spec  ^-  (unit spot)
  ?+  -.s  ~
  %dbug   (some p.s)
  %gist   $(s q.s)         
  %made   $(s q.s)  
  %make               (hoon-spot p.s)  
  %name                      $(s q.s)
  %over                      $(s q.s)
  %bcbc           $(s p.s)
  %bcbr                      $(s p.s)
  %bccl          $(s i.p.s) 
  %bccn         $(s i.p.s)  
  %bcdt         $(s p.s)  
  %bcgr      (both2 p.s q.s)
  %bcgl      (both2 p.s q.s)
  %bchp      (both2 p.s q.s)
  %bckt      (both2 p.s q.s)
  %bcls                    $(s q.s)  
  %bcfs         $(s p.s)
  %bcmc       (hoon-spot p.s)  
  %bccb       (hoon-spot p.s)
  %bcpm                    $(s p.s)
  %bcsg                    $(s q.s)  
  %bctc         $(s p.s)  
  %bcts                    $(s q.s)  
  %bcpt                    $(s p.s)  
  %bcwt         $(s i.p.s)  
  %bczp         $(s p.s)  
  ==

++  both2  |=  [a=spec b=spec]
  (replace (spec-spot a) |.((spec-spot b)))

++  replace
  |*  [a=(unit) b=(trap (unit))]
  ^+  a
  ?~(a $:b a)
::
++  both
  |=  [a=hoon b=hoon]
  (replace (hoon-spot a) |.((hoon-spot b)))

++  hoon-spot  |=  h=hoon  ^-  (unit spot)
  ?+  -.h  ~
  %dbug  (some p.h)
  :: TODO save the note too might be useful
  %note   ::~&  >  note=h
          $(h q.h)           
  %ktcl   (spec-spot p.h)    
  %brcl  (both p.h q.h)
  %tsls  (both p.h q.h)
  %tsgr  (both p.h q.h)
  ==
::     [%base p=base]                                      ::  base spec
::     [%bust p=base]                                      ::  bunt base
::     [%dbug p=spot q=hoon]                               ::  debug info in trace
::     [%eror p=tape]                                      ::  assembly error
::     [%hand p=type q=nock]                               ::  premade result
::     [%fits p=hoon q=wing]                               ::  underlying ?=
::     [%knit p=(list woof)]                               ::  assemble string
::     [%leaf p=(pair term @)]                             ::  symbol spec
::     [%limb p=term]                                      ::  take limb
::     [%lost p=hoon]                                      ::  not to be taken
::     [%rock p=term q=*]                                  ::  fixed constant
::     [%sand p=term q=*]                                  ::  unfixed constant
::     [%tell p=(list hoon)]                               ::  render as tape
::     [%tune p=$@(term tune)]                             ::  minimal face
::     [%wing p=wing]                                      ::  take wing
::     [%yell p=(list hoon)]                               ::  render as tank
::     [%xray p=manx:hoot]                                 ::  ;foo; templating
::   ::                                            ::::::  cores
::     [%brbc sample=(lest term) body=spec]                ::  |$
::     [%brcb p=spec q=alas r=(map term tome)]             ::  |_
::     [%brcn p=(unit term) q=(map term tome)]             ::  |%
::     [%brdt p=hoon]                                      ::  |.
::     [%brkt p=hoon q=(map term tome)]                    ::  |^
::     [%brhp p=hoon]                                      ::  |-
::     [%brsg p=spec q=hoon]                               ::  |~
::     [%brtr p=spec q=hoon]                               ::  |*
::     [%brts p=spec q=hoon]                               ::  |=
::     [%brpt p=(unit term) q=(map term tome)]             ::  |@
::     [%brwt p=hoon]                                      ::  |?
::   ::                                            ::::::  tuples
::     [%clcb p=hoon q=hoon]                               ::  :_ [q p]
::     [%clkt p=hoon q=hoon r=hoon s=hoon]                 ::  :^ [p q r s]
::     [%clhp p=hoon q=hoon]                               ::  :- [p q]
::     [%clls p=hoon q=hoon r=hoon]                        ::  :+ [p q r]
::     [%clsg p=(list hoon)]                               ::  :~ [p ~]
::     [%cltr p=(list hoon)]                               ::  :* p as a tuple
::   ::                                            ::::::  invocations
::     [%cncb p=wing q=(list (pair wing hoon))]            ::  %_
::     [%cndt p=hoon q=hoon]                               ::  %.
::     [%cnhp p=hoon q=hoon]                               ::  %-
::     [%cncl p=hoon q=(list hoon)]                        ::  %:
::     [%cntr p=wing q=hoon r=(list (pair wing hoon))]     ::  %*
::     [%cnkt p=hoon q=hoon r=hoon s=hoon]                 ::  %^
::     [%cnls p=hoon q=hoon r=hoon]                        ::  %+
::     [%cnsg p=wing q=hoon r=(list hoon)]                 ::  %~
::     [%cnts p=wing q=(list (pair wing hoon))]            ::  %=
::   ::                                            ::::::  nock
::     [%dtkt p=spec q=hoon]                               ::  .^  nock 11
::     [%dtls p=hoon]                                      ::  .+  nock 4
::     [%dttr p=hoon q=hoon]                               ::  .*  nock 2
::     [%dtts p=hoon q=hoon]                               ::  .=  nock 5
::     [%dtwt p=hoon]                                      ::  .?  nock 3
::   ::                                            ::::::  type conversion
::     [%ktbr p=hoon]                                      ::  ^|  contravariant
::     [%ktdt p=hoon q=hoon]                               ::  ^.  self-cast
::     [%ktls p=hoon q=hoon]                               ::  ^+  expression cast
::     [%kthp p=spec q=hoon]                               ::  ^-  structure cast
::     [%ktpm p=hoon]                                      ::  ^&  covariant
::     [%ktsg p=hoon]                                      ::  ^~  constant
::     [%ktts p=skin q=hoon]                               ::  ^=  label
::     [%ktwt p=hoon]                                      ::  ^?  bivariant
::     [%kttr p=spec]                                      ::  ^*  example
::   ::                                            ::::::  hints
::     [%sgbr p=hoon q=hoon]                               ::  ~|  sell on trace
::     [%sgcb p=hoon q=hoon]                               ::  ~_  tank on trace
::     [%sgcn p=chum q=hoon r=tyre s=hoon]                 ::  ~%  general jet hint
::     [%sgfs p=chum q=hoon]                               ::  ~/  function j-hint
::     [%sggl p=$@(term [p=term q=hoon]) q=hoon]           ::  ~<  backward hint
::     [%sggr p=$@(term [p=term q=hoon]) q=hoon]           ::  ~>  forward hint
::     [%sgbc p=term q=hoon]                               ::  ~$  profiler hit
::     [%sgls p=@ q=hoon]                                  ::  ~+  cache=memoize
::     [%sgpm p=@ud q=hoon r=hoon]                         ::  ~&  printf=priority
::     [%sgts p=hoon q=hoon]                               ::  ~=  don't duplicate
::     [%sgwt p=@ud q=hoon r=hoon s=hoon]                  ::  ~?  tested printf
::     [%sgzp p=hoon q=hoon]                               ::  ~!  type on trace
::   ::                                            ::::::  miscellaneous
::     [%mcts p=marl:hoot]                                 ::  ;=  list templating
::     [%mccl p=hoon q=(list hoon)]                        ::  ;:  binary to nary
::     [%mcfs p=hoon]                                      ::  ;/  [%$ [%$ p ~] ~]
::     [%mcgl p=spec q=hoon r=hoon s=hoon]                 ::  ;<  bind
::     [%mcsg p=hoon q=(list hoon)]                        ::  ;~  kleisli arrow
::     [%mcmc p=spec q=hoon]                               ::  ;;  normalize
::   ::                                            ::::::  compositions
::     [%tsbr p=spec q=hoon]                               ::  =|  push bunt
::     [%tscl p=(list (pair wing hoon)) q=hoon]            ::  =:  q w= p changes
::     [%tsfs p=skin q=hoon r=hoon]                        ::  =/  typed variable
::     [%tsmc p=skin q=hoon r=hoon]                        ::  =;  =/(q p r)
::     [%tsdt p=wing q=hoon r=hoon]                        ::  =.  r with p as q
::     [%tswt p=wing q=hoon r=hoon s=hoon]                 ::  =?  conditional =.
::     [%tsgl p=hoon q=hoon]                               ::  =<  =>(q p)
::     [%tshp p=hoon q=hoon]                               ::  =-  =+(q p)
::     [%tskt p=skin q=wing r=hoon s=hoon]                 ::  =^  state machine
::     [%tssg p=(list hoon)]                               ::  =~  hoon stack
::     [%tstr p=(pair term (unit spec)) q=hoon r=hoon]     ::  =*  new style
::     [%tscm p=hoon q=hoon]                               ::  =,  overload p in q
::   ::                                            ::::::  conditionals
::     [%wtbr p=(list hoon)]                               ::  ?|  loobean or
::     [%wthp p=wing q=(list (pair spec hoon))]            ::  ?-  pick case in q
::     [%wtcl p=hoon q=hoon r=hoon]                        ::  ?:  if=then=else
::     [%wtdt p=hoon q=hoon r=hoon]                        ::  ?.  ?:(p r q)
::     [%wtkt p=wing q=hoon r=hoon]                        ::  ?^  if p is a cell
::     [%wtgl p=hoon q=hoon]                               ::  ?<  ?:(p !! q)
::     [%wtgr p=hoon q=hoon]                               ::  ?>  ?:(p q !!)
::     [%wtls p=wing q=hoon r=(list (pair spec hoon))]     ::  ?+  ?-  w=default
::     [%wtpm p=(list hoon)]                               ::  ?&  loobean and
::     [%wtpt p=wing q=hoon r=hoon]                        ::  ?@  if p is atom
::     [%wtsg p=wing q=hoon r=hoon]                        ::  ?~  if p is null
::     [%wthx p=skin q=wing]                               ::  ?#  if q matches p
::     [%wtts p=spec q=wing]                               ::  ?=  if q matches p
::     [%wtzp p=hoon]                                      ::  ?!  loobean not
::   ::                                            ::::::  special
::     [%zpcm p=hoon q=hoon]                               ::  !,
::     [%zpgr p=hoon]                                      ::  !>
::     [%zpgl p=spec q=hoon]                               ::  !<
::     [%zpmc p=hoon q=hoon]                               ::  !;
::     [%zpts p=hoon]                                      ::  !=
::     [%zppt p=(list wing) q=hoon r=hoon]                 ::  !@
::     [%zpwt p=$@(p=@ [p=@ q=@]) q=hoon]                  ::  !?
::     [%zpzp ~]                                           ::  !!
::   ==                                                    ::
--
