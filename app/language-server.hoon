/-  lsp-sur=language-server, *sole
/+  *server, dbug, dprint,
    auto=language-server-complete,
    lsp-parser=language-server-parser,
    easy-print=language-server-easy-print,
    rs=language-server-rune-snippet,
    build=language-server-build,
    default-agent, verb, sr=language-server-sortug,
    async=language-server-async,
    deep=language-server-deep
!:
=/  debug  |
|%
+$  card  card:agent:gall
+$  lsp-req
  $:  uri=@t
      $%  [%sync changes=(list change)]
          [%completion position]
          [%commit @ud]
          [%hover position]
      ==
  ==
::
+$  change
  $:  range=(unit range)
      range-length=(unit @ud)
      text=@t
  ==
::
+$  range
  $:  start=position
      end=position
  ==
::
+$  position
  [row=@ud col=@ud]
::
+$  state-two
  $:  %2
      bufs=(map uri=@t buf=wall)
      builds=(map uri=@t =vase)
      ford-diagnostics=(map uri=@t (list diagnostic:lsp-sur))
      preludes=(map uri=@t type)
      pending=(list card)
      stdlib=[hoon=wall zuse=wall lull=wall arvo=wall]
  ==

+$  state-one
  $:  %1
      bufs=(map uri=@t buf=wall)
      builds=(map uri=@t =vase)
      ford-diagnostics=(map uri=@t (list diagnostic:lsp-sur))
      preludes=(map uri=@t type)
      pending=(list card)
  ==
+$  state-zero
  $:  %0
      bufs=(map uri=@t buf=wall)
      builds=(map uri=@t =vase)
      ford-diagnostics=(map uri=@t (list diagnostic:lsp-sur))
      preludes=(map uri=@t type)
      pending=(list card)
  ==
+$  versioned-state
  $%
    state-zero
    state-one
    state-two
  ==
++  jack
  |=  v=versioned-state
  =*  old  +.v
  ?-  -.v
    %0  *state-two
    ::
    %1  *state-two
    %2  v(pending ~)
  ==
::
+$  adapted-state  $&(versioned-state jack)
--
=|  adapted-state
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this      .
      lsp-core  +>
      lsp       ~(. lsp-core bowl)
      def       ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^+  on-init:*agent:gall
    ^-  (quip card _this)
    ~&  >  %lsp-init
    :_  this  :-
    :*  %pass  /connect
        %arvo  %e
        %connect  [~ /'~language-server-protocol']  %language-server
    ==
    ~
    :: init-stdlib:lsp
  ::
  ++  on-save   !>(state)
  ++  on-load
    ^+  on-load:*agent:gall
    |=  =vase
    ^-  (quip card _this)
    ~&  >  %lsp-upgrade
    =+  !<(old=versioned-state vase)
    =-  [~ this(state -)]
    ?-    -.old
        %0  ;;(adapted-state !<(versioned-state vase))
        %1  ;;(adapted-state !<(versioned-state vase))
        %2  old
    ==
  ::
  ++  on-poke
    ^+  on-poke:*agent:gall
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
    ~&  on-poke=mark
      ?+    mark  (on-poke:def mark vase)
        ::   %language-server-rpc-notification
        :: (on-notification:lsp !<(all:notification:lsp-sur vase))
          %language-server-rpc-request
        (on-request:lsp !<(all:request:lsp-sur vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ?:  ?=([%primary ~] path)
      `this
    ?.  ?=([%http-response @ ~] path)
      (on-watch:def path)
    `this
  ++  on-leave  on-leave:def
  ++  on-peek  on-peek:def
  ++  on-agent  on-agent:def
  ++  on-arvo
    ^+  on-arvo:*agent:gall
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  sign-arvo  (on-arvo:def wire sign-arvo)
        [%eyre %bound *]  `state
        [%clay *]  (handle-build:lsp wire +.sign-arvo)
        [%behn *]  ~?  >  debug  [%behn pending:state]  [pending:state state(pending done)]
        [%khan %arow *]  ~&  khan=-.p.sign-arvo
          ?:  ?=(%.n -.p.sign-arvo)
            ((slog leaf+<p.p.sign-arvo> ~) `state)
          :: =/  v  !<(vase q.p.p.sign-arvo)
          =/  =type  -.q.p.p.sign-arvo
          :: =/  lol  ?@  type  ~&  atom=type  ~  ~&  >  cell=-.type  ~
          =/  spt  (find-spot:sr -.wire type)
          ?~  spt  `state
          (serve-definition wire u.spt)
          
      ==
    [cards this]
  ::
  ++  on-fail   on-fail:def
  --
::
|_  bow=bowl:gall
::
++  give-rpc-notification
  |=  res=out:notification:lsp-sur
  ^-  (list card)
  :_  ~
  [%give %fact ~[/primary] %language-server-rpc-notification !>(res)]
::
++  on-notification
  |=  not=all:notification:lsp-sur
  ^-  (quip card _state)
  =^  cards  state
    ?+  -.not  [~ state]
        %text-document--did-open    (handle-did-open +.not)
        %text-document--did-change  (handle-did-change +.not)
        %text-document--did-save    (handle-did-save +.not)
        %text-document--did-close   (handle-did-close +.not)
        %progress                   (handle-progress +.not)
        %exit                       handle-exit
    ==
  [cards state]
++  on-request
  |=  req=all:request:lsp-sur
  ^-  (quip card _state)
  =^  cards  state
  :: TODO just send the json here, marks are gay
  ~&  >>  req=-.req
    ?+  -.req  [~ state]
      %text-document--hover       ~?  >  debug  %loading  (handle-loading req)
      %text-document--hover-complete  ~?  >  debug  [%hover req]  (handle-hover req)
      %text-document--completion  (handle-completion req)
      %text-document--definition  (handle-definition req)
    ==
  [cards state]
::
++  get-subject
  |=  uri=@t
  ^-  type
  (~(gut by preludes) uri -:!>(..zuse))
::
++  handle-completion
  |=  com=text-document--completion:request:lsp-sur
  ^-  (quip card _state)
  :_  state
  %^  give-rpc-response  %text-document--completion  id.com
  =/  buf=wall
    (~(got by bufs) uri.com)
  =/  txt=tape
    (zing (join "\0a" buf))
  =/  pos
    (get-pos buf row.com col.com)
  =/  rune  (rs (swag [(safe-sub pos 2) 2] txt))
  ?^  rune  rune
  =/  tab-list
    %^  tab-list-tape:auto
      (~(gut by preludes) uri.com -:!>(..zuse))
    pos  txt
  ?:  ?=(%| -.tab-list)  ~
  ?~  p.tab-list  ~
  ?~  u.p.tab-list  ~
  (turn u.p.tab-list make-completion-item)
::
++  make-completion-item
  |=  [name=term =type]
  ^-  completion-item:lsp-sur
  =/  doc
    %-  crip
    ;:  weld
      "`"
      ~(ram re ~(duck easy-print type))
      "`"
    ==
  [name 1 doc '' name 1]
::
:: 
++  handle-definition
  |=  com=text-document--definition:request:lsp-sur
  ^-  (quip card _state)
  =/  pat  (uri-to-path:build uri.com)
  =/  longpath  (parse-uri:build uri.com)
  =/  longpat  [id.com +.longpath]
  ~&  com=[com pat]
  =/  ted  (~(build-file async bow) [pat longpat])
  :_  state
  :~  ted  ==
++  serve-definition
  |=  [=wire =spot]
  ^-  (quip card _state)
  :: TODO NO FUCKING DESK ON THE +spot
  :: stuff in scope can only possibly be in the same desk or in %base
  ~&  spot=[wire spot]
  =/  id  -.wire
  =/  wires  (split-paths:build +.wire)
  =/  nwire  (weld -.wires p.spot)
  =/  uri  (cat 3 ['file:///' (spat nwire)])
  :_  state
  %^  give-rpc-response  %text-document--definition  id  
  [%location ~[[uri q.spot]]]
::
:: 
++  give-rpc-response
  |=  res=all:response:lsp-sur
  ^-  (list card)
  :_  ~
  [%give %fact ~[/primary] %language-server-rpc-response !>(res)]
::
++  handle-progress
  |=  pog=[title=@t kind=@t message=(unit @t) percentage=(unit @ud)]
  ^-  (quip card _state)
  ~?  >  debug  progress+pog
  [~ state]
::
++  handle-exit
  ^-  (quip card _state)
  ~&  >  %lsp-shutdown
  :_  *state-two
  %+  turn
    ~(tap in ~(key by builds))
  |=  uri=@t
  [%pass /ford/[uri] %arvo %c %warp our.bow %base ~]
::
++  handle-did-close

  |=  [uri=@t version=(unit @)]
  ^-  (quip card _state)
  =.  bufs
    (~(del by bufs) uri)
  =.  ford-diagnostics
    (~(del by ford-diagnostics) uri)
  =.  builds
    (~(del by builds) uri)
  :_  state
  [%pass /ford/[uri] %arvo %c %warp our.bow %base ~]~
::
++  handle-did-save
  |=  [uri=@t version=(unit @)]
  ^-  (quip card _state)
  ~?  >  debug  handle-did-save+uri
  =/  =path  (uri-to-path:build uri)
  =/  =desk  (find-desk uri)
  ~?  >  debug  handle-did-save+desk
  :_  state
  %+  weld  [(build-file & uri path `desk) ~]
  =-  %+  weld  -  (give-rpc-notification (get-diagnostics uri))
  =-  (hite desk path -)
   %-  crip  (zing (join "\0a" `wall`(~(got by bufs) uri)))
::
++  handle-did-change
  |=  [document=versioned-doc-id:lsp-sur changes=(list change:lsp-sur)]
  ^-  (quip card _state)
  ~&  changes=changes
  =/  updated=wall
    (sync-buf (~(got by bufs) uri.document) changes)
    ~&  updated-len=(lent updated)
  =.  bufs
    (~(put by bufs) uri.document updated)
  `state
::
++  handle-build
  |=  [path=(pole knot) =gift:clay]
  ^-  (quip card _state)
  ?>  ?=([%writ *] gift)
  ?+  path  `state
    [%ford %stdlib file=@t ~]  (handle-build-stdlib file.path p.gift)
    [%ford uri=@t ~]  (handle-build-old path p.gift)
  ==
++  handle-build-stdlib
  |=  [file=@t =riot:clay]
  =/  =path  /base/sys/[file]/hoon
  ?~  riot  `state
  :: =/  txt  .^(@t %cx /(scot %p our.bow)/base/(scot %da now.bow)/sys/[file]/hoon))
  :: =/  txt  !<(@t +.r.u.riot)
  :: =/  file=wall  (to-wall (trip txt))
  :: =.  stdlib
  ::   ?:  .=('arvo' file)  stdlib(arvo file)
  ::   ?:  .=('hoon' file)  stdlib(hoon file)
  ::   ?:  .=('zuse' file)  stdlib(zuse file)
  ::   ?:  .=('lull' file)  stdlib(lull file)
  ::     stdlib
  `state
++  handle-build-old
  |=  [=path =riot:clay]
  =/  uri=@t
    (snag 1 path)
  ~?  >  debug  handle-did-build+uri
  =/  loc=^path  (uri-to-path:build uri)
  =;  [res=(quip card _state) dek=desk]
    [(snoc -.res (build-file | uri loc `dek)) +.res]
  ?~  riot
    [[~ state] %base]
  =.  builds
    (~(put by builds) uri q.r.u.riot)
  =.  ford-diagnostics
    (~(del by ford-diagnostics) uri)
  =/  bek  byk.bow(r da+now.bow)
  =/  desks=(list desk)  ~(tap in .^((set desk) %cd (en-beam bek /)))
  =|  dek=desk
  |-
  ?~  desks  [[~ state] %base]
  =.  dek  ?:  =(%kids i.desks)  %base  i.desks
  =/  exists=?  .^(? %cu (en-beam bek(q dek) loc))
  ?.  exists  $(desks t.desks)
  =/  scrypat  /(scot %p our.bow)/[dek]/(scot %da now.bow)/open/foo
  ~&  >>  openpath=[dek=dek loc=loc scrypat]
  =+  .^(=open:clay %cs scrypat)
  =/  =type  -:(open loc)
  ~&  >>  "built {<path>} at {<dek>}"
  =/  file=wall
    %-  to-wall  (trip .^(@t %cx (en-beam bek(q dek) loc)))
  =.  preludes
    (~(put by preludes) uri type)
  :_  dek
  :_  state
  (give-rpc-notification (get-diagnostics uri))
::
++  get-diagnostics
  |=  uri=@t
  ^-  out:notification:lsp-sur
  :+  %text-document--publish-diagnostics
    uri
  %+  weld
    (~(gut by ford-diagnostics) uri ~)
  (get-parser-diagnostics uri)
::
++  build-file
  |=  [eager=? uri=@t =path desk=(unit desk)]
  ~&  >>  build-file=[eager uri path desk]
  ^-  card
  =/  =rave:clay
    ?:  eager
      [%sing %a da+now.bow path]
    [%next %a da+now.bow path]
  =/  des=^desk  ?^  desk  u.desk  %base
  [%pass /ford/[uri] %arvo %c %warp our.bow des `rave]
::
::  for finding a desk when saving a file
::  ignores files in %base
::  if there are multiple desks, picks the first one
::  TODO allow user to specify which desk to use
++  find-desk
  |=  uri=@t
  =/  bek  byk.bow
  =/  =path
    (uri-to-path:build uri)
  =/  desks=(list desk)  ~(tap in .^((set desk) %cd (en-beam bek /)))
  =|  dek=desk
  |-
  ?~  desks  %$
  =.  dek  ?:  =(%kids i.desks)  %base  i.desks
  ::  to avoid saving to %base
  ?:  =(dek %base)  $(desks t.desks)
  =/  exists=?  .^(? %cu (en-beam bek(q dek) path))
  ?.  exists  $(desks t.desks)
  dek
++  handle-did-open
  |=  item=text-document-item:lsp-sur
  ^-  (quip card _state)
  ~?  >  debug  handle-did-open+uri.item
  =/  =path
    (uri-to-path:build uri.item)
  ~&  handle-did-open=[path uri.item]
  =/  bek  byk.bow
  =/  desks=(list desk)  ~(tap in .^((set desk) %cd (en-beam bek /)))
  =|  dek=desk
  |-
  ?~  desks  [~ state]
  =.  dek  ?:  =(%kids i.desks)  %base  i.desks
  =/  exists=?  .^(? %cu (en-beam bek(q dek) path))
  ?.  exists  $(desks t.desks)
  ?~  path  `state
  ~?  >  debug  path
  :: ?:  ?=([%lib %language-server @ @ ~] path)
  ::   `state
  :: ?:  ?=(%sys -.path)
  ::   `state
  ~?  >  debug  "%source-desk for {<path>}: {<dek>}"
  =/  buf=wall
    (to-wall (trip text.item))
  =.  bufs
    (~(put by bufs) uri.item buf)
  :_  state
  %+  weld
    (give-rpc-notification (get-diagnostics uri.item))
  [(build-file & uri.item path `dek) ~]
::
++  get-parser-diagnostics
  |=  uri=@t
  ^-  (list diagnostic:lsp-sur)
  =/  t=tape
    (zing (join "\0a" `wall`(~(got by bufs) uri)))
  =/  parse
    (lily:auto t (lsp-parser (uri-to-path:build uri)))
  ?.  ?=(%| -.parse)
    ~
  =/  loc=position:lsp-sur
    [(dec -.p.parse) +.p.parse]
  :_  ~
  [[loc loc] 1 'Syntax Error']
::
++  handle-loading
  |=  hov=text-document--hover:request:lsp-sur
  ^-  (quip card _state)
  =/  hover=card
  :*
    %pass
    /lsp
    %agent
    [our.bow %language-server]
    %poke
    %language-server-rpc-request
    !>([%text-document--hover-complete hov])
  ==
  ?~  pending.state
    ~?  >  debug  "in loading no pending: {<pending.state>}"
    [(welp ~[(wait /lsp now.bow ~s0)] loading) state(pending ~[hover])]
  =/  ped=(list card)  pending.state
  ?+    -.ped  ~?  >  debug  "in loading default: {<pending.state>}"  [~ state]
      [%pass *]
    ~?  >  debug  "in loading pass: {<pending.state>}"
    :_  state(pending ~)  done
      [%give %fact * * *]
    =-  :_  state(pending -)
    (welp ~[hover (wait /lsp now.bow ~s2)] -)
    =+  !<(=all:notification:lsp-sur q.cage.p.->.ped)
    ~?  >  debug  "in loading give: {<pending.state>} {<all>}"
    ?>  =(-.all %progress)
    =/  pro  ;;(progress:notification:lsp-sur all)
    ?:  (gte (need percentage.pro) 99)  ~
    =.  percentage.pro  `(add 1 (need percentage.pro))
    (give-rpc-notification pro)
  ==
++  handle-hover
  |=  cop=text-document--hover-complete:request:lsp-sur
  ^-  (quip card _state)
  =/  hov  hov.cop
  ~&  >  handle-hover=uri.hov
  =|  item=text-document-item:lsp-sur
  ?.  (~(has by bufs) uri.hov)  (handle-did-open item(uri uri.hov))
  :: :_  state
  :: %^  give-rpc-response  %text-document--hover  id.hov
  :: 
  =/  test  (rust "lol" bar)
  =/  buf=wall
    ~|  "{<uri.hov>} not found"  (~(got by bufs) uri.hov)
  ?~  buf  ~&  "no buffer data, reloading"
    (handle-did-open item(uri uri.hov))
  =/  txt
    (zing (join "\0a" buf))
  ~&  >  txt-len=(lent txt)
  =/  pos  (get-pos buf row.hov col.hov)
  =/  magicked  txt:(insert-magic:auto pos txt)
  =/  pax  (uri-to-path:build uri.hov)
  =/  scrypat  /(scot %p our.bow)/argo/(scot %da now.bow)/open/foo
  ~&  >>  openpath=[`path`scrypat pax=pax]
  =+  .^(=open:clay %cs scrypat)
  :: =/  sut  (~(gut by preludes) uri.hov -:!>(..zuse))
  :: =/  sut  -:!>(..zuse)

  =/  sut=type  -:(open pax)
  :: =/  an1  (analyze-type:sr sut)
  :: =/  an2  ~&  "zuse-sut"  (analyze-type:sr -:!>(..zuse))
  :: =/  an3  ~&  "arvo-sut"  (analyze-type:sr -:!>(..arvo))
  :: =/  an4  ~&  "hoon-sut"  (analyze-type:sr -:!>(..hoon))
  :: =/  an5  ~&  "lull-sut"  (analyze-type:sr -:!>(..lull))
  :: =/  advanced  (advance-tape:auto sut pos txt)
  :: ~&  >  adv=advanced
  :: =/  pile  (tape-to-pile:auto pax txt)
  =/  pile  (tape-to-pile:auto pax magicked)
  ~&  pile=[sur.pile lib.pile]
  =/  typmul=(unit [term type spot])  (find-type-mule:auto sut hoon.pile)
  ?~  typmul  ~&  "no-typmul  "  `state
  ~&  >>  spot=[-.u.typmul +>.u.typmul]
  
  =/  snippet  (print-hover:sr u.typmul)
  =/  docs  (crip (to-tape snippet))
  :: ~&  >  snip=docs
  :: =/  types  (exact-list-tape:auto sut pos magicked)
  :: ~&  >  exact=types
  :_  state  %^  give-rpc-response  %text-document--hover  id.hov  `docs
++  handle-hover-old
  |=  cop=text-document--hover-complete:request:lsp-sur
  ^-  (quip card _state)
  =/  hov  hov.cop
  =|  item=text-document-item:lsp-sur
  ?.  (~(has by bufs) uri.hov)  (handle-did-open item(uri uri.hov))
  :_  state
  %^  give-rpc-response  %text-document--hover  id.hov
  =/  buf=wall
    ~|  "{<uri.hov>} not found"  (~(got by bufs) uri.hov)
  =/  txt
    (zing (join "\0a" buf))
  =/  pos  (get-pos buf row.hov col.hov)
  =/  sut  (~(gut by preludes) uri.hov -:!>(..zuse))
  =/  hon  (tape-to-hoon:auto sut pos txt)
  =/  docs=(unit @t)
    %+  biff  (find-type-mule:auto sut hon)
    |=  [id=term typ=type spt=spot]
    ~?  >  debug  "looking for type: {<id>}"
    =+  to-display=(mule |.((find-item-in-type:dprint ~[id] typ)))
    :-  ~
    %-  crip
    %-  to-tape
    ?:  ?=(%| -.to-display)
      [%tan [%leaf "Could not find help A"] p.to-display]~
    ?~  p.to-display
      [%tan [%leaf "Could not find help B"]~]~
    =/  item  (mule |.((print-item:dprint u.p.to-display)))
    ?:  ?=(%| -.item)
      [%tan [%leaf "Could not find help C"] p.item]~
    p.item
  :-  ~
  =;  result
    ~?  >  debug  "output: {<result>}"  result
  ::  =-  ~?  >  debug  "exact: {<(crip -)>}"  (crip -)
  ?^  docs  u.docs
  %-  crip
  =;  exact=tape
    "```hoon\0a {exact} \0a```"
  =/  types  (=<(exact-list-tape auto) sut pos txt)
  ~?  >  debug  "types: {<types>}"
  ?:  ?=(%| -.types)  (trip (need missing-type))
  ?~  p.types  (trip (need missing-type))
  %-  zing
  %+  join  "\0a"
  =/  [=type rep=(unit tape)]  detail.u.p.types
  ~?  >  debug  "detail: {<[type rep]>}"
  ?^  rep  ~[u.rep]
  ?:  =(type -:!>(**))  ~[(trip (need missing-type))]
  (~(win re ~(duck easy-print type)) 0 140)
::
++  sync-buf
  |=  [buf=wall changes=(list change:lsp-sur)]
  
  |-  ^-  wall
  ?~  changes
    buf
  :: CHANGE it almost always has null range-length this is a terrible idea
  ?~  range.i.changes
  :: ?:  ?|(?=(~ range.i.changes) ?=(~ range-length.i.changes))
    =/  =wain  (to-wain:format text.i.changes)
    =.  buf  (turn wain trip)
    $(changes t.changes)
  =/  =tape      (zing (join "\0a" buf))
  =/  start-pos  (get-pos buf start.u.range.i.changes)
  =/  end-pos    (get-pos buf end.u.range.i.changes)
  =.  tape
    ;:  weld
      (scag start-pos tape)
      (trip text.i.changes)
      (slag end-pos tape)
    ==
  =.  buf  (to-wall tape)
  $(changes t.changes)
::
++  to-wall
  |=  =tape
  ^-  wall
  %+  roll  (flop tape)
  |=  [char=@tD =wall]
  ?~  wall
    [[char ~] ~]
  ?:  =('\0a' char)
    [~ wall]
  [[char i.wall] t.wall]
::
++  wush
  |=  [wid=@u tan=tang]
  ^-  tape
  =,  format
  (of-wall (turn (flop tan) |=(a=tank (of-wall (wash 0^wid a)))))
::
++  murge                                              
  |=  a=styx  ^-  tape
  %-  zing  %+  turn  a
  |=  a=_?>(?=(^ a) i.a)
  ?@  a  (trip a)
  ?~  p.p.a  ^$(a q.a)
  ?+  u.p.p.a  ^$(a q.a)
    %bl  "{^$(a q.a)}\0a"
      %br
    ?~  q.q.p.a  "{^$(a q.a)}\0a"
    ?+    u.q.q.p.a  "{^$(a q.a)}\0a"
        %g
      =/  name=tape  ^$(a q.a)
      ?:  =(0 (lent name))  ""  "### {name}\0a"
        %b
      "---\0a __{^$(a q.a)}__ "
    ==
    %un  "_{^$(a q.a)}_"
  ==
++  to-tape
  |=  sols=(list sole-effect)
  ^-  tape
  %-  zing
  =-  (turn sols -)
  |=  sef=sole-effect
    ^-  tape
    ?+    -.sef
              ~|(unsupported-effect+-.sef !!)
        %mor  (zing (turn p.sef |=(a=sole-effect ^$(sef a))))
          %txt
        :(weld "\0a" "```hoon\0a" (tape p.sef) "\0a```\0a")
        ::
        %tan  (tape (wush 160 p.sef))
        %klr  (tape (murge p.sef))  ::
    ::
        ?(%bel %clr %nex %bye)
      (weld <-.sef> <+.sef>)
    ==

++  get-pos
  |=  [buf=wall position]
  ^-  @ud
  ?~  buf
    0
  ?:  =(0 row)
    col
  %+  add  +((lent i.buf))  ::  +1 because newline
  $(row (dec row), buf t.buf)
::
++  safe-sub
  |=  [a=@ b=@]
  ?:  (gth b a)
    0
  (sub a b)
::
++  missing-type  ^-  (unit @t)  [~ 'No type found']
++  loading  ^-  (list card)  (give-rpc-notification [%progress 'Loading...' 'begin' ~ `0])
++  done  ^-  (list card)  (give-rpc-notification [%progress 'Done.' 'end' ~ `100])
++  wait
  |=  [=path now=@da time=@dr]
  ^-  card
  [%pass [%timer path] %arvo %b %wait (add now time)]
::    +cite
::
::  write file to desk.
++  cite
  |=  [=desk =path data=cage]
  =*  bek  byk.bow
  ~&  >>  "saving to {<desk>} at {<path>}"
  ~|  "failed write to {<desk>} at {<path>}"
  =-  [%pass /lsp/write %arvo %c %info -]~
  =/  fath=^path  (weld /(scot %p our.bow)/[desk]/(scot %da now.bow) path)
  (foal:space:userlib fath data)
::  **** ~polwex MY EDITS ****
:: ::
++  init-stdlib
  :: one clay poke per file
  :~
  [%pass /ford/stdlib/arvo %arvo %c %warp our.bow %base `[%sing %a da+now.bow /sys/arvo/hoon]]
  [%pass /ford/stdlib/hoon %arvo %c %warp our.bow %base `[%sing %a da+now.bow /sys/hoon/hoon]]
  [%pass /ford/stdlib/zuse %arvo %c %warp our.bow %base `[%sing %a da+now.bow /sys/zuse/hoon]]
  [%pass /ford/stdlib/lull %arvo %c %warp our.bow %base `[%sing %a da+now.bow /sys/lull/hoon]]
  ==
:: ::
::    +hite
::
::  write text as hoon to desk.
++  hite
      |=  [=desk =path txt=@t]
      =/  =mark  (rear path)
      =/  =type  [%atom %t ~]
      =-  (cite desk path -)
      [mark [type ?:(=(%hoon mark) txt (need (de:json:html txt)))]]
--

