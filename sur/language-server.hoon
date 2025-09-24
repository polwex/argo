|%
::
+$  versioned-doc-id
  [uri=@t version=(unit @)]
::
++  request
  |%
  +$  all
    $%
      text-document--hover
      text-document--hover-complete
      text-document--completion
      text-document--definition
      unknown
    ==
  +$  text-document--hover
    [%text-document--hover id=cord position versioned-doc-id]
  +$  text-document--hover-complete
    [%text-document--hover-complete hov=text-document--hover]
  +$  text-document--completion
    [%text-document--completion id=cord position versioned-doc-id]
  +$  text-document--definition
    [%text-document--definition id=cord position versioned-doc-id]
  +$  unknown
    [%unknown json]
  --
++  response
  |%
  +$  all
    $%
      text-document--hover
      text-document--completion
      text-document--definition
    ==
  +$  text-document--hover
    [%text-document--hover id=cord contents=(unit @t)]
  +$  text-document--completion
    [%text-document--completion id=cord completion=(list completion-item)]
  +$  text-document--definition
    [%text-document--definition id=cord =location]
  --
::
+$  completion-item
  $:
    label=cord
    kind=@ud
    detail=cord
    doc=cord
    insert-text=cord
    insert-text-format=@ud
  ==
::
+$  location
  $%  [%location p=(list [uri=@t =range])]
      [%link p=(list [target-uri=@t target-range=range target-selection-range=range origin-selection-range=(unit range)])]
      [%empty ~]
  ==
:: 
+$  diagnostic
  [=range severity=@ud message=@t]
::
+$  position
  [row=@ud col=@ud]
::
+$  text-document-item
  [uri=@t version=(unit @) text=@t]
::
++  notification
  |%
  ::
  +$  in
    $%
      text-document--did-change
      text-document--did-open
      text-document--did-save
      text-document--did-close
      exit
      unknown

    ==
  ::
  +$  out
    $%
      text-document--publish-diagnostics
      progress
    ==
  ::
  +$  all
    $%
      out
      in
    ==
  ::
  +$  text-document--did-change
    [%text-document--did-change versioned-doc-id changes=(list change)]
  ::
  +$  text-document--did-open
    [%text-document--did-open text-document-item]
  ::
  +$  text-document--did-save
    [%text-document--did-save versioned-doc-id]
  ::
  +$  text-document--did-close
    [%text-document--did-close versioned-doc-id]
  ::
  +$  exit
    [%exit ~]
  ::
  +$  progress
    [%progress title=@t kind=@t message=(unit @t) percentage=(unit @ud)]
  ::
  +$  unknown
    [%unknown =json]
  ::
  +$  text-document--publish-diagnostics
    [%text-document--publish-diagnostics uri=@t diagnostics=(list diagnostic)]
  ::
  --
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
--
