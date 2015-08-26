# taskharrier
Small script to parse ics and execute taskwarrior to generate a new task
from vcEvents from the email invitations people tend to spread around. 

Use it, for example, by including it into your mailcap file.

<pre>
application/ics; taskharrier '%s'; needsterminal; description=Add Event Invitations to taskwarrior; nametemplate=%s.ics
</pre>

## Compilation

Follow the basic (sandboxed) [installation
guide](https://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes).

<pre>
cabal sandbox init
cabal install --only-dependencies 
cabal build      
</pre>
