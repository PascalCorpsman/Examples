TPlayingCard
============

Description
===========

Written for Delphi 2.0 (32 bit)

This is a simple component descended from TGraphicControl, which
displays the back, face or other image of a playing card on a form.
Also, global functions are available to display a card image on any
canvas, and manage the images displayed by the cards.

It incorporates features which you might find lacking in some other
TCard style components, though I stress that the various TCard style
components have been the prime mover for this little project.

Yes, it's freeware. The source is included, and you may use and modify
it as you wish, but since I've done all the hard work, I ask that you
e-mail a nice thank you note if you use it in anything that you write
yourself. I do not however accept any liability for the consequences
of using this component in your own projects.


Features
========

The corners of playing cards are curved, meaning that the corners
of the bitmaps that form the card images should be transparent. Most
card components simply plonk the bitmap on the screen - TPlayingCard
displays the card with transparent corners, meaning that the card or
canvas behind can still be seen at the card's corners.

TPlayingCard components are sizeable - the bitmap that forms the card
is scaled to fit in the card area as specified at design-time.

Jokers are included - one red, and one black.

Utility cards (such as the red cross on green background) are
available as usual, as are card back images, but they are separated
each other in implementation. In other words, the card is in one of
three states; 1) face up, 2) face down or 3) tool card

Tool and back images are user definable. Simply add a TBitmap or
bitmap file to the global lists (using the component's methods, or via
the global functions) and all TPlayingCard components in the
application can access it via it's index, as with any of the existing
'default' images.

Card values can be set either via an index specifier which conforms to
the cards.dll style index (eg. 1 = Ace clubs, 52 = King Spades etc),
or via independent Value and Suit properties.


Component Installation
======================

Install the component in the usual way specifying cards.pas or
cards.dcu as the installable unit. This places TPlayingCard in the
Samples palette. You'll recognise it - it's the Ace of Spades.


Demo Project
============

To see the TPlayingCard component in action, open the test.dpr
project in Delphi 2.0. Each of the buttons on the main form increments
the selected playing card's corresponding property by one.
The 'Add Tool' and 'Add Back' buttons allow you to add a bitmap file
to the global list of tool images and back images, which are available
to all three TPlayingCard components on the main form.
Select and deselect the cards by clicking them.


Notes
=====

The card size is by default 71 x 96. Displaying this size of
TPlayingCard is very quick, because BitBlt is used instead of
StretchBlt. When you use cards of other sizes though, be prepared
for a noticeable decrease in speed.

When using the AddTool or AddBack methods (or equivalent), the
bitmaps should normally be 71 x 96 with 16 colours. Other combinations
may cause odd effects at the corners, and with colours.


The Author
==========

Any comments or suggestions can be sent to the following address:

-----------
Simon Fitch
Royal Road
Tombeau Bay
Mauritius
-----------

My e-mail address is:

----------------------
xcalibur@bow.intnet.mu
----------------------

Adaption for Delhpi 1 done by Simon Reinhardt. I just built a new 
16Bit-resource and did some slight changes in the sourcecode. My
e-mail adress is:
-------------------
S.Reinhardt@WTal.de
-------------------

Adaption for Lazarus done by Uwe Schächterle. I just converted
all to lazarus and brought it into ObjFPC style.
e-mail adress is:
-------------------
support@corpsman.de
-------------------

Thanks for reading this far. Have fun.

Simon Fitch


