SCUMM Explorer

A new implementation.


Copyright (C) 2015, Daniel England.
All Rights Reserved.


GPL v3 Licensed.

-------------------------------------------------------------------------------
  This program is free software: you can redistribute it and/or modify it 
  under the terms of the GNU General Public License as published by the 
  Free Software Foundation, either version 3 of the License, or (at your 
  option) any later version.

  This program is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
  or FITNESS FOR A PARTICULAR PURPOSE.  

  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------


Description
-----------

Whilst there have been a number of SCUMM related tools developed in the past, 
none of them seem particulary stable, well maintained or flexible (able to 
handle a range of SCUMM versions).  Most do not have any source code available, 
are limited to a single platform and perform only a single task.

I have developed this project to solve this situation.  It is open source,
designed to allow for all SCUMM versions and is well written.  Eventually, it 
will also be available on all platforms.

What does it do?  Currently, the functionality is rather limited but it will
eventually allow browsing through all of any SCUMM game's resources.  The 
browsing is done via a virtual file system back-end and a typical file browser
type front-end.

There was another project called "SCUMM Explorer" available only on the 
Macintosh platform but as far as I'm aware, it is no longer maintained and the
source code is unavailable.  It was not widely distributed as far as I am 
aware.  I am hoping there won't be too much of a problem with reusing the name
because it was really the only name that seemed to fit the functionality.


Compiling
---------

Presently, I am using Delphi XE8 to build/compile the project.  Any XE version,
XE2 or above should work with no problems.

Of course, in order to support more platforms, I will need to port the project 
to FreePascal.  I am currently using some functionality that is not available 
in the FPC libraries.  However, the functionality could be implemented or 
replaced without too much work.  

The other issue for a FPC port, is the fact that I'm currently using a simple
plug-in architecture for the game decoding.  FPC does not support dynamic
packages at this time (as far as I'm aware).  This could be worked-around with
the right kind of a approach and the whole project built into a single
executable.


Running
-------

Presently, SCUMM Explorer works only on MS Windows, Vista or above.

At this time, only V2 SCUMM games can be decoded and only a relatively small
number of resouces can be viewed.  The knowledge base files have only been
populated with data for Maniac Mansion and Zak McKracken Amiga versions and 
the decoder expects to only see those two games.  However, due to the way the
code has been written, it should be very simple to add support for the DOS
versions as well as any other V2 game.  

There is some framework already implemented for it and support for other game 
versions and resource types will be added in the near future.  

It is intended that you will configure SCUMM Explorer with a selection of games
and that those games will then be browsable in the interface.  At the moment,
the configuration interface has not been implemented and you are taken directly
to a folder selection dialog.  You should select the folder containing the LFL 
files for Maniac Mansion or Zak McKracken.  When you have done this, the game 
root will be added to the explorer's folder pane.

The games are presented as a virtual file system where each resource and 
resource type (as well as meta data) can be viewed in the viewing pane by
browsing "folders" and selecting "items".  To begin browsing, simply select or
expand the game root "folder".  From here, you are presented with child folders
in the folder pane and folders and items in the items pane.

There are a number of simple file formats which can be viewed in the 
preview pane but the decoders can implement more complex ones for particular
resources.  Such a viewer has been created for the costume resources.  
Selecting an item in the items pane will show the preview, if one is available.

It is intended that different icons will be shown for each resource folder type
as well as for each item type in order to show what kind of preview will be
available but this has not yet been implemented.

Eventually, saving or exporting the resources will also be implemented.


Contact
-------

I'd appreciate feedback about any bugs or suggestions.  I can be contacted as 
below:
	"mewpokemon",$40,"hotmail",$2E,"com"
	
Please include "SCUMMExplorer" in the subject line.



Daniel England.