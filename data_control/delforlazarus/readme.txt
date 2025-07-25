Program:
        delforlazus, Lazarus Formatter

Info:  
        delforlazarus the more or less translation from "DelforEx" for 
        the win32 DelphiX version. Made by Egbert van Nes

Version:
        for Lazarus ( Linux ), Lazarus ( Win ) not tested yet.        
        
Category:
        Programmers tool

Description:
        Delforlaz is a customizable source code formatter.
        It can improve the indentation, spacing, capitalization and
        the use of blank lines of Lazarus source code.
        In the default settings, the style of the Borland source code
        is followed closely.
        It is an expert that is integrated in the Lazarus IDE.

Status:
        The program is released as FREEWARE to improve the productivity
        of Lazarus. You may distribute the files freely as long as you don't
        make money by it. The use of the program is at own risk. (see also 
        license.txt)

Files:
        delforengine.pas	Engine that does all the work
        delforinterf.pas	Interface Unit
        delforlazarus.lpk	Package to install delforlazarus into the Lazarus IDE
        delforlazarus.pas	Sourceodefile for the package
        delforlaz.cfg		the configurationfile for delforlaz
        delforsource.pas	the source code for the package
        delfortypes.pas		some i.o. types like TSettings
        License.txt     	license notes 
        oobjects.pas		some objects used by the package
        readme.txt  	 	this file
        Settings_dlg		Holds the Program to create cfg files
                                 project1.lpi	Project file
                                 project1.lpr	Project file
                                 unit1.lfm	Project file
                                 unit1.pas	Project file
                                 unit2.lfm	Project file
                                 unit2.pas	Project file

Install:
        Copy all Files in the Lazarus/components/delforlazarus/directory
        open the delforlazarus.lpk with the IDE and klick install.

        via "Strg + D" you should be able to format the Code.
        compile and run the Settings_dlg if you want to use a other settings confifuration.

DeInstall:
        Delete the Package from the Lazarus Package list, rebuild the IDE
        Delete all Files.

Contact:

If you have any questions about the package and integration source code
       Uwe Schächterle
       http://www.Corpsman.de

If you have any questions about the orig formater source code
        Egbert van Nes
        http://www.dow.wau.nl/aew/DelForExp.html (NOTE: HAS CHANGED)
        egbert.vannes@wur.nl
	Wageningen University
	The Netherlands

Known problems:
        (1)
        Compiler {$IFDEF} + {$ELSE} directives may be nested to 3 levels and break
        into blocks of code. After the third nested level the right indentation
        is not guaranteed.

        (2)
        After some options (align, adding line breaks) the indentation might
        be not correct. Rerunning DelFor can fix the problems.

        (3)
        It is tried to indent function directives after function declarations.
        In some cases this does not happen.

        (4)
        After formatting the positions of breakpoints and bookmarks are not changed 
        (all bookmarks are moved and stacked at the end of the file) breakpoints 
        are removed) If someone knows how to get and set the locations of these points
        I would be happy to hear from you.


FAQ
       Q: Where can I download this Package
       A: Go the www.Corpsman.de and search in the rubric "projects".

        

