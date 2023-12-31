# Introduction
This Repository collects a lot of usefull and mostly not trivial examples that can be used to start a own application.

## ! Attention !
If you get a "could not find" error when compiling the examples, this could be caused by the directory name change during translation and uploading into this repository. Add the missing file's by hand with the project inspector from the IDE.

## Overview
Shows the dependencies of the demos to the external libraries (see [dependencies](#Dependencies) how to get to the missing code).
![](Overview.png)

# Details

## OpenGL
Collection of classes and examples that are mainly related to OpenGL implementations

| Example | Description |
| --- | --- |
| Animation_Editor | Editor to create and edit .ani files |
| Mapviewer | Include Google maps into your own application using OpenGL as render engine. Chaches maptiles on local drive for offline use. |
| umapviewer.pas | LCL-Component to make google mapviewer in OpenGL possible |
| uopengl_animation.pas | Class to load and render .ani files (see Animation_Editor) |
| uopengl_ascii_font.pas | DOS-stile ASCII-Font to easily show texts in OpenGL windows |
| uopengl_graphikengine.pas | Class to load and render graphics into the OpenGL window |
| uopengl_primitives.pas | Collection of higher level primitive objects (sphere, cone, cube..) |
| uopengl_partikelengine.pas | Class to render thousands of particle graphics into the OpenGL window |
| uopengl_spriteengine.pas | Class to render multiple imageparts within defined delta times |
| uopengl_truetype_font.pas | Class to render truetype fonts to OpenGL windows |
| uopengl_widgetset.pas | TButton, TScrollbar, TListbox.. for a OpenGL Renderingcontext |

## TCP_IP
Collection of classes and examples that are mainly related to Serial and Network communications

| Example | Description |
| --- | --- |
| MQTT_Broker | Shows a implementation of uMQTTbroker.pas (only publish / subscibe no evaluations) |
| MQTT_Publisher | Demo that can send MQTT Publish messages to a given IP-Address (no security login) |
| Modbus_Client | Application to emulate a MODBUS-Slave device |
| Modbus_Diagnostic | Shows a implementation for the MODBUS RTU, MODBUS-TCP and MODBUS-TCP RTU version, use to detect address offset and encoding of the MODBUS SLAVE |
| Socket | Demo application that shows how to establish a TCP-IP connection with L-Net |
| uMQTTbroker.pas | a simpliest MQTT broker, only capable of receiving publishing clients (need L-Net) |
| uchunkmanager.pas | Server / Client component to send packages of data through a TCP-IP connection (need L-Net) |
| uhtml.pas | HTML-DOM parser |
| uip.pas | Some helper routines to work and get IP's |
| umultipartformdatastream.pas | Class to create HTML Multipartforms as a stream |
| utcp.pas | abstract wrapper for TLTcp (need L-net) |
| uupdate.pas | class to easy automate update the application when developed |

## data_control
Collection of classes and examples corresponding to data processing, math and corresponding stuff

| Example | Description |
| --- | --- |
| JSON_Analyzer | demo application for uJSON.pas |
| LZW | demo application for uLZW.pas (Lempel-Ziv-Welch algorithm) |
| QuadTree | demo application for uquadtree.pas |
| Vectormath | Testcases for uvectormath.pas |
| uJSON.pas | JSON-Loader / writer class |
| uLZW.pas | Implementation of Lempel-Ziv-Welch algorithm with two different dictionaries |
| ubitstream.pas | Stream to store and load bitwise data (typically only needed in compression algorithms) |
| ucrc.pas | Generic crc calculation class |
| udomxml.pas | generic xml parser |
| ueventer.pas| class toc reate events for components that are not derived from LCL-Components |
| ufifo.pas | generic first in first out class |
| ufilo.pas | generic first in last out clas |
| ugenmathcalc.pas | generic parser / solver for binary and unary operands (typically used to parse mathematical formulas) |
| uimodbus.pas | Modbus server class for Modbus TCP or Modbus RTU |
| uinterpreter.pas | Pascal Interpreter |
| uiwrapper.pas | data source wrapper for uimodbus.pas (Lnet or Synapse) | 
| ulogger.pas | general data logging framework to be included into own application | 
| umathsolver.pas | String solver for mathematical expressions like "1+2 shl 3" (need MP-Arith) | 
| uuncommenter.pas | class to rulebased remove text from text (e.g. when parsing source code) | 
| uneuralnetwork.pas | generic neural network implementation (not performant but simple) |
| uquadtree.pas | own implementation for a quadtree compression for greyscale .bmp images |
| usqlite_helper.pas | helper routines to access to SQLLite3 databases (needs SQLDBLaz package)|
| utokenizer.pas | generic lexer class to lex strings into tokens |
| uuart_deprecated.pas | wrapper for synaser.pas (see Synapse) |
| uuart.pas | thread based wrapper for synaser.pas (see Synapse) |
| uvectormath.pas | Math library for multidimensional things (e.g. OpenGL, Matrix ..) |

## graphics
Collection of classes and examples corresponding to graphic / image processing

| Example | Description |
| --- | --- |
| CirclePackChart | Demo application for ucirclepackchart.pas |
| ColorDialog | Demo application for ucolordialog.pas |
| SimpleChart | Demo application for usimplechart.pas |
| SunburstChart | Demo application for usunburstchart.pas |
| Yeargrid | Demo application for uyeargrid.pas |
| ubmp_graphikengine.pas | Class to organise multiple graphiks within a application  |
| ucirclepackchart.pas | LCL-Component to display packed circles |
| ucolordialog.pas | A alternate TColordialog variant (oriented on KDE Colour Picker) |
| ugraphics.pas | collection of algorithms to manipulate images and do color calculations |
| ugwavi.pas | Class to create .avi files out of images and waves |
| usimplechart.pas | a Tchart like component to create images with containing pointseries (especially designed to be part of a console application) |
| usunburstchart.pas | LCL-Component to display a sunburstchart diagram |
| uyeargrid.pas | LCL-Component to display a year on the formular |

## sound
Collection of classes and examples corresponding to sound / music processing
| Example | Description |
| --- | --- |
| uwave.pas | class to access .wav files |

## Dependencies
Some examples may have dependencies to external libraries here is the collection where to find and download this libraries (as they are not part of this repository):

| libname | Description | Link |
|---|---|---|
| Bass | Audio library | https://www.un4seen.com/ |
| DGLOpenGL | OpenGL header translation | https://github.com/saschawillems/dglopengl |
| Lnet | Lightweight Networking library | https://github.com/almindor/lnet |
| MP-Arith | Math library for big numbers | https://web.archive.org/web/20190628091417/http://www.wolfgang-ehrhardt.de/index.html |
| Synapse | Synapse TCP/IP and serial library | http://www.ararat.cz/synapse/doku.php/download |

## License
All sourcecode files do have their own license header included at the top, for all other files in this repository see license.md for further informations.
