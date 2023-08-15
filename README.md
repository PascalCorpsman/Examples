# Introduction
This Repository collects a lot of usefull and mostly not trivial examples that can be used to start a own application.

## ! Attention !
If you get a "could not find" error when compiling the examples, this could be caused by the directory name change during translation and uploading into this repository. Add the missing file's by hand with the project inspector from the IDE.

## Overview
![](Overview.png)

# Details

## OpenGL

Collection of classes and examples that are mainly related to OpenGL implementations

| Example | Description |
| --- | --- |
| Mapviewer | Include Google maps into your own application using OpenGL as render engine. Chaches maptiles on local drive for offline use. |
| uopengl_ascii_font.pas | DOS-stile ASCII-Font to easily show texts in OpenGL windows |
| uopengl_graphikengine.pas | Class to load and render graphics into the OpenGL window |

## TCP_IP

Collection of classes and examples that are mainly related to Serial and Network communications

| Example | Description |
| --- | --- |
| Modbus_Diagnostic | Shows a implementation for the MODBUS RTU, MODBUS-TCP and MODBUS-TCP RTU version, use to detect address offset and encoding of the MODBUS SLAVE |
| utcp.pas | abstract wrapper for TLTcp (from L-net) |

## data_control

Collection of classes and examples corresponding to data processing, math and corresponding stuff

| Example | Description |
| --- | --- |
| uJSON.pas | JSON-Loader / writer class |
| ucrc.pas | Generic crc calculation class |
| udomxml.pas | generic xml parser |
| ufifo.pas | generic first in first out class |
| uimodbus.pas | Modbus server class for Modbus TCP or Modbus RTU |
| uiwrapper.pas | data source wrapper for uimodbus.pas (L-Net or Synapse) | 
| umapviewer.pas | see OpenGL "Mapviewer" |
| uneuralnetwork.pas | generic neural network implementation (not performant but simple) |
| uuart_deprecated.pas | wrapper for synaser.pas (see Synapse) |
| uvectormath.pas | Math library for multidimensional things (e.g. OpenGL, Matrix ..) |
## graphics

Collection of classes and examples corresponding to graphic / image processing

| Example | Description |
| --- | --- |
| CirclePackChart | Demo application for ucirclepackchart.pas |
| SunburstChart | Demo application for usunburstchart.pas |
| ugraphics.pas | collection of algorithms to manipulate images and do color calculations |
| ucirclepackchart.pas | LCL-Component to display packed circles |
| usunburstchart.pas | LCL-Component to display a sunburstchart diagram |

## Dependencies
Some examples may have dependencies to external libraries here is the collection where to find and download this libraries (as they are not part of this repository):

| libname | Description | Link |
|---|---|---|
| Synapse | Synapse TCP/IP and serial library | http://www.ararat.cz/synapse/doku.php/download |
| DGLOpenGL | OpenGL Header translation | https://github.com/saschawillems/dglopengl |
| MP-Arith | Mathlibrary for big numbers | https://web.archive.org/web/20190628091417/http://www.wolfgang-ehrhardt.de/index.html |
| Lnet | Lightweight Networking Library | https://github.com/almindor/lnet |

