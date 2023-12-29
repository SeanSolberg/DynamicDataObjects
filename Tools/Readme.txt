This directory contains a few tools that are built with the DynamicDataObjects Library. 
There are two sets of tools:  32bit set and a 64bit set in their respective subdirectories. 

NOTE:  The 64bit set doesn't have the installer built yet and I don't have a 64 bit Explorer Previewer yet.


1.  DataObjectInstaller32.exe - This is a simple nsis installer that will install the three tools listed below.  This is the easyest way to get everything setup correctly as it will register the supported file formats with windows to easily edit these files with the editor and it will register the explorer plugin. 

2.  DataObjectEditor.exe - This is a simple executable that will open any of the files supported by the dataObjects serializers.  It is very fast and can handle very large files in the tree-orientated UI. 

3.  DataObjPreviewer.dll - This is a windows explorer preview plugin that will show the contents of any of the supported formats in the windows explorer preview side-panel.   This must be registered with "regsvr32 DataObjPreviewer.dll"

4.  DataObj.exe - This is a simple command line tool that lets you convert from any of the supported formats to any other format from within the command shell.  For example:  ".\DataObj.exe -i *.cbor -f json -od c:\temp" will take all cbor files in the current directory and convert them to corresponding json files and place them into the c:\temp directory. 




MIT License

Copyright (c) 2022 Sean Solberg

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.