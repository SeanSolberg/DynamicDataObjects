{ This file defines a few compiler settings for the DataObjects code library }

{********************************************************************************}
{                                                                                }
{                         Dynamic Data Objects Library                           }
{                                                                                }
{                                                                                }
{ MIT License                                                                    }
{                                                                                }
{ Copyright (c) 2022 Sean Solberg                                                }
{                                                                                }
{ Permission is hereby granted, free of charge, to any person obtaining a copy   }
{ of this software and associated documentation files (the "Software"), to deal  }
{ in the Software without restriction, including without limitation the rights   }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      }
{ copies of the Software, and to permit persons to whom the Software is          }
{ furnished to do so, subject to the following conditions:                       }
{                                                                                }
{ The above copyright notice and this permission notice shall be included in all }
{ copies or substantial portions of the Software.                                }
{                                                                                }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  }
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  }
{ SOFTWARE.                                                                      }
{                                                                                }
{********************************************************************************}


(* Case Sensitivity.
   By default, this code uses the CompareTextUnicode function defined in UnicodeData.pas to do all case insensitive comparisons because
   it compares all unicode uppercase-to-lowercase characters where Delphi's CompareText function only considers the core ascii characters
   for case insensitive comparison, which is clearly wrong given the fact that the parameters to the function are "strings" which are unicode strings.
   If you want to use Delphi's CompareText function instead, you can define CompareTextUsingDelphiCompareText below.  The CompareTextUnicode function
   is more complete,  CompareText is a bit faster, but that's because it only works for ascii characters.
*)
//{$define CompareTextUsingDelphiCompareText}


(* Backward Compatibiltiy Option.
   If you enable cMakeMoreCompatibleWithOldDataObjects then it makes this code more compatible with the old dataObjects library I used to use.  This
   is just used for an occasional convenience and should not be turned on normally.
*)
//{$Define cMakeMoreCompatibleWithOldDataObjects}


const
  // this constant defines how many slots in a frame warrant it getting big enough to benefit from a slot name index.
  // Frames with less than this threshold won't have or need an index, but if building a frame grows over this threshold, then an index is created for faster lookups
  // experimentally, this is best somewhere around 20 to 25 slots, but it varies a bit on the strings and their order in how they are added to the frame and how much re-balancing is needed when the initial index is created.
  // It also takes some work to build the index to start, so being on the higher side might actually be beneficial to delay it to only when it's needed.
  cDefaultSlotNameIndexThreshold = 20;

  // TDataObj, and specifically the internal TDataFrame container can have case-sensitive or case-insensitive slotnames (field names)
  // TDataObj has the SlotnameIsCaseSensitive property which sets whether slotnames are case sensitive or not for everything within that object's contained data hierarchy.
  // When creating a TDataObj as a root level object, this constant sets whether these new objects are set up as caseSensitive or not.
  // But it can be changed at runtime for that particular object instance.   If two slotnames with the same text but different case are added to a TDataFrame,
  // and then you change the TDataObj from case insensitive to case sensitive, no checking of the underlying data will happen, but you will then be unable to access
  // one of those slots as finding a slot with the text of that slotname containing any case variances will always return the first one it finds and the other one will be unfindable.
  // So, setup your case property right away before putting data into the TDataObj.  The JSON serializer will set the TDataObj's caseSensitive property when reading JSON according to it's CaseSensitive property.
  cDefaultSlotnameCaseSensitive = true;

  // This setting is used by the nested data finding calls FindSlotByPath and NewSlotByPath to define the default identifier separator.
  // This default gets loaded into gSlotNamePathDelimeter at startup and this global variable can be changed at anytime.  When doing so, all calls from all code then use this
  // new path delimeter variable.  Alternatively, there are versions of FindSlotByPath and NewSlotByPath where you can pass in your desired delimeter.
  cDefaultSlotNamePathDelimeter = '\';

