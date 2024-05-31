unit DataObjects2BinaryJData;

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

interface

// This unit implements the Binary_JData specification.     See https://github.com/OpenJData
// Note that OpenJData is derived from UBJSON.  So, we will code this unit that way in that this class is inherited from the UBJSON implementation.

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils, DataObjects2UBJSON;

type
{: This class will decode and encode a JDATA stream.   See https://github.com/OpenJData for details. }
  TJDataStreamer = class(TUBJSONStreamer)
  protected
    procedure DecodeType(aType: byte; aDataObj: TDataObj); override;
  public
    class function FileExtension: string; override;
    class function Name: string; override;
    class function Description: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function Priority: cardinal; override;

//    procedure Decode(aDataObj: TDataObj); override;   No need to override as the ancestors implementation is sufficient and the Virtual DecodeType handles it.
    procedure Encode(aDataObj: TDataObj); override;
  end;


implementation

{ TJDataStreamer }

class function TJDataStreamer.Priority: cardinal;
begin
  result := 40;
end;

procedure TJDataStreamer.DecodeType(aType: byte; aDataObj: TDataObj);
var
  i: integer;
  lWord: Word;          // UInt16
  lCardinal: Cardinal;  // UInt32
  lUInt64: UInt64;
  lHalfFloat: THalfFloat;
  lChildType: byte;
  lTempType: byte;
  lSize: integer;
  lInt64: int64;
begin
  inherited;

  case aType of
    byte('u'): begin   // UInt16
      DoRead(lWord, 2);
      aDataObj.AsInt32 := lWord;      // NOTE:  DataObjects doesn't natively model unsigned Int16 so we must store in 32bit int.
    end;

    byte('m'): begin   // UInt32
      DoRead(lCardinal, 4);
      if lCardinal >= $80000000 then
        aDataObj.AsInt64 := lCardinal   // if the incoming UInt32 has most significant bit set then we can't store it into an int because the int is signed which uses that bit.  Need to upsize.
      else
        aDataObj.AsInt32 := lCardinal;    // if the shortInt is negative, then we must store in an Int32 because our internal byte data type is unsigned.
    end;

    byte('M'): begin   //UInt64
      DoRead(lUInt64, 8);

      if lUInt64 >= $8000000000000000 then
        aDataObj.AsInt64 := lCardinal   // if the incoming UInt32 has most significant bit set then we can't store it because it won't fit in our int64.
      else
        aDataObj.AsInt64 := lCardinal;    // if the shortInt is negative, then we must store in an Int32 because our internal byte data type is unsigned.
    end;

    byte('h'): begin   // Half - Float16
      DoRead(lHalfFloat, 2);              // NOTE:  This block of code has never been tested.
      aDataObj.AsSingle := lHalfFloat;    // Don't support half floats directly so must store as a single.
    end;

    byte('['): begin   // Array.
      // Even though the ancestor can already handle decoding arrays, this JData option supports an additinal possible feature: "Optimized N-Dimensional Array".
      // So we have to re-implement array decoding here with the ability to possibly handle n-dimensional arrays.
      // DataObjects internals don't natively handle the concept of an optimized n-dimensional array, but maybe we will add that in the future.
      lChildType := 0;
      lSize := -1;  // means "not defined"

      // pick up the next type to see if its one of our special "optimizing types"
      lTempType := ReadTypeByte;
      if lTempType = byte('$') then
      begin
        // This means that all of the items in this array will be of the following type.  Note that reading a no-op means that all the types in the array will
        // be no-ops so we read the byte directly instead of going through ReadTypeByte because it ignores no-ops
        DoRead(lChildType, 1);
        // Read the next type.
        lTempType := ReadTypeByte;
      end;

      if lTempType = byte('#') then
      begin
        lTempType := ReadTypeByte;            // first, read the size type which should be one of the number types, but could possibly be a [ character to signify an n-dimensional array.
        if lTempType = byte('[') then
        begin
          // FINISH - Need to decode an N-Dimensional Array.
          // [[] [$] [type] [#] [[] [$] [nx type] [#] [ndim type] [ndim] [nx ny nz ...]  [nx*ny*nz*...*sizeof(type)]
          //  or
          // [[] [$] [type] [#] [[] [nx type] [nx] [ny type] [ny] [nz type] [nz] ... []] [nx*ny*nz*...*sizeof(type)]

        end
        else
        begin
          lInt64 := DecodeSize(lTempType);   // Try to read the size for this array.

          //Check that the size we read is in a useable range
          if lInt64 > $7FFFFFFF  then
            RaiseParsingException('Size of '+IntTostr(lint64)+' is too large to use for an optimized array length. Limit is 2147483647');
          if lInt64<0 then
            RaiseParsingException('Cannot use a negative size of '+Inttostr(lint64)+' for an optimized array length.');
          //FINISH - Check to see if a huge size is given to us causing unrealistic memory allocation and thus a crash.

          lSize := lInt64;
        end;
      end
      else
      begin
        // if we did not read a # type code but we did read a $ type code, then we have an error condition because if the optimized type is defined, then the count must also be defined.
        if lChildType <> 0 then
        begin
          RaiseParsingException('Optimized type code($) wasn''t followed by an optimized count code(#).');
        end;
      end;

      if lSize >= 0 then
      begin
        // we know what the size is so loop through and read what is expected.
        // if the lChildType is defined then we already know the type and we just need to read the data portion
        if lChildType <> 0 then
        begin
          for i := 0 to lSize-1 do
          begin
            DecodeType(lChildType, aDataObj.AsArray.NewSlot);
          end;
          // Note there is no array ending byte when we know the count up front
        end
        else
        begin
          for i := 0 to lSize-1 do
          begin
            // we do not have optimized child type defined so we need to read the type for each item in the array because each item could possibly be a different type.
            lChildType := ReadTypeByte;
            DecodeType(lChildType, aDataObj.AsArray.NewSlot);
          end;
          // Note there is no array ending byte when we know the count up front
        end;
      end
      else
      begin
        // We do not have a defined size with the Optimized mechanism so read child elements until we hit the "]" type.
        lChildType := lTempType;
        while lChildType <> byte(']') do
        begin
          DecodeType(lChildType, aDataObj.AsArray.NewSlot);
          lChildType := ReadTypeByte;   // pickup the next type or the end type "]"
        end;
      end;
    end;

    else
      // if we didn't pick up any of the jdata specific types, then call up to the ancestor to see if the type is a base type from UBJSON.
      inherited DecodeType(aType, aDataObj);
  end;
end;

class function TJDataStreamer.Description: string;
begin
  result :=  'Binary JData format. https://github.com/NeuroJSON/bjdata and https://github.com/OpenJData/jdata/blob/master/JData_specification.md';
end;

procedure TJDataStreamer.Encode(aDataObj: TDataObj);
begin
  inherited;



end;

class function TJDataStreamer.FileExtension: string;
begin
  result := 'bjd';
end;

class function TJDataStreamer.GetFileFilter: string;
begin
  result := 'JDATA Files (*.bjd)|*.bjd';
end;

class function TJDataStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.bjd') or SameText(aStr, 'bjd');
end;

class function TJDataStreamer.Name: string;
begin
  result := 'Binary JData';
end;

initialization
  RegisterDataObjStreamer(TJDataStreamer);

end.
