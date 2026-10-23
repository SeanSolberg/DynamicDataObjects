unit DataObjects2Utils;

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

uses DateUtils, SysUtils;

{$define cPurePascal}

type
  TBigTimestampValue = packed record
    N: byte;
    ExtType: shortInt;
    NanoSeconds: Cardinal;
    Seconds: int64;
  end;

  // This record gives us the ability to write a buffer of data where we have a 1-byte code followed by either a 1 byte value, 2 byte value,
  // 4 byte value, 8 byte value, or a TBigTimeStamp value.   When using this, we call the right GetFunctions or SetProcedures and internally
  // the value is stored in BigEndian form and not LittleEndian form.   That way, we can put data into this structure and then follow it up with
  // a stream write call.  Or, we can do a stream read call and then read the data from this structure and this record handles the proper byte swapping.
  // Note that this mechanism can be used by multiple different serializers.
  TNumBytes = packed record
    Code: byte;

    procedure SetByteValue(aValue: byte);
    procedure SetIntValue(aValue: integer);
    procedure SetSingleValue(aValue: Single);
    procedure SetInt64Value(aValue: Int64);
    procedure SetUInt64Value(aValue: UInt64);
    procedure SetDoubleValue(aValue: double);
    procedure SetUnsignedIntValue(aValue: Cardinal);
    procedure SetUnsignedInt16Value(aValue: Word);
    procedure SetBigTimestampValue(aSeconds: int64; aNanoSeconds: Cardinal);
  strict private
    //    fBytes: array[0..7] of byte;
    case byte of
      0: (fByteValue: byte;);
      1: (fIntValue: Integer;);  //signed
      2: (fUInt64Value: UInt64;);
      3: (fInt64Value: Int64;);  //signed
      4: (fUnsignedIntValue: Cardinal;);  // unsigned 32 bit
      5: (fUInt16Value: Word);
      6: (fBigTimestampValue: TBigTimestampValue);
    end;



function SwapBytesFromDouble(aDouble: double): UInt64; register;

function SwapBytesToDouble(aDoubleAsUInt64: UInt64): double; register;

function SwapBytesSingle(aSingle: single): Cardinal; overload; register;

function SwapBytesSingle(aSingleAsCardinal: Cardinal): Single; overload; register;

function SwapBytes(aInt64: UInt64): UInt64; overload; register;

function SwapBytes(aInt64: Int64): Int64; overload; register;

function SwapBytes(aValue: Int32): Int32; overload; register;

function SwapBytes(aValue: UInt32): UInt32; overload; register;

function SwapBytes(aValue: UInt16): UInt16; overload; register;

function SwapBytes(aValue: Int16): Int16; overload; register;

function DateTimeToISO8601Str(aDateTime: TDateTime): String;

function DateToISO8601Str(aDateTime: TDateTime): String;

function TimeToISO8601Str(aDateTime: TDateTime): String;

// Support for the Float16 (HalfFloat)
type
  THalfFloat = type Word;
(*  THalfFloat = record
    Value: Word;
    class operator Implicit(a: Single): THalfFloat;
    class operator Implicit(a: THalfFloat): Single;
    class operator Implicit(a: Double): THalfFloat;
    class operator Implicit(a: THalfFloat): Double;
    class operator Explicit(a: Single): THalfFloat;
    class operator Explicit(a: THalfFloat): Single;
    class operator Explicit(a: Double): THalfFloat;
    class operator Explicit(a: THalfFloat): Double;
  end;*)

const
  HalfMin:     Single = 5.96046448e-08; // Smallest positive half
  HalfMinNorm: Single = 6.10351562e-05; // Smallest positive normalized half
  HalfMax:     Single = 65504.0;        // Largest positive half
  // Smallest positive e for which half (1.0 + e) != half (1.0)
  HalfEpsilon: Single = 0.00097656;
  HalfNaN:     THalfFloat = 65535;
  HalfPosInf:  THalfFloat = 31744;
  HalfNegInf:  THalfFloat = 64512;

function HalfToFloat(Half: THalfFloat): Single;
function FloatToHalf(Float: Single): THalfFloat;


implementation


// NOTE:  when swapping bytes on a double floating point number, the resulting bytes could turn into a floating point number that is technically invalid.
//        These are mostly when the Exponent works out to be $FF or $00
// Delphi's assignment of such a single value will result in an exception.  SO, we CANNOT SwapBytes on a double and put the result into a double.
// Rather, we can swap bytes from a double (8 bytes) to a UINT64 (8 bytes) and from a UInt64 (8 bytes) to a Double (8 bytes).
// This whole situation also applies to floating point Singles.
function SwapBytesFromDouble(aDouble: double): UInt64; register;
type
  //enumeration used in variant record
  BytePos = (cDoubleVal, cByteVal, cUInt64Val);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      cDoubleVal: (DoubleVal: double);
       //Overlapping array of bytes
      cByteVal: (Bytes: array[0..7] of byte);
       //Overlapping Cardinal(4bytes) of the single
      cUInt64Val: (UInt64Val: UInt64);
  end;
var
  a,b: EndianCnvRec;
begin
  a.DoubleVal := aDouble;
  B.Bytes[0] := A.Bytes[7];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[6];
  B.Bytes[2] := A.Bytes[5];
  B.Bytes[3] := A.Bytes[4];
  B.Bytes[4] := A.Bytes[3];
  B.Bytes[5] := A.Bytes[2];
  B.Bytes[6] := A.Bytes[1];
  B.Bytes[7] := A.Bytes[0];
  result := b.UInt64Val;
end;

function SwapBytesToDouble(aDoubleAsUInt64: UInt64): double; register;
type
  //enumeration used in variant record
  BytePos = (cDoubleVal, cByteVal, cUInt64Val);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      cDoubleVal: (DoubleVal: double);
       //Overlapping array of bytes
      cByteVal: (Bytes: array[0..7] of byte);
       //Overlapping Cardinal(4bytes) of the single
      cUInt64Val: (UInt64Val: UInt64);
  end;
var
  a,b: EndianCnvRec;
begin
  a.UInt64Val := aDoubleAsUInt64;
  B.Bytes[0] := A.Bytes[7];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[6];
  B.Bytes[2] := A.Bytes[5];
  B.Bytes[3] := A.Bytes[4];
  B.Bytes[4] := A.Bytes[3];
  B.Bytes[5] := A.Bytes[2];
  B.Bytes[6] := A.Bytes[1];
  B.Bytes[7] := A.Bytes[0];
  result := b.DoubleVal;
end;

function SwapBytesSingle(aSingle: single): Cardinal; overload; register;
type
  //enumeration used in variant record
  BytePos = (cSingleVal, cByteVal, cCardinalVal);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      cSingleVal: (SingleVal: single);
       //Overlapping array of bytes
      cByteVal: (Bytes: array[0..3] of byte);
       //Overlapping Cardinal(4bytes) of the single
      cCardinalVal: (CardinalVal: Cardinal);
  end;
var
  a,b: EndianCnvRec;
begin
  a.SingleVal := aSingle;
  B.Bytes[0] := A.Bytes[3];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[2];
  B.Bytes[2] := A.Bytes[1];
  B.Bytes[3] := A.Bytes[0];
  result := b.CardinalVal;
end;

function SwapBytesSingle(aSingleAsCardinal: Cardinal): Single; overload; register;
type
  //enumeration used in variant record
  BytePos = (cSingleVal, cByteVal, cCardinalVal);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      cSingleVal: (SingleVal: single);
       //Overlapping array of bytes
      cByteVal: (Bytes: array[0..3] of byte);
       //Overlapping Cardinal(4bytes) of the single
      cCardinalVal: (CardinalVal: Cardinal);
  end;
var
  a,b: EndianCnvRec;
begin
  a.CardinalVal := aSingleAsCardinal;
  B.Bytes[0] := A.Bytes[3];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[2];
  B.Bytes[2] := A.Bytes[1];
  B.Bytes[3] := A.Bytes[0];
  result := b.SingleVal;
end;

function SwapBytes(aInt64: UInt64): UInt64; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  PEndianCnvRec = ^EndianCnvRec;

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: UInt64);
       //Overlapping bytes of the double
      ByteVal: (Bytes: array[0..SizeOf(UInt64)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aInt64;
  B.Bytes[0] := A.Bytes[7];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[6];
  B.Bytes[2] := A.Bytes[5];
  B.Bytes[3] := A.Bytes[4];
  B.Bytes[4] := A.Bytes[3];
  B.Bytes[5] := A.Bytes[2];
  B.Bytes[6] := A.Bytes[1];
  B.Bytes[7] := A.Bytes[0];
  result := b.EndianVal;
end;

function SwapBytes(aInt64: Int64): Int64; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  PEndianCnvRec = ^EndianCnvRec;

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: Int64);
       //Overlapping bytes of the int64
      ByteVal: (Bytes: array[0..SizeOf(Int64)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aInt64;
  B.Bytes[0] := A.Bytes[7];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[6];
  B.Bytes[2] := A.Bytes[5];
  B.Bytes[3] := A.Bytes[4];
  B.Bytes[4] := A.Bytes[3];
  B.Bytes[5] := A.Bytes[2];
  B.Bytes[6] := A.Bytes[1];
  B.Bytes[7] := A.Bytes[0];
  result := b.EndianVal;
end;

{$ifdef cPurePascal}
function SwapBytes(aValue: UInt16): UInt16; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: UInt16);
       //Overlapping bytes of the Int32
      ByteVal: (Bytes: array[0..1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aValue;
  B.Bytes[0] := A.Bytes[1];
  B.Bytes[1] := A.Bytes[0];
  result := b.EndianVal;
//  result := (aValue shr 8) or (aValue shl 8);    was generating a RangeType error but only on some builds.
end;

function SwapBytes(aValue: Int32): Int32; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: Int32);
       //Overlapping bytes of the Int32
      ByteVal: (Bytes: array[0..SizeOf(Int32)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aValue;
  B.Bytes[0] := A.Bytes[3];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[2];
  B.Bytes[2] := A.Bytes[1];
  B.Bytes[3] := A.Bytes[0];
  result := b.EndianVal;
end;

function SwapBytes(aValue: UInt32): UInt32; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: UInt32);
       //Overlapping bytes of the UInt32
      ByteVal: (Bytes: array[0..SizeOf(UInt32)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aValue;
  B.Bytes[0] := A.Bytes[3];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[2];
  B.Bytes[2] := A.Bytes[1];
  B.Bytes[3] := A.Bytes[0];
  result := b.EndianVal;
end;

{$else}
  {$ifdef WIN64}
    function SwapBytes(aValue: Int32): Int32; overload; register;
    asm
      bswap ecx       // param comes in in ecx
      mov eax, ecx;   // result goes out in eax
    end;

    function SwapBytes(aValue: UInt32): UInt32; overload; register;
    asm
      bswap ecx       // param comes in in ecx
      mov eax, ecx;   // result goes out in eax
    end;

    function SwapBytes(aValue: UInt16): UInt16; overload; register;
    asm
      rol cx, 8       // param comes in in ecx
      mov eax, ecx;   // result goes out in eax
    end;
  {$else}
    // 32 bit versions of these swap bytes
    function SwapBytes(aValue: Int32): Int32; overload; register;
    asm
      bswap eax      // param comes in in eax, result goes out in eax.
    end;

    function SwapBytes(aValue: UInt32): UInt32; overload; register;
    asm
      bswap eax     // param comes in in eax, result goes out in eax.
    end;

    function SwapBytes(aValue: UInt16): UInt16; overload; register;
    asm
      rol ax, 8      // param comes in in eax, result goes out in eax.
    end;
  {$endif}
{$endif}

function SwapBytes(aValue: Int16): Int16; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  PEndianCnvRec = ^EndianCnvRec;

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: smallInt);
       //Overlapping bytes of the double
      ByteVal: (Bytes: array[0..SizeOf(smallInt)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aValue;
  B.Bytes[0] := A.Bytes[1];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[0];
  result := b.EndianVal;
end;

function DateTimeToISO8601Str(aDateTime: TDateTime): String;
var
  lYears: Word;
  lMonths: Word;
  lDays: Word;
  lHours: Word;
  lMinutes: Word;
  lSeconds: Word;
  lMilliSeconds: Word;
begin
  DecodeDateTime(aDateTime, lYears, lMonths, lDays, lHours, lMinutes, lSeconds, lMilliSeconds);
  result := Format('%.4d-%.2d-%.2dT%.2d%s%.2d%s%.2d%s%.3dZ', [lYears, lMonths, lDays, lHours, ':', lMinutes, ':', lSeconds, '.', lMilliSeconds]);
end;

function DateToISO8601Str(aDateTime: TDateTime): String;
var
  lYears: Word;
  lMonths: Word;
  lDays: Word;
  lHours: Word;
  lMinutes: Word;
  lSeconds: Word;
  lMilliSeconds: Word;
begin
  DecodeDateTime(aDateTime, lYears, lMonths, lDays, lHours, lMinutes, lSeconds, lMilliSeconds);
  result := Format('%.4d-%.2d-%.2d', [lYears, lMonths, lDays]);
end;

function TimeToISO8601Str(aDateTime: TDateTime): String;
var
  lYears: Word;
  lMonths: Word;
  lDays: Word;
  lHours: Word;
  lMinutes: Word;
  lSeconds: Word;
  lMilliSeconds: Word;
begin
  DecodeDateTime(aDateTime, lYears, lMonths, lDays, lHours, lMinutes, lSeconds, lMilliSeconds);
  result := Format('%.2d%s%.2d%s%.2d%s%.3dZ', [lHours, ':', lMinutes, ':', lSeconds, '.', lMilliSeconds]);
end;

{ TNumBytes }
procedure TNumBytes.SetByteValue(aValue: byte);
begin
  fByteValue := aValue;
end;

procedure TNumBytes.SetIntValue(aValue: Integer);
begin
  fIntValue := SwapBytes(aValue);
end;

procedure TNumBytes.SetSingleValue(aValue: Single);
begin
  fUnsignedIntValue := SwapBytesSingle(aValue);
end;

procedure TNumBytes.SetInt64Value(aValue: Int64);
begin
  fInt64Value := SwapBytes(aValue);
end;

procedure TNumBytes.SetDoubleValue(aValue: double);
begin
  fUInt64Value := SwapBytesFromDouble(aValue);
end;

procedure TNumBytes.SetUnsignedIntValue(aValue: Cardinal);
begin
  fUnsignedIntValue := SwapBytes(aValue);
end;

procedure TNumBytes.SetUInt64Value(aValue: UInt64);
begin
  fUInt64Value := aValue;
end;

procedure TNumBytes.SetUnsignedInt16Value(aValue: Word);
begin
  fUInt16Value := SwapBytes(aValue);
end;

procedure TNumBytes.SetBigTimestampValue(aSeconds: int64; aNanoSeconds: Cardinal);
begin
  fBigTimestampValue.N := 12;
  fBigTimeStampValue.ExtType := -1;
  fBigTimestampValue.NanoSeconds := SwapBytes(aNanoSeconds);
  fBigTimestampValue.Seconds := SwapBytes(aSeconds);
end;

function FloatToHalf(Float: Single): THalfFloat;
var
  Src: LongWord;
  Sign, Exp, Mantissa: LongInt;
begin
  Src := PLongWord(@Float)^;
  // Extract sign, exponent, and mantissa from Single number
  Sign := LongInt(Src shr 31);
  Exp := LongInt((Src and $7F800000) shr 23) - 127 + 15;
  Mantissa := LongInt(Src and $007FFFFF);

  if (Exp > 0) and (Exp < 30) then
  begin
    // Simple case - round the significand and combine it with the sign and exponent
    Result := THalfFloat((Sign shl 15) or (Exp shl 10) or ((Mantissa + $00001000) shr 13));
  end
  else if Src = 0 then
  begin
    // Input float is zero - return zero
    Result := 0;
  end
  else
  begin
    // Difficult case - lengthy conversion
    if Exp <= 0 then
    begin
      if Exp < -10 then
      begin
        // Input float's value is less than HalfMin, return zero
         Result := 0;
      end
      else
      begin
        // Float is a normalized Single whose magnitude is less than HalfNormMin.
        // We convert it to denormalized half.
        Mantissa := (Mantissa or $00800000) shr (1 - Exp);
        // Round to nearest
        if (Mantissa and $00001000) > 0 then
          Mantissa := Mantissa + $00002000;
        // Assemble Sign and Mantissa (Exp is zero to get denormalized number)
        Result := THalfFloat((Sign shl 15) or (Mantissa shr 13));
      end;
    end
    else if Exp = 255 - 127 + 15 then
    begin
      if Mantissa = 0 then
      begin
        // Input float is infinity, create infinity half with original sign
        Result := THalfFloat((Sign shl 15) or $7C00);
      end
      else
      begin
        // Input float is NaN, create half NaN with original sign and mantissa
        Result := THalfFloat((Sign shl 15) or $7C00 or (Mantissa shr 13));
      end;
    end
    else
    begin
      // Exp is > 0 so input float is normalized Single

      // Round to nearest
      if (Mantissa and $00001000) > 0 then
      begin
        Mantissa := Mantissa + $00002000;
        if (Mantissa and $00800000) > 0 then
        begin
          Mantissa := 0;
          Exp := Exp + 1;
        end;
      end;

      if Exp > 30 then
      begin
        // Exponent overflow - return infinity half
        Result := THalfFloat((Sign shl 15) or $7C00);
      end
      else
        // Assemble normalized half
        Result := THalfFloat((Sign shl 15) or (Exp shl 10) or (Mantissa shr 13));
    end;
  end;
end;

function HalfToFloat(Half: THalfFloat): Single;
var
  Dst, Sign, Mantissa: LongWord;
  Exp: LongInt;
begin
  // Extract sign, exponent, and mantissa from half number
  Sign := Half shr 15;
  Exp := (Half and $7C00) shr 10;
  Mantissa := Half and 1023;

  if (Exp > 0) and (Exp < 31) then
  begin
    // Common normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (LongWord(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, Exp - 15) * (1 + Mantissa / 1024);
  end
  else if (Exp = 0) and (Mantissa = 0) then
  begin
    // Zero - preserve sign
    Dst := Sign shl 31;
  end
  else if (Exp = 0) and (Mantissa <> 0) then
  begin
    // Denormalized number - renormalize it
    while (Mantissa and $00000400) = 0 do
    begin
      Mantissa := Mantissa shl 1;
      Dec(Exp);
    end;
    Inc(Exp);
    Mantissa := Mantissa and not $00000400;
    // Now assemble normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (LongWord(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, -14) * (Mantissa / 1024);
  end
  else if (Exp = 31) and (Mantissa = 0) then
  begin
    // +/- infinity
    Dst := (Sign shl 31) or $7F800000;
  end
  else //if (Exp = 31) and (Mantisa <> 0) then
  begin
    // Not a number - preserve sign and mantissa
    Dst := (Sign shl 31) or $7F800000 or (Mantissa shl 13);
  end;

  // Reinterpret LongWord as Single
  Result := PSingle(@Dst)^;
end;

(*  NOTE SURE ABOUT THE OPERATORS YET - FINISH

{ THalfFloat }

class operator THalfFloat.Explicit(a: Single): THalfFloat;
begin
  result := FloatToHalf(a);
end;

class operator THalfFloat.Explicit(a: THalfFloat): Single;
begin
  result := HalfToFloat(a);
end;

class operator THalfFloat.Explicit(a: THalfFloat): Double;
begin
  result := HalfToFloat(a);
end;

class operator THalfFloat.Explicit(a: Double): THalfFloat;
begin
  result := FloatToHalf(a);
end;

class operator THalfFloat.Implicit(a: THalfFloat): Single;
begin
  result := HalfToFloat(a);
end;

class operator THalfFloat.Implicit(a: Single): THalfFloat;
begin
  result.Value := FloatToHalf(a);
end;

class operator THalfFloat.Implicit(a: Double): THalfFloat;
begin
  result := FloatToHalf(a);
end;

class operator THalfFloat.Implicit(a: THalfFloat): Double;
begin
  result := HalfToFloat(a);
end;

*)

end.
