unit VarInt;

interface

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

uses sysUtils, classes{, math};

(* This code implements a 64 bit VarInt as defined by Google's protocol buffers
   as well as a 64 bit Unsigned VarInt that is the same except it doesn't do the zigZag Encoding part.

   When the number is serialized, it will produce 1 to 10 bytes to represent the number.
   The MSB in each serialized byte defines if the byte is an intermediate byte or the last byte.
   1 means there are more bytes that follow
   0 means that this byte is the last byte in the stream for this VarInt value.

   for a full 64 bit integer, the MSB is the negative bit.  This bit is rotated to the LSB before serializing
   so that small negative numbers only serialize with one byte.  I believe they call this zig-zag encoding.
   So for the 7 bits that make up the number, we can only go from
   -64 to 63 within the 1 byte serialization.
   -8192 to 8191 within the 2 byte serialization, etc.
*)


type


  // signed 32bit VarInt with zig-zag encoding
  TVarInt32 = record
  private
    Value: int32;
  public
    class function ZigZagEncode32(aValue: integer): Cardinal; static;
    class function ZigZagDecode32(aValue: Cardinal): integer; static;

    procedure WriteToStream(aStream: TStream);
    procedure ReadFromStream(aStream: TStream);

    class operator Implicit(aValue: integer): TVarInt32;
    class operator Implicit(aValue: TVarInt32): integer;
  end;

  // Unsigned VarInt without zig-zag encoding
  TUVarInt32 = record
  private
    Value: UInt32;
  public
    procedure WriteToStream(aStream: TStream);
    procedure ReadFromStream(aStream: TStream);

    class operator Implicit(aValue: UInt32): TUVarInt32;
    class operator Implicit(aValue: TUVarInt32): UInt32;
  end;

// signed 64bit VarInt with zig-zag encoding
  TVarInt64 = record
  private
    Value: int64;
  public
    class function ZigZagEncode64(aValue: int64): Uint64; static;
    class function ZigZagDecode64(aValue: Uint64): int64; static;

    procedure WriteToStream(aStream: TStream);
    procedure ReadFromStream(aStream: TStream);

    class operator Implicit(aValue: Int64): TVarInt64;
    class operator Implicit(aValue: TVarInt64): Int64;
  end;

  // Unsigned VarInt without zig-zag encoding
  TUVarInt64 = record
  private
    Value: UInt64;
  public
    procedure WriteToStream(aStream: TStream);
    procedure ReadFromStream(aStream: TStream);

    class operator Implicit(aValue: UInt64): TUVarInt64;
    class operator Implicit(aValue: TUVarInt64): UInt64;
  end;

  EVarIntException = class(Exception);

ResourceString
  strUnableToReadByte = 'Unable to read a byte from a stream when reading a VarInt.';
  strUnableToReadNumTooBig = 'Error reading VarInt from a stream.  Number is too big for Int64';

//{$define cUseASM}

implementation

{ TVarInt64 }

{$if defined(WIN64) and defined(cUseASM)}
//Encode the signed int64 bit number into a zig-zag encoded unsigned 64 number.
// Pseudocode: return (i >> 31) ^ (i << 1)   // the shift right here is an arithmatic shift right which delphi Pascal does not have, so we do the If below to make it.

class function TVarInt64.ZigZagEncode64(aValue: int64): Uint64; Register;
asm
  mov rax, rcx
  sar rcx, 63
  shl rax, 1
  xor rax, rcx
end;

class function TVarInt64.ZigZagDecode64(aValue: Uint64): int64; Register;  // aValue comes in on RCX, result goes out in RAX
asm
  mov rax, rcx
  shr rcx, 1
  and rax, 1
  neg rax
  xor rax, rcx
end;

{$else}

class function TVarInt64.ZigZagEncode64(aValue: int64): Uint64; Register;
var
  lPart: UInt64;
  lValue: UInt64;
begin
  lPart := $0;
  lValue := UInt64(aValue);    // take the bits as-is into the unsigned space.
  if aValue<0 then
    lPart := UInt64($FFFFFFFFFFFFFFFF);

  result := lPart xor (lValue shl 1);
end;

class function TVarInt64.ZigZagDecode64(aValue: Uint64): int64; Register;
var
  lInt: Int64;
  lUInt: UInt64;
begin
//  result := (aValue shr 1) xor -(aValue and 1);
  // Note: we need to be careful on each operation if it working on a signed value or unsigned value to prevent numbers getting outside a valid value.
  lInt := int64((aValue and 1));   // should result in only a 0 or 1, but must be into a signed into so we can negate it below.
  lUInt := aValue shr 1;
  result := int64(lUint) xor -lInt;
end;

{$endif}




class operator TVarInt64.Implicit(aValue: Int64): TVarInt64;
begin
  result.Value := aValue;
end;

class operator TVarInt64.Implicit(aValue: TVarInt64): Int64;
begin
  result := aValue.Value;
end;


procedure TVarInt64.ReadFromStream(aStream: TStream);
var
  lCount: byte;  //number of bytes read so far.
  lByte: byte;
  lValue: Uint64;
  lShifter: byte;
begin
  lCount := 0;
  lValue := 0;
  lShifter := 0;
  repeat
    if aStream.Read(lByte, 1) = 0 then
      raise EVarIntException.Create(strUnableToReadByte);

    if lCount<9 then
    begin
      lValue := lValue or (Uint64(lByte and $7F) shl lShifter);
      lShifter := lShifter + 7;
    end
    else
    begin
      // read the final (10th) byte, but we can only use one bit from it.  9*7=63 bits, so we only need one more bit from the 10th byte.
      if lByte > 1 then
        raise Exception.Create(strUnableToReadNumTooBig)
      else if lByte = 1 then
        lValue := lValue + $8000000000000000;
    end;
    inc(lCount);
  until (lByte and $80) = 0;

  Value := ZigZagDecode64(lValue);
end;

procedure TVarInt64.WriteToStream(aStream: TStream);
var
  lBuffer: array[0..9] of byte;
  lValue: Uint64;
  i: byte;
begin
  i:=0;
  lValue := ZigZagEncode64(Value);
  while true do
  begin
    lBuffer[i] := lValue and $7F;    // take only 7 bits
    if lValue < $80 then
      break
    else
    begin
      lBuffer[i] := lbuffer[i] or $80;  // set the msb of this byte to signal that there are more bytes to follow.
      inc(i);
      lValue := lValue shr 7;           // more bytes to follow so prepare to pickup the next 7 bits.
    end;
  end;
  aStream.write(lBuffer,i+1);
end;



{ TUVarInt64 }

class operator TUVarInt64.Implicit(aValue: UInt64): TUVarInt64;
begin
  result.Value := aValue;
end;

class operator TUVarInt64.Implicit(aValue: TUVarInt64): UInt64;
begin
  result := aValue.Value;
end;


procedure TUVarInt64.ReadFromStream(aStream: TStream);
var
  lCount: byte;  //number of bytes read so far.
  lByte: byte;
  lShifter: byte;
begin
  lCount := 0;
  Value := 0;
  lShifter := 0;
  repeat
    if aStream.Read(lByte, 1) = 0 then
      raise EVarIntException.Create(strUnableToReadByte);

    if lCount<9 then
    begin
      Value := Value or (Uint64(lByte and $7F) shl lShifter);
      lShifter := lShifter + 7;
    end
    else
    begin
      // read the final (10th) byte, but we can only use one bit from it.  9*7=63 bits, so we only need one more bit from the 10th byte.
      if lByte > 1 then
        raise Exception.Create(strUnableToReadNumTooBig)
      else if lByte = 1 then
        Value := Value + $8000000000000000;
    end;
    inc(lCount);
  until (lByte and $80) = 0;

end;

procedure TUVarInt64.WriteToStream(aStream: TStream);
var
  lBuffer: array[0..9] of byte;
  lValue: Uint64;
  i: byte;
begin
  i:=0;
  lValue := Value;
  while true do
  begin
    lBuffer[i] := lValue and $7F;    // take only 7 bits
    if lValue < $80 then
      break
    else
    begin
      lBuffer[i] := lbuffer[i] or $80;  // set the msb of this byte to signal that there are more bytes to follow.
      inc(i);
      lValue := lValue shr 7;           // more bytes to follow so prepare to pickup the next 7 bits.
    end;
  end;
  aStream.write(lBuffer,i+1);
end;

{ TUVarInt32 }

class operator TUVarInt32.Implicit(aValue: UInt32): TUVarInt32;
begin
  result.Value := aValue;
end;

class operator TUVarInt32.Implicit(aValue: TUVarInt32): UInt32;
begin
  result := aValue.Value;
end;

procedure TUVarInt32.ReadFromStream(aStream: TStream);
var
  lCount: byte;  //number of bytes read so far.
  lByte: byte;
  lShifter: byte;
begin
  lCount := 0;
  Value := 0;
  lShifter := 0;
  repeat
    if aStream.Read(lByte, 1) = 0 then
      raise EVarIntException.Create(strUnableToReadByte);

    if lCount<4 then
    begin
      Value := Value or (Uint32(lByte and $7F) shl lShifter);
      lShifter := lShifter + 7;
    end
    else
    begin
      // read the final (5th) byte, but we can only use 5 bits from it.  5*7=35 bits, so we only need 5 more bit5 from the 5th byte.
      if lByte > $1F then
        raise Exception.Create(strUnableToReadNumTooBig)
      else
        Value := Value or Uint32(lByte) shl 28;   // get the top 5 bits in place.
    end;
    inc(lCount);
  until (lByte and $80) = 0;

end;

procedure TUVarInt32.WriteToStream(aStream: TStream);
var
  lBuffer: array[0..5] of byte;
  lValue: UInt32;
  i: byte;
begin
  i:=0;
  lValue := Value;
  while true do
  begin
    lBuffer[i] := lValue and $7F;    // take only 7 bits
    if lValue < $80 then
      break
    else
    begin
      lBuffer[i] := lbuffer[i] or $80;  // set the msb of this byte to signal that there are more bytes to follow.
      inc(i);
      lValue := lValue shr 7;           // more bytes to follow so prepare to pickup the next 7 bits.
    end;
  end;
  aStream.write(lBuffer,i+1);
end;

{ TVarInt32 }

class operator TVarInt32.Implicit(aValue: integer): TVarInt32;
begin
  result.Value := aValue;
end;

class operator TVarInt32.Implicit(aValue: TVarInt32): integer;
begin
  result := aValue.Value;
end;

procedure TVarInt32.ReadFromStream(aStream: TStream);
var
  lCount: byte;  //number of bytes read so far.
  lByte: byte;
  lValue: Uint32;
  lShifter: byte;
begin
  lCount := 0;
  lValue := 0;
  lShifter := 0;
  repeat
    if aStream.Read(lByte, 1) = 0 then
      raise EVarIntException.Create(strUnableToReadByte);


    if lCount=4 then
    begin
      // reading the final (5th) byte, but we can only use 4 bit from it.  4*7=28 bits. If we are given more, then we have error.
      if lByte > $F then
        raise Exception.Create(strUnableToReadNumTooBig)
    end;

    lValue := lValue or (Uint32(lByte and $7F) shl lShifter);
    lShifter := lShifter + 7;

    inc(lCount);
  until (lByte and $80) = 0;

  Value := ZigZagDecode32(lValue);
end;

procedure TVarInt32.WriteToStream(aStream: TStream);
var
  lBuffer: array[0..4] of byte;
  lValue: Uint32;
  i: byte;
begin
  i:=0;
  lValue := ZigZagEncode32(Value);
  while true do
  begin
    lBuffer[i] := lValue and $7F;    // take only 7 bits
    if lValue < $80 then
      break
    else
    begin
      lBuffer[i] := lbuffer[i] or $80;  // set the msb of this byte to signal that there are more bytes to follow.
      inc(i);
      lValue := lValue shr 7;           // more bytes to follow so prepare to pickup the next 7 bits.
    end;
  end;
  aStream.write(lBuffer,i+1);
end;


{$if defined(WIN64) and defined(cUseASM)}
class function TVarInt32.ZigZagEncode32(aValue: int32): Uint32;  Register;  // For 64bit compiler, aValue comes in on ECX, result goes out in EAX
asm
  // For 64bit compiler only
  mov eax, ecx
  sar ecx, 31
  shl eax, 1
  xor eax, ecx
end;

class function TVarInt32.ZigZagDecode32(aValue: Uint32): int32;  Register; // For 64bit compiler, aValue comes in on ECX, result goes out in EAX
asm
  // For 64bit compiler only
  mov eax, ecx
  shr ecx, 1
  and eax, 1
  neg eax
  xor eax, ecx
end;

{$elseif defined(WIN32) and defined(cUseASM)}

class function TVarInt32.ZigZagEncode32(aValue: int32): Uint32;  Register;  // For 32bit compiler, // aValue comes in on EAX, result goes out in EAX
asm
  // For 32bit compiler only
  mov ecx, eax
  sar ecx, 31
  shl eax, 1
  xor eax, ecx
end;

class function TVarInt32.ZigZagDecode32(aValue: Uint32): int32;  Register;  // For 32bit compiler, // aValue comes in on EAX, result goes out in EAX
asm
  // For 32bit compiler only
  mov ecx, eax
  shr ecx, 1
  and eax, 1
  neg eax
  xor eax, ecx
end;

{$else}

// This section is for compilers where we don't have an assembly version.
class function TVarInt32.ZigZagEncode32(aValue: int32): Uint32;  Register;  // For 32bit compiler, // aValue comes in on EAX, result goes out in EAX
var
  lPart: UInt32;
  lValue: UInt32;
begin
  lPart := $0;
  lValue := UInt32(aValue);    // take the bits as-is into the unsigned space.
  if aValue<0 then
    lPart := UInt32($FFFFFFFF);

  result := lPart xor (lValue shl 1);
end;

class function TVarInt32.ZigZagDecode32(aValue: Uint32): int32;  Register;  // For 32bit compiler, // aValue comes in on EAX, result goes out in EAX
var
  lInt: Int32;
  lUInt: UInt32;
begin
//  result := (aValue shr 1) xor -(aValue and 1);
  // Note: we need to be careful on each operation if it working on a signed value or unsigned value to prevent numbers getting outside a valid value.
  lInt := int32((aValue and 1));   // should result in only a 0 or 1, but must be into a signed into so we can negate it below.
  lUInt := aValue shr 1;
  result := int32(lUint) xor -lInt;
end;


{$endif}





end.
