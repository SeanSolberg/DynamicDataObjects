unit SlotNameIndex;

{********************************************************************************}
{                                                                                }
{                         Dynamic Data Objects Library                           }
{                                                                                }
{                                                                                }
{ MIT License                                                                    }
{                                                                                }
{ Copyright (c) 2025 Sean Solberg                                                }
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

{ This unit provides a TSlotNameIndex that implements a hash-based string-to-value lookup very similar to a TDictionary.
  However, this index handles deleting specially so that when an item in the index is deleted, all items are decremented that
  have an index value after the one being deleted. }


interface

//uses TextFileStream, classes;

type

  TIndexEntry = packed record
    HashCode: Integer;
    Name: String;
    Index: Integer;
  end;
  TMapEntryArray = array of TIndexEntry;



  TSlotNameIndex = record
  private const
    EMPTY_HASH = -1;
  private
    FEntries: TMapEntryArray;
    FCount: Integer;
    FCapacity: Integer;
    FGrowThreshold: Integer;   // if adding and our count gets over this number, then we will perform a grow.  We set this to about 75% of capacity to keep collisions from getting to be too much.
    FCaseSensitive: boolean;   // Changing this really should be done before any calls to Add.  You can change it though, which will cause a rebuild of the index.
  private
    function FindEntryIndex(const aKey: string; aHashCode: Integer): Integer;  // returns -1 if not found.
    procedure Resize(ANewSize: Integer);
    procedure SetCaseSensitive(aValue: boolean);
    function MakeHash(var aKey: string): Integer;     // NOte that the MakeHash call could possibly change the case on aKey if we are setup for a case in-sensitive map
  public
    procedure Initialize;
    procedure Free;
    procedure Clear;
    function Get(const aKey: String): Integer;    // returns -1 if not found.
    function Add(const aKey: String; const AIndex: Integer): integer;
    function Delete(const aKey: String): boolean;  // returns true if aKey was found an a delete happened.
//    function TryGetValue(const aKey: string; var aValue: integer): boolean;

    property CaseSensitive: boolean read fCaseSensitive write SetCaseSensitive;
    property Count: integer read fCount;
  end;


{ Computes a 32-bit MurmurHash2 value, as described here:
    https://sites.google.com/site/murmurhash/
  This hash algorithm is fast and very resistant to hash collisions, making it
  ideal for hash tables.

  Parameters:
    AData: pointer to the data to hash.
    ALen: size of the data.

  Returns:
    The hash code. The hash code is guaranteed never to be negative.

  NOTE: There is a newer version called MurmurHash3, which is potentially even
  faster. However, it uses some C language features (to achieve this
  performance) that are not available in Delphi. }
function MurmurHash2(const AData; ALen: Integer): Integer;


implementation

uses SysUtils;

{TSlotNameIndex}

// will return the same AIndex passed in if the index performed an add.
// If aKey already exists, then this will return the index value from that key.
function TSlotNameIndex.Add(const aKey: String; const AIndex: Integer): integer;
var
  lMask, lEntryIndex, lHashCode, lHC: Integer;
  lKey: string;
begin
  lKey := aKey;
  if (FCount >= FGrowThreshold) then
    Resize(FCapacity * 2);

  if (lKey = '') then
    lHashCode := 0
  else
    lHashCode := MakeHash(lKey);   // Note that lKey could be modified by MakeHash based on case sensitivity.
  lMask := FCapacity - 1;          // Note that fCapacity is always expected to be a multiple of 2 for bit alignment.
  lEntryIndex := lHashCode and lMask;

  while True do
  begin
    lHC := FEntries[lEntryIndex].HashCode;
    if (lHC = EMPTY_HASH) then
      Break;

    if (lHC = lHashCode) and (FEntries[lEntryIndex].Name = lKey) then
    begin
      // We were able to find this key in this index already, so return it's Index value instead of performing an add operation.
      Exit(FEntries[lEntryIndex].Index);
    end;

    lEntryIndex := (lEntryIndex + 1) and lMask;
  end;

  FEntries[lEntryIndex].HashCode := lHashCode;
  FEntries[lEntryIndex].Name := lKey;    // This has to be the same as what was hashed, possibly case sensitive or case insensitive.
  FEntries[lEntryIndex].Index := AIndex;
  result := aIndex;                      // return the new entry's index value
  Inc(FCount);
end;

procedure TSlotNameIndex.Clear;
var
  I: Integer;
begin
  for I := 0 to FCapacity - 1 do
  begin
    FEntries[I].Name := '';
    FEntries[I].HashCode := EMPTY_HASH;
  end;
  FCount := 0;

//  Just because we are clearing the index doesn't mean we should re-allocated fEntries.
//  We might be re-building from the CreateSlotnameIndex call, so leave this allocation intact.
//  SetLength(fEntries, 0);
//  FCapacity:=0;
//  FGrowThreshold:=0;
end;

function TSlotNameIndex.Delete(const aKey: String): boolean;   // returns true if the aKey was found and a delete happened.
var
  lGapEntryIndex: integer;
  lCheckEntryIndex: integer;
  lStartingEntryIndex: integer;
  lHashBasedEntryIndex: integer;
  lHashCode: integer;
  lDeletingIndexVal: integer;
  lCheckIndexVal: integer;
  lCheckHashCode: integer;
  lMask: integer;
  lCanMove: boolean;
  lKey: String;

  function InCircularRange(aBottom, aItem, aTopInc: integer): Boolean;
  begin
    Result := (aBottom < aItem) and (aItem <= aTopInc)      // normal
           or (aTopInc < aBottom) and (aItem > aBottom)     // top wrapped
           or (aTopInc < aBottom) and (aItem <= aTopInc);   // top and item wrapped
  end;
begin
  lMask := FCapacity - 1;         // FCapacity is always a multiple of 2 and must only be a multiple of 2 so that our mask works.
  lKey := aKey;
  lHashCode := MakeHash(lKey);    // note that MakeHash can possibly update lKey if we are using case Insensitive keys.

  lStartingEntryIndex := FindEntryIndex(lKey, lHashCode);   // this call finds the exact
  if (lStartingEntryIndex = -1) then
    Exit(false);

  // We found the lStartingEntryIndex, for lKey, so we can continue on and do the delete operation
  lDeletingIndexVal := FEntries[lStartingEntryIndex].Index;
  FEntries[lStartingEntryIndex].HashCode := EMPTY_HASH;
  FEntries[lStartingEntryIndex].Name := default(string);   // Don't have to do this, but it removes the ref to the string allowing it to be freed.
  FEntries[lStartingEntryIndex].Index := default(integer);
  Dec(FCount);

  // The process is to update the FEntries looking at two things that need to change:
  // 1. any entries that have an Index value greater than the one we are deleting should be decremented.
  //    because of this, we must go through the whole FEntries array including wrapping to the beginning and all the way up to the item we started with.
  // 2. possibly shift following entries into the place of the one we just deleted for the ones that have the same hash, continuing until we hit a gap.
  //    we will keep shifting entries back to the gap, but also skipping any that we hit that are not allowed to be moved back because their starting hash index is after the gap.
  lGapEntryIndex := lStartingEntryIndex;
  lCheckEntryIndex := (lStartingEntryIndex + 1) and lMask;  // Get the Next Entry Index accomodating wrapping.
  lCanMove := true;
  while lCheckEntryIndex <> lStartingEntryIndex do    // loop until we hit our starting point again, we must go through the whole circular list.
  begin
    //1. See if we should be decrementing this entry's Index value.
    lCheckIndexVal := FEntries[lCheckEntryIndex].Index;
    if lCheckIndexVal > lDeletingIndexVal then
    begin
      FEntries[lCheckEntryIndex].Index := lCheckIndexVal - 1;   // decrement this entry's index val because it's higher than the one we are deleting.
    end;

    //2. See if we can move this entry back to the gap
    if lCanMove then
    begin
      lCheckHashCode := FEntries[lCheckEntryIndex].HashCode;
      if lCheckHashCode = EMPTY_HASH then
      begin
        lCanMove := false;    // We are done moving entries.
      end
      else
      begin
        lHashBasedEntryIndex := lCheckHashCode and lMask;

        // If the hash->to->EntryIndex value is between the Gap and our current EntryIndex we are checking then we CANNOT move it because it won't be findable in the future then.
        if not InCircularRange(lGapEntryIndex, lHashBasedEntryIndex, lCheckEntryIndex) then
        begin
          // This Entry needs to be moved back to the gap and it becomes the new gap with an empty hash.
          FEntries[lGapEntryIndex] := FEntries[lCheckEntryIndex];
          lGapEntryIndex := lCheckEntryIndex;

          FEntries[lCheckEntryIndex].HashCode := EMPTY_HASH;
          FEntries[lCheckEntryIndex].Name := default(string);    // we are doing the effort to remove the reference to Name so that it can possibly be freed.
          //FEntries[lCheckEntryIndex].Index := default(integer);  // Unnecessary
        end;
      end;
    end;

    // Get the Next Entry Index accomodating wrapping.
    lCheckEntryIndex := (lCheckEntryIndex + 1) and lMask;
  end;
  result := true;
end;

procedure TSlotNameIndex.Free;
begin
  Clear;
  SetLength(FEntries,0);
end;

// Note that aKey must be exact to what is in FEntries.
function TSlotNameIndex.FindEntryIndex(const aKey: string; aHashCode: Integer): Integer;  // returns -1 if not found.
var
  Mask, lEntryIndex, HC: Integer;
begin
  if (FCount = 0) then
    Exit(-1);

  Mask := FCapacity - 1;
  lEntryIndex := aHashCode and Mask;

  while True do
  begin
    HC := FEntries[lEntryIndex].HashCode;
    if (HC = EMPTY_HASH) then
      Exit(-1);

    if (HC = aHashCode) and (FEntries[lEntryIndex].Name = aKey) then
      Exit(lEntryIndex);

    lEntryIndex := (lEntryIndex + 1) and Mask;
  end;
end;

function TSlotNameIndex.Get(const aKey: String): Integer;  // returns -1 if not found.
var
  lEntryIndex: Integer;
  lHashCode: integer;
  lKey: string;
begin
  lKey := aKey;
  lHashCode := MakeHash(lKey);

  lEntryIndex := FindEntryIndex(lKey, lHashCode);
  if (lEntryIndex = -1) then
    Exit(-1);

  Exit(FEntries[lEntryIndex].Index);    // return the actual index value we are looking for
end;

procedure TSlotNameIndex.Initialize;
begin
  FEntries:=nil;
  FCount:=0;
  FCapacity:=0;
  FGrowThreshold:=0;
  FCaseSensitive:=true;
end;

function TSlotNameIndex.MakeHash(var aKey: string): Integer;   // Note that aKey could possibly be modified within this function.
begin
  if not fCaseSensitive then
  begin
    aKey := AnsiLowerCase(aKey);  // we are updating aKey so the caller can have it if needed.
  end;

  result := MurmurHash2(aKey[Low(String)], Length(aKey) * SizeOf(Char));
end;

procedure TSlotNameIndex.Resize(ANewSize: Integer);
const
  MIN_SIZE = 32; // Must be POT and >= 1.33 * INDICES_COUNT_THRESHOLD
var
  i: integer;
  lNewMask, lNewIndex: Integer;
  lNewEntries: TMapEntryArray;
begin
  if (ANewSize < MIN_SIZE) then
    ANewSize := MIN_SIZE;
  lNewMask := ANewSize - 1;

  SetLength(lNewEntries, ANewSize);
  for i := 0 to ANewSize-1 do
    lNewEntries[i].HashCode := EMPTY_HASH;

  for i := 0 to FCapacity - 1 do
  begin
    if (fEntries[i].HashCode <> EMPTY_HASH) then
    begin
      lNewIndex := fEntries[i].HashCode and lNewMask;
      while (lNewEntries[lNewIndex].HashCode <> EMPTY_HASH) do
        lNewIndex := (lNewIndex + 1) and lNewMask;
      lNewEntries[lNewIndex] := fEntries[I];
    end;
  end;

  { Release original entries (and names) }
  for I := 0 to FCapacity - 1 do
    fEntries[I].Name := '';
  SetLength(FEntries,0);

  FCapacity := ANewSize;
  FEntries := lNewEntries;
  FGrowThreshold := (ANewSize * 3) shr 2; // 75%
end;

procedure TSlotNameIndex.SetCaseSensitive(aValue: boolean);
begin
  if FCaseSensitive <> aValue then
  begin
    FCaseSensitive := aValue;

    //FINISH - If we are changing the Case Sensitivity, should we be re-building the entire internal FEntries.
    //Yes, we should, but for now, we will make sure to set this property before making any calls to Add
  end;
end;

{$OVERFLOWCHECKS OFF}

function MurmurHash2(const AData; ALen: Integer): Integer;
{ https://sites.google.com/site/murmurhash/MurmurHash2.cpp?attredirects=0 }
const
  M = $5bd1e995;
  R = 24;
var
  H, K: Cardinal;
  Data: PByte;
  lResult: Cardinal;
label
  label1, label2, label3, finish;
begin
  H := Cardinal(ALen);
  Data := @AData;

  while (ALen >= 4) do
  begin
    K := PCardinal(Data)^;

    K := K * M;
    K := K xor (K shr R);
    K := K * M;

    H := H * M;
    H := H xor K;

    Inc(Data, 4);
    Dec(ALen, 4);
  end;

  case ALen of
    3: goto label3;
    2: goto label2;
    1: goto label1;
  else
    goto finish;
  end;

label3:
  H := H xor (Data[2] shl 16);

label2:
  H := H xor (Data[1] shl 8);

label1:
  H := H xor Data[0];
  H := H * M;

finish:
  H := H xor (H shr 13);
  H := H * M;
  lResult := (H xor (H shr 15)) and $7FFFFFFF;

  result := PInteger(@lResult)^;               // Bitwise cast: Reinterprets the raw bits as a signed integer without changing them or checking range.
end;


end.
