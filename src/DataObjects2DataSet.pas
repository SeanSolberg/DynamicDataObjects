unit DataObjects2DataSet;

{ This unit will copy data from an active dataSet into a dataObject structured as an array of Frames.

  in the future, this will be modified to allow for variations in how the table data should be structured within the dataObject such as
  possibly Array Of Frames, Array of Arrays in a Data field with a separate Fields field, whether to include null fields or not, etc.

  Also, someday we will probably go from DataObj to DataSet as well with either straight appending or updating/Appending}

interface

uses Classes, DB, DataObjects2;

procedure DataSetToDataObj(aDataSet: TDataSet; aDataObj: TDataObj);
procedure FieldToDataObj(aField: TField; aFieldDataObj: TDataObj);

implementation

// This procedure will fill a DataObj from a TDataset and have that data structured as array of frames with fields that are Null being skipped.
procedure DataSetToDataObj(aDataSet: TDataSet; aDataObj: TDataObj);
var
  i: Integer;
  lField: TField;
  lRecordFrame: TDataFrame;
  lFieldDataObj: TDataObj;
begin
  while not aDataSet.eof do
  begin
    lRecordFrame := aDataObj.AsArray.NewSlot.AsFrame;

    for i := 0 to aDataSet.FieldCount-1 do
    begin
       lField := aDataSet.Fields[i];
       if not lField.IsNull then
       begin
         lFieldDataObj := lRecordFrame.newSlot(lField.fieldname);
         FieldToDataObj(lField, lFieldDataObj);
       end;
    end;
    aDataSet.Next;
  end;
end;

// This procedure will fill a DataObj from the data in a TField
procedure FieldToDataObj(aField: TField; aFieldDataObj: TDataObj);
var
  j: integer;
  lBytes: TArray<Byte>;
  lBlobStream: TStream;
begin
   case aField.DataType of
     ftUnknown: ;  // nothing to do.
     ftString: aFieldDataObj.AsString := aField.AsString;
     ftSmallint,
     ftInteger,
     ftWord: aFieldDataObj.AsInt32 := aField.AsInteger;
     ftBoolean: aFieldDataObj.AsBoolean := aField.AsBoolean;
     ftFloat,
     ftCurrency: aFieldDataObj.AsDouble := aField.AsFloat;  //DataObject doesn't have a currency, so treat it as a double.
     ftBCD: begin
       with aFieldDataObj.AsBinary do
       begin
         WriteData(aField.AsBCD.Precision,1);
         WriteData(aField.AsBCD.SignSpecialPlaces,1);
         WriteData(aField.AsBCD.Fraction,sizeof(aField.AsBCD.Fraction));
       end;
     end;
     ftDate: aFieldDataObj.AsDate := aField.AsDateTime;
     ftTime: aFieldDataObj.AsTime := aField.AsDateTime;
     ftDateTime: aFieldDataObj.AsDateTime := aField.AsDateTime;
     ftBytes,
     ftVarBytes: begin
       lBytes := aField.AsBytes;
       aFieldDataObj.AsBinary.WriteData(lBytes, length(lBytes));
     end;
     ftAutoInc: aFieldDataObj.AsInt32 := aField.AsInteger;     // Delphi documentation says it is a 32bit int.  Really? not 64bit int?
     ftBlob: begin
       lBlobStream := aField.DataSet.CreateBlobStream(aField, TBlobStreamMode.bmRead);
       aFieldDataObj.AsBinary.CopyFrom(lBlobStream);
     end;
     ftMemo: aFieldDataObj.AsStringList.Text := afield.AsString;
     ftGraphic: begin
       if aField.IsBlob then
       begin
         lBlobStream := aField.DataSet.CreateBlobStream(aField, TBlobStreamMode.bmRead);
         aFieldDataObj.AsBinary.CopyFrom(lBlobStream);
       end
       else
       begin
         // If the graphic is not in blob, but is rather in bytes field.  not really sure if this is reality.
         lBytes := aField.AsBytes;
         aFieldDataObj.AsBinary.WriteData(lBytes, length(lBytes));
       end;
     end;
     ftFmtMemo: aFieldDataObj.AsStringList.Text := afield.AsString;
     //ftParadoxOle: ;   Not supported
     //ftDBaseOle: ;     Not supported
     ftTypedBinary: begin
       lBlobStream := aField.DataSet.CreateBlobStream(aField, TBlobStreamMode.bmRead);
       aFieldDataObj.AsBinary.CopyFrom(lBlobStream);
     end;
     //ftCursor: ;
     ftFixedChar,
     ftWideString: aFieldDataObj.AsString := afield.AsString;
     ftLargeint: aFieldDataObj.AsInt64 := aField.AsLargeInt;
     // ftADT: Not supported
     ftArray,
     ftReference,
     ftDataSet: begin
       // each of these field types are TObjectField descendants.
       for j := 0 to TObjectField(aField).FieldCount-1 do
       begin
         FieldToDataObj(TObjectField(aField).Fields[j], aFieldDataObj.AsFrame.NewSlot(TObjectField(aField).Fields[j].FieldName));    // Recursion happening here.
       end;
     end;
     ftOraBlob: begin
       lBlobStream := aField.DataSet.CreateBlobStream(aField, TBlobStreamMode.bmRead);
       aFieldDataObj.AsBinary.CopyFrom(lBlobStream);
     end;
     ftOraClob: begin
       aFieldDataObj.AsStringList.text := aField.AsString;
     end;
     // ftVariant: aFieldDataObj. ;   FINISH
     //ftInterface: ;  Not supported
     //ftIDispatch: ;  Not supported
     ftGuid: aFieldDataObj.AsGUID.GUID := aField.AsGuid;
     ftTimeStamp: aFieldDataObj.AsDateTime := aField.AsDateTime;
     //ftFMTBcd: ;  FINISH
     ftFixedWideChar: aFieldDataObj.AsString := aField.AsString;
     ftWideMemo: aFieldDataObj.AsStringList.text := aField.AsString;
     ftOraTimeStamp: aFieldDataObj.AsDateTime := aField.AsDateTime;
     //ftOraInterval: Not supported;
     ftLongWord,
     ftShortint: aFieldDataObj.AsInt64 := aField.AsLongWord;
     ftByte: aFieldDataObj.AsByte := aField.AsInteger;
     // ftExtended: ;  FINISH
     //ftConnection: Not supported;
     //ftParams: Not supported;;
     //ftStream: Not Supported
     // ftTimeStampOffset: ;
     ftObject: ;
     ftSingle: aFieldDataObj.AsSingle := aField.AsSingle;
   end;
end;


end.
