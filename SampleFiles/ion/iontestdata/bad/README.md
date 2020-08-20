IonTests Bad Data Files
=======================

The `bad` directory contains examples which Ion implementation should identify
and fail gracefully while parsing.

Where possible, these files contain comments identifying why the file should
be treated as invalid. Since the binary Ion representation does not preserve
comments, this file contains descriptions of binary failures.

annotationLengthTooLongScalar.10n
---------------------------------
Contains an Annotation wrapper whose declared length is too long for its
subfields (including its wrapped scalar value).

annotationLengthTooLongContainer.10n
---------------------------------
Contains an Annotation wrapper whose declared length is too long for its
subfields (including its wrapped container value).

annotationLengthTooShortScalar.10n
---------------------------------
Contains an Annotation wrapper whose declared length is too short for its
subfields (including its wrapped scalar value).

annotationLengthTooShortContainer.10n
---------------------------------
Contains an Annotation wrapper whose declared length is too short for its
subfields (including its wrapped container value).

annotationNested.10n
--------------------
Contains an Annotation wrapper which contains another annotation wrapper as
its value.

annotationWithNoValue.10n
-------------------------
Contains an Annotation wrapper with no value.

badMagic1015.10n
----------------
Contains the invalid BVM 0x10150100.

badMagicE00100E0.10n
--------------------
Contains the invalid BVM 0xE00100E0.

blobLenTooLarge.10n
-------------------
Contains a Blob whose length is specified as 15 bytes, but only 14 bytes of
data are available.

boolWithInvalidLength_1.10n
---------------------------
Contains a Bool whose _L_ value is `3`.

boolWithInvalidLength_2.10n
---------------------------
Contains a Bool whose _L_ value is `14`.

clobLenTooLarge.10n
-------------------
Contains a Clob whose length is specified as 5,400 bytes, but only 16 bytes of
data are available.

decimalExpTooLarge.10n
----------------------
This file contains a decimal who's exponent exceeds the length defined by the
decimal container's length.

decimalLenCauses64BitOverflow.10n
---------------------------------
This file contains a decimal who's total length is 2^64-1, larger than the
datagram size, and when combined with a buffer offset, is likely to cause an
overflow when calculating the end index of the value.

decimalLenTooLarge.10n
----------------------
Contains a Clob whose length is specified as 34 bytes, but only 24 bytes of
data are available.

emptyAnnotatedInt.10n
---------------------
Contains an Annotation wrapper with an *annot_length* subfield value of zero,
which is illegal because at least one annotation must exist.

floatLenTooLarge.10n
--------------------
Contains a Float whose length is specified as 8 bytes, but only 7 bytes of data
are available.

listWithValueLargerThanSize.10n
-------------------------------
Contains a List whose length is specified as 1 byte, but the value contained
by the list occupies 2 bytes.

minLongWithLenTooLarge.10n
--------------------------
Contains an Int whose length is specified as 9 byte, but only 8 bytes of data
are available.

minLongWithLenTooSmall.10n
--------------------------
Contains an Int whose length is specified as 7 bytes, but contains 8 bytes of
data. The trailing byte is `0x00` (a Null with an invalid _L_ value).

negativeIntZero
-----------------
Contains a negative integer with length of 1 and value of zero (hex: `31 00`).

negativeIntZero
---------------
Contains a negative integer with length zero (hex: `30`).

nopPadTooShort.10n
------------------
Contains a NOP pad with a declared length of 16 bytes that ends after only 15
bytes.

nullBadTD.10n
-------------
Contains an Null with an invalid _L_ value of `0`.

stringLenTooLarge.10n
---------------------
Contains a String whose length is specified as 44 bytes, but only 38 bytes of
data are available.

stringWithLatinEncoding.10n
---------------------------
Contains a String with several valid Latin-1 (ISO-8859-1) characters which do
not produce valid UTF-8 code points.

structOrderedEmpty.10n
----------------------
Contains an ordered Struct (type ID `0xD1`) with a length of `0` (`0x80`).
Ordered structs must contain at least one symbol/value pair.

symbolExplicitZero.10n
----------------------
This file contains a symbol with the SID `0`.

symbolLenTooLarge.10n
---------------------
Contains a Symbol whose length is specified as 2 bytes, but only 1 byte of data
is available.

timestamp/timestampLenTooLarge.10n
----------------------------------
Contains a Timestamp whose length is specified as 25 bytes, but only 24 bytes
of data are available.

timestamp/timestampSept31.10n
-----------------------------
This file contains a Timestamp with an invalid day component.

