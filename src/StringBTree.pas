unit StringBTree;

{This unit provides a mechanism to store a list of String-Integer pairs in memory organized into an automatically-balanced BTree so
 that we can very quickly look up a node by the String value and retrieve the associated Integer value (ID).

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

(**************************************************************************************************************************************
 This code was derived from a code file in the Alcino project and all the extra unnecessary methods and inheritance were taken out
 and made to be specific to our needs here.
 Thank you to the Alcinoe team, they do a very good job writing high-quality code.
 See https://github.com/MagicFoundation/Alcinoe/blob/master/license.txt for license  }

                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

   1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

   END OF TERMS AND CONDITIONS

   APPENDIX: How to apply the Apache License to your work.

      To apply the Apache License to your work, attach the following
      boilerplate notice, with the fields enclosed by brackets "{}"
      replaced with your own identifying information. (Don't include
      the brackets!)  The text should be enclosed in the appropriate
      comment syntax for the file format. We also recommend that a
      file or class name and description of purpose be included on the
      same "printed page" as the copyright notice for easier
      identification within third-party archives.

   Copyright {yyyy} {name of copyright owner}

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

interface

uses System.SysUtils;

type
  {Forwards}
  TStringBinaryTreeNode = class;
  TStringBinaryTree = class;

  {iterate function}
  TStringBinaryTreeIterateFunc = procedure(
                                  aTree: TStringBinaryTree;
                                  aNode: TStringBinaryTreeNode;
                                  aExtData: Pointer;
                                  Var aContinue: Boolean);

  TStringBinaryTreeNode = class(Tobject)
  Private
  Protected
    fChildNodes: array[Boolean] of TStringBinaryTreeNode;
    fBal: -1..1;
  Public
    fString: String;
    fID: integer;
    Constructor Create; virtual;
  end;

  TStringBinaryTree = class(TObject)
  private
    FHead: TStringBinaryTreeNode;
    FNodeCount: Integer;
  protected
    procedure FreeNodeObj(aNode: TStringBinaryTreeNode);
    Function  CompareNode(IdVal: pointer; ANode: TStringBinaryTreeNode): Integer; overload;   {compares IdVal and aNode.keydata and returns 0 if they are equal. If IdVal is greater than aNode.keydata, returns an integer greater than 0. If aNode.keydata is less than IdVal, returns an integer less than 0.}
    Function  CompareNode(aNode1, ANode2: TStringBinaryTreeNode): Integer; overload;   {compares aNode1 and aNode2 and returns 0 if they are equal. If aNode1 is greater than aNode2, returns an integer greater than 0. If aNode1 is less than aNode2, returns an integer less than 0.}
    function  CreateNode: TStringBinaryTreeNode;
    procedure InternalIterate(Action: TStringBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer);
    function  InternalAddNode(aNode: TStringBinaryTreeNode): Boolean;
    Function  InternalExtractNode(IdVal: Pointer): TStringBinaryTreeNode;
    Function  InternalDeleteNode(IdVal: Pointer): Boolean;
    Procedure InternalClear;
    Function  InternalGetHead: TStringBinaryTreeNode;
    function  InternalFindNode(idVal: pointer): TStringBinaryTreeNode;
    function  InternalFirst: TStringBinaryTreeNode;  {Return the smallest-value node in the tree}
    function  InternalLast: TStringBinaryTreeNode;  {Return the largest-value node in the tree}
    function  InternalNext(aNode: TStringBinaryTreeNode): TStringBinaryTreeNode;  {Return the next node whose value is larger than aNode}
    function  InternalPrev(aNode: TStringBinaryTreeNode): TStringBinaryTreeNode;  {Return the largest node whose value is smaller than aNode}
    Function  InternalGetNodeCount: integer;
  public
    Constructor Create;
    Destructor  Destroy; Override;
    procedure   Iterate(Action: TStringBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer);
    function    AddNode(aNode: TStringBinaryTreeNode): Boolean;
    function    AddString(aStr: String; aID: integer): Boolean;
    Function    ExtractNode(IdVal: Pointer): TStringBinaryTreeNode;
    Function    DeleteNode(IdVal: Pointer): Boolean;
    Procedure   Clear;
    Function    Head: TStringBinaryTreeNode;
    function    FindNode(idVal: pointer): TStringBinaryTreeNode; overload;
    function    FindNode(const idVal: String): TStringBinaryTreeNode; overload;
    function    First: TStringBinaryTreeNode;  {Return the smallest-value node in the tree}
    function    Last: TStringBinaryTreeNode;  {Return the largest-value node in the tree}
    function    Next(aNode: TStringBinaryTreeNode): TStringBinaryTreeNode;  {Return the next node whose value is larger than aNode}
    function    Prev(aNode: TStringBinaryTreeNode): TStringBinaryTreeNode;  {Return the largest node whose value is smaller than aNode}
    Function    NodeCount: integer;
  end;

var
  gInsBalanceCount: integer;

implementation


const
  cStringBinaryTree_StackSize = 40;
  cStringBinaryTree_LeftChild = False;
  cStringBinaryTree_RightChild = True;

type
  TStringBinaryTree_StackNode = record
    Node : TStringBinaryTreeNode;
    Comparison : Integer;
  end;
  TStringBinaryTree_StackArray = array[1..cStringBinaryTree_StackSize] of TStringBinaryTree_StackNode;

function StringBinaryTree_Sign(I: Integer): Integer;
begin
  if I < 0 then Result := -1
  else if I > 0 then Result := +1
  else Result := 0;
end;

procedure StringBinaryTree_IterateDestroyNodeFunc(
            aTree: TStringBinaryTree;
            aNode: TStringBinaryTreeNode;
            aExtData: Pointer;
            Var aContinue: Boolean);

begin
  aTree.FreeNodeObj(aNode);
  acontinue := True;
end;


procedure StringBinaryTree_DelBalance(
            var aNode: TStringBinaryTreeNode;
            var SubTreeDec: Boolean;
            CmpRes: Integer);
var N1, N2: TStringBinaryTreeNode;
    B1, B2: Integer;
    LR: Boolean;
begin
  CmpRes := StringBinaryTree_Sign(CmpRes);
  if aNode.fBal = CmpRes then aNode.fBal := 0
  else if aNode.fBal = 0 then begin
    aNode.fBal := -CmpRes;
    SubTreeDec := False;
  end
  else begin
    LR := (CmpRes < 0);
    N1 := aNode.fChildNodes[LR];
    B1 := N1.fBal;
    if (B1 = 0) or (B1 = -CmpRes) then begin
      {Single RR or LL rotation}
      aNode.fChildNodes[LR] := N1.fChildNodes[not LR];
      N1.fChildNodes[not LR] := aNode;
      if B1 = 0 then begin
        aNode.fBal := -CmpRes;
        N1.fBal := CmpRes;
        SubTreeDec := False;
      end
      else begin
        aNode.fBal := 0;
        N1.fBal := 0;
      end;
      aNode := N1;
    end
    else begin
      {Double RL or LR rotation}
      N2 := N1.fChildNodes[not LR];
      B2 := N2.fBal;
      N1.fChildNodes[not LR] := N2.fChildNodes[LR];
      N2.fChildNodes[LR] := N1;
      aNode.fChildNodes[LR] := N2.fChildNodes[not LR];
      N2.fChildNodes[not LR] := aNode;
      if B2 = -CmpRes then aNode.fBal := CmpRes
      else aNode.fBal := 0;
      if B2 = CmpRes then N1.fBal := -CmpRes
      else N1.fBal := 0;
      aNode := N2;
      N2.fBal := 0;
    end;
  end;
end;

procedure StringBinaryTree_InsBalance(
            var aNode: TStringBinaryTreeNode;
            var SubTreeInc: Boolean;
            CmpRes: Integer);
var N1: TStringBinaryTreeNode;
    N2: TStringBinaryTreeNode;
    LR: Boolean;
begin
  CmpRes := StringBinaryTree_Sign(CmpRes);
  if aNode.fBal = -CmpRes then
  begin
    aNode.fBal := 0;
    SubTreeInc := False;
  end
  else if aNode.fBal = 0 then
  begin
    aNode.fBal := CmpRes;
  end
  else
  begin
    inc(gInsBalanceCount);

    LR := (CmpRes > 0);
    N1 := aNode.fChildNodes[LR];
    if N1.fBal = CmpRes then
    begin
      aNode.fChildNodes[LR] := N1.fChildNodes[not LR];
      N1.fChildNodes[not LR] := aNode;
      aNode.fBal := 0;
      aNode := N1;
    end
    else
    begin
      N2 := N1.fChildNodes[not LR];
      N1.fChildNodes[not LR] := N2.fChildNodes[LR];
      N2.fChildNodes[LR] := N1;
      aNode.fChildNodes[LR] := N2.fChildNodes[not LR];
      N2.fChildNodes[not LR] := aNode;
      if N2.fBal = CmpRes then aNode.fBal := -CmpRes
      else aNode.fBal := 0;
      if N2.fBal = -CmpRes then N1.fBal := CmpRes
      else N1.fBal := 0;
      aNode := N2;
    end;
    aNode.fBal := 0;
    SubTreeInc := False;
  end;
end;

constructor TStringBinaryTreeNode.Create;
begin
 fChildNodes[cStringBinaryTree_LeftChild] := nil;
 fChildNodes[cStringBinaryTree_RightChild] := nil;
 fBal := 0;
 fString:='';
 fID:=-1;
end;

Constructor TStringBinaryTree.Create;
begin
  FHead := Nil;
  FNodeCount := 0;
  randomize;
  Inherited;
end;

function TStringBinaryTree.CreateNode: TStringBinaryTreeNode;
begin
  result := TStringBinaryTreeNode.Create;
end;

Destructor TStringBinaryTree.Destroy;
begin
  InternalClear;
  Inherited;
end;

procedure TStringBinaryTree.InternalIterate(
            Action: TStringBinaryTreeIterateFunc;
            Up: Boolean;
            ExtData: Pointer);
var N1: TStringBinaryTreeNode;
    N2: TStringBinaryTreeNode;
    StackPos: Integer;
    Stack: TStringBinaryTree_StackArray;
    Continue: Boolean;
begin
  Continue := True;
  StackPos := 0;
  N1 := Fhead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.fChildNodes[not Up];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    N2 := N1;
    N1 := N1.fChildNodes[Up];

    Action(Self, N2, ExtData, Continue);
    if not continue then Exit;
  until False;
end;


function TStringBinaryTree.InternalAddNode(aNode: TStringBinaryTreeNode): Boolean;
var N1: TStringBinaryTreeNode;
    CmpRes: Integer;
    StackPos: Integer;
    Stack: TStringBinaryTree_StackArray;
    SubTreeInc: Boolean;
begin
  {exit if node is nil}
  if not Assigned(aNode) then begin
    Result := False;
    Exit;
  end;

  {Handle first node}
  N1 := FHead;
  if not Assigned(N1) then begin
    Fhead := aNode;
    Inc(FNodeCount);
    result := True;
    Exit;
  end;

  {Find where new node should fit in tree}
  StackPos := 0;
  CmpRes := 0;
  while Assigned(N1) do begin

    {compare node}
    CmpRes := CompareNode(aNode, N1);

    {node already exist, so exit}
    if CmpRes = 0 then begin
      Result := False;
      Exit;
    end;

    {Build the stack}
    Inc(StackPos);
    with Stack[StackPos] do begin
      Node := N1;
      Comparison := CmpRes;
    end;

    {continue the loop}
    N1 := N1.fChildNodes[CmpRes > 0];

  end;

  {Insert new node}
  Stack[StackPos].Node.fChildNodes[CmpRes > 0] := aNode;
  Inc(FNodeCount);
  result := True;

  {Unwind the stack and rebalance}
  SubTreeInc := True;
  while (StackPos > 0) and SubTreeInc do
  begin
    if StackPos = 1 then
    begin
      StringBinaryTree_InsBalance(Fhead, SubTreeInc, Stack[1].Comparison);
    end
    else with Stack[StackPos-1] do
    begin
      StringBinaryTree_InsBalance(Node.fChildNodes[Comparison > 0], SubTreeInc, Stack[StackPos].Comparison);
    end;
    dec(StackPos);
  end;
end;

procedure TStringBinaryTree.InternalClear;
begin
  InternalIterate(
    StringBinaryTree_IterateDestroyNodeFunc,
    True,
    nil);
  FHead := nil;
  FNodeCount := 0;
end;

function TStringBinaryTree.InternalGetHead: TStringBinaryTreeNode;
begin
  Result := Fhead;
end;

procedure TStringBinaryTree.FreeNodeObj(aNode: TStringBinaryTreeNode);
begin
  aNode.Free;
end;

function TStringBinaryTree.InternalExtractNode(IdVal: Pointer): TStringBinaryTreeNode;
var N1: TStringBinaryTreeNode;
    N2: TStringBinaryTreeNode;
    TmpNode: TStringBinaryTreeNode;
    CmpRes: Integer;
    Found: Boolean;
    SubTreeDec: Boolean;
    StackPos: Integer;
    StackParentPos: integer;
    Stack: TStringBinaryTree_StackArray;
begin
  {exit if head is nil}
  N1 := Fhead;
  if not Assigned(N1) then begin
    result := nil;
    Exit;
  end;

  {Find node to delete and stack the nodes to reach it}
  Found := False;
  StackPos := 0;
  while not Found do begin

    {compare node}
    CmpRes := CompareNode(IdVal, N1);
    Inc(StackPos);

    {Found node}
    if CmpRes = 0 then begin
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := -1;
      end;
      Found := True;
    end

    {not found yet, continue the search}
    else begin
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := CmpRes;
      end;
      N1 := N1.fChildNodes[CmpRes > 0];

      {Node not found, then exit}
      if not Assigned(N1) then begin
        Result := nil;
        Exit;
      end;
    end;

  end;

  {save the position of the parent of the node to delete in the stack}
  StackParentPos := StackPos - 1;

  {Delete the node found}
  N2 := N1;
  if (not Assigned(N2.fChildNodes[cStringBinaryTree_RightChild])) or (not Assigned(N2.fChildNodes[cStringBinaryTree_LeftChild])) then begin
    {Node has at most one branch}
    Dec(StackPos);
    N1 := N2.fChildNodes[Assigned(N2.fChildNodes[cStringBinaryTree_RightChild])];
    if StackPos = 0 then Fhead := N1
    else with Stack[StackPos] do
      Node.fChildNodes[Comparison > 0] := N1;
  end
  else begin
    {Node has two branches; stack nodes to reach one with no right child}
    N1 := N2.fChildNodes[cStringBinaryTree_LeftChild];
    while Assigned(N1.fChildNodes[cStringBinaryTree_RightChild]) do begin
      Inc(StackPos);
      with Stack[StackPos] do begin
        Node := N1;
        Comparison := 1;
      end;
      N1 := N1.fChildNodes[cStringBinaryTree_RightChild];
    end;

    {Swap the node to delete with the terminal node}
    N1.fBal := N2.fBal;
    If StackParentPos = 0 then Fhead := N1
    else with Stack[StackParentPos] do
      Node.fChildNodes[Comparison > 0] := N1;

    with Stack[StackParentPos+1] do
      Node := N1;

    tmpnode := N1.fChildNodes[cStringBinaryTree_LeftChild];
    N1.fChildNodes[cStringBinaryTree_RightChild] := N2.fChildNodes[cStringBinaryTree_RightChild];
    N1.fChildNodes[cStringBinaryTree_LeftChild] := N2.fChildNodes[cStringBinaryTree_LeftChild];

    with Stack[StackPos] do
      Node.fChildNodes[Comparison > 0] := tmpnode;
  end;

  {return the deleted node}
  result := N2;
  Dec(FNodeCount);

  {Unwind the stack and rebalance}
  SubTreeDec := True;
  while (StackPos > 0) and SubTreeDec do begin
    if StackPos = 1 then StringBinaryTree_DelBalance(Fhead, SubTreeDec, Stack[1].Comparison)
    else with Stack[StackPos-1] do
      StringBinaryTree_DelBalance(Node.fChildNodes[Comparison > 0], SubTreeDec, Stack[StackPos].Comparison);
    dec(StackPos);
  end;
end;

function TStringBinaryTree.InternalDeleteNode(IdVal: Pointer): Boolean;
var N1: TStringBinaryTreeNode;
begin
  N1 := InternalExtractNode(IdVal);
  if assigned(N1) then begin
    result := True;
    FreeNodeObj(N1);
  end
  else result := False;
end;


function TStringBinaryTree.InternalFindNode(idVal: pointer): TStringBinaryTreeNode;
var N1: TStringBinaryTreeNode;
    CmpRes: Integer;
begin
  N1 := FHead;
  while Assigned(N1) do begin
    CmpRes := CompareNode(IdVal, N1);
    if CmpRes = 0 then begin
      Result := N1;
      Exit;
    end
    else N1 := N1.fChildNodes[CmpRes > 0];
  end;

  Result := nil;
end;

function TStringBinaryTree.InternalFirst: TStringBinaryTreeNode;
begin
  if FNodeCount = 0 then Result := nil
  else begin
    Result := Fhead;
    while Assigned(Result.fChildNodes[cStringBinaryTree_LeftChild]) do
      Result := Result.fChildNodes[cStringBinaryTree_LeftChild];
  end;
end;

function TStringBinaryTree.InternalLast: TStringBinaryTreeNode;
begin
  if FNodeCount = 0 then Result := nil
  else begin
    Result := FHead;
    while Assigned(Result.fChildNodes[cStringBinaryTree_RightChild]) do
      Result := Result.fChildNodes[cStringBinaryTree_RightChild];
  end;
end;

function TStringBinaryTree.InternalNext(aNode: TStringBinaryTreeNode): TStringBinaryTreeNode;
var Found: Word;
    N1: TStringBinaryTreeNode;
    StackPos: Integer;
    Stack: TStringBinaryTree_StackArray;
begin
  Result := nil;
  Found := 0;
  StackPos := 0;
  N1 := FHead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.fChildNodes[cStringBinaryTree_LeftChild];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    if Found = 1 then begin
      Result := N1;
      Exit;
    end;
    if N1 = aNode then Inc(Found);
    N1 := N1.fChildNodes[cStringBinaryTree_RightChild];
  until False;
end;

function TStringBinaryTree.InternalPrev(aNode: TStringBinaryTreeNode): TStringBinaryTreeNode;
var Found: Word;
    N1: TStringBinaryTreeNode;
    StackPos: Integer;
    Stack: TStringBinaryTree_StackArray;
begin
  Result := nil;
  Found := 0;
  StackPos := 0;
  N1 := FHead;
  repeat
    while Assigned(N1) do begin
      Inc(StackPos);
      Stack[StackPos].Node := N1;
      N1 := N1.fChildNodes[cStringBinaryTree_RightChild];
    end;
    if StackPos = 0 then Exit;

    N1 := Stack[StackPos].Node;
    Dec(StackPos);
    if Found = 1 then begin
      Result := N1;
      Exit;
    end;
    if N1 = aNode then
      Inc(Found);
    N1 := N1.fChildNodes[cStringBinaryTree_LeftChild];
  until False;
end;

function TStringBinaryTree.InternalGetNodeCount: integer;
begin
  Result := FnodeCount;
end;

function TStringBinaryTree.AddNode(aNode: TStringBinaryTreeNode): Boolean;
begin
  Result := InternalAddNode(aNode);
end;

function TStringBinaryTree.AddString(aStr: String; aID: integer): boolean;
var
  lNode: TStringBinaryTreeNode;
begin
  lNode:=TStringBinaryTreeNode.create;
  lNode.fString := aStr;
  lNode.fID := aID;
  result := AddNode(lNode);  // If this returns false, the node could not be added.   Probably because aStr is already contained within.
  if result = false then
  begin
    lNode.Free;
  end;
end;

procedure TStringBinaryTree.Clear;
begin
  InternalClear;
end;

function TStringBinaryTree.CompareNode(IdVal: pointer; ANode: TStringBinaryTreeNode): Integer;
begin
  Result := CompareStr(PString(IdVal)^,aNode.fString);
end;

function TStringBinaryTree.CompareNode(aNode1, ANode2: TStringBinaryTreeNode): Integer;
begin
  result := CompareStr(aNode1.fString, aNode2.fString);
end;

function TStringBinaryTree.ExtractNode(IdVal: Pointer): TStringBinaryTreeNode;
begin
  Result := InternalExtractNode(IdVal);
end;

function TStringBinaryTree.DeleteNode(IdVal: Pointer): Boolean;
begin
  Result := InternalDeleteNode(IdVal);
end;

function TStringBinaryTree.FindNode(idVal: pointer): TStringBinaryTreeNode;
begin
  Result := InternalFindNode(idVal);
end;

function TStringBinaryTree.FindNode(const idVal: String): TStringBinaryTreeNode;
begin
  Result := FindNode(@idVal);
end;

function TStringBinaryTree.First: TStringBinaryTreeNode;
begin
  result := InternalFirst;
end;

function TStringBinaryTree.Head: TStringBinaryTreeNode;
begin
  Result := InternalGetHead;
end;

procedure TStringBinaryTree.Iterate(Action: TStringBinaryTreeIterateFunc; Up: Boolean; ExtData: Pointer);
begin
  InternalIterate(Action, Up, ExtData);
end;

function TStringBinaryTree.Last: TStringBinaryTreeNode;
begin
  Result := InternalLast;
end;

function TStringBinaryTree.Next(aNode: TStringBinaryTreeNode): TStringBinaryTreeNode;
begin
  Result := InternalNext(aNode);
end;

function TStringBinaryTree.Prev(aNode: TStringBinaryTreeNode): TStringBinaryTreeNode;
begin
  Result := InternalPrev(aNode);
end;


function TStringBinaryTree.NodeCount: integer;
begin
  Result := InternalGetNodeCount;
end;


end.
