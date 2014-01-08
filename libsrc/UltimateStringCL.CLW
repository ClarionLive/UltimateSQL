                    MEMBER
                    MAP
                    END

! Generic string handling class method declarations.

!*****************************************************************************************************************
!Copyright (C) 2007-2011 Rick Martin, rick.martin@upperparksolutions.com
!This software is provided 'as-is', without any express or implied warranty. In no event will the authors 
!be held liable for any damages arising from the use of this software. 
!Permission is granted to anyone to use this software for any purpose, 
!including commercial applications, subject to the following restrictions:
!1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. 
!   If you use this software in a product, an acknowledgment in the product documentation would be appreciated 
!   but is not required.
!2. You may not use this software in a commerical product whose primary purpose is string manipluation. 
!3. This notice may not be removed or altered from any source distribution.
!*****************************************************************************************************************
                    
  INCLUDE('UltimateStringCL.INC'),ONCE

  !Public Methods
!!!  
!!! <summary>Assign a new value to the string class</summary>
!!! <param name="NewValue">The new string to assign to the class</param>
!!! <remarks>A new value can be assigned to the class regardless
!!! if it already has a value. The old value is automatically disposed.</remarks>
UltimateStringCL.Assign  PROCEDURE(STRING pNewValue)
strLen  LONG,AUTO
  CODE
    Self.DisposeStr()
    strLen = LEN(pNewValue)
    IF strLen > 0
      Self.Value &= NEW STRING(strLen)
      Self.Value = pNewValue
    END
    
!!!
!!! <summary>Append the new value to the end of the existing class string.</summary>
!!! <remarks>If no value already exists then the new value is assigned
!!! as if Assign had been called instead of Append.</remarks>
UltimateStringCL.Append  PROCEDURE(STRING pNewValue)
  CODE
    IF NOT Self.Value &= NULL
      Self.Assign(Self.Value & pNewValue)
    ELSE
      Self.Assign(pNewValue)
    END

!!!
!!! <summary>Return the offset of the sub-string in existing class string.</summary>
!!! <param name="TestValue">Sub-string to search for</param>
!!! <param name="NoCase">Optional parameter to ignore case. Default is case-sensitive.</param>
!!! <param name="StartPos">Optional parameter start search at position other than beginnnig of the string</param>
!!! <remarks>If the sub-string does not exist then zero is returned.</remarks>
UltimateStringCL.Contains    PROCEDURE(STRING pTestValue, LONG pNoCase=0, LONG pStartPos=1)!,LONG
  CODE
  RETURN InString(Choose(pNoCase=TRUE,UPPER(pTestValue),pTestValue),Choose(pNoCase=TRUE,UPPER(Self.Value),Self.Value),1,pStartPos)

!!!
!!! <summary>Append the new value to the beginning of the existing class string.</summary>
!!! <remarks>If no value already exists then the new value is assigned
!!! as if Assign had been called instead of Append.</remarks>
UltimateStringCL.PreAppend   PROCEDURE(STRING pNewValue)
  CODE
    IF NOT Self.Value &= NULL
      Self.Assign(pNewValue & Self.Value)
    ELSE
      Self.Assign(pNewValue)
    END
!!!
!!! <summary>Deallocate dynamic memory when class goes out of scope.</summary>
UltimateStringCL.Destruct    PROCEDURE()
  CODE
    Self.DisposeStr()
    Self.DisposeLines()

!!!
!!! <summary>Return current string</summary>
!!! <remarks>If no string has been assigned an empty string is returned.</remarks>
UltimateStringCL.Get     PROCEDURE() !,STRING
  CODE
    IF NOT Self.Value &= NULL
      RETURN Self.Value
    ELSE
      RETURN ''
    END

!!!
!!! <summary>Return the length of the existing string value.</summary>
!!! <remarks>If no string has been assigned zero is returned.</remarks>
UltimateStringCL.Length  PROCEDURE() !,LONG
  CODE
    IF NOT Self.Value &= NULL
      RETURN LEN(Self.Value)
    ELSE
      RETURN 0
    END

!!!
!!! <summary>Replace occurences of one string with another in class value.</summary>
!!! <param name="OldValue">Sub-string to search for</param>
!!! <param name="NewValue">New value to replace with</param>
!!! <param name="Count">Optional parameter: How many occurences to replace. Default is all.</param>
!!! <remarks>This operation is non-overlapping. If the OldValue occurs in the NewValue the
!!! occurences from inserting NewValue will not be replaced.</remarks>
UltimateStringCL.Replace PROCEDURE(STRING pOldValue, STRING pNewValue,<LONG pCount>)
lCount                LONG,AUTO
lStrPos               LONG,AUTO
lStartPos             LONG(1)
  CODE
    IF NOT Self.Value &= NULL
      LOOP
        lStrPos = INSTRING(pOldValue,Self.Value,1,lStartPos)
        IF lStrPos
          Self.Assign(Self.Value[1 : lStrPos-1 ] & pNewValue & Choose(lStrPos + LEN(pOldValue) > Len(Self.Value),'', Self.Value[ (lStrPos + LEN(pOldValue)) : Len(Self.Value) ]))
          lStartPos = lStrPos + LEN(pNewValue)
          lCount += 1
          IF NOT OMITTED(pCount) AND lCount = pCount
            BREAK
          END
        ELSE
          BREAK
        END
      END
    END

!!!
!!! <summary>Count the occurences of a sub-string in class value.</summary>
!!! <param name="SearchValue">Sub-string to search for</param>
!!! <param name="StartPos">Optional parameter to indicate what position to start search. Default is beginning.</param>
!!! <param name="EndPos">Optional parameter to indicate what position to end search. Default is end of string.</param>
!!! <param name="NoCase">Optional parameter: Ignore case in comparision. Default is case-sensitive.</param>
UltimateStringCL.Count   PROCEDURE(STRING pSearchValue, <LONG pStartPos>, <LONG pEndPos>, BYTE pNoCase=0) !,LONG
lCount                LONG(0)
lStrPos               LONG,AUTO
lStartPos             LONG(1)
SearchString          UltimateStringCL
  CODE
    IF NOT Self.Value &= NULL
      IF OMITTED(pStartPos) AND OMITTED(pEndPos)
        SearchString.Assign(Self.Value)
      ELSIF OMITTED(pStartPos) 
        SearchString.Assign(Self.SubString(1,pEndPos))
      ELSE
        SearchString.Assign(Self.SubString(pStartPos,Self.Length()))
      END
      IF pNoCase
        SearchString.Assign(UPPER(SearchString.Get()))
        pSearchValue = UPPER(pSearchValue)
      END
      LOOP
        lStrPos = INSTRING(pSearchValue,SearchString.Get(),1,lStartPos)
        IF lStrPos
          lStartPos = lStrPos + LEN(pSearchValue)
          lCount += 1
        ELSE
          BREAK
        END
      END
    END
    RETURN lCount

!!!
!!! <summary>Return specific line after calling Split method.</summary>
!!! <param name="LineNumber">Line to return. If LineNumber is greater than the number of lines in queue
!!! then an empty string is returned.</param>
!!! <remarks>If split has not been called an empty string is returned.</remarks>
UltimateStringCL.GetLine PROCEDURE(LONG pLineNumber) !,STRING
  CODE
    IF Self.Lines &= NULL
      RETURN ''
    ELSE
      GET(Self.Lines,pLineNumber)
      IF ERRORCODE()
        RETURN ''
      ELSE
        RETURN Self.Lines.Line
      END
    END

!!!
!!! <summary>Return the position in the split queue of the passed string value.</summary>
!!! <remarks>If split has not been called or entry is not found zero is returned unless optional value is passed.</remarks>
UltimateStringCL.GetStrPos                PROCEDURE(STRING pFindString, LONG pNullValue=0) !,LONG
  CODE
  If Self.Lines &= NULL
    Return pNullValue
  Else
    Self.Lines.SortValue = pFindString
    Get(Self.Lines,Self.Lines.SortValue)
    If ErrorCode()
      Return pNullValue
    Else
      Return (Pointer(Self.Lines))
    End
  End
!!!
!!! <summary>Return the number of lines a string value was broken into after calling Split.</summary>
!!! <remarks>If split has not been called zero is returned.</remarks>
UltimateStringCL.Records PROCEDURE() !,LONG
  CODE
    IF Self.Lines &= NULL
      RETURN 0
    ELSE
      RETURN RECORDS(Self.Lines)
    END

!!!
!!! <summary>Breakdown the current string value into a series of string. Use the passed string value
!!! as a delimiter.</summary>
!!! <param name="SplitStr">Sub-String used to break up string. </param>
!!! <remarks>The sub-string is consumed by the command and does not appear in the lines.
!!! Use Records and GetLine methods to return information about the split queue.</remarks>
UltimateStringCL.Split   PROCEDURE(STRING pSplitStr)
SplitStrPos           LONG,AUTO
StartPos              LONG(1)
  CODE
    IF NOT Self.Value &= NULL
      Self.DisposeLines
      Self.Lines &= NEW(SplitStringQType)
      LOOP
        SplitStrPos = INSTRING(pSplitStr,Self.Value,1,StartPos)
        IF SplitStrPos
          Self.Lines.Line &= NEW(STRING(LEN(Self.Value[StartPos : SplitStrPos-1])))
          Self.Lines.Line = Self.Value[StartPos : SplitStrPos-1]
          Self.Lines.SortValue = Self.Lines.Line
          ADD(Self.Lines)
          StartPos = SplitStrPos + LEN(pSplitStr)
          IF StartPos > LEN(Self.Value)
            BREAK
          END
          IF STARTPOS + 100 > LEN(Self.Value)
            Z# =  1 !Debug
          END
        ELSE
          Self.Lines.Line &= NEW(STRING(LEN(Self.Value[StartPos : LEN(Self.Value)])))
          Self.Lines.Line = Self.Value[StartPos : LEN(Self.Value)]
          Self.Lines.SortValue = Self.Lines.Line
          ADD(Self.Lines)
          BREAK
        END
      END
    END

!!!
!!! <summary>Return sub-string from the current string value.</summary>
!!! <param name="Start">Start of sub-string.</param>
!!! <param name="Stop">Stop position of sub-string.</param>
!!! <remarks>If the Stop position is greater than the length of the string the string length is used
!!! as the stop position. If the Start position greater than the stop position or the length
!!! of the string then an empty string is returned.</remarks>
UltimateStringCL.SubString   PROCEDURE(LONG pStart, LONG pStop) !,STRING
  CODE
    IF pStop > LEN(SELF.Value)
      pSTOP = LEN(SELF.VALUE)
    ELSIF pStart > LEN(Self.Value)
      RETURN ''
    ELSIF pStart > pStop
      RETURN ''
    END
    RETURN SELF.Value[pStart : pStop]  

  !Private Methods
!!!
!!! <summary>Private method to dispose of dynamic memory allocated by Split method.</summary>
UltimateStringCL.DisposeLines    PROCEDURE() !,PRIVATE
I                             LONG
  CODE
    IF NOT Self.Lines &= NULL
      LOOP I = 1 TO RECORDS(Self.Lines)
        GET(Self.Lines,I)
        DISPOSE(Self.Lines)
      END
      FREE(Self.Lines)
    END

!!!
!!! <summary>Private method to dispose of string value.</summary>
UltimateStringCL.DisposeStr  PROCEDURE() !,PRIVATE
  CODE
    IF NOT Self.Value &= NULL
      DISPOSE(Self.Value)
    END
