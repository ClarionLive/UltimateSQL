  OMIT('_EndOfInclude_',_UltimateStringCLClass_)
_UltimateStringCLClass_     EQUATE(1)

  ! Generic string handling class definition.

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
                    
                    
                    
SplitStringQType    Queue,TYPE
Line                  &STRING
SortValue             STRING(50)
                    END


UltimateStringCL         CLASS,TYPE,MODULE('UltimateStringCL.CLW'),LINK('UltimateStringCL.CLW')    ! ,_RSMCLASSESLINKMode_),DLL(_RSMCLASSESDllMode_)
Value                 &STRING,PRIVATE
Lines                 &SplitStringQType,PRIVATE
                      !Public Methods
Append                PROCEDURE(STRING pNewValue)
Assign                PROCEDURE(STRING pNewValue)
Contains              PROCEDURE(STRING pTestValue, LONG pNoCase=0, LONG pStartPos=1),LONG
Count                 PROCEDURE(STRING pSearchValue, <LONG pStartPos>, <LONG pEndPos>, BYTE pNoCase=0),LONG
Destruct              PROCEDURE()
Get                   PROCEDURE(),STRING
GetLine               PROCEDURE(LONG pLineNumber),STRING
GetStrPos             PROCEDURE(STRING pFindString, LONG pNullValue=0),LONG
Length                PROCEDURE(),LONG
PreAppend             PROCEDURE(STRING pNewValue)
Records               PROCEDURE(),LONG
Replace               PROCEDURE(STRING pOldValue, STRING pNewValue,<LONG pCount>)
Split                 PROCEDURE(STRING pSplitStr)
SubString             PROCEDURE(LONG pStart, LONG pStop),STRING

                      !Private Methods
DisposeLines          PROCEDURE(),PRIVATE
DisposeStr            PROCEDURE(),PRIVATE
                    END

  _EndOfInclude_
