{

 FPDF Pascal
 https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal

 Copyright (C) 2023 Projeto ACBr - Daniel Simões de Almeida

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.

 Except as contained in this notice, the name of <Projeto ACBr> shall not be
 used in advertising or otherwise to promote the sale, use or other dealings in
 this Software without prior written authorization from <Projeto ACBr>.

 Based on:
 - The library FPDF written in PHP by Olivier PLATHEY
   http://www.fpdf.org/
 - Free JPDF Pascal from Jean Patrick e Gilson Nunes
   https://github.com/jepafi/Free-JPDF-Pascal
}

// Define USE_SYNAPSE if you want to force use of Units from synapse
//   Needed in PDF Protection "SetProtection"
//   Needed in "GetImageFromURL"
//   http://www.ararat.cz/synapse
{$DEFINE USE_SYNAPSE}

// If you don't want the AnsiString vs String warnings to bother you
{$DEFINE REMOVE_CAST_WARN}

// If you need QRCode support
// https://github.com/foxitsoftware/DelphiZXingQRCode
{$DEFINE DelphiZXingQRCode}

{$IfDef FPC}
  {$Mode delphi}{$H+}
  {$Define USE_UTF8}
  {$Define HAS_HTTP}
{$Else}
  {$IfDef REMOVE_CAST_WARN}
   {$If CompilerVersion >= 20}       // VER200 	Delphi 2009 / C++Builder 2009
    {$WARN IMPLICIT_STRING_CAST OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
   {$IfEnd}
  {$EndIf}
{$EndIf}

{$IfDef POSIX}
  {$Define HAS_SYSTEM_GENERICS}
  {$Define FMX}

  {$IfDef LINUX}
    {$Define CONVERT_TO_ANSI}
    {$Define USE_UTF8}
  {$EndIf}
{$EndIf}

{$IfDef POSIX}
  {$IfDef LINUX}
    {$Define USE_UTF8}
  {$EndIf}
  {$Define FMX}
{$EndIf}

{$IfDef NEXTGEN}
  {$Define HAS_SYSTEM_GENERICS}
  {$Define USE_UTF8}
  {$ZEROBASEDSTRINGS OFF}
{$EndIf}

{$IfDef USE_SYNAPSE}
  {$DEFINE HAS_PROTECTION}
  {$Define HAS_HTTP}
{$EndIf}

{$IfNDef FPC}
  {$IfNDef HAS_SYSTEM_GENERICS}
     {$IF CompilerVersion >= 23}  // VER230 	Delphi XE2 / C++Builder XE2
       {$Define HAS_SYSTEM_CONTNRS}
     {$IfEnd}
  {$EndIf}
{$EndIf}

