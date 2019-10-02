{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}
unit UtilUnit;

{$mode objfpc}{$H+}        

interface

Uses SysUtils, IniFiles, Classes, ACBrUtil, FileInfo ;

Function AcertaPath(APath : String): String;
function Converte(cmd: String): String;
Procedure GravaINICrypt(INI : TIniFile; Section, Ident, AString, Pass : String );
Function LeINICrypt(INI : TIniFile; Section, Ident, Pass : String) : String ;
function VersaoACBr(): String;
Function VerificaArquivoDesatualizado(APath: String):Boolean;

implementation

{------------------------------------------------------------------------------}
function AcertaPath(APath : String): String;
begin
  Result := trim(APath) ;
  if Result <> '' then
     if pos(PathDelim, APath) = 0 then
        Result := ExtractFilePath( ParamStr(0) ) + APath ;
end;

{------------------------------------------------------------------------------}
function Converte(cmd: String): String;
var A : Integer ;
begin
  Result := '' ;
  For A := 1 to length( cmd ) do
  begin
     if not (cmd[A] in ['A'..'Z','a'..'z','0'..'9',
                        ' ','.',',','/','?','<','>',';',':',']','[','{','}',
                        '\','|','=','+','-','_',')','(','*','&','^','%','$',
                        '#','@','!','~' ]) then
        Result := Result + '#' + IntToStr(ord( cmd[A] )) + ' '
     else
        Result := Result + cmd[A] + ' ';
  end ;
end;

{------------------------------------------------------------------------------}
Function LeINICrypt(INI : TIniFile; Section, Ident, Pass : String) : String ;
  var SStream  : TStringStream ;
      CryptStr : String ;
begin
   SStream := TStringStream.Create('') ;
   try
      INI.ReadBinaryStream(Section, Ident, SStream) ;
      CryptStr := SStream.DataString  ;
      Result   := StrCrypt( CryptStr, Pass) ;
   finally
      SStream.Free ;
   end ;
end ;

function VersaoACBr: String;
var
  FileVerInfo: TFileVersionInfo;
begin
  Result:= '';
  FileVerInfo:= TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=paramstr(0);
    FileVerInfo.ReadFileInfo;
    Result := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

{------------------------------------------------------------------------------}
Procedure GravaINICrypt(INI : TIniFile; Section, Ident, AString, Pass : String );
 var SStream  : TStringStream ;
     CryptStr : String ;
begin
  CryptStr := StrCrypt( AString, Pass) ;
  SStream := TStringStream.Create(CryptStr) ;
  try
     INI.WriteBinaryStream(Section, Ident, SStream) ;
  finally
     SStream.Free ;
  end ;
end ;

Function VerificaArquivoDesatualizado(APath: String):Boolean;
var
  PathLocal: String;
  NomeArquivo: String;
  date: String;
begin
  result:= False;
  if FileExists(APath) then
  begin
    NomeArquivo:= ExtractFileName(APath);
    PathLocal:= PathWithDelim( ExtractFilePath( ParamStr(0) ) ) + NomeArquivo;
    if FileExists(PathLocal) and ( APath <> PathLocal ) then
      result:= (FileDateToDateTime( FileAge( APath ) )) < (FileDateToDateTime( FileAge( PathLocal ) ));
  end;

end;

end.
