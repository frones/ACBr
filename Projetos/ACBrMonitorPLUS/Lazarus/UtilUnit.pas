{*******************************************************************************}
{ Projeto: ACBrMonitor                                                         }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2010 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Juliana Rodrigues Prado Tamizou                  }
{                              Jean Patrick F. dos Santos (envio de e-mails)    }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}
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
