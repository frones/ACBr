{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
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

{$I ACBr.inc}

{$IfDef FPC}
{$modeswitch TypeHelpers}
{$EndIf}

unit ACBrLibHelpersIni;
  
interface

uses
  Classes, SysUtils, IniFiles;
 
type
  TACBrMemIniFileHelper = class helper for TMemIniFile
    procedure LoadFromString(const IniString: string);
    procedure LoadFromFile(const FileName: string); overload;
    procedure LoadFromStream(Stream: TStream); overload;
    function AsString: string;
  end;

  TACBrCustomIniFileHelper = class helper for TCustomIniFile
    procedure WriteStringLine(const Section, Ident, Value: String);
    procedure ClearEmptySections;
  end;

implementation

uses
  ACBrLibComum, ACBrUtil.Strings;

{ TACBrMemIniFileHelper }
procedure TACBrMemIniFileHelper.LoadFromString(const IniString: string);
Var
  FIniFile: TStringList;
begin
  if not StringEhIni(IniString) then Exit;

  FIniFile := TStringList.Create;

  try
    FIniFile.Text := IniString;
    Self.SetStrings(FIniFile);
  finally
    FIniFile.Free;
  end;
end;

procedure TACBrMemIniFileHelper.LoadFromFile(const FileName: string);
Var
  FIniFile: TStringList;
begin
  FIniFile := TStringList.Create;

  try
    FIniFile.LoadFromFile(FileName);
    Self.SetStrings(FIniFile);
  finally
    FIniFile.Free;
  end;
end;

procedure TACBrMemIniFileHelper.LoadFromStream(Stream: TStream);
Var
  FIniFile: TStringList;
begin
  FIniFile := TStringList.Create;

  try
    FIniFile.LoadFromStream(Stream);
    Self.SetStrings(FIniFile);
  finally
    FIniFile.Free;
  end;
end;

function TACBrMemIniFileHelper.AsString: string;
Var
  FIniFile: TStringList;
begin
  FIniFile := TStringList.Create;
  try
    Self.GetStrings(FIniFile);
    Result := FIniFile.Text;
  finally
    FIniFile.Free;
  end;
end;

 { TACBrCustomIniFileHelper }
procedure TACBrCustomIniFileHelper.WriteStringLine(const Section, Ident, Value: String);
begin
  Self.WriteString(Section, Ident, ChangeLineBreak(Value, ''));
end;

procedure TACBrCustomIniFileHelper.ClearEmptySections;
Var
  FSections, FValues: TStringList;
  I: Integer;
begin
  FSections := TStringList.Create;
  FValues := TStringList.Create;

  try
    Self.ReadSections(FSections);
    for I := 0 to FSections.Count - 1 do
    begin
      FValues.Clear;
      Self.ReadSectionValues(FSections.Strings[I], FValues);
      if FValues.Count = 0 then
        Self.EraseSection(FSections.Strings[I]);
    end;
  finally
    FSections.Free;
    FValues.Free;
  end;
end;

end.
