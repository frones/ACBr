{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida                }
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

{$modeswitch TypeHelpers}

unit ACBrLibHelpers;
  
interface

uses
  Classes, SysUtils,
  rttiutils, ACBrRtti;
 
type
  TACBrPropInfoListHelper = class helper for TPropInfoList
    function GetProperties: TArray<TRttiProperty>;
  end;

  TEnum = record
    class function TryParse<T>(const Value: Integer; out AEnum: T): Boolean; overload; static;
    class function TryParse<T>(const Value: string; out AEnum: T): Boolean; overload; static;
    class function Parse<T>(const Value: Integer): T; overload; static;
    class function Parse<T>(const Value: String): T; overload; static;
  end;

implementation

uses
  TypInfo, ACBrUtil.Strings;

{ TACBrPropInfoListHelper }

function TACBrPropInfoListHelper.GetProperties: TArray<TRttiProperty>;
Var
  i: Integer;
begin
  SetLength(Result, Self.Count);
  for i := 0 to Self.Count - 1 do
  begin
    Result[i] := TRttiProperty.Create(Self.Items[i]);
  end;
end;

{ TEnum }

class function TEnum.TryParse<T>(const Value: Integer; out AEnum: T): Boolean;
begin
  if PTypeInfo(TypeInfo(T))^.Kind <> tkEnumeration then
      Exit;

  try
    AEnum := T(GetEnumValue(TypeInfo(T), GetEnumName(TypeInfo(T), Value)));
    Result := True;
  except
    Result := false;
  end;
end;

class function TEnum.TryParse<T>(const Value: String; out AEnum: T): Boolean;
begin
  if PTypeInfo(TypeInfo(T))^.Kind <> tkEnumeration then
      Exit;

  try
    AEnum := T(GetEnumValue(TypeInfo(T), Value));
    Result := True;
  except
    Result := false;
  end;
end;

class function TEnum.Parse<T>(const Value: Integer): T;
begin
  if PTypeInfo(TypeInfo(T))^.Kind <> tkEnumeration then
      Exit;

  Result := T(GetEnumValue(TypeInfo(T), GetEnumName(TypeInfo(T), Value)));
end;

class function TEnum.Parse<T>(const Value: String): T;
begin
  if PTypeInfo(TypeInfo(T))^.Kind <> tkEnumeration then
      Exit;

  Result := T(GetEnumValue(TypeInfo(T), Value));
end;

end.
