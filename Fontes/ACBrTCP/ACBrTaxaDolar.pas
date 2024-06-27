{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Jefferson                                      }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrTaxaDolar;

interface

uses
  SysUtils, Classes, ACBrSocket;

type

  EACBrTaxaDolarException = class ( Exception );

  { TACBrTaxaDolar }

  TACBrTaxaDolar = class(TACBrHTTP)
  private
    FTaxaDeVenda: Double;
    FTaxaDeCompra: Double;
    FData: TDateTime;
    FTermoResponsabilidade: String;
  protected
  public
    function Consulta(): Boolean;
  published
    property Data: TDateTime Read FData;
    property TaxaDeCompra: Double Read FTaxaDeCompra;
    property TaxaDeVenda: Double Read FTaxaDeVenda;
    property TermoResponsabilidade : String Read FTermoResponsabilidade;
  end;

implementation

uses
  ACBrUtil.Strings;

function TACBrTaxaDolar.Consulta: Boolean;
var
  Buffer: String;
begin
  Self.HTTPGet('http://www4.bcb.gov.br/pec/taxas/batch/taxas.asp');
  Buffer := StripHTML(DecodeToString(HTTPResponse, RespIsUTF8));

  //DEBUG
  //WriteToTXT( 'c:\temp\bobo.txt', Buffer, False)

  (*Html := StringReplace(Html, #13#10, '', [rfReplaceAll]);

  if Trim(Html) <> '' then
  begin
    Result                 := True;
    Html:= StrEntreStr(Html, '<!--TAG_CONTEUDO_INICIO-->', '<!--TAG_CONTEUDO_FIM-->');
    FConteudoHtml := Html;
    FData                  := GetData;
    FTaxaDeVenda           := GetTaxaDeVenda;
    FTaxaDeCompra          := GetTaxaDeCompra;
    FTermoResponsabilidade := GetTermoResponsabilidade;
    if (FData = 0) then
      raise EACBrTaxaDolarException.Create('Não foi possível obter os dados.');
  end
  else
  begin
    raise EACBrTaxaDolarException.Create(Html);
  end;*)
end;

end.

function TACBrTaxaDolar.GetData: TDateTime;
var
  S: String;
begin
  S := Html;
  S := StrEntreStr(S, '<td ALIGN="CENTER" class="fundoPadraoBClaro2">', '</td>');
  Result := StrToDate(S);
end;

function TACBrTaxaDolar.GetTaxaDeCompra: Double;
var
  S: String;
begin
  S := Html;
  S := StrEntreStr(S, '<td ALIGN="right" class="fundoPadraoBClaro2">', '</td>');
  Result := StrToFloatDef(S,0);
end;

function TACBrTaxaDolar.GetTaxaDeVenda: Double;
var
  S: String;
begin
  S := Html;
  S := StrPularStr(S, '<td ALIGN="right" class="fundoPadraoBClaro2">');
  S := StrEntreStr(S, '<td ALIGN="right" class="fundoPadraoBClaro2">', '</td>');
  Result := StrToFloatDef(S,0);
end;

function TACBrTaxaDolar.GetTermoResponsabilidade: String;
var
  S: String;
begin
  S := Html;
  S := StrPularStr(S, '</blockquote>');
  S := StrEntreStr(S, '<img src="http://www.bcb.gov.br/img/BulletAzul2.gif" alt=" ">', '</div>');
  S := StringReplace(S, '&nbsp;', '', [rfReplaceAll]);
  Result := S;
end;


function StrPularStr(Str, StrPular: String): String;
  var
    Ini: Integer;
begin
  Ini:= Pos(StrPular, Str);
  if Ini > 0 then
    Result:= Copy(Str, Ini + Length(StrPular), Length(Str))
  else
    Result:= Str;
end;

function StrEntreStr(Str, StrInicial, StrFinal: String; ComecarDe: Integer = 1): String;
var
  Ini, Fim: Integer;
begin
  Ini:= PosEx(StrInicial, Str, ComecarDe) + Length(StrInicial);
  if Ini > 0 then
  begin
    Fim:= PosEx(StrFinal, Str, Ini);
    if Fim > 0 then
      Result:= Copy(Str, Ini, Fim - Ini)
    else
      Result:= '';
  end
  else
    Result:= '';
end;


function GetURLSepara(URL: String): String;
var
  I, R: Integer;
  MyUrl : String;
begin
  MyUrl := URL;
  R:= Length(MyUrl);
  try
    for I := 0 to Length(MyUrl) do
      if Copy(MyUrl, i, 1) = '/' then
        R:= I;
  except
  end;
  Result:= Copy(URL, 1, R);
end;


