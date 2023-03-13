{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

{$I ACBr.inc}

unit Saatri.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrNFSeXLerXml_ABRASFv2;

type
  { TNFSeR_Saatri201 }

  TNFSeR_Saatri201 = class(TNFSeR_ABRASFv2)
  protected

  public
//    function LerXml: Boolean; override;

  end;

implementation

uses
  ACBrJSON;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Saatri
//==============================================================================

{ TNFSeR_Saatri201 }
(*
function TNFSeR_Saatri201.LerXml: Boolean;
var
  xDiscriminacao: string;
  json, jsonItem: TACBrJsonObject;
  i: Integer;
begin
  Result := inherited LerXml;

  // Tratar a Discriminacao do serviço

  xDiscriminacao := NFSe.Servico.Discriminacao;
  FpAOwner.ConfigGeral.DetalharServico := False;

  if (Pos('[', xDiscriminacao) > 0) and (Pos(']', xDiscriminacao) > 0) and
     (Pos('{', xDiscriminacao) > 0) and (Pos('}', xDiscriminacao) > 0) then
  begin
    FpAOwner.ConfigGeral.DetalharServico := True;

    xDiscriminacao := '{"a": ' + xDiscriminacao + '}';
    Json := TACBrJsonObject.Parse(xDiscriminacao);

    for i := 0 to json.AsJSONArray['a'].Count -1 do
    begin
      jsonItem := json.AsJSONArray['a'].ItemAsJSONObject[i];

      with NFSe.Servico.ItemServico.New do
      begin
        Descricao := jsonItem.AsString['Descricao'];
        ValorUnitario := jsonItem.AsCurrency['ValorUnitario'];
        Quantidade := jsonItem.AsCurrency['Quantidade'];
        ValorTotal := jsonItem.AsCurrency['ValorTotal'];
      end;
    end;
  end;
end;
*)
end.
