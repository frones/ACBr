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

unit Bauhaus.GravarJson;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  pcnAuxiliar, pcnConsts,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { TNFSeW_Bauhaus }

  TNFSeW_Bauhaus = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
//  {$IfDef USE_JSONDATAOBJECTS_UNIT}
//    JsonDataObjects_ACBr,
//  {$Else}
    Jsons,
//  {$EndIf}
  ACBrNFSeX;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o Json do RPS do provedor:
//     Bauhaus
//==============================================================================

{ TNFSeW_Bauhaus }

function TNFSeW_Bauhaus.GerarXml: Boolean;
var
  Data: string;
  Json: TJSONObject;
begin
  Json := TJsonObject.Create;

  try
    {
    Json.Add('numeroConvenio').Value.AsNumber                         := StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0);
    Json.Add('numeroCarteira').Value.AsInteger                        := StrToIntDef(OnlyNumber(Titulos.Carteira),0);
    Json.Add('numeroVariacaoCarteira').Value.AsInteger                := StrToIntDef(OnlyNumber(Boleto.Cedente.Modalidade),0);
    Json.Add('codigoModalidade').Value.AsInteger                      := 1;

    Json.Add('dataEmissao').Value.AsString                            := FormatDateBr(Titulos.DataDocumento, 'DD.MM.YYYY');
    Json.Add('dataVencimento').Value.AsString                         := FormatDateBr(Titulos.Vencimento, 'DD.MM.YYYY');
    Json.Add('valorOriginal').Value.AsNumber                          := Titulos.ValorDocumento;
    Json.Add('valorAbatimento').Value.AsNumber                        := Titulos.ValorAbatimento;

    if (Titulos.DataProtesto > 0) then
      Json.Add('quantidadeDiasProtesto').Value.AsInteger              := Trunc(Titulos.DataProtesto - Titulos.Vencimento);

    if (Titulos.DataLimitePagto > 0 ) then
    begin
      Json.Add('indicadorAceiteTituloVencido').Value.AsString         := 'S';
      Json.Add('numeroDiasLimiteRecebimento').Value.AsInteger         := Trunc(Titulos.DataLimitePagto - Titulos.Vencimento);
    end;

    Json.Add('codigoAceite').Value.AsString                           := IfThen(Titulos.Aceite = atSim,'A','N');
    Json.Add('codigoTipoTitulo').Value.AsInteger                      := codigoTipoTitulo(Titulos.EspecieDoc);
    Json.Add('descricaoTipoTitulo').Value.AsString                    := Titulos.EspecieDoc;
    //Json.Add('indicadorPermissaoRecebimentoParcial').Value.AsString := 'N';
    Json.Add('numeroTituloBeneficiario').Value.AsString               := Copy(Trim(UpperCase(Titulos.NumeroDocumento)),0,15);
    Json.Add('campoUtilizacaoBeneficiario').Value.AsString            := Copy(Trim(StringReplace(UpperCase(Titulos.Mensagem.Text),'\r\n',' ',[rfReplaceAll])),0,30);
    Json.Add('numeroTituloCliente').Value.AsString                    := Boleto.Banco.MontarCampoNossoNumero(Titulos);
    Json.Add('mensagemBloquetoOcorrencia').Value.AsString             := UpperCase(Copy(Trim(Titulos.Instrucao1 +' '+Titulos.Instrucao2+' '+Titulos.Instrucao3),0,165));

    GerarDesconto(Json);
    GerarJuros(Json);
    GerarMulta(Json);
    GerarPagador(Json);
    GerarBenificiarioFinal(Json);

    if (Titulos.DiasDeNegativacao > 0) then
    begin
      Json.Add('quantidadeDiasNegativacao').Value.AsInteger           := Titulos.DiasDeNegativacao;
      Json.Add('orgaoNegativador').Value.AsInteger                    := StrToInt64Def(Titulos.orgaoNegativador,0);
    end;

    Json.Add('indicadorPix').Value.AsString := IfThen(Boleto.Cedente.CedenteWS.IndicadorPix,'S','N');

    Data := Json.Stringify;

    FPDadosMsg := Data;
    }
  finally
    Json.Free;
  end;

  Result := True;
end;

procedure TNFSeW_Bauhaus.Configuracao;
begin
  inherited Configuracao;

end;

end.
