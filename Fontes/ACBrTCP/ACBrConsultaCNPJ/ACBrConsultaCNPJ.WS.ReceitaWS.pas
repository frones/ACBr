{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H Gonzales - Pandaaa                     }
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
unit ACBrConsultaCNPJ.WS.ReceitaWS;

interface
uses
  ACBrJSON, Jsons, SysUtils, ACBrConsultaCNPJ.WS;
type
  EACBrConsultaCNPJWSException = class ( Exception );

  { TACBrConsultaCNPJWS }
  TACBrConsultaCNPJWSReceitaWS = class(TACBrConsultaCNPJWS)
    public
      function Executar:boolean; override;
  end;
const
  C_URL = 'https://receitaws.com.br/v1/cnpj/';

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  blcksock,
  Classes,
  synautil, httpsend;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWSReceitaWS.Executar: boolean;
var
  LJSon: TJson;
  LJsonObject : TJsonObject;
  LRetorno, LAuxiliar : String;
  LJsonArray: TJsonArray;
  I, Z, LResultCode : Integer;
begin
  inherited Executar;
  ClearHeaderParams;
  if FUsuario <> '' then
  begin
    AddHeaderParam('Authorization','Bearer ' + FUsuario);
    LAuxiliar := '/days/'+IntToStr(FDefasagemMaxima);
  end;

  LResultCode := SendHttp('GET',C_URL +  OnlyNumber(FCNPJ) + LAuxiliar, LRetorno);

  LJSon := TJson.Create;
  try
    LJSon.Parse('[' + UTF8ToNativeString(LRetorno) + ']');
    for I := 0 to Pred(LJSon.Count) do
    begin
      LJsonObject := LJSon.Get(I).AsObject;
      if LJsonObject.Values['status'].AsString = 'OK' then
      begin
        FResposta.RazaoSocial          := LJsonObject.Values['nome'].AsString;
        FResposta.CNPJ                 := LJsonObject.Values['cnpj'].AsString;
        FResposta.Fantasia             := LJsonObject.Values['fantasia'].AsString;
        FResposta.Abertura             := StringToDateTimeDef(LJsonObject.Values['abertura'].AsString,0);
        FResposta.Porte                := LJsonObject.Values['porte'].AsString;

        LJsonArray := LJsonObject.Values['atividade_principal'].AsArray;
        if LJsonArray.Count > 0 then
          FResposta.CNAE1              := LJsonArray[0].AsObject.Values['code'].AsString + ' ' + LJsonArray[0].AsObject.Values['text'].AsString;

        LJsonArray := LJsonObject.Values['atividades_secundarias'].AsArray;
        for Z := 0 to Pred(LJsonArray.Count) do
        FResposta.CNAE2.Add(LJsonArray[Z].AsObject.Values['code'].AsString + ' ' + LJsonArray[Z].AsObject.Values['text'].AsString);

        FResposta.EmpresaTipo          := LJsonObject.Values['tipo'].AsString;
        FResposta.Endereco             := LJsonObject.Values['logradouro'].AsString;
        FResposta.Numero               := LJsonObject.Values['numero'].AsString;
        FResposta.Complemento          := LJsonObject.Values['complemento'].AsString;
        FResposta.CEP                  := OnlyNumber( LJsonObject.Values['cep'].AsString);
        FResposta.Bairro               := LJsonObject.Values['bairro'].AsString;
        FResposta.Cidade               := LJsonObject.Values['municipio'].AsString;
        FResposta.UF                   := LJsonObject.Values['uf'].AsString;
        FResposta.Situacao             := LJsonObject.Values['situacao'].AsString;
        FResposta.SituacaoEspecial     := LJsonObject.Values['situacao_especial'].AsString;
        FResposta.DataSituacao         := StringToDateTimeDef(LJsonObject.Values['data_situacao'].AsString,0);
        FResposta.DataSituacaoEspecial := StringToDateTimeDef(LJsonObject.Values['data_situacao_especial'].AsString,0);
        FResposta.NaturezaJuridica     := LJsonObject.Values['natureza_juridica'].AsString;
        FResposta.EndEletronico        := LJsonObject.Values['email'].AsString;
        FResposta.Telefone             := LJsonObject.Values['telefone'].AsString;
        FResposta.EFR                  := LJsonObject.Values['efr'].AsString;
        FResposta.MotivoSituacaoCad    := LJsonObject.Values['motivo_situacao'].AsString;
        Result := true;
      end else
      begin
        if (Trim(LJsonObject.Values['message'].AsString) <> '') then
          raise EACBrConsultaCNPJWSException.Create('Erro:'+IntToStr(LResultCode) + ' - ' +LJsonObject.Values['message'].AsString);
      end;
    end;
  finally
    LJSon.Free;
  end;
end;

end.
