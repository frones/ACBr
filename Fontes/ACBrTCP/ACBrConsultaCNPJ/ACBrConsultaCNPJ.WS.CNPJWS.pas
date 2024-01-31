{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Luis - Minf Informática                   }
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
unit ACBrConsultaCNPJ.WS.CNPJWS;

interface
uses
  ACBrJSON, Jsons, SysUtils, ACBrConsultaCNPJ.WS;
type
  EACBrConsultaCNPJWSException = class ( Exception );

  { TACBrConsultaCNPJWS }
  TACBrConsultaCNPJWSCNPJWS = class(TACBrConsultaCNPJWS)
    public
      function Executar:boolean; override;
  end;
const
  C_URL_PUBLICA   = 'https://publica.cnpj.ws/cnpj/';
  C_URL_COMERCIAL = 'https://comercial.cnpj.ws/cnpj/';

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  blcksock,
  Classes,
  synautil, httpsend;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWSCNPJWS.Executar: boolean;
var
  LJSon: TJson;
  LJsonObject : TJsonObject;
  LStream : TStringStream;
  LRetorno : String;
  LJsonArray: TJsonArray;
  I, Z : Integer;
  LJsonObjestabelecimento : Jsons.TJsonObject;
  LJsonObj2               : Jsons.TJsonObject;
  LURL : String;
begin
  inherited Executar;

  ClearHeaderParams;
  if FUsuario <> '' then
  begin
    AddHeaderParam('x_api_token', FUsuario);
    LURL := C_URL_COMERCIAL;
  end else
    LURL := C_URL_PUBLICA;

  SendHttp('GET', LURL +  OnlyNumber(FCNPJ), LRetorno);

  LJSon := TJson.Create;
  try
    LJSon.Parse('[' + UTF8ToNativeString(LRetorno) + ']');
    for I := 0 to Pred(LJSon.Count) do
    begin
      LJsonObject := LJSon.Get(I).AsObject;
      if LJsonObject.Values['status'].AsString = '' then
      begin
        FResposta.RazaoSocial := LJsonObject.Values['razao_social'].AsString;

        LJsonObjestabelecimento := Jsons.TJsonObject.Create;
        LJsonObjestabelecimento.Parse( LJsonObject.Values['estabelecimento'].Stringify );
        FResposta.Fantasia             := LJsonObjestabelecimento.Values['nome_fantasia'].AsString;
        FResposta.CNPJ                 := LJsonObjestabelecimento.Values['cnpj'].AsString;
        FResposta.Abertura             := StringToDateTimeDef(LJsonObjestabelecimento.Values['data_inicio_atividade'].AsString,0,'yyyy/mm/dd');
        FResposta.Endereco             := LJsonObjestabelecimento.Values['tipo_logradouro'].AsString + ' ' +LJsonObject.Values['logradouro'].AsString;
        FResposta.Numero               := LJsonObjestabelecimento.Values['numero'].AsString;
        FResposta.Complemento          := LJsonObjestabelecimento.Values['complemento'].AsString;
        FResposta.CEP                  := OnlyNumber( LJsonObjestabelecimento.Values['cep'].AsString);
        FResposta.Bairro               := LJsonObjestabelecimento.Values['bairro'].AsString;
        FResposta.UF                   := LJsonObjestabelecimento.Values['uf'].AsString;
        FResposta.EndEletronico        := LJsonObjestabelecimento.Values['email'].AsString;
        FResposta.Telefone             := LJsonObjestabelecimento.Values['ddd1'].AsString + LJsonObjestabelecimento.Values['telefone1'].AsString;
        FResposta.Situacao             := LJsonObjestabelecimento.Values['situacao_cadastral'].AsString;
        FResposta.DataSituacao         := StringToDateTimeDef(LJsonObjestabelecimento.Values['data_situacao'].AsString,0,'yyyy/mm/dd');
        FResposta.EmpresaTipo          := LJsonObjestabelecimento.Values['tipo'].AsString;

        LJsonObj2 := TJsonObject.Create;

        if LJsonObjestabelecimento.IsJsonObject( LJsonObjestabelecimento.Values['motivo_situacao_cadastral'].Stringify ) then
        begin
           LJsonObj2.Parse( LJsonObjestabelecimento.Values['motivo_situacao_cadastral'].Stringify );
           FResposta.MotivoSituacaoCad := LJsonObj2.Values['id'].AsString + ' ' + LJsonObj2.Values['descricao'].AsString;
        end;

        FResposta.SituacaoEspecial     := LJsonObjestabelecimento.Values['situacao_especial'].AsString;
        FResposta.DataSituacaoEspecial := StringToDateTimeDef(LJsonObject.Values['data_situacao_especial'].AsString,0,'yyyy/mm/dd');

        LJsonObj2.Parse( LJsonObjestabelecimento.Values['cidade'].Stringify );
        FResposta.Cidade := LJsonObj2.Values['nome'].AsString;

        LJsonObj2.Parse( LJsonObjestabelecimento.Values['estado'].Stringify );
        FResposta.UF := LJsonObj2.Values['sigla'].AsString;

        LJsonObj2.Parse( LJsonObjestabelecimento.Values['atividade_principal'].Stringify );
        FResposta.CNAE1 := LJsonObj2.Values['id'].AsString + ' ' + LJsonObj2.Values['descricao'].AsString;

        LJsonArray := LJsonObjestabelecimento.Values['atividades_secundarias'].AsArray;
        for Z := 0 to Pred(LJsonArray.Count) do
           FResposta.CNAE2.Add(LJsonArray[Z].AsObject.Values['id'].AsString + ' ' + LJsonArray[Z].AsObject.Values['descricao'].AsString);

        LJsonObj2.Parse( LJsonObject.Values['porte'].Stringify );
        FResposta.Porte := LJsonObj2.Values['descricao'].AsString;

        LJsonObj2.Parse( LJsonObject.Values['natureza_juridica'].Stringify );
        FResposta.NaturezaJuridica     := LJsonObj2.Values['descricao'].AsString;
        FResposta.EFR                  := '';

        if LJsonObjestabelecimento.Values['inscricoes_estaduais'].AsArray.Count >0 then
        begin
           LJsonArray := LJsonObjestabelecimento.Values['inscricoes_estaduais'].AsArray;
           if LJsonArray[0].AsObject['ativo'].AsBoolean then
              FResposta.InscricaoEstadual := LJsonArray[0].AsObject['inscricao_estadual'].AsString;
        end;

        Result := true;
      end else
      begin
        if (Trim(LJsonObject.Values['titulo'].AsString) <> '') then
          raise EACBrConsultaCNPJWSException.Create('Erro: '+LJsonObject.Values['status'].AsString + ' - ' +LJsonObject.Values['detalhes'].AsString);
      end;
    end;
  finally
    LJSon.Free;
  end;
end;

end.