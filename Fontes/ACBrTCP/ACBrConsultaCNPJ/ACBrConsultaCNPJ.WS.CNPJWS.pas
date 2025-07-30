{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para intera��o com equipa- }
{ mentos de Automa��o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H Gonzales - Pandaaa                     }
{                              Andr� Luis - Minf Inform�tica                   }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Sim�es de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatu� - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}
unit ACBrConsultaCNPJ.WS.CNPJWS;

interface
uses
  ACBrConsultaCNPJ.WS,
  SysUtils;

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
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrJSON;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWSCNPJWS.Executar: boolean;
var
  LJson, LJsonObject : TACBrJSONObject;
  LJsonArray: TACBrJSONArray;
  LRetorno : String;
  I, LCodigoRetorno : Integer;
  LURL : String;
  LValor: String;
begin
  Result := False;

  inherited Executar;

  ClearHeaderParams;
  if FUsuario <> '' then
  begin
    AddHeaderParam('x_api_token', FUsuario);
    LURL := C_URL_COMERCIAL;
  end else
    LURL := C_URL_PUBLICA;

  LCodigoRetorno := SendHttp('GET', LURL +  OnlyNumber(FCNPJ), LRetorno);

  LJson := TACBrJSONObject.Parse( UTF8ToNativeString(LRetorno) );
  try
    if (LJson.AsString['status'] = '') and (LCodigoRetorno < 299) then
    begin
      FResposta.RazaoSocial       := LJson.AsString['razao_social'];
      FResposta.Porte             := LJson.AsJSONObject['porte'].AsString['descricao'];
      FResposta.NaturezaJuridica  := LJson.AsJSONObject['natureza_juridica'].AsString['descricao'];

      LJsonObject := LJson.AsJSONObject['estabelecimento'];
      FResposta.CNPJ           := LJsonObject.AsString['cnpj'];
      FResposta.Fantasia       := LJsonObject.AsString['nome_fantasia'];
      FResposta.Abertura       := StringToDateTimeDef(LJsonObject.AsString['data_inicio_atividade'],0,'yyyy/mm/dd');

      FResposta.Endereco       := LJsonObject.AsString['logradouro'];
      LValor := Trim(LJsonObject.AsString['tipo_logradouro']);
      if LValor <> '' then
      begin
        if AnsiUpperCase(Copy(FResposta.Endereco, 1, Length(LValor))) <> AnsiUpperCase(LValor) then
          FResposta.Endereco := LValor + ' ' + FResposta.Endereco;
      end;

      FResposta.Numero         := LJsonObject.AsString['numero'];
      FResposta.Complemento    := LJsonObject.AsString['complemento'];
      FResposta.CEP            := OnlyNumber( LJsonObject.AsString['cep']);
      FResposta.Bairro         := LJsonObject.AsString['bairro'];
      FResposta.UF             := LJsonObject.AsString['uf'];
      FResposta.EndEletronico  := LJsonObject.AsString['email'];
      FResposta.Telefone       := LJsonObject.AsString['ddd1'] + LJsonObject.AsString['telefone1'];
      FResposta.Situacao       := LJsonObject.AsString['situacao_cadastral'];
      FResposta.DataSituacao   := StringToDateTimeDef(LJsonObject.AsString['data_situacao'],0,'yyyy/mm/dd');
      FResposta.EmpresaTipo    := LJsonObject.AsString['tipo'];
      FResposta.SituacaoEspecial     := LJsonObject.AsString['situacao_especial'];
      FResposta.DataSituacaoEspecial := StringToDateTimeDef(LJsonObject.AsString['data_situacao_especial'],0,'yyyy/mm/dd');
      FResposta.Cidade               := LJsonObject.AsJSONObject['cidade'].AsString['nome'];
      FResposta.UF                   := LJsonObject.AsJSONObject['estado'].AsString['sigla'];
      FResposta.CodigoIBGE     := LJsonObject.AsJSONObject['cidade'].AsString['ibge_id'];

      FResposta.EFR                  := '';
      FResposta.CapitalSocial        := StrToFloatDef(StringReplace(LJson.AsString['capital_social'],'.',',',[rfReplaceAll]),0);

      FResposta.Simples              := LJson.AsJSONObject['simples'].AsString['simples'] = 'Sim';
      FResposta.DataOpcaoSimples     := StringToDateTimeDef(LJson.AsJSONObject['simples'].AsString['data_opcao_simples'],0,'yyyy/mm/dd');
      FResposta.DataExclusaoSimples  := StringToDateTimeDef(LJson.AsJSONObject['simples'].AsString['data_exclusao_simples'],0,'yyyy/mm/dd');
      FResposta.Mei                  := LJson.AsJSONObject['simples'].AsString['mei']= 'Sim';
      FResposta.DataOpcaoMei         := StringToDateTimeDef(LJson.AsJSONObject['simples'].AsString['data_opcao_mei'],0,'yyyy/mm/dd');
      FResposta.DataExclusaoMei      := StringToDateTimeDef(LJson.AsJSONObject['simples'].AsString['data_exclusao_mei'],0,'yyyy/mm/dd');

      FResposta.CNAE1 := LJsonObject.AsJSONObject['atividade_principal'].AsString['id'] + ' ' +
                         LJsonObject.AsJSONObject['atividade_principal'].AsString['descricao'];

      LJsonArray := LJsonObject.AsJSONArray['atividades_secundarias'];
      for I := 0 to Pred(LJsonArray.Count) do
        FResposta.CNAE2.Add(LJsonArray.ItemAsJSONObject[I].AsString['id'] + ' ' +
                            LJsonArray.ItemAsJSONObject[I].AsString['descricao']);

      LJsonArray := LJsonObject.AsJSONArray['inscricoes_estaduais'];
      for I := 0 to Pred(LJsonArray.Count) do
         if (LJsonArray.ItemAsJSONObject[I].AsBoolean['ativo']) and
            (LJsonArray.ItemAsJSONObject[I].AsJSONObject['estado'].AsString['sigla'] = LJsonObject.AsJSONObject['estado'].AsString['sigla']) then
            FResposta.InscricaoEstadual := LJsonArray.ItemAsJSONObject[I].AsString['inscricao_estadual'];

      LJsonObject := LJson.AsJSONObject['motivo_situacao_cadastral'];
      if LJson.IsJSONObject('motivo_situacao_cadastral' ) then
         FResposta.MotivoSituacaoCad := LJsonObject.AsString['id'] + ' ' + LJsonObject.AsString['descricao'];

      Result := true;
    end else
    begin
      if (Trim(LJSon.AsString['titulo']) <> '') or (LCodigoRetorno > 299) then
        raise EACBrConsultaCNPJWSException.Create('Erro: '+LJSon.AsString['status'] + ' - ' +LJSon.AsString['detalhes'] + 'C�digo:' + IntToStr(LCodigoRetorno) + ' - '+ ResultString);
    end;
  finally
    LJSon.Free;
  end;
end;

end.
