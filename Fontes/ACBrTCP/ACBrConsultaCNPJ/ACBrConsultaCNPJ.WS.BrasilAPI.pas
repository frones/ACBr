{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para intera��o com equipa- }
{ mentos de Automa��o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H Gonzales - Pandaaa                     }
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
unit ACBrConsultaCNPJ.WS.BrasilAPI;

interface
uses
  ACBrConsultaCNPJ.WS,
  SysUtils;

type
  EACBrConsultaCNPJWSException = class ( Exception );

  { TACBrConsultaCNPJWS }
  TACBrConsultaCNPJWSBrasilAPI = class(TACBrConsultaCNPJWS)
    public
      function Executar:boolean; override;
  end;
const
  C_URL = 'https://brasilapi.com.br/api/cnpj/v1/';

implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrJSON;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWSBrasilAPI.Executar: boolean;
var
  LJSon: TACBrJSONArray;
  LJsonObject : TACBrJSONObject;
  LJsonArray: TACBrJSONArray;
  LRetorno : String;
  I, Z, LResultCode : Integer;
begin
  Result := False;

  inherited Executar;

  LResultCode := SendHttp('GET',C_URL +  OnlyNumber(FCNPJ), LRetorno);

  LJSon := TACBrJSONArray.Parse('[' + UTF8ToNativeString(LRetorno) + ']');

  try
    for I := 0 to Pred(LJSon.Count) do
    begin

      LJsonObject := LJSon.ItemAsJSONObject[I];

      if LResultCode = 200 then
      begin
        FResposta.RazaoSocial          := LJsonObject.AsString['razao_social'];
        FResposta.CNPJ                 := LJsonObject.AsString['cnpj'];
        FResposta.Fantasia             := LJsonObject.AsString['nome_fantasia'];
        FResposta.Abertura             := StringToDateTimeDef(LJsonObject.AsString['data_inicio_atividade'],0,'yyyy/mm/dd');
        FResposta.Porte                := LJsonObject.AsString['porte'];

        FResposta.CNAE1                := IntToStr(LJsonObject.AsInteger['cnae_fiscal']) + ' ' + LJsonObject.AsString['cnae_fiscal_descricao'];

        LJsonArray := LJsonObject.AsJSONArray['cnaes_secundarios'];

        for Z := 0 to Pred(LJsonArray.Count) do
          FResposta.CNAE2.Add(IntToStr(LJsonArray.ItemAsJSONObject[Z].AsInteger['codigo']) + ' ' + LJsonArray.ItemAsJSONObject[Z].AsString['descricao']);

        FResposta.EmpresaTipo          := LJsonObject.AsString['descricao_identificador_matriz_filial'];
        FResposta.Endereco             := Trim(LJsonObject.AsString['descricao_tipo_de_logradouro'] + ' ' + LJsonObject.AsString['logradouro']);
        FResposta.Numero               := LJsonObject.AsString['numero'];
        FResposta.Complemento          := LJsonObject.AsString['complemento'];
        FResposta.CEP                  := FormatFloat('00000000', LJsonObject.AsInteger['cep']);
        FResposta.Bairro               := LJsonObject.AsString['bairro'];
        FResposta.Cidade               := LJsonObject.AsString['municipio'];
        FResposta.CodigoIBGE           := IntToStr(LJsonObject.AsInteger['codigo_municipio_ibge']);
        FResposta.UF                   := LJsonObject.AsString['uf'];
        FResposta.Situacao             := LJsonObject.AsString['descricao_situacao_cadastral'];
        FResposta.SituacaoEspecial     := LJsonObject.AsString['situacao_especial'];
        FResposta.DataSituacao         := StringToDateTimeDef(LJsonObject.AsString['data_situacao_cadastral'],0,'yyyy/mm/dd');
        FResposta.DataSituacaoEspecial := StringToDateTimeDef(LJsonObject.AsString['data_situacao_especial'],0,'yyyy/mm/dd');
        FResposta.NaturezaJuridica     := IntToStr(LJsonObject.AsInteger['codigo_natureza_juridica']);
        FResposta.EndEletronico        := LJsonObject.AsString['email'];
        FResposta.Telefone             := LJsonObject.AsString['ddd_telefone_1'];
        FResposta.EFR                  := '';
        FResposta.CapitalSocial        := LJsonObject.AsFloat['capital_social'];

        FResposta.Simples              := LJsonObject.AsBoolean['opcao_pelo_simples'];
        FResposta.DataOpcaoSimples     := StringToDateTimeDef(LJsonObject.AsString['data_opcao_pelo_simples'],0,'yyyy/mm/dd');
        FResposta.DataExclusaoSimples  := StringToDateTimeDef(LJsonObject.AsString['data_exclusao_do_simples'],0,'yyyy/mm/dd');
        FResposta.Mei                  := LJsonObject.AsBoolean['opcao_pelo_mei'];
        FResposta.DataOpcaoMei         := StringToDateTimeDef(LJsonObject.AsString['data_opcao_pelo_mei'],0,'yyyy/mm/dd');
        FResposta.DataExclusaoMei      := StringToDateTimeDef(LJsonObject.AsString['data_exclusao_do_mei'],0,'yyyy/mm/dd');

        FResposta.MotivoSituacaoCad    := LJsonObject.AsString['descricao_motivo_situacao_cadastral'];

        Result := True;
      end else
      begin
        if (Trim(LJsonObject.AsString['message']) <> '') then
          raise EACBrConsultaCNPJWSException.Create('Erro:'+IntToStr(LResultCode) + ' - ' +LJsonObject.AsString['message']);
      end;
    end;
    if (LResultCode > 299) then
      raise EACBrConsultaCNPJWSException.Create('Erro:'+IntToStr(LResultCode) + ' - ' +ResultString);
  finally
    LJSon.Free;
  end;
end;

end.
