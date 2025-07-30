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
unit ACBrConsultaCNPJ.WS.ReceitaWS;

interface
uses
  ACBrConsultaCNPJ.WS,
  SysUtils;

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
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrJSON;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWSReceitaWS.Executar: boolean;
var
  LJSon: TACBrJSONArray;
  LJsonObject : TACBrJSONObject;
  LJsonArray: TACBrJSONArray;
  LRetorno, LAuxiliar : String;
  I, Z, LResultCode : Integer;
begin
  Result := False;

  inherited Executar;
  ClearHeaderParams;
  if FUsuario <> '' then
  begin
    AddHeaderParam('Authorization','Bearer ' + FUsuario);
    LAuxiliar := '/days/'+IntToStr(FDefasagemMaxima);
  end;

  LResultCode := SendHttp('GET',C_URL +  OnlyNumber(FCNPJ) + LAuxiliar, LRetorno);

  LJSon := TACBrJSONArray.Parse('[' + UTF8ToNativeString(LRetorno) + ']');

  try
    for I := 0 to Pred(LJSon.Count) do
    begin

      LJsonObject := LJSon.ItemAsJSONObject[I];

      if LJsonObject.AsString['status'] = 'OK' then
      begin
        FResposta.RazaoSocial          := LJsonObject.AsString['nome'];
        FResposta.CNPJ                 := LJsonObject.AsString['cnpj'];
        FResposta.Fantasia             := LJsonObject.AsString['fantasia'];
        FResposta.Abertura             := StringToDateTimeDef(LJsonObject.AsString['abertura'],0);
        FResposta.Porte                := LJsonObject.AsString['porte'];

        LJsonArray := LJsonObject.AsJSONArray['atividade_principal'];
        if LJsonArray.Count > 0 then
          FResposta.CNAE1              := LJsonArray.ItemAsJSONObject[0].AsString['code'] + ' ' + LJsonArray.ItemAsJSONObject[0].AsString['text'];

        LJsonArray := LJsonObject.AsJSONArray['atividades_secundarias'];
        for Z := 0 to Pred(LJsonArray.Count) do
          FResposta.CNAE2.Add(LJsonArray.ItemAsJSONObject[Z].AsString['code'] + ' ' + LJsonArray.ItemAsJSONObject[Z].AsString['text']);

        FResposta.EmpresaTipo          := LJsonObject.AsString['tipo'];
        FResposta.Endereco             := LJsonObject.AsString['logradouro'];
        FResposta.Numero               := LJsonObject.AsString['numero'];
        FResposta.Complemento          := LJsonObject.AsString['complemento'];
        FResposta.CEP                  := OnlyNumber( LJsonObject.AsString['cep']);
        FResposta.Bairro               := LJsonObject.AsString['bairro'];
        FResposta.Cidade               := LJsonObject.AsString['municipio'];
        FResposta.UF                   := LJsonObject.AsString['uf'];
        FResposta.Situacao             := LJsonObject.AsString['situacao'];
        FResposta.SituacaoEspecial     := LJsonObject.AsString['situacao_especial'];
        FResposta.DataSituacao         := StringToDateTimeDef(LJsonObject.AsString['data_situacao'],0);
        FResposta.DataSituacaoEspecial := StringToDateTimeDef(LJsonObject.AsString['data_situacao_especial'],0);
        FResposta.NaturezaJuridica     := LJsonObject.AsString['natureza_juridica'];
        FResposta.EndEletronico        := LJsonObject.AsString['email'];
        FResposta.Telefone             := LJsonObject.AsString['telefone'];
        FResposta.EFR                  := LJsonObject.AsString['efr'];
        FResposta.MotivoSituacaoCad    := LJsonObject.AsString['motivo_situacao'];
        FResposta.CapitalSocial        := LJsonObject.AsFloat['capital_social'];
        FResposta.Simples              := LJsonObject.AsJSONObject['simples'].AsBoolean['optante'];
        FResposta.DataOpcaoSimples     := StringToDateTimeDef(LJsonObject.AsJSONObject['simples'].AsString['data_opcao'],0);
        FResposta.DataExclusaoSimples  := StringToDateTimeDef(LJsonObject.AsJSONObject['simples'].AsString['data_exclusao'],0);
        FResposta.Mei                  := LJsonObject.AsJSONObject['simei'].AsBoolean['optante'];
        FResposta.DataOpcaoMei         := StringToDateTimeDef(LJsonObject.AsJSONObject['simei'].AsString['data_opcao'],0);
        FResposta.DataExclusaoMei      := StringToDateTimeDef(LJsonObject.AsJSONObject['simei'].AsString['data_exclusao'],0);
        Result := true;
      end else
      begin
        if (Trim(LJsonObject.AsString['message']) <> '') then
          raise EACBrConsultaCNPJWSException.Create('Erro:'+IntToStr(LResultCode) + ' - ' +LJsonObject.AsString['message'])
        else
          raise EACBrConsultaCNPJWSException.Create('Erro:'+IntToStr(LResultCode) + ' - Status [' + LJsonObject.AsString['status'] + ']')
      end;
    end;
    if (LResultCode > 299) then
      raise EACBrConsultaCNPJWSException.Create('Erro:'+IntToStr(LResultCode) + ' - ' +ResultString);
  finally
    LJSon.Free;
  end;
end;

end.
