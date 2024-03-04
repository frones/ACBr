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
        Result := true;
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
