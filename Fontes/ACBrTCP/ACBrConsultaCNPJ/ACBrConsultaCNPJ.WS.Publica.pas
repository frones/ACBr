<<<<<<< .mine
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
unit ACBrConsultaCNPJ.WS.Publica;

interface
uses
  ACBrJSON, Jsons, SysUtils, ACBrConsultaCNPJ.WS;
type
  EACBrConsultaCNPJWSException = class ( Exception );

  { TACBrConsultaCNPJWS }
  TACBrConsultaCNPJWSPublica = class(TACBrConsultaCNPJWS)
    public
      function Executar:boolean; override;
  end;
const
  C_URL = 'https://publica.cnpj.ws/cnpj/';

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  blcksock,
  Classes,
  synautil, httpsend;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWSPublica.Executar: boolean;
var
  LJSon: TJson;
  LJsonObject : TJsonObject;
  LStream : TStringStream;
  LRetorno : String;
  LJsonArray: TJsonArray;
  I, Z : Integer;
  JsonObjestabelecimento : Jsons.TJsonObject;
  JsonObj2               : Jsons.TJsonObject;
begin
  inherited Executar;

  SendHttp('GET',C_URL +  OnlyNumber(FCNPJ), LRetorno);

  LJSon := TJson.Create;
  try
    LJSon.Parse('[' + UTF8ToNativeString(LRetorno) + ']');
    for I := 0 to Pred(LJSon.Count) do
    begin
      LJsonObject := LJSon.Get(I).AsObject;
      if LJsonObject.Values['status'].AsString = '' then
      begin
        FResposta.RazaoSocial := LJsonObject.Values['razao_social'].AsString;

        JsonObjestabelecimento := Jsons.TJsonObject.Create;
        JsonObjestabelecimento.Parse( LJsonObject.Values['estabelecimento'].Stringify );
        FResposta.Fantasia             := JsonObjestabelecimento.Values['nome_fantasia'].AsString;
        FResposta.CNPJ                 := JsonObjestabelecimento.Values['cnpj'].AsString;
        FResposta.Abertura             := StringToDateTimeDef(JsonObjestabelecimento.Values['data_inicio_atividade'].AsString,0,'yyyy/mm/dd');
        FResposta.Endereco             := JsonObjestabelecimento.Values['tipo_logradouro'].AsString + ' ' +LJsonObject.Values['logradouro'].AsString;
        FResposta.Numero               := JsonObjestabelecimento.Values['numero'].AsString;
        FResposta.Complemento          := JsonObjestabelecimento.Values['complemento'].AsString;
        FResposta.CEP                  := OnlyNumber( JsonObjestabelecimento.Values['cep'].AsString);
        FResposta.Bairro               := JsonObjestabelecimento.Values['bairro'].AsString;
        FResposta.UF                   := JsonObjestabelecimento.Values['uf'].AsString;
        FResposta.EndEletronico        := JsonObjestabelecimento.Values['email'].AsString;
        FResposta.Telefone             := JsonObjestabelecimento.Values['ddd1'].AsString + JsonObjestabelecimento.Values['telefone1'].AsString;
        FResposta.Situacao             := JsonObjestabelecimento.Values['situacao_cadastral'].AsString;
        FResposta.DataSituacao         := StringToDateTimeDef(JsonObjestabelecimento.Values['data_situacao'].AsString,0,'yyyy/mm/dd');
        FResposta.EmpresaTipo          := JsonObjestabelecimento.Values['tipo'].AsString;

        JsonObj2 := TJsonObject.Create;

        if JsonObjestabelecimento.IsJsonObject( JsonObjestabelecimento.Values['motivo_situacao_cadastral'].Stringify ) then
        begin
           JsonObj2.Parse( JsonObjestabelecimento.Values['motivo_situacao_cadastral'].Stringify );
           FResposta.MotivoSituacaoCad := JsonObj2.Values['id'].AsString + ' ' + JsonObj2.Values['descricao'].AsString;
        end;

        FResposta.SituacaoEspecial     := JsonObjestabelecimento.Values['situacao_especial'].AsString;
        FResposta.DataSituacaoEspecial := StringToDateTimeDef(LJsonObject.Values['data_situacao_especial'].AsString,0,'yyyy/mm/dd');

        JsonObj2.Parse( JsonObjestabelecimento.Values['cidade'].Stringify );
        FResposta.Cidade := JsonObj2.Values['nome'].AsString;

        JsonObj2.Parse( JsonObjestabelecimento.Values['estado'].Stringify );
        FResposta.UF := JsonObj2.Values['sigla'].AsString;

        JsonObj2.Parse( JsonObjestabelecimento.Values['atividade_principal'].Stringify );
        FResposta.CNAE1 := JsonObj2.Values['id'].AsString + ' ' + JsonObj2.Values['descricao'].AsString;

        LJsonArray := JsonObjestabelecimento.Values['atividades_secundarias'].AsArray;
        for Z := 0 to Pred(LJsonArray.Count) do
           FResposta.CNAE2.Add(LJsonArray[Z].AsObject.Values['id'].AsString + ' ' + LJsonArray[Z].AsObject.Values['descricao'].AsString);

        JsonObj2.Parse( LJsonObject.Values['porte'].Stringify );
        FResposta.Porte := JsonObj2.Values['descricao'].AsString;

        JsonObj2.Parse( LJsonObject.Values['natureza_juridica'].Stringify );
        FResposta.NaturezaJuridica     := JsonObj2.Values['descricao'].AsString;
        FResposta.EFR                  := '';

        if JsonObjestabelecimento.Values['inscricoes_estaduais'].AsArray.Count >0 then
        begin
           LJsonArray := JsonObjestabelecimento.Values['inscricoes_estaduais'].AsArray;
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
||||||| .r0
=======
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
unit ACBrConsultaCNPJ.WS.Publica;

interface
uses
  ACBrJSON, Jsons, SysUtils, ACBrConsultaCNPJ.WS;
type
  EACBrConsultaCNPJWSException = class ( Exception );

  { TACBrConsultaCNPJWS }
  TACBrConsultaCNPJWSPublica = class(TACBrConsultaCNPJWS)
    public
      function Executar:boolean; override;
  end;
const
  C_URL = 'https://publica.cnpj.ws/cnpj/';

implementation

uses
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  blcksock,
  Classes,
  synautil, httpsend;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWSPublica.Executar: boolean;
var
  LJSon: TJson;
  LJsonObject : TJsonObject;
  LStream : TStringStream;
  LRetorno : String;
  LJsonArray: TJsonArray;
  I, Z : Integer;
  JsonObjestabelecimento : Jsons.TJsonObject;
  JsonObj2               : Jsons.TJsonObject;
  HTTPSend: THTTPSend;
begin
  inherited Executar;
  HTTPSend := THTTPSend.Create;
  try
    LStream  := TStringStream.Create('');
    try
      HTTPSend.OutputStream := LStream;
      HTTPSend.Clear;
      HTTPSend.Headers.Clear;
      HTTPSend.Headers.Add('Accept: application/json');
{      if FUsuario <> '' then
        HTTPSend.Headers.Add('Authorization: Bearer ' + FUsuario);

      HTTPSend.MimeType := 'application/x-www-form-urlencoded';
}
      HTTPSend.Sock.SSL.SSLType := LT_TLSv1_2;

      HTTPSend.HTTPMethod('GET',C_URL +  OnlyNumber(FCNPJ));

      HTTPSend.Document.Position:= 0;
      LRetorno := ReadStrFromStream(LStream, LStream.Size);

      LJSon := TJson.Create;
      try
        LJSon.Parse('[' + UTF8ToNativeString(LRetorno) + ']');
        for I := 0 to Pred(LJSon.Count) do
        begin
          LJsonObject := LJSon.Get(I).AsObject;
          if LJsonObject.Values['status'].AsString = '' then
          begin
            FResposta.RazaoSocial := LJsonObject.Values['razao_social'].AsString;

            JsonObjestabelecimento := Jsons.TJsonObject.Create;
            JsonObjestabelecimento.Parse( LJsonObject.Values['estabelecimento'].Stringify );
            FResposta.Fantasia             := JsonObjestabelecimento.Values['nome_fantasia'].AsString;
            FResposta.CNPJ                 := JsonObjestabelecimento.Values['cnpj'].AsString;
            FResposta.Abertura             := StringToDateTimeDef(JsonObjestabelecimento.Values['data_inicio_atividade'].AsString,0,'yyyy/mm/dd');
            FResposta.Endereco             := JsonObjestabelecimento.Values['tipo_logradouro'].AsString + ' ' +LJsonObject.Values['logradouro'].AsString;
            FResposta.Numero               := JsonObjestabelecimento.Values['numero'].AsString;
            FResposta.Complemento          := JsonObjestabelecimento.Values['complemento'].AsString;
            FResposta.CEP                  := OnlyNumber( JsonObjestabelecimento.Values['cep'].AsString);
            FResposta.Bairro               := JsonObjestabelecimento.Values['bairro'].AsString;
            FResposta.UF                   := JsonObjestabelecimento.Values['uf'].AsString;
            FResposta.EndEletronico        := JsonObjestabelecimento.Values['email'].AsString;
            FResposta.Telefone             := JsonObjestabelecimento.Values['ddd1'].AsString + JsonObjestabelecimento.Values['telefone1'].AsString;
            FResposta.Situacao             := JsonObjestabelecimento.Values['situacao_cadastral'].AsString;
            FResposta.DataSituacao         := StringToDateTimeDef(JsonObjestabelecimento.Values['data_situacao'].AsString,0,'yyyy/mm/dd');
            FResposta.EmpresaTipo          := JsonObjestabelecimento.Values['tipo'].AsString;

            JsonObj2 := TJsonObject.Create;

            if JsonObjestabelecimento.IsJsonObject( JsonObjestabelecimento.Values['motivo_situacao_cadastral'].Stringify ) then
            begin
               JsonObj2.Parse( JsonObjestabelecimento.Values['motivo_situacao_cadastral'].Stringify );
               FResposta.MotivoSituacaoCad := JsonObj2.Values['id'].AsString + ' ' + JsonObj2.Values['descricao'].AsString;
            end;

            FResposta.SituacaoEspecial     := JsonObjestabelecimento.Values['situacao_especial'].AsString;
            FResposta.DataSituacaoEspecial := StringToDateTimeDef(LJsonObject.Values['data_situacao_especial'].AsString,0,'yyyy/mm/dd');

            JsonObj2.Parse( JsonObjestabelecimento.Values['cidade'].Stringify );
            FResposta.Cidade := JsonObj2.Values['nome'].AsString;

            JsonObj2.Parse( JsonObjestabelecimento.Values['estado'].Stringify );
            FResposta.UF := JsonObj2.Values['sigla'].AsString;

            JsonObj2.Parse( JsonObjestabelecimento.Values['atividade_principal'].Stringify );
            FResposta.CNAE1 := JsonObj2.Values['id'].AsString + ' ' + JsonObj2.Values['descricao'].AsString;

            LJsonArray := JsonObjestabelecimento.Values['atividades_secundarias'].AsArray;
            for Z := 0 to Pred(LJsonArray.Count) do
               FResposta.CNAE2.Add(LJsonArray[Z].AsObject.Values['id'].AsString + ' ' + LJsonArray[Z].AsObject.Values['descricao'].AsString);

            JsonObj2.Parse( LJsonObject.Values['porte'].Stringify );
            FResposta.Porte := JsonObj2.Values['descricao'].AsString;

            JsonObj2.Parse( LJsonObject.Values['natureza_juridica'].Stringify );
            FResposta.NaturezaJuridica     := JsonObj2.Values['descricao'].AsString;
            FResposta.EFR                  := '';

            if JsonObjestabelecimento.Values['inscricoes_estaduais'].AsArray.Count >0 then
            begin
               LJsonArray := JsonObjestabelecimento.Values['inscricoes_estaduais'].AsArray;
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
    finally
      LStream.free;
    end;
  finally
    HTTPSend.Free;
  end;
end;

end.
>>>>>>> .r32147
