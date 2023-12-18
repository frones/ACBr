unit ACBrConsultaCNPJ.WS.BrasilAPI;

interface
uses
  ACBrJSON, Jsons, SysUtils, ACBrConsultaCNPJ.WS;
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
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  blcksock,
  System.Classes,
  synautil, httpsend;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWSBrasilAPI.Executar: boolean;
var
  LJSon: TJson;
  LJsonObject : TJsonObject;
  LStream : TStringStream;
  LRetorno : String;
  LJsonArray: TJsonArray;
  I, Z : Integer;
begin
  inherited;
  HTTPSend := THTTPSend.Create;
  LStream  := TStringStream.Create('');
  try
    HTTPSend.OutputStream := LStream;
    HTTPSend.Clear;
    HTTPSend.Headers.Clear;
    HTTPSend.Headers.Add('Accept: application/json');

    HTTPSend.MimeType := 'application/json';

    HTTPSend.Sock.SSL.SSLType := LT_TLSv1_2;

    HTTPSend.HTTPMethod('GET',C_URL +  OnlyNumber(FCNPJ));

    HTTPSend.Document.Position:= 0;
    LRetorno := ReadStrFromStream(LStream, LStream.Size);

    LJSon := TJson.Create;
    LJSon.Parse('[' + UTF8ToNativeString(LRetorno) + ']');
    for I := 0 to Pred(LJSon.Count) do
    begin
      LJsonObject := LJSon.Get(I).AsObject;
      if HTTPSend.ResultCode = 200 then
      begin
        FResposta.RazaoSocial          := ACBrStr(LJsonObject.Values['razao_social'].AsString);
        FResposta.CNPJ                 := ACBrStr(LJsonObject.Values['cnpj'].AsString);
        FResposta.Fantasia             := ACBrStr(LJsonObject.Values['nome_fantasia'].AsString);
        FResposta.Abertura             := StringToDateTimeDef(LJsonObject.Values['data_inicio_atividade'].AsString,0,'yyyy/mm/dd');
        FResposta.Porte                := ACBrStr(LJsonObject.Values['descricao_porte'].AsString);

        FResposta.CNAE1                := ACBrStr(IntToStr(LJsonObject.Values['cnae_fiscal'].AsInteger) + ' ' + LJsonObject.Values['cnae_fiscal_descricao'].AsString);

        LJsonArray := LJsonObject.Values['cnaes_secundarias'].AsArray;
        for Z := 0 to Pred(LJsonArray.Count) do
          FResposta.CNAE2.Add(ACBrStr(IntToStr(LJsonArray[Z].AsObject.Values['codigo'].AsInteger) + ' ' + LJsonArray[Z].AsObject.Values['descricao'].AsString));

        FResposta.EmpresaTipo          := ACBrStr(LJsonObject.Values['descricao_matriz_filial'].AsString);
        FResposta.Endereco             := ACBrStr(LJsonObject.Values['logradouro'].AsString);
        FResposta.Numero               := ACBrStr(LJsonObject.Values['numero'].AsString);
        FResposta.Complemento          := ACBrStr(LJsonObject.Values['complemento'].AsString);
        FResposta.CEP                  := IntToStr( LJsonObject.Values['cep'].AsInteger);
        FResposta.Bairro               := ACBrStr(LJsonObject.Values['bairro'].AsString);
        FResposta.Cidade               := ACBrStr(LJsonObject.Values['municipio'].AsString);
        FResposta.CodigoIBGE           := IntToStr(LJsonObject.Values['codigo_municipio'].AsInteger);
        FResposta.UF                   := ACBrStr(LJsonObject.Values['uf'].AsString);
        FResposta.Situacao             := ACBrStr(LJsonObject.Values['descricao_situacao_cadastral'].AsString);
        FResposta.SituacaoEspecial     := ACBrStr(LJsonObject.Values['situacao_especial'].AsString);
        FResposta.DataSituacao         := StringToDateTimeDef(LJsonObject.Values['data_situacao_cadastral'].AsString,0,'yyyy/mm/dd');
        FResposta.DataSituacaoEspecial := StringToDateTimeDef(LJsonObject.Values['data_situacao_especial'].AsString,0,'yyyy/mm/dd');
        FResposta.NaturezaJuridica     := IntToStr(LJsonObject.Values['codigo_natureza_juridica'].AsInteger);
        FResposta.EndEletronico        := ACBrStr(LJsonObject.Values[''].AsString);
        FResposta.Telefone             := ACBrStr(LJsonObject.Values['ddd_telefone_1'].AsString);
        FResposta.EFR                  := '';

        FResposta.MotivoSituacaoCad    := ACBrStr(LJsonObject.Values['motivo_situacao_cadastral'].AsString);

        Result := true;
      end;
    end;
  finally
    LJSon.Free;
    HTTPSend.Free;
  end;
end;

end.
