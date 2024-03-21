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
unit ACBrConsultaCNPJ.WS;

interface
uses
  ACBrJSON,
  SysUtils,
  ACBrValidador,
  httpsend,
  Classes;
type
  TParams =
    record  prName,PrValue:String;
  end;
  EACBrConsultaCNPJWSException = class ( Exception );
  TACBrConsultaCNPJWSResposta = class (TObject)
    NaturezaJuridica     : String ;
    EmpresaTipo          : String;
    Abertura             : TDateTime;
    RazaoSocial          : String;
    Fantasia             : String;
    Porte                : String;
    CNAE1                : String;
    CNAE2                : TStringList;
    Endereco             : String;
    Numero               : String;
    Complemento          : String;
    CEP                  : String;
    Bairro               : String;
    Cidade               : String;
    UF                   : String;
    Situacao             : String;
    SituacaoEspecial     : String;
    CNPJ                 : String;
    DataSituacao         : TDateTime;
    DataSituacaoEspecial : TDateTime;
    EndEletronico        : String;
    Telefone             : String;
    EFR                  : string;
    MotivoSituacaoCad    : string;
    CodigoIBGE           : String;
    InscricaoEstadual    : String;
  end;
  { TACBrConsultaCNPJWS }
  TACBrConsultaCNPJWS = class( TObject )
    FCNPJ : string;
    FUsuario : String;
    FSenha : String;
    FResposta : TACBrConsultaCNPJWSResposta;
    FHeaderParamsList : Array of TParams;
    FDefasagemMaxima : Integer;
    FProxyHost: String;
    FProxyPort: String;
    FProxyUser: String;
    FProxyPass: String;
    private
    FHTTPSend: THTTPSend;
    FResultString : String;
    public
      constructor Create(const ACNPJ : string; const AUsuario : string = ''; const ASenha: string = ''; const ADefasagemMaxima : Integer = 0);
      destructor Destroy; override;
      function Executar : boolean; virtual;
      function SendHttp(const AMethod : string; const AURL : String; out LRetorno : String):Integer;
      function AddHeaderParam(const AParamName, AParamValue : String) : TACBrConsultaCNPJWS;
      procedure ClearHeaderParams;

      property ProxyHost: String read FProxyHost;
      property ProxyPort: String read FProxyPort;
      property ProxyUser: String read FProxyUser;
      property ProxyPass: String read FProxyPass;
      property ResultString: String read FResultString;

  end;
implementation

uses
  blcksock,
  synautil,
  ACBrUtil.XMLHTML;

{ TACBrConsultaCNPJWS }

function TACBrConsultaCNPJWS.AddHeaderParam(const AParamName, AParamValue: String): TACBrConsultaCNPJWS;
begin
  Result := Self;
  SetLength(FHeaderParamsList,Length(FHeaderParamsList)+1);
  FHeaderParamsList[Length(FHeaderParamsList)-1].prName  := AParamName;
  FHeaderParamsList[Length(FHeaderParamsList)-1].prValue := AParamValue;
end;

procedure TACBrConsultaCNPJWS.ClearHeaderParams;
begin
  SetLength(FHeaderParamsList,0);
end;

constructor TACBrConsultaCNPJWS.Create(const ACNPJ : string; const AUsuario : string = ''; const ASenha: string = ''; const ADefasagemMaxima : Integer = 0);
begin
  FCNPJ     := ACNPJ;
  FUsuario  := AUsuario;
  FSenha    := ASenha;
  FResposta := TACBrConsultaCNPJWSResposta.Create;
  FResposta.CNAE2     := TStringList.Create;
  FDefasagemMaxima := ADefasagemMaxima;
end;

destructor TACBrConsultaCNPJWS.Destroy;
begin
  FResposta.CNAE2.Free;
  FResposta.Free;
  inherited;
end;

function TACBrConsultaCNPJWS.Executar: boolean;
var LErro : String;
begin
  Result := False;
  LErro := ValidarCNPJ( FCNPJ ) ;
  if LErro <> '' then
    raise EACBrConsultaCNPJWSException.Create(LErro);
end;

function TACBrConsultaCNPJWS.SendHttp(const AMethod: string; const AURL: String; out LRetorno: String): Integer;
var
  LStream : TStringStream;
  LHeaders : TStringList;
  I : Integer;
begin
  FHTTPSend := THTTPSend.Create;
  LStream  := TStringStream.Create('');
  try
    FHTTPSend.Clear;

    FHTTPSend.ProxyHost := ProxyHost;
    FHTTPSend.ProxyPort := ProxyPort;
    FHTTPSend.ProxyUser := ProxyUser;
    FHTTPSend.ProxyPass := ProxyPass;
    FHTTPSend.OutputStream := LStream;

    FHTTPSend.Headers.Clear;
    FHTTPSend.Headers.Add('Accept: application/json');

    LHeaders := TStringList.Create;
    try
      for I := 0  to Length(FHeaderParamsList) -1 do
        LHeaders.Add(FHeaderParamsList[I].prName+': '+FHeaderParamsList[I].prValue);
      FHTTPSend.Headers.AddStrings(LHeaders);
    finally
      LHeaders.Free;
    end;

    FHTTPSend.Sock.SSL.SSLType := LT_TLSv1_2;

    FHTTPSend.HTTPMethod(AMethod, AURL);

    FHTTPSend.Document.Position:= 0;

    LRetorno := ReadStrFromStream(LStream, LStream.Size);

    Result := FHTTPSend.ResultCode;
    FResultString := ParseText( FHTTPSend.ResultString );
    if (Result >= 300) then
      FResultString := FResultString +' '+ FHTTPSend.Sock.LastErrorDesc;

  finally
    LStream.Free;
    FHTTPSend.Free;
  end;
end;

end.
