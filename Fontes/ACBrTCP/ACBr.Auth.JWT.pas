{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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
unit ACBr.Auth.JWT;

interface
uses
  Classes;

type
  TACBrJWTAuth = class
    FSecretKey: String;
  private

    function HMACSHA256(const AData, AKey: String): String;
  public
    constructor Create(const ASecretKey: String);
    function GenerateJWT(const APayload: String): String;
    function Signature(const APayload : String) : String;
    function ValidateJWT(const AToken: String): Boolean;
    function Base64UrlEncode(const AInput: String; AEncodeBase64 : boolean = true): String;
  end;

implementation

uses
  SysUtils,
  synacode,
  ACBrBase,
  ACBrUtil.Strings,
  ACBrOpenSSLUtils;

{ TACBrJWTAuth }

constructor TACBrJWTAuth.Create(const ASecretKey: String);
begin
  FSecretKey := ASecretKey;
end;

function TACBrJWTAuth.HMACSHA256(const AData, AKey: String): String;
var LACBrSSLUtils : TACBrOpenSSLUtils;
begin
  LACBrSSLUtils := TACBrOpenSSLUtils.Create(nil);
  try
    Result := LACBrSSLUtils.HMACFromString(AData, AKey, algSHA256);
  finally
    LACBrSSLUtils.Free;
  end;
end;

function TACBrJWTAuth.Signature(const APayload: String): String;
var LACBrOpenSSLUtils : TACBrOpenSSLUtils;
begin
  LACBrOpenSSLUtils := TACBrOpenSSLUtils.Create(nil);
  try
    LACBrOpenSSLUtils.LoadPrivateKeyFromString(FSecretKey);
    Result := Base64UrlEncode(LACBrOpenSSLUtils.CalcHashFromString(APayload, algSHA256, sttBase64, True), False);
  finally
    LACBrOpenSSLUtils.Free;
  end;
end;

function TACBrJWTAuth.Base64UrlEncode(const AInput: String; AEncodeBase64 : boolean = true): String;
begin
  Result := AInput;
  if AEncodeBase64 then
    Result := synacode.EncodeBase64(Result);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]); // Remove CR
  Result := StringReplace(Result, #10, '', [rfReplaceAll]); // Remove LF
  Result := StringReplace(Result, ' ', '', [rfReplaceAll]); // Remove espaços
end;

function TACBrJWTAuth.GenerateJWT(const APayload: String): String;
var
  LHeader, LToken: String;

begin
  LHeader := '{"alg": "RS256","typ": "JWT"}';
  LToken := Base64UrlEncode(AnsiString(LHeader)) + '.' + Base64UrlEncode(AnsiString(APayload));
  Result := LToken + '.' + Signature(LToken);
end;

function TACBrJWTAuth.ValidateJWT(const AToken: String): Boolean;
var
  LHeaderPayload, LSignature, LExpectedSignature: String;
  LPosDelimiter: Integer;
begin

  LPosDelimiter := PosLast('.', AToken);
  LHeaderPayload := Copy(AToken, 1, LPosDelimiter - 1);
  LSignature := Copy(AToken, LPosDelimiter + 1, Length(AToken) - LPosDelimiter);
  LExpectedSignature := synacode.EncodeBase64(HMACSHA256(LHeaderPayload, FSecretKey));

  Result := LExpectedSignature = LSignature;
end;


end.
