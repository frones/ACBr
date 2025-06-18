{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXSchemasRecParticipantes;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXBase;

type

  { TACBrPIXRecParticipante }

  TACBrPIXRecParticipante = class(TACBrPIXSchema)
  private
    fagencia: String;
    fcnpj: String;
    fcodMun: String;
    fconta: String;
    fconvenio: String;
    fcpf: String;
    fispbParticipante: String;
    fnome: String;
    ftipoConta: TACBrPIXTipoConta;
    procedure SetAgencia(AValue: String);
    procedure SetCnpj(aValue: String);
    procedure SetCodMun(AValue: String);
    procedure SetConta(AValue: String);
    procedure SetConvenio(AValue: String);
    procedure SetCpf(aValue: String);
    procedure SetIspbParticipante(AValue: String);
    procedure SetNome(aValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property nome: String read fnome write SetNome;
    property conta: String read fconta write SetConta;
    property codMun: String read fcodMun write SetCodMun;
    property agencia: String read fagencia write SetAgencia;
    property convenio: String read fconvenio write SetConvenio;
    property tipoConta: TACBrPIXTipoConta read ftipoConta write ftipoConta;
    property ispbParticipante: String read fispbParticipante write SetIspbParticipante;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecParticipante); virtual;
  end;

  { TACBrPIXRecDevedor }

  TACBrPIXRecDevedor = class(TACBrPIXRecParticipante)
  public
    property cpf;
    property cnpj;
    property nome;
  end;

  { TACBrPIXRecRecebedor }

  TACBrPIXRecRecebedor = class(TACBrPIXRecParticipante)
  public
    property cpf;
    property cnpj;
    property nome;
    property convenio;
  end;

  { TACBrPIXRecSolicRecebedor }

  TACBrPIXRecSolicRecebedor = class(TACBrPIXRecParticipante)
  public
    property convenio;
  end;

  { TACBrPIXRecPagador }

  TACBrPIXRecPagador = class(TACBrPIXRecParticipante)
  public
    property cpf;
    property cnpj;
    property nome;
    property codMun;
    property ispbParticipante;
  end;

  { TACBrPIXRecDestinatario }

  TACBrPIXRecDestinatario = class(TACBrPIXRecParticipante)
  public
    property cpf;
    property cnpj;
    property conta;
    property agencia;
    property ispbParticipante;
  end;

  { TACBrPIXCobRRecebedor }

  TACBrPIXCobRRecebedor = class(TACBrPIXRecParticipante)
  public
    property conta;
    property agencia;
    property tipoConta;
  end;

implementation

uses
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

{ TACBrPIXRecParticipante }

procedure TACBrPIXRecParticipante.SetCnpj(aValue: String);
begin
  if (Length(aValue) <> 14) then
    EACBrPixException.CreateFmt(ACBrStr(sErroTamanhoCampo), ['CNPJ', 14]);
  fcnpj := aValue;
end;

procedure TACBrPIXRecParticipante.SetAgencia(AValue: String);
begin
  if fagencia = AValue then Exit; 
  fagencia := Copy(AValue, 1, 4);
end;

procedure TACBrPIXRecParticipante.SetCodMun(AValue: String);
begin
  if (Length(aValue) <> 7) then
    EACBrPixException.CreateFmt(ACBrStr(sErroTamanhoCampo), ['CodMun', 7]);
  fcodMun := AValue;
end;

procedure TACBrPIXRecParticipante.SetConta(AValue: String);
begin
  if fconta = AValue then Exit;
  fconta := Copy(AValue, 1, 20);
end;

procedure TACBrPIXRecParticipante.SetConvenio(AValue: String);
begin
  if fconvenio = AValue then Exit;
  fconvenio := Copy(AValue, 1, 60);
end;

procedure TACBrPIXRecParticipante.SetCpf(aValue: String);
begin
  if (Length(aValue) <> 11) then
    EACBrPixException.CreateFmt(ACBrStr(sErroTamanhoCampo), ['CPF', 11]);
  fcpf := aValue;
end;

procedure TACBrPIXRecParticipante.SetIspbParticipante(AValue: String);
begin
  if (Length(aValue) <> 8) then
    EACBrPixException.CreateFmt(ACBrStr(sErroTamanhoCampo), ['IspbParticipante', 8]);
  fispbParticipante := AValue;
end;

procedure TACBrPIXRecParticipante.SetNome(aValue: String);
begin
  if (aValue = fnome) then Exit;
  fnome := Copy(aValue, 1, 140);
end;

procedure TACBrPIXRecParticipante.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('cpf', fcpf, False)
    .AddPair('cnpj', fcnpj, False)
    .AddPair('nome', fnome, False)
    .AddPair('convenio', fconvenio, False)
    .AddPair('codMun', fcodMun, False)
    .AddPair('ispbParticipante', fispbParticipante, False)
    .AddPair('agencia', fagencia, False)
    .AddPair('conta', fconta, False)
    .AddPair('tipoConta', PIXTipoContaToString(ftipoConta), False);
end;

procedure TACBrPIXRecParticipante.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wTipoConta: String;
begin
  {$IFDEF FPC}
  wTipoConta := EmptyStr;
  {$ENDIF}
  AJSon
    .Value('cpf', fcpf)
    .Value('cnpj', fcnpj)
    .Value('nome', fnome)
    .Value('convenio', fconvenio)
    .Value('codMun', fcodMun)
    .Value('ispbParticipante', fispbParticipante)
    .Value('agencia', fagencia)
    .Value('conta', fconta)
    .Value('tipoConta', wTipoConta);

  if NaoEstaVazio(wTipoConta) then
    ftipoConta := StringToPIXTipoConta(wTipoConta);
end;

constructor TACBrPIXRecParticipante.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXRecParticipante.Clear;
begin
  fcpf := EmptyStr;
  fcnpj := EmptyStr;
  fnome := EmptyStr;
  fconvenio := EmptyStr;
  fcodMun := EmptyStr;
  fispbParticipante := EmptyStr;
  fagencia := EmptyStr;
  fconta := EmptyStr;
  ftipoConta := ptcNENHUM;
end;

function TACBrPIXRecParticipante.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcpf) and
            EstaVazio(fcnpj) and
            EstaVazio(fnome) and
            EstaVazio(fconvenio) and
            EstaVazio(fcodMun) and
            EstaVazio(fispbParticipante) and
            EstaVazio(fagencia) and
            EstaVazio(fconta) and
            (ftipoConta = ptcNENHUM);
end;

procedure TACBrPIXRecParticipante.Assign(Source: TACBrPIXRecParticipante);
begin
  Clear;
  if not Assigned(Source) then
    Exit;
  fcpf := Source.fcpf;
  fcnpj := Source.fcnpj;
  fnome := Source.fnome;
  fconvenio := Source.fconvenio;
  fcodMun := Source.fcodMun;
  fispbParticipante := Source.fispbParticipante;
  fagencia := Source.fagencia;
  fconta := Source.fconta;
  ftipoConta := Source.ftipoConta;
end;

end.

