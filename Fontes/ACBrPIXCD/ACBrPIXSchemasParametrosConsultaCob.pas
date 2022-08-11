{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrPIXSchemasParametrosConsultaCob;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXSchemasPaginacao, ACBrPIXBase;

type

  { TACBrPIXParametrosConsultaCob }

  TACBrPIXParametrosConsultaCob = class(TACBrPIXSchema)
  private
    fcnpj: String;
    fcpf: String;
    flocationPresente: Boolean;
    ffim: TDateTime;
    finicio: TDateTime;
    fpaginacao: TACBrPIXPaginacao;
    fstatus: String;
  private
    procedure SetCnpj(AValue: String);
    procedure SetCpf(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXParametrosConsultaCob);

    property inicio: TDateTime read finicio write finicio;
    property fim: TDateTime read ffim write ffim;
    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property locationPresente: Boolean read flocationPresente write flocationPresente;
    property status: String read fstatus write fstatus;
    property paginacao: TACBrPIXPaginacao read fpaginacao;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrValidador;

{ TACBrPIXParametrosConsultaCob }

constructor TACBrPIXParametrosConsultaCob.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fpaginacao := TACBrPIXPaginacao.Create('paginacao');
  Clear;
end;

destructor TACBrPIXParametrosConsultaCob.Destroy;
begin
  fpaginacao.Free;
  inherited Destroy;
end;

procedure TACBrPIXParametrosConsultaCob.Clear;
begin
  fcnpj := '';
  fcpf := '';
  fstatus := '';
  ffim := 0;
  finicio := 0;
  flocationPresente := False;
  fpaginacao.Clear
end;

function TACBrPIXParametrosConsultaCob.IsEmpty: Boolean;
begin
  Result := (fcnpj = '') and
            (fcpf = '') and
            (fstatus = '') and
            (ffim = 0) and
            (finicio = 0) and   //flocationPresente := False;
            fpaginacao.IsEmpty;
end;

procedure TACBrPIXParametrosConsultaCob.Assign(Source: TACBrPIXParametrosConsultaCob);
begin
  fcnpj := Source.cnpj;
  fcpf := Source.cpf;
  fstatus := Source.status;
  ffim := Source.fim;
  finicio := Source.inicio;
  flocationPresente := Source.locationPresente;
  fpaginacao.Assign(Source.paginacao);
end;

procedure TACBrPIXParametrosConsultaCob.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPairISODateTime('inicio', finicio)
    .AddPairISODateTime('fim', ffim)
    .AddPair('cpf', fcpf, False)
    .AddPair('cnpj', fcnpj, False)
    .AddPair('locationPresente', flocationPresente)
    .AddPair('status', fstatus);
  fpaginacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXParametrosConsultaCob.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .ValueISODateTime('inicio', finicio)
    .ValueISODateTime('fim', ffim)
    .Value('cpf', fcpf)
    .Value('cnpj', fcnpj)
    .Value('locationPresente', flocationPresente)
    .Value('status', fstatus);
  fpaginacao.ReadFromJSon(AJSon);
end;

procedure TACBrPIXParametrosConsultaCob.SetCnpj(AValue: String);
var
  s, e: String;
begin
  if fcnpj = AValue then
    Exit;

  s := OnlyNumber(AValue);
  if (s <> '') then
  begin
    e := ValidarCNPJ(s);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fcnpj := s;
end;

procedure TACBrPIXParametrosConsultaCob.SetCpf(AValue: String);
var
  s, e: String;
begin
  if fcpf = AValue then
    Exit;

  s := OnlyNumber(AValue);
  if (s <> '') then
  begin
    e := ValidarCPF(s);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fcpf := s;
end;

end.

