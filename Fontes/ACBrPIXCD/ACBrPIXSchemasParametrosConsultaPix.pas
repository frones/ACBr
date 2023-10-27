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

unit ACBrPIXSchemasParametrosConsultaPix;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXSchemasPaginacao, ACBrPIXBase;

type

  { TACBrPIXParametrosConsultaPix }

  TACBrPIXParametrosConsultaPix = class(TACBrPIXSchema)
  private
    fcnpj: String;
    fcpf: String;
    fdevolucaoPresente: Boolean;
    ffim: TDateTime;
    finicio: TDateTime;
    fpaginacao: TACBrPIXPaginacao;
    ftxid: String;
    ftxIdPresente: Boolean;
  private
    procedure SetCnpj(AValue: String);
    procedure SetCpf(AValue: String);
    procedure SetTxid(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXParametrosConsultaPix);

    property inicio: TDateTime read finicio write finicio;
    property fim: TDateTime read ffim write ffim;
    property txid: String read ftxid write SetTxid;
    property txIdPresente: Boolean read ftxIdPresente write ftxIdPresente;
    property devolucaoPresente: Boolean read fdevolucaoPresente write fdevolucaoPresente;
    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property paginacao: TACBrPIXPaginacao read fpaginacao;
  end;


implementation

uses
  ACBrUtil.Strings,
  ACBrValidador,
  ACBrPIXUtil;

{ TACBrPIXParametrosConsultaPix }

constructor TACBrPIXParametrosConsultaPix.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fpaginacao := TACBrPIXPaginacao.Create('paginacao');
  Clear;
end;

destructor TACBrPIXParametrosConsultaPix.Destroy;
begin
  fpaginacao.Free;
  inherited Destroy;
end;

procedure TACBrPIXParametrosConsultaPix.Clear;
begin
  fcnpj := '';
  fcpf := '';
  ftxid := '';
  ffim := 0;
  finicio := 0;
  fdevolucaoPresente := False;
  ftxIdPresente := False;
  fpaginacao.Clear
end;

procedure TACBrPIXParametrosConsultaPix.Assign(Source: TACBrPIXParametrosConsultaPix);
begin
  finicio := Source.inicio;
  ffim := Source.fim;
  ftxid := Source.txid;
  ftxIdPresente := Source.txIdPresente;
  fdevolucaoPresente := Source.devolucaoPresente;
  fcpf := Source.cpf;
  fcnpj := Source.cnpj;
  fpaginacao.Assign(Source.paginacao);
end;

procedure TACBrPIXParametrosConsultaPix.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPairISODateTime('inicio', finicio)
    .AddPairISODateTime('fim', ffim)
    .AddPair('txid', ftxid, False)
    .AddPair('txIdPresente', ftxIdPresente)
    .AddPair('devolucaoPresente', fdevolucaoPresente)
    .AddPair('cpf', fcpf, False)
    .AddPair('cnpj', fcnpj, False);
  fpaginacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXParametrosConsultaPix.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .ValueISODateTime('inicio', finicio)
    .ValueISODateTime('fim', ffim)
    .Value('txid', ftxid)
    .Value('txIdPresente', ftxIdPresente)
    .Value('devolucaoPresente', fdevolucaoPresente)
    .Value('cpf', fcpf)
    .Value('cnpj', fcnpj);
  fpaginacao.ReadFromJSon(AJSon);
end;

procedure TACBrPIXParametrosConsultaPix.SetCnpj(AValue: String);
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

procedure TACBrPIXParametrosConsultaPix.SetCpf(AValue: String);
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

procedure TACBrPIXParametrosConsultaPix.SetTxid(AValue: String);
var
  s, e: String;
begin
  if ftxid = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') and fIsBacen then
  begin
    e := ValidarTxId(s, 35, 1);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

end.

