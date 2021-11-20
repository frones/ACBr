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
  Classes, SysUtils,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf},
  ACBrPIXSchemasPaginacao, ACBrPIXBase;

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
  public
    constructor Create;
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

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;


implementation

uses
  ACBrUtil, ACBrValidador,
  ACBrPIXUtil;

{ TACBrPIXParametrosConsultaPix }

constructor TACBrPIXParametrosConsultaPix.Create;
begin
  inherited;
  fpaginacao := TACBrPIXPaginacao.Create;
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

procedure TACBrPIXParametrosConsultaPix.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['inicio'] := DateTimeToIso8601( finicio );
   AJSon.S['fim'] := DateTimeToIso8601( ffim );
   AJSon.S['txid'] := ftxid;
   AJSon.B['txIdPresente'] := ftxIdPresente;
   AJSon.B['devolucaoPresente'] := fdevolucaoPresente;
   AJSon.S['cpf'] := fcpf;
   AJSon.S['cnpj'] := fcnpj;
   fpaginacao.WriteToJSon(AJSon.O['paginacao']);
  {$Else}
   AJSon['inicio'].AsString := DateTimeToIso8601( finicio );
   AJSon['fim'].AsString := DateTimeToIso8601( ffim );
   AJSon['txid'].AsString := ftxid;
   AJSon['txIdPresente'].AsBoolean := ftxIdPresente;
   AJSon['devolucaoPresente'].AsBoolean := fdevolucaoPresente;
   AJSon['cpf'].AsString := fcpf;
   AJSon['cnpj'].AsString := fcnpj;
   fpaginacao.WriteToJSon(AJSon['paginacao'].AsObject);
  {$EndIf}
end;

procedure TACBrPIXParametrosConsultaPix.ReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   finicio := Iso8601ToDateTime( AJSon.S['inicio'] );
   ffim := Iso8601ToDateTime( AJSon.S['fim'] );
   ftxid := AJSon.S['txid'];
   ftxIdPresente := AJSon.B['txIdPresente'];
   fdevolucaoPresente := AJSon.B['devolucaoPresente'];
   fcpf := AJSon.S['cpf'];
   fcnpj := AJSon.S['cnpj'];
   fpaginacao.ReadFromJSon(AJSon.O['paginacao']);
  {$Else}
   finicio := Iso8601ToDateTime( AJSon['inicio'].AsString );
   ffim := Iso8601ToDateTime( AJSon['fim'].AsString );
   ftxid := AJSon['txid'].AsString;
   ftxIdPresente := AJSon['txIdPresente'].AsBoolean;
   fdevolucaoPresente := AJSon['devolucaoPresente'].AsBoolean;
   fcpf := AJSon['cpf'].AsString;
   fcnpj := AJSon['cnpj'].AsString;
   fpaginacao.ReadFromJSon(AJSon['paginacao'].AsObject);
  {$EndIf}
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
  if (s <> '') then
  begin
    e := ValidarTxId(s, 35, 1);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

end.

