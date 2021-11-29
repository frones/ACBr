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

unit ACBrPIXSchemasDevedor;

interface

uses
  Classes, SysUtils,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr,
  {$Else}
   Jsons,
  {$EndIf}
  ACBrPIXBase;

type

  { TACBrPIXDevedorBase }

  TACBrPIXDevedorBase = class(TACBrPIXSchema)
  private
    fcnpj: String;
    fcpf: String;
    fnome: String;
    fcep: String;
    fcidade: String;
    femail: String;
    flogradouro: String;
    fuf: String;
    procedure SetCnpj(AValue: String);
    procedure SetCpf(AValue: String);
    procedure SetCEP(AValue: String);
    procedure SetCidade(AValue: String);
    procedure SetEmail(AValue: String);
    procedure SetLogradouro(AValue: String);
    procedure SetUF(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;

    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property nome: String read fnome write fnome;
    property email: String read femail write SetEmail;
    property logradouro: String read flogradouro write SetLogradouro;
    property cidade: String read fcidade write SetCidade;
    property uf: String read fuf write SetUF;
    property cep: String read fcep write SetCEP;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXDevedorBase);
  end;


  TACBrPIXDevedor = class(TACBrPIXDevedorBase)
  public
    property cpf;
    property cnpj;
    property nome;
  end;

  TACBrPIXDadosDevedor = class(TACBrPIXDevedor)
  public
    property email;
    property logradouro;
    property cidade;
    property uf;
    property cep;
  end;

implementation

uses
  ACBrValidador, ACBrUtil;

{ TACBrPIXDevedorBase }

constructor TACBrPIXDevedorBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXDevedorBase.Clear;
begin
  fcpf := '';
  fcnpj := '';
  fnome := '';
  femail := '';
  fcep := '';
  fcidade := '';
  flogradouro := '';
  fuf := '';
end;

function TACBrPIXDevedorBase.IsEmpty: Boolean;
begin
  Result := (fcpf = '') and
            (fcnpj = '') and
            (fnome = '') and
            (femail = '') and
            (fcep = '') and
            (fcidade = '') and
            (flogradouro = '') and
            (fuf = '');
end;

procedure TACBrPIXDevedorBase.Assign(Source: TACBrPIXDevedorBase);
begin
  fcpf := Source.cpf;
  fcnpj := Source.cnpj;
  fnome := Source.nome;
  femail := Source.email;
  fcep := Source.cep;
  fcidade := Source.cidade;
  flogradouro := Source.logradouro;
  fuf := Source.uf;
end;

procedure TACBrPIXDevedorBase.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (cnpj <> '') then
     AJSon.S['cnpj'] := cnpj
   else if (cpf <> '') then
     AJSon.S['cpf'] := cpf;
   if (nome <> '') then
     AJSon.S['nome'] := nome;
   if (femail <> '') then
     AJSon.S['email'] := femail;
   if (flogradouro <> '') then
     AJSon.S['logradouro'] := flogradouro;
   if (fcidade <> '') then
     AJSon.S['cidade'] := fcidade;
   if (fcep <> '') then
     AJSon.S['cep'] := fcep;
   if (fuf <> '') then
     AJSon.S['uf'] := fuf;
  {$Else}
   if (cnpj <> '') then
     AJSon['cnpj'].AsString := cnpj
   else if (cpf <> '') then
     AJSon['cpf'].AsString := cpf;
   if (nome <> '') then
     AJSon['nome'].AsString := nome;
   if (femail <> '') then
     AJSon['email'].AsString := femail;
   if (flogradouro <> '') then
     AJSon['logradouro'].AsString := flogradouro;
   if (fcidade <> '') then
     AJSon['cidade'].AsString := fcidade;
   if (fcep <> '') then
     AJSon['cep'].AsString := fcep;
   if (fuf <> '') then
     AJSon['uf'].AsString := fuf;
  {$EndIf}
end;

procedure TACBrPIXDevedorBase.DoReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   cnpj := AJSon.S['cnpj'];
   cpf  := AJSon.S['cpf'];
   nome := AJSon.S['nome'];
   femail := AJSon.S['email'];
   flogradouro := AJSon.S['logradouro'];
   fcidade := AJSon.S['cidade'];
   fcep := AJSon.S['cep'];
   fuf := AJSon.S['uf'];
  {$Else}
   cnpj := AJSon['cnpj'].AsString;
   cpf  := AJSon['cpf'].AsString;
   nome := AJSon['nome'].AsString;
   femail := AJSon['email'].AsString;
   flogradouro := AJSon['logradouro'].AsString;
   fcidade := AJSon['cidade'].AsString;
   fcep := AJSon['cep'].AsString;
   fuf := AJSon['uf'].AsString;
  {$EndIf}
end;

procedure TACBrPIXDevedorBase.SetCnpj(AValue: String);
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
    fcpf := '';
  end;

  fcnpj := s;
end;

procedure TACBrPIXDevedorBase.SetCpf(AValue: String);
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
    fcnpj := '';
  end;

  fcpf := s;
end;

procedure TACBrPIXDevedorBase.SetCEP(AValue: String);
var
  s: String;
begin
  if fcep = AValue then
    Exit;

  s := OnlyNumber(AValue);
  if (s <> '') then
    s := PadLeft(s, 8, '0') ;

  fcep := s;
end;

procedure TACBrPIXDevedorBase.SetCidade(AValue: String);
begin
  if fcidade = AValue then
    Exit;
  fcidade := copy(AValue, 1, 200);
end;

procedure TACBrPIXDevedorBase.SetEmail(AValue: String);
var
  s, e: String;
begin
  if femail = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') then
  begin
    e := ValidarEmail(s);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  femail := s;
end;

procedure TACBrPIXDevedorBase.SetLogradouro(AValue: String);
begin
  if flogradouro = AValue then
    Exit;
  flogradouro := copy(AValue, 1, 200);
end;

procedure TACBrPIXDevedorBase.SetUF(AValue: String);
var
  s, e: String;
begin
  if fuf = AValue then
    Exit;

  s := UpperCase(Trim(AValue));
  if (s <> '') then
  begin
    e := ValidarUF(s);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fuf := s;
end;

end.

