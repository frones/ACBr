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

  { TACBrPIXDevedorRecebedorBase }

  TACBrPIXDevedorRecebedorBase = class(TACBrPIXSchema)
  private
    fcnpj: String;
    fcpf: String;
    fnome: String;
    fcep: String;
    fcidade: String;
    femail: String;
    flogradouro: String;
    fnomeFantasia: String;
    fuf: String;
    procedure SetCnpj(AValue: String);
    procedure SetCpf(AValue: String);
    procedure SetCEP(AValue: String);
    procedure SetCidade(AValue: String);
    procedure SetEmail(AValue: String);
    procedure SetLogradouro(AValue: String);
    procedure SetNome(AValue: String);
    procedure SetNomeFantasia(AValue: String);
    procedure SetUF(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;

    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property nome: String read fnome write SetNome;
    property nomeFantasia: String read fnomeFantasia write SetNomeFantasia;
    property email: String read femail write SetEmail;
    property logradouro: String read flogradouro write SetLogradouro;
    property cidade: String read fcidade write SetCidade;
    property uf: String read fuf write SetUF;
    property cep: String read fcep write SetCEP;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXDevedorRecebedorBase);
  end;


  TACBrPIXDevedor = class(TACBrPIXDevedorRecebedorBase)
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

  TACBrPIXDadosRecebedor = class(TACBrPIXDevedor)
  public
    property nomeFantasia;
    property logradouro;
    property cidade;
    property uf;
    property cep;
  end;

implementation

uses
  ACBrValidador,
  ACBrUtil.Strings;

{ TACBrPIXDevedorRecebedorBase }

constructor TACBrPIXDevedorRecebedorBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXDevedorRecebedorBase.Clear;
begin
  fcpf := '';
  fcnpj := '';
  fnome := '';
  fnomeFantasia := '';
  femail := '';
  fcep := '';
  fcidade := '';
  flogradouro := '';
  fuf := '';
end;

function TACBrPIXDevedorRecebedorBase.IsEmpty: Boolean;
begin
  Result := (fcpf = '') and
            (fcnpj = '') and
            (fnome = '') and
            (fnomeFantasia = '') and
            (femail = '') and
            (fcep = '') and
            (fcidade = '') and
            (flogradouro = '') and
            (fuf = '');
end;

procedure TACBrPIXDevedorRecebedorBase.Assign(Source: TACBrPIXDevedorRecebedorBase);
begin
  fcpf := Source.cpf;
  fcnpj := Source.cnpj;
  fnome := Source.nome;
  fnomeFantasia := Source.nomeFantasia;
  femail := Source.email;
  fcep := Source.cep;
  fcidade := Source.cidade;
  flogradouro := Source.logradouro;
  fuf := Source.uf;
end;

procedure TACBrPIXDevedorRecebedorBase.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (fcnpj <> '') then
     AJSon.S['cnpj'] := fcnpj
   else if (fcpf <> '') then
     AJSon.S['cpf'] := fcpf;
   if (fnome <> '') then
     AJSon.S['nome'] := fnome;
   if (fnomeFantasia <> '') then
     AJSon.S['nomeFantasia'] := fnomeFantasia;
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
   if (fcnpj <> '') then
     AJSon['cnpj'].AsString := fcnpj
   else if (fcpf <> '') then
     AJSon['cpf'].AsString := fcpf;
   if (fnome <> '') then
     AJSon['nome'].AsString := fnome;
   if (fnomeFantasia <> '') then
     AJSon['nomeFantasia'].AsString := fnomeFantasia;
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

procedure TACBrPIXDevedorRecebedorBase.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fcnpj := AJSon.S['cnpj'];
   fcpf  := AJSon.S['cpf'];
   fnome := AJSon.S['nome'];
   fnomeFantasia := AJSon.S['nomeFantasia'];
   femail := AJSon.S['email'];
   flogradouro := AJSon.S['logradouro'];
   fcidade := AJSon.S['cidade'];
   fcep := AJSon.S['cep'];
   fuf := AJSon.S['uf'];
  {$Else}
   fcnpj := AJSon['cnpj'].AsString;
   fcpf  := AJSon['cpf'].AsString;
   fnome := AJSon['nome'].AsString;
   fnomeFantasia := AJSon['nomeFantasia'].AsString;
   femail := AJSon['email'].AsString;
   flogradouro := AJSon['logradouro'].AsString;
   fcidade := AJSon['cidade'].AsString;
   fcep := AJSon['cep'].AsString;
   fuf := AJSon['uf'].AsString;
  {$EndIf}
end;

procedure TACBrPIXDevedorRecebedorBase.SetCnpj(AValue: String);
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

procedure TACBrPIXDevedorRecebedorBase.SetCpf(AValue: String);
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

procedure TACBrPIXDevedorRecebedorBase.SetCEP(AValue: String);
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

procedure TACBrPIXDevedorRecebedorBase.SetCidade(AValue: String);
begin
  if fcidade = AValue then
    Exit;
  fcidade := copy(AValue, 1, 200);
end;

procedure TACBrPIXDevedorRecebedorBase.SetEmail(AValue: String);
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

procedure TACBrPIXDevedorRecebedorBase.SetLogradouro(AValue: String);
begin
  if flogradouro = AValue then
    Exit;
  flogradouro := copy(AValue, 1, 200);
end;

procedure TACBrPIXDevedorRecebedorBase.SetNome(AValue: String);
begin
  if fnome = AValue then
    Exit;
  fnome := copy(AValue, 1, 200);
end;

procedure TACBrPIXDevedorRecebedorBase.SetNomeFantasia(AValue: String);
begin
  if fnomeFantasia = AValue then
    Exit;
  fnomeFantasia := copy(AValue, 1, 200);
end;

procedure TACBrPIXDevedorRecebedorBase.SetUF(AValue: String);
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

