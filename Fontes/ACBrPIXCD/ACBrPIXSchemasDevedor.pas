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

  { TACBrPIXDevedor }

  TACBrPIXDevedor = class(TACBrPIXSchema)
  private
    fcnpj: String;
    fcpf: String;
    fnome: String;
    procedure SetCnpj(AValue: String);
    procedure SetCpf(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXDevedor);

    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property nome: String read fnome write fnome;
  end;

implementation

uses
  ACBrValidador, ACBrUtil;

{ TACBrPIXDevedor }

constructor TACBrPIXDevedor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXDevedor.Clear;
begin
  fcpf := '';
  fcnpj := '';
  fnome := '';
end;

function TACBrPIXDevedor.IsEmpty: Boolean;
begin
  Result := (fcpf = '') and (fcnpj = '') and (fnome = '')
end;

procedure TACBrPIXDevedor.Assign(Source: TACBrPIXDevedor);
begin
  fcpf := Source.cpf;
  fcnpj := Source.cnpj;
  fnome := Source.nome;
end;

procedure TACBrPIXDevedor.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (cnpj <> '') then
     AJSon.S['cnpj'] := cnpj
   else if (cpf <> '') then
     AJSon.S['cpf'] := cpf;
   if (nome <> '') then
     AJSon.S['nome'] := nome;
  {$Else}
   if (cnpj <> '') then
     AJSon['cnpj'].AsString := cnpj
   else if (cpf <> '') then
     AJSon['cpf'].AsString := cpf;
   if (nome <> '') then
     AJSon['nome'].AsString := nome;
  {$EndIf}
end;

procedure TACBrPIXDevedor.DoReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   cnpj := AJSon.S['cnpj'];
   cpf  := AJSon.S['cpf'];
   nome := AJSon.S['nome'];
  {$Else}
   cnpj := AJSon['cnpj'].AsString;
   cpf  := AJSon['cpf'].AsString;
   nome := AJSon['nome'].AsString;
  {$EndIf}
end;

procedure TACBrPIXDevedor.SetCnpj(AValue: String);
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

procedure TACBrPIXDevedor.SetCpf(AValue: String);
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

end.

