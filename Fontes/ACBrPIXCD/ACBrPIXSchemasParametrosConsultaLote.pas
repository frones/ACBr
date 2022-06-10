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

unit ACBrPIXSchemasParametrosConsultaLote;

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

  { TACBrPIXParametrosConsultaLote }

  TACBrPIXParametrosConsultaLote = class(TACBrPIXSchema)
  private
    ffim: TDateTime;
    finicio: TDateTime;
    fpaginacao: TACBrPIXPaginacao;
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;

  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXParametrosConsultaLote);

    property inicio: TDateTime read finicio write finicio;
    property fim: TDateTime read ffim write ffim;
    property paginacao: TACBrPIXPaginacao read fpaginacao;
  end;

implementation

uses
  ACBrUtil.DateTime;

{ TACBrPIXParametrosConsultaLote }

constructor TACBrPIXParametrosConsultaLote.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fpaginacao := TACBrPIXPaginacao.Create('paginacao');
  Clear;
end;

destructor TACBrPIXParametrosConsultaLote.Destroy;
begin
  fpaginacao.Free;
  inherited Destroy;
end;

procedure TACBrPIXParametrosConsultaLote.Clear;
begin
  ffim := 0;
  finicio := 0;
  fpaginacao.Clear
end;

function TACBrPIXParametrosConsultaLote.IsEmpty: Boolean;
begin
  Result := (ffim = 0) and
            (finicio = 0) and
            fpaginacao.IsEmpty;
end;

procedure TACBrPIXParametrosConsultaLote.Assign(
  Source: TACBrPIXParametrosConsultaLote);
begin
  ffim := Source.fim;
  finicio := Source.inicio;
  fpaginacao.Assign(Source.paginacao);
end;

procedure TACBrPIXParametrosConsultaLote.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['inicio'] := DateTimeToIso8601( finicio );
   AJSon.S['fim'] := DateTimeToIso8601( ffim );
   fpaginacao.WriteToJSon(AJSon);
  {$Else}
   AJSon['inicio'].AsString := DateTimeToIso8601( finicio );
   AJSon['fim'].AsString := DateTimeToIso8601( ffim );
   fpaginacao.WriteToJSon(AJSon);
  {$EndIf}
end;

procedure TACBrPIXParametrosConsultaLote.DoReadFromJSon(AJSon: TJsonObject);
var
  s: String;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   s := AJSon.S['inicio'];
   if (s <> '') then
     finicio := Iso8601ToDateTime(s);
   s := AJSon.S['fim'];
   if (s <> '') then
     ffim := Iso8601ToDateTime(s);
   fpaginacao.ReadFromJSon(AJSon);
  {$Else}
   s := AJSon['inicio'].AsString;
   if (s <> '') then
     finicio := Iso8601ToDateTime(s);
   s := AJSon['fim'].AsString;
   if (s <> '') then
     ffim := Iso8601ToDateTime(s);
   fpaginacao.ReadFromJSon(AJSon);
  {$EndIf}
end;

end.

