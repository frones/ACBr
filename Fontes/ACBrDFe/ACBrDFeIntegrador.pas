{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2015 Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}
unit ACBrDFeIntegrador;

interface

uses
  Classes, SysUtils, ACBrDFeSSL,  pcnGerador, ACBrIntegradorUtil;

type

  { TDFeIntegrador }

  TDFeIntegrador = class
  private
    FGerador: TGerador;
    FComandoMFE: TComandoIntegrador;
    FIdentificador: TIdentificador;
    FParametro: TParametro;
    FMetodo: TMetodo;
    FRetornoLst : TStringList ;

    FPastaInput : String;
    FPastaOutput : String;
    FTimeout : Integer;
    FNomeComponente: String;
    procedure SetPastaInput(AValue: String);
    procedure SetPastaOutput(AValue: String);
    procedure SetTimeout(AValue: Integer);

  public
    constructor Create;
    destructor Destroy;

    function Enviar(const numeroSessao: Integer; const ConteudoXML: String; const Metodo: String): String;
  published
    property PastaInput  : String  read FPastaInput  write SetPastaInput;
    property PastaOutput : String  read FPastaOutput write SetPastaOutput;
    property Timeout     : Integer read FTimeout     write SetTimeout default 30;
    property NomeComponente: String read FNomeComponente write FNomeComponente;

    property Gerador   : TGerador   read FGerador;
    property Metodo    : TMetodo    read FMetodo;
    property Parametro : TParametro read FParametro;
  end;


implementation

Uses pcnConversao, synacode;

{ TDFeIntegrador }

procedure TDFeIntegrador.SetPastaInput(AValue: String);
begin
  if FPastaInput=AValue then
    Exit;
  FPastaInput := AValue;
  FComandoMFE.PastaInput := FPastaInput;
end;

procedure TDFeIntegrador.SetPastaOutput(AValue: String);
begin
  if FPastaOutput=AValue then
    Exit;
  FPastaOutput := AValue;
  FComandoMFE.PastaOutput := FPastaOutput;
end;

procedure TDFeIntegrador.SetTimeout(AValue: Integer);
begin
  if FTimeout=AValue then
    Exit;
  FTimeout := AValue;
  FComandoMFE.Timeout := FTimeout;
end;

constructor TDFeIntegrador.Create;
begin
  FGerador       := TGerador.Create;
  FComandoMFE    := TComandoIntegrador.Create;
  FIdentificador := TIdentificador.Create(FGerador);
  FParametro     := TParametro.Create(FGerador);
  FMetodo        := TMetodo.Create(FGerador);
  FRetornoLst    := TStringList.Create;

  FPastaInput  := 'C:\Integrador\Input\';
  FPastaOutput := 'C:\Integrador\Output\';
  FTimeout     := 30;

  FComandoMFE.PastaInput := FPastaInput;
  FComandoMFE.PastaOutput := FPastaOutput;
  FComandoMFE.Timeout     := FTimeout;
end;

destructor TDFeIntegrador.Destroy;
begin
  FIdentificador.Free;
  FParametro.Free;
  FMetodo.Free;
  FComandoMFE.Free;
  FGerador.Free;
  FRetornoLst.Free;

  inherited Destroy;
end;

function TDFeIntegrador.Enviar(const numeroSessao: Integer; const ConteudoXML: String; const Metodo: String): String;
Var
  Resp : String;
begin
  Resp := FComandoMFE.PegaResposta(FComandoMFE.EnviaComando(numeroSessao,Metodo+'-'+FormatDateTime('yyyymmddhhnnss', Now),ConteudoXML));

  FRetornoLst.Delimiter := '|';
  {$IFDEF FPC}
   FRetornoLst.StrictDelimiter := True;
  {$ELSE}
   Resp := '"' + StringReplace(Resp, FRetornoLst.Delimiter,
                            '"' + FRetornoLst.Delimiter + '"', [rfReplaceAll]) +
             '"';
  {$ENDIF}
  FRetornoLst.DelimitedText := Resp;

  if FRetornoLst.Count >= 6 then
    Resp := DecodeBase64(FRetornoLst[6]);

  Result :=  Resp;
end ;


end.

