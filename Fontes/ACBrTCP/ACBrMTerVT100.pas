{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016 Elias César Vieira                     }
{                                                                              }
{ Colaboradores nesse arquivo: Daniel Simões de Almeida                        }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 17/05/2016: Elias César Vieira
|*  - Primeira Versao ACBrMTerVT100
******************************************************************************}

{$I ACBr.inc}

unit ACBrMTerVT100;

interface

uses
  Classes, SysUtils, ACBrMTerClass, ACBrConsts;

type

  { TACBrMTerVT100 }

  TACBrMTerVT100 = class(TACBrMTerClass)
  private
    { Private declarations }
  public
    constructor Create(aOwner: TComponent);

    function ComandoBackSpace: AnsiString; override;
    function ComandoBeep(aTempo: Integer = 0): AnsiString; override;
    function ComandoBoasVindas: AnsiString; override;
    function ComandoDeslocarCursor(aValue: Integer): AnsiString; override;
    function ComandoDeslocarLinha(aValue: Integer): AnsiString; override;
    function ComandoEnviarParaParalela(const aDados: AnsiString): AnsiString; override;
    function ComandoEnviarParaSerial(const aDados: AnsiString; aSerial: Byte = 0): AnsiString; override;
    function ComandoEnviarTexto(const aTexto: AnsiString): AnsiString; override;
    function ComandoLimparLinha(aLinha: Integer): AnsiString; override;
    function ComandoPosicionarCursor(aLinha, aColuna: Integer): AnsiString; override;
    function ComandoLimparDisplay: AnsiString; override;
  end;

implementation

uses ACBrUtil;

{ TACBrMTerVT100 }

constructor TACBrMTerVT100.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  fpModeloStr := 'VT100';
end;

function TACBrMTerVT100.ComandoBackSpace: AnsiString;
begin
  Result := BS;
end;

function TACBrMTerVT100.ComandoBeep(aTempo: Integer): AnsiString;
begin
  Result := ESC + '[TB';
end;

function TACBrMTerVT100.ComandoBoasVindas: AnsiString;
begin
  Result := '';
end;

function TACBrMTerVT100.ComandoDeslocarCursor(aValue: Integer): AnsiString;
var
  I: Integer;
begin
  Result := '';

  if (aValue < 0) then
    raise Exception.Create(ACBrStr('Válidos apenas números positivos'));

  for I := 1 to aValue do
    Result := Result + ESC + '[C';
end;

function TACBrMTerVT100.ComandoDeslocarLinha(aValue: Integer): AnsiString;
var
  wCmd: String;
  I: Integer;
begin
  Result := '';

  if (aValue > 0) then
    wCmd := LF           // Comando coloca cursor na linha de baixo
  else if (aValue < 0) then
    wCmd := ESC + '[A'   // Comando coloca cursor na linha de cima
  else
    Exit;

  for I := 1 to abs(aValue) do
    Result := Result + wCmd;
end;

function TACBrMTerVT100.ComandoEnviarParaParalela(const aDados: AnsiString ): AnsiString;
begin
  Result := ESC + '[?24l';  // Seleciona porta paralela

  Result := Result + ESC + '[5i';  // Habilita serviço de impressão
  Result := Result + aDados;       // Envia Dados
  Result := Result + ESC + '[4i';  // Desabilita serviço de impressão
end;

function TACBrMTerVT100.ComandoEnviarParaSerial(const aDados: AnsiString; aSerial: Byte): AnsiString;
begin
  if (aSerial = 2) then
    Result := ESC + '[?24r'   // Seleciona porta serial 2
  else
    Result := ESC + '[?24h';  // Seleciona porta serial padrão(0)

  Result := Result + ESC + '[5i';  // Habilita serviço de impressão
  Result := Result + aDados;       // Envia Dados
  Result := Result + ESC + '[4i';  // Desabilita serviço de impressão
end;

function TACBrMTerVT100.ComandoEnviarTexto(const aTexto: AnsiString): AnsiString;
begin
  Result := aTexto;
end;

function TACBrMTerVT100.ComandoLimparLinha(aLinha: Integer): AnsiString;
begin
  Result := ComandoPosicionarCursor(aLinha, 1) + ESC + '[K';
end;

function TACBrMTerVT100.ComandoPosicionarCursor(aLinha, aColuna: Integer): AnsiString;
var
  wL, wC: AnsiString;
begin
  wL := IntToStrZero(aLinha, 2);
  wC := IntToStrZero(aColuna, 2);

  Result := ESC + '[' + wL + ';' + wC + 'H';
end;

function TACBrMTerVT100.ComandoLimparDisplay: AnsiString;
begin
  Result := ESC + '[H' + ESC + '[J';
end;

end.

