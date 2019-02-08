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
|*  - Primeira Versao ACBrMTerSB100
******************************************************************************}

{$I ACBr.inc}

unit ACBrMTerSB100;

interface

uses
  Classes, SysUtils,
  ACBrMTerClass;

type
  { TACBrMTerSB100 }

  TACBrMTerSB100 = class(TACBrMTerClass)
  private
    function PrepararCmd(const aCmd: Char; const aParams: AnsiString = ''): AnsiString;
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
    function ComandoOnline: AnsiString; override;
    function ComandoPosicionarCursor(aLinha, aColuna: Integer): AnsiString; override;
    function ComandoLimparDisplay: AnsiString; override;
  end;

implementation

uses
  ACBrConsts, ACBrUtil;

{ TACBrMTerSB100 }

function TACBrMTerSB100.PrepararCmd(const aCmd: Char; const aParams: AnsiString): AnsiString;
begin
  Result := STX + aCmd + aParams + ETX;
end;

constructor TACBrMTerSB100.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  fpModeloStr := 'SB100';
end;

function TACBrMTerSB100.ComandoBackSpace: AnsiString;
begin
  Result := ComandoEnviarTexto(BS);
end;

function TACBrMTerSB100.ComandoBeep(aTempo: Integer): AnsiString;
var
  wTempo: Integer;
begin
  // Ajustando tempo mínimo/máximo
  wTempo := aTempo;
  if (wTempo < 1) then
    wTempo := 1
  else if (wTempo > 9) then
    wTempo := 9;

  Result := PrepararCmd('Z', IntToStr(wTempo));
end;

function TACBrMTerSB100.ComandoBoasVindas: AnsiString;
begin
  Result := '';
end;

function TACBrMTerSB100.ComandoDeslocarCursor(aValue: Integer): AnsiString;
begin
  Result := '';

  while (aValue > 0) do
  begin
    Result := Result + PrepararCmd('O', DC4);
    aValue := aValue - 1;
  end;
end;

function TACBrMTerSB100.ComandoDeslocarLinha(aValue: Integer): AnsiString;
var
  wLinha: Integer;
begin
  // Validando valores máximos/mínimos
  wLinha := (aValue - 1);
  if (wLinha < 0) then
    wLinha := 0
  else if (wLinha > 3) then
    wLinha := 3;

  Result := PrepararCmd('P', IntToStr(wLinha) + '00');
end;

function TACBrMTerSB100.ComandoEnviarParaParalela(const aDados: AnsiString): AnsiString;
begin
  Result := '';
end;

function TACBrMTerSB100.ComandoEnviarParaSerial(const aDados: AnsiString;
  aSerial: Byte): AnsiString;
var
  wPorta: Char;
  I: Integer;
begin
  Result := '';

  if (aSerial = 2) then
    wPorta := 'R'        // Seleciona porta serial 2
  else
    wPorta := 'S';       // Seleciona porta serial padrão(0)

  for I := 1 to Length(aDados) do
    Result := Result + PrepararCmd(wPorta, aDados[I]);
end;

function TACBrMTerSB100.ComandoEnviarTexto(const aTexto: AnsiString): AnsiString;
begin
  Result := PrepararCmd('Y', 'n' + aTexto);
end;

function TACBrMTerSB100.ComandoOnline: AnsiString;
begin
  Result := PrepararCmd('V');
end;

function TACBrMTerSB100.ComandoPosicionarCursor(aLinha, aColuna: Integer): AnsiString;
var
  wCol, wLinha: Integer;
begin
  wLinha := (aLinha - 1);
  if (wLinha < 0) then
    wLinha := 0
  else if (wLinha > 3) then
    wLinha := 3;

  wCol := (aColuna - 1);
  if (wCol < 0) then
    wCol := 0
  else if (wCol > 40) then
    wCol := 40;

  Result := PrepararCmd('P', IntToStr(wLinha) + IntToStrZero(wCol, 2));
end;

function TACBrMTerSB100.ComandoLimparDisplay: AnsiString;
begin
  Result := PrepararCmd('C');
end;

end.

