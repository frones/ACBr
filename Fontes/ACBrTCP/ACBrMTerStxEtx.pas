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
|*  - Primeira Versao ACBrMTerStxEtx
******************************************************************************}

{$I ACBr.inc}

unit ACBrMTerStxEtx;

interface

uses
  Classes, SysUtils, ACBrMTerClass, ACBrConsts, ACBrUtil;

type

  { TACBrMTerStxEtx }

  TACBrMTerStxEtx = class(TACBrMTerClass)
  private
    function PrepararCmd(aCmd: Char; const aParams: AnsiString = ''): AnsiString;
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

{ TACBrMTerStxEtx }

function TACBrMTerStxEtx.PrepararCmd(aCmd: Char; const aParams: AnsiString): AnsiString;
begin
  Result := STX + aCmd + aParams + ETX;
end;

constructor TACBrMTerStxEtx.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  fpModeloStr := 'STX-ETX';
end;

function TACBrMTerStxEtx.ComandoBackSpace: AnsiString;
begin
  Result := PrepararCmd('D', BS);
end;

function TACBrMTerStxEtx.ComandoBeep(aTempo: Integer): AnsiString;
begin
  //Result := STX + #90 + '9' + ETX;
  //Result := STX + 'D' + BELL + ETX;
  Result := '';  //TODO:
end;

function TACBrMTerStxEtx.ComandoBoasVindas: AnsiString;
begin
  Result := '';
end;

function TACBrMTerStxEtx.ComandoDeslocarCursor(aValue: Integer): AnsiString;
begin
  Result := '';

  while (aValue > 0) do
  begin
    Result := Result + PrepararCmd('O', DC4);
    aValue := aValue - 1;
  end;
end;

function TACBrMTerStxEtx.ComandoDeslocarLinha(aValue: Integer): AnsiString;
begin
  Result := PrepararCmd('C', '100');
  end;

function TACBrMTerStxEtx.ComandoEnviarParaParalela(const aDados: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(aDados) do
    Result := Result + PrepararCmd('P', aDados[I]);
end;

function TACBrMTerStxEtx.ComandoEnviarParaSerial(const aDados: AnsiString; aSerial: Byte): AnsiString;
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

function TACBrMTerStxEtx.ComandoEnviarTexto(const aTexto: AnsiString): AnsiString;
begin
  Result := PrepararCmd('D', aTexto);
end;

function TACBrMTerStxEtx.ComandoOnline: AnsiString;
begin
  Result := PrepararCmd('T');
end;

function TACBrMTerStxEtx.ComandoPosicionarCursor(aLinha, aColuna: Integer): AnsiString;
var
  wCol, wLinha: Integer;
begin
  wLinha := (aLinha - 1);
  wCol   := (aColuna - 1);
  Result := PrepararCmd('C', IntToStr(wLinha) + IntToStrZero(wCol, 2));
end;

function TACBrMTerStxEtx.ComandoLimparDisplay: AnsiString;
begin
  Result := PrepararCmd('L');
end;

end.

