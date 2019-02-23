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

    procedure ComandoBackSpace(Comandos: TACBrMTerComandos); override;
    procedure ComandoBeep(Comandos: TACBrMTerComandos; const aTempo: Integer = 0);
      override;
    procedure ComandoDeslocarCursor(Comandos: TACBrMTerComandos; const aValue: Integer);
      override;
    procedure ComandoDeslocarLinha(Comandos: TACBrMTerComandos; aValue: Integer);
      override;
    procedure ComandoEnviarParaParalela(Comandos: TACBrMTerComandos;
      const aDados: AnsiString); override;
    procedure ComandoEnviarParaSerial(Comandos: TACBrMTerComandos;
      const aDados: AnsiString; aSerial: Byte = 0); override;
    procedure ComandoEnviarTexto(Comandos: TACBrMTerComandos; const aTexto: AnsiString);
      override;
    procedure ComandoLimparLinha(Comandos: TACBrMTerComandos; const aLinha: Integer);
      override;
    procedure ComandoPosicionarCursor(Comandos: TACBrMTerComandos; const aLinha,
      aColuna: Integer); override;
    procedure ComandoLimparDisplay(Comandos: TACBrMTerComandos); override;
  end;

implementation

uses
  ACBrUtil;

{ TACBrMTerVT100 }

constructor TACBrMTerVT100.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  fpModeloStr := 'VT100';
end;

procedure TACBrMTerVT100.ComandoBackSpace(Comandos: TACBrMTerComandos);
begin
  Comandos.New( BS );
end;

procedure TACBrMTerVT100.ComandoBeep(Comandos: TACBrMTerComandos;
  const aTempo: Integer);
begin
  Comandos.New( ESC + '[TB' );
end;

procedure TACBrMTerVT100.ComandoDeslocarCursor(Comandos: TACBrMTerComandos;
  const aValue: Integer);
var
  wValor: Integer;
begin
  wValor := aValue;
  while (wValor > 0) do
  begin
    Comandos.New( ESC + '[C' );
    Dec( wValor );
  end;
end;

procedure TACBrMTerVT100.ComandoDeslocarLinha(Comandos: TACBrMTerComandos;
  aValue: Integer);
var
  wCmd: String;
  I: Integer;
begin
  if (aValue > 0) then
    wCmd := LF           // Comando coloca cursor na linha de baixo
  else if (aValue < 0) then
    wCmd := ESC + '[A'   // Comando coloca cursor na linha de cima
  else
    Exit;

  for I := 1 to abs(aValue) do
    Comandos.New( wCmd );
end;

procedure TACBrMTerVT100.ComandoEnviarParaParalela(Comandos: TACBrMTerComandos;
  const aDados: AnsiString);
begin
  Comandos.New( ESC + '[?24l' );  // Seleciona porta paralela
  Comandos.New( ESC + '[5i' );  // Habilita serviço de impressão
  Comandos.New( aDados );       // Envia Dados
  Comandos.New( ESC + '[4i' );  // Desabilita serviço de impressão
end;

procedure TACBrMTerVT100.ComandoEnviarParaSerial(Comandos: TACBrMTerComandos;
  const aDados: AnsiString; aSerial: Byte);
begin
  if (aSerial = 2) then
    Comandos.New( ESC + '[?24r' )   // Seleciona porta serial 2
  else
    Comandos.New( ESC + '[?24h' );  // Seleciona porta serial padrão(0)

  Comandos.New( ESC + '[5i' +   // Habilita serviço de impressão
                aDados      +   // Envia Dados
                ESC + '[4i' );  // Desabilita serviço de impressão
end;

procedure TACBrMTerVT100.ComandoEnviarTexto(Comandos: TACBrMTerComandos;
  const aTexto: AnsiString);
begin
  Comandos.New(aTexto);
end;

procedure TACBrMTerVT100.ComandoLimparLinha(Comandos: TACBrMTerComandos;
  const aLinha: Integer);
begin
  ComandoPosicionarCursor(Comandos, aLinha, 1);
  Comandos.New(ESC + '[K');
end;

procedure TACBrMTerVT100.ComandoPosicionarCursor(Comandos: TACBrMTerComandos;
  const aLinha, aColuna: Integer);
var
  wL, wC: AnsiString;
begin
  wL := IntToStrZero(aLinha, 2);
  wC := IntToStrZero(aColuna, 2);

  Comandos.New( ESC + '[' + wL + ';' + wC + 'H' );
end;

procedure TACBrMTerVT100.ComandoLimparDisplay(Comandos: TACBrMTerComandos);
begin
  Comandos.New( ESC + '[H' + ESC + '[J' );
end;

end.

