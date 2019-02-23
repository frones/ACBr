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
  ACBrMTerClass, ACBrMTerStxEtx;

type
  { TACBrMTerSB100 }

  TACBrMTerSB100 = class(TACBrMTerStxEtx)
  public
    constructor Create(aOwner: TComponent);

    procedure ComandoBeep(Comandos: TACBrMTerComandos; const aTempo: Integer = 0);
      override;
    procedure ComandoDeslocarLinha(Comandos: TACBrMTerComandos; aValue: Integer);
      override;
    procedure ComandoEnviarParaParalela(Comandos: TACBrMTerComandos;
      const aDados: AnsiString); override;
    procedure ComandoEnviarTexto(Comandos: TACBrMTerComandos; const aTexto: AnsiString);
      override;
    procedure ComandoOnline(Comandos: TACBrMTerComandos); override;
    procedure ComandoPosicionarCursor(Comandos: TACBrMTerComandos; const aLinha,
      aColuna: Integer); override;
    procedure ComandoLimparDisplay(Comandos: TACBrMTerComandos); override;
  end;

implementation

uses
  math,
  ACBrConsts, ACBrUtil;

{ TACBrMTerSB100 }

constructor TACBrMTerSB100.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  fpModeloStr := 'SB100';
  fpRetornaACK := True;
end;

procedure TACBrMTerSB100.ComandoBeep(Comandos: TACBrMTerComandos;
  const aTempo: Integer);
var
  wTempo: Integer;
begin
  // Ajustando tempo mínimo/máximo
  wTempo := min(max(ceil(aTempo/1000),1),9);
  Comandos.New( PrepararCmd('Z', IntToStr(wTempo)), TimeOut );
end;

procedure TACBrMTerSB100.ComandoDeslocarLinha(Comandos: TACBrMTerComandos;
  aValue: Integer);
var
  wLinha: Integer;
begin
  wLinha := min(max(aValue - 1, 0), 3);
  Comandos.New( PrepararCmd('P', IntToStr(wLinha) + '00'), TimeOut );
end;

procedure TACBrMTerSB100.ComandoEnviarParaParalela(Comandos: TACBrMTerComandos;
  const aDados: AnsiString);
begin
  DisparaErroNaoImplementado('ComandoEnviarParaParalela');
end;

procedure TACBrMTerSB100.ComandoEnviarTexto(Comandos: TACBrMTerComandos;
  const aTexto: AnsiString);
begin
  Comandos.New( PrepararCmd('Y', 'n' + aTexto), TimeOut );
end;

procedure TACBrMTerSB100.ComandoOnline(Comandos: TACBrMTerComandos);
begin
  Comandos.New( PrepararCmd('V'), TimeOut );
end;

procedure TACBrMTerSB100.ComandoPosicionarCursor(Comandos: TACBrMTerComandos;
  const aLinha, aColuna: Integer);
var
  wCol, wLinha: Integer;
begin
  wLinha := min(max(aLinha - 1, 0), 3);
  wCol := min(max(aColuna - 1, 0), 39);
  Comandos.New( PrepararCmd('P', IntToStr(wLinha) + IntToStrZero(wCol, 2)), TimeOut );
end;

procedure TACBrMTerSB100.ComandoLimparDisplay(Comandos: TACBrMTerComandos);
begin
  Comandos.New( PrepararCmd('C'), TimeOut );
end;

end.

