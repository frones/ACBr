{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Elias César Vieira                             }
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
  Classes, SysUtils,
  ACBrMTerClass
  {$IfDef NEXTGEN}
   ,ACBrBase
  {$EndIf};

type

  { TACBrMTerStxEtx }

  TACBrMTerStxEtx = class(TACBrMTerClass)
  protected
    fpRetornaACK: Boolean;

    function PrepararCmd(aCmd: Char; const aParams: AnsiString = ''): AnsiString;
    function TimeOutDef: Integer;
  public
    constructor Create(aOwner: TComponent);

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
    procedure ComandoOnline(Comandos: TACBrMTerComandos); override;
    procedure ComandoPosicionarCursor(Comandos: TACBrMTerComandos; const aLinha,
      aColuna: Integer); override;
    procedure ComandoLimparDisplay(Comandos: TACBrMTerComandos); override;
  end;

implementation

uses
   ACBrConsts, ACBrUtil;

{ TACBrMTerStxEtx }

function TACBrMTerStxEtx.PrepararCmd(aCmd: Char; const aParams: AnsiString): AnsiString;
begin
  Result := STX + aCmd + aParams + ETX;
end;

function TACBrMTerStxEtx.TimeOutDef: Integer;
begin
  if fpRetornaACK then
    Result := TimeOut
  else
    Result := 0;
end;

constructor TACBrMTerStxEtx.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  fpModeloStr := 'STX-ETX';
  fpRetornaACK := False;
end;

procedure TACBrMTerStxEtx.ComandoBeep(Comandos: TACBrMTerComandos;
  const aTempo: Integer);
begin
  //TODO::
  //Result := STX + #90 + '9' + ETX;
  //Result := STX + 'D' + BELL + ETX;
end;

procedure TACBrMTerStxEtx.ComandoDeslocarCursor(Comandos: TACBrMTerComandos;
  const aValue: Integer);
var
  wValor: Integer;
begin
  wValor := aValue;
  while (wValor > 0) do
  begin
    Comandos.New( PrepararCmd('O', DC4), TimeOutDef );
    Dec( wValor );
  end;
end;

procedure TACBrMTerStxEtx.ComandoDeslocarLinha(Comandos: TACBrMTerComandos;
  aValue: Integer);
begin
  Comandos.New( PrepararCmd('C', '100'), TimeOutDef );       // TODO::
end;

procedure TACBrMTerStxEtx.ComandoEnviarParaParalela(
  Comandos: TACBrMTerComandos; const aDados: AnsiString);
begin
  Comandos.New( PrepararCmd('P', aDados), TimeOutDef );
end;

procedure TACBrMTerStxEtx.ComandoEnviarParaSerial(Comandos: TACBrMTerComandos;
  const aDados: AnsiString; aSerial: Byte);
var
  wPorta: Char;
begin
  if (aSerial = 2) then
    wPorta := 'R'        // Seleciona porta serial 2
  else
    wPorta := 'S';       // Seleciona porta serial padrão(0)

  Comandos.New( PrepararCmd(wPorta, aDados), TimeOutDef );
end;

procedure TACBrMTerStxEtx.ComandoEnviarTexto(Comandos: TACBrMTerComandos;
  const aTexto: AnsiString);
begin
  Comandos.New( PrepararCmd('D', aTexto), TimeOutDef );
end;

procedure TACBrMTerStxEtx.ComandoOnline(Comandos: TACBrMTerComandos);
begin
  Comandos.New( PrepararCmd('T'), TimeOut );
end;

procedure TACBrMTerStxEtx.ComandoPosicionarCursor(Comandos: TACBrMTerComandos;
  const aLinha, aColuna: Integer);
begin
  Comandos.New( PrepararCmd('C', IntToStr(aLinha-1) + IntToStrZero(aColuna-1, 2)), TimeOutDef );
end;

procedure TACBrMTerStxEtx.ComandoLimparDisplay(Comandos: TACBrMTerComandos);
begin
  Comandos.New( PrepararCmd('L'), TimeOutDef );
end;

end.

