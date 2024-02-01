{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit frExibeMensagem;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFormExibeMensagem }

  TFormExibeMensagem = class(TForm)
    btOk: TButton;
    lMensagem: TLabel;
    pMensagem: TPanel;
    tEspera: TTimer;
    procedure btOkKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tEsperaTimer(Sender: TObject);
    procedure lMensagemClick(Sender: TObject);
  private
    fTempoEspera: Integer;
    fFinalEspera: TDateTime;
    function GetMensagem: String;
    procedure SetMensagem(AValue: String);
    procedure AbortarTempoEspera;
  public
    property Mensagem: String read GetMensagem write SetMensagem;
    property TempoEspera: Integer read fTempoEspera write fTempoEspera;
  end;

implementation

uses
  dateutils,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrConsts;

{$R *.dfm}

{ TFormExibeMensagem }

procedure TFormExibeMensagem.FormShow(Sender: TObject);
begin
  if (TempoEspera > 0) then
  begin
    fFinalEspera := IncMilliSecond(Now, fTempoEspera);
    tEspera.Enabled := True;
  end
  else
  begin
    fFinalEspera := 0;
    tEspera.Enabled := False;
  end;
end;

procedure TFormExibeMensagem.btOkKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  AbortarTempoEspera;
end;

procedure TFormExibeMensagem.FormClick(Sender: TObject);
begin
  AbortarTempoEspera;
end;

procedure TFormExibeMensagem.lMensagemClick(Sender: TObject);
begin
  AbortarTempoEspera;
end;

procedure TFormExibeMensagem.tEsperaTimer(Sender: TObject);
var
  SegundosRestantes: Integer;
begin
  SegundosRestantes := SecondsBetween(fFinalEspera, Now);
  if SegundosRestantes <= 0 then
    ModalResult := mrOK
  else
    btOk.Caption := Format('OK (%d)', [SegundosRestantes]);
end;

function TFormExibeMensagem.GetMensagem: String;
begin
  Result := lMensagem.Caption;
end;

procedure TFormExibeMensagem.SetMensagem(AValue: String);
var
  NumLin, AltLin: Integer;
begin
  lMensagem.Caption := AValue;

  // Se houver quebra de linhas na msg, aumente o formulário...
  NumLin := CountStr(AValue, CR);
  if (NumLin > 0) then
  begin
    AltLin := lMensagem.Canvas.TextHeight('H');
    Height := Height + (NumLin * AltLin);
  end;
end;

procedure TFormExibeMensagem.AbortarTempoEspera;
begin
  tEspera.Enabled := False;
  btOk.Caption := 'OK';
end;

end.

