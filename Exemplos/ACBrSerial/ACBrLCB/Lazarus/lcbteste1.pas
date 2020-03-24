{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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

unit LCBTeste1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Windows, ACBrBase, ACBrDevice, ACBrDeviceSerial, ACBrLCB;

type

  { TForm1 }

  TForm1 = class(TForm)
    mProdutos: TMemo;
    Label1: TLabel;
    cbFila: TCheckBox;
    edIntervalo: TEdit;
    Label3: TLabel;
    cbxPorta: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    edSufixo: TEdit;
    mFila: TMemo;
    Label6: TLabel;
    bEmulador: TButton;
    cbExcluirSufixo: TCheckBox;
    Label2: TLabel;
    edAtraso: TEdit;
    pAtraso: TPanel;
    bApagarFila: TButton;
    bLerFila: TButton;
    lUltimaLeitura: TLabel;
    ACBrLCB1: TACBrLCB;
    Label7: TLabel;
    cbxBaud: TComboBox;
    Label8: TLabel;
    EditData: TEdit;
    Label9: TLabel;
    chbHard: TCheckBox;
    Label10: TLabel;
    chbSoft: TCheckBox;
    cbxParidade: TComboBox;
    cbxHandShake: TComboBox;
    cbxStop: TComboBox;
    Label11: TLabel;
    bAtivar: TButton;
    procedure ACBrLCB1LeCodigo(Sender: TObject);
    procedure bEmuladorClick(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edIntervaloChange(Sender: TObject);
    procedure cbFilaClick(Sender: TObject);
    procedure bApagarFilaClick(Sender: TObject);
    procedure bLerFilaClick(Sender: TObject);
    procedure ACBrLCB1LeFila(Sender: TObject);
    procedure bAtivarClick(Sender: TObject);
    procedure cbExcluirSufixoClick(Sender: TObject);
    procedure cbxHandShakeChange(Sender: TObject);
    procedure chbSoftClick(Sender: TObject);
    procedure chbHardClick(Sender: TObject);
  private
    { Private declarations }
    Function Converte( cmd : String) : String;
    procedure Vende ;
    procedure VerificaFlow;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

function TForm1.Converte(cmd: String): String;
var A : Integer ;
begin
  Result := '' ;
  For A := 1 to length( cmd ) do
  begin
     if not (cmd[A] in ['A'..'Z','a'..'z','0'..'9',
                        ' ','.',',','/','?','<','>',';',':',']','[','{','}',
                        '\','|','=','+','-','_',')','(','*','&','^','%','$',
                        '#','@','!','~' ]) then
        Result := Result + '#' + IntToStr(ord( cmd[A] )) + ' '
     else
        Result := Result + cmd[A] + ' ';
  end ;
end;

procedure TForm1.bEmuladorClick(Sender: TObject);
var
  vShow : Integer;
  ConnectCommand : PChar;
  Command : String ;
begin
  Command := 'EmulaLCB.EXE' ;
  ConnectCommand := PChar(Command);
  vShow := sw_ShowNormal;
  winexec(ConnectCommand, vShow);
end;

procedure TForm1.cbxPortaChange(Sender: TObject);
begin
  if ACBrLCB1.Ativo then
     bAtivar.Click ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cbxPorta.Text    := ACBrLCB1.Porta ;
  edIntervalo.Text := IntToStr(ACBrLCB1.Intervalo) ;
  edSufixo.Text    := ACBrLCB1.Sufixo ;
  cbFila.Checked   := ACBrLCB1.UsarFila ;
  cbExcluirSufixo.Checked := ACBrLCB1.ExcluirSufixo ;
  VerificaFlow ;

  cbFilaClick( Sender );
end;

procedure TForm1.edIntervaloChange(Sender: TObject);
begin
  ACBrLCB1.Intervalo := StrToInt(edIntervalo.Text) ;
end;

procedure TForm1.cbFilaClick(Sender: TObject);
begin
  ACBrLCB1.UsarFila   := cbFila.Checked ;
  edSufixo.Text       := ACBrLCB1.Sufixo ;
  bLerFila.Enabled    := ACBrLCB1.UsarFila ;
  bApagarFila.Enabled := ACBrLCB1.UsarFila ;
end;

procedure TForm1.bApagarFilaClick(Sender: TObject);
begin
  ACBrLCB1.ApagarFila ;
end;

procedure TForm1.bLerFilaClick(Sender: TObject);
begin
  ACBrLCB1.LerFila ;
  ACBrLCB1LeCodigo( Sender );
end;

procedure TForm1.ACBrLCB1LeCodigo(Sender: TObject);
begin
  lUltimaLeitura.Caption := Converte( ACBrLCB1.UltimaLeitura ) ;

  if not ACBrLCB1.UsarFila then
     Vende
  else
     mFila.Lines.Assign( ACBrLCB1.Fila );
end;

procedure TForm1.ACBrLCB1LeFila(Sender: TObject);
begin
  mFila.Lines.Assign( ACBrLCB1.Fila );
  Vende ;
end;

procedure TForm1.Vende;
Var Atraso : Integer ;
begin
  mProdutos.Lines.Add( 'Cód: '+ACBrLCB1.UltimoCodigo ) ;

  Atraso := StrToIntDef(edAtraso.Text,0) ;
  if Atraso > 0 then
  begin
     pAtraso.Visible := true ;
     Application.ProcessMessages ;
     Sleep( Atraso );
     pAtraso.Visible := false ;
  end ;

end;

procedure TForm1.bAtivarClick(Sender: TObject);
begin

  if ACBrLCB1.Ativo then
   begin
     ACBrLCB1.Desativar;
     bAtivar.Caption := '&Ativar' ;
     mProdutos.Lines.Add('ACBrLCB - DESATIVADO') ;
   end
  else
   begin
     ACBrLCB1.Porta       := cbxPorta.Text ;
     ACBrLCB1.UsarFila    := cbFila.Checked ;
     ACBrLCB1.Device.Baud := StrToInt(cbxBaud.text) ;
     ACBrLCB1.Device.Data := StrToInt(EditData.text);

     ACBrLCB1.Sufixo        := edSufixo.Text ;
     ACBrLCB1.ExcluirSufixo := cbExcluirSufixo.Checked ;

     ACBrLCB1.Device.HandShake := TACBrHandShake( cbxHandShake.ItemIndex ) ;
     ACBrLCB1.Device.HardFlow  := chbHard.Checked;
     ACBrLCB1.Device.SoftFlow  := chbSoft.Checked;

     ACBrLCB1.Device.Parity := TACBrSerialParity( cbxParidade.ItemIndex ) ;
     ACBrLCB1.Device.Stop   := TACBrSerialStop( cbxStop.ItemIndex ) ;

     ACBrLCB1.Ativar ;

     if ACBrLCB1.Ativo then
     begin
        bAtivar.Caption := '&Desativar' ;
        mProdutos.Lines.Add('ACBrLCB - ATIVADO') ;
     end ;
   end ;
end;

procedure TForm1.cbExcluirSufixoClick(Sender: TObject);
begin
  ACBrLCB1.ExcluirSufixo := cbExcluirSufixo.Checked ;
end;

procedure TForm1.VerificaFlow ;
begin
  cbxHandShake.ItemIndex := Integer( ACBrLCB1.Device.HandShake ) ;
  chbHard.Checked        := ACBrLCB1.Device.HardFlow ;
  chbSoft.Checked        := ACBrLCB1.Device.SoftFlow ;
end ;

procedure TForm1.cbxHandShakeChange(Sender: TObject);
begin
  if ACBrLCB1.Ativo then
     bAtivar.Click ;

  ACBrLCB1.Device.HandShake := TACBrHandShake( cbxHandShake.ItemIndex ) ;
  VerificaFlow ;
end;

procedure TForm1.chbSoftClick(Sender: TObject);
begin
  if ACBrLCB1.Ativo then
     bAtivar.Click ;

  ACBrLCB1.Device.SoftFlow  := chbSoft.Checked ;
  VerificaFlow ;
end;

procedure TForm1.chbHardClick(Sender: TObject);
begin
  if ACBrLCB1.Ativo then
     bAtivar.Click ;

  ACBrLCB1.Device.HardFlow  := chbHard.Checked ;
  VerificaFlow ;
end;

initialization
  {$I lcbteste1.lrs}

end.

