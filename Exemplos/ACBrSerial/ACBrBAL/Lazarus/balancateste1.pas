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

unit BalancaTeste1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ACBrDevice, ACBrBAL;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrBAL1: TACBrBAL;
    btAtivarTara: TButton;
    btDesativarTara: TButton;
    btDesligarDisplay: TButton;
    btEnviarPrecoKg: TButton;
    btLigarDisplay: TButton;
    btnConectar: TButton;
    btnDesconectar: TButton;
    btnLerPeso: TButton;
    btSearchPorts: TSpeedButton;
    btZerarDispositivo: TButton;
    chbMonitorar: TCheckBox;
    edLog: TEdit;
    edPrecoKg: TEdit;
    edtTimeOut: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label11: TLabel;
    Memo1: TMemo;
    cmbBalanca: TComboBox;
    cmbBaudRate: TComboBox;
    cmbDataBits: TComboBox;
    cmbHandShaking: TComboBox;
    cmbParity: TComboBox;
    cmbPortaSerial: TComboBox;
    cmbStopBits: TComboBox;
    pnConfig: TPanel;
    pnOpcoesAdicionais: TPanel;
    pnOpcoes: TPanel;
    SbArqLog: TSpeedButton;
    sttPeso: TStaticText;
    sttResposta: TStaticText;
    procedure btAtivarTaraClick(Sender: TObject);
    procedure btDesativarTaraClick(Sender: TObject);
    procedure btDesligarDisplayClick(Sender: TObject);
    procedure btLigarDisplayClick(Sender: TObject);
    procedure btnConectarClick(Sender: TObject);
    procedure btnDesconectarClick(Sender: TObject);
    procedure btnLerPesoClick(Sender: TObject);
    procedure btEnviarPrecoKgClick(Sender: TObject);
    procedure btSearchPortsClick(Sender: TObject);
    procedure btZerarDispositivoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure edtTimeOutKeyPress(Sender: TObject; var Key: Char);
    procedure chbMonitorarClick(Sender: TObject);
    procedure ACBrBAL1LePeso(Peso: Double; Resposta: String);
    procedure FormCreate(Sender : TObject) ;
    procedure SbArqLogClick(Sender: TObject);
  private
    { private declarations }
    function Converte(cmd: String): String;

    procedure InicializarBalanca(Ativar: Boolean);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  typinfo, ACBrDeviceSerial, ACBrBALSelfCheckout,
  ACBrUtil.Base,
  ACBrUtil.FilesIO;

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

procedure TForm1.InicializarBalanca(Ativar: Boolean);
begin
  ACBrBAL1.Desativar;

  if Ativar then
  begin
    // configura porta de comunicação
    ACBrBAL1.Modelo           := TACBrBALModelo( cmbBalanca.ItemIndex );
    ACBrBAL1.Device.HandShake := TACBrHandShake( cmbHandShaking.ItemIndex );
    ACBrBAL1.Device.Parity    := TACBrSerialParity( cmbParity.ItemIndex );
    ACBrBAL1.Device.Stop      := TACBrSerialStop( cmbStopBits.ItemIndex );
    ACBrBAL1.Device.Data      := StrToInt( cmbDataBits.text );
    ACBrBAL1.Device.Baud      := StrToInt( cmbBaudRate.Text );
    ACBrBAL1.Device.Porta     := cmbPortaSerial.Text;
    ACBrBAL1.ArqLOG           := edLog.Text;

    // Conecta com a balança
    ACBrBAL1.Ativar;
  end;

  btnLerPeso.Enabled := Ativar;
  edPrecoKg.Enabled := Ativar;
  btEnviarPrecoKg.Enabled := Ativar;
  pnConfig.Enabled := (not Ativar);

  btnConectar.Enabled    := (not Ativar);
  btnConectar.Visible    := (not Ativar);
  btnDesconectar.Enabled := Ativar;
  btnDesconectar.Visible := Ativar;

  pnOpcoesAdicionais.Visible := Ativar and (ACBrBAL1.BAL is TACBrBALSelfCheckout);
end;

procedure TForm1.btnConectarClick(Sender: TObject);
begin
  InicializarBalanca(True);
end;

procedure TForm1.btDesativarTaraClick(Sender: TObject);
begin
  if (ACBrBAL1.BAL is TACBrBALSelfCheckout) then
    TACBrBALSelfCheckout(ACBrBAL1.BAL).DesativarTara;
end;

procedure TForm1.btDesligarDisplayClick(Sender: TObject);
begin
  if (ACBrBAL1.BAL is TACBrBALSelfCheckout) then
    TACBrBALSelfCheckout(ACBrBAL1.BAL).DesligarDisplay;
end;

procedure TForm1.btLigarDisplayClick(Sender: TObject);
begin
  if (ACBrBAL1.BAL is TACBrBALSelfCheckout) then
    TACBrBALSelfCheckout(ACBrBAL1.BAL).LigarDisplay;
end;

procedure TForm1.btAtivarTaraClick(Sender: TObject);
begin
  if (ACBrBAL1.BAL is TACBrBALSelfCheckout) then
    TACBrBALSelfCheckout(ACBrBAL1.BAL).AtivarTara;
end;

procedure TForm1.btnDesconectarClick(Sender: TObject);
begin
  InicializarBalanca(False);
end;

procedure TForm1.btnLerPesoClick(Sender: TObject);
Var TimeOut : Integer ;
begin
   try
      TimeOut := StrToInt( edtTimeOut.Text ) ;
   except
      TimeOut := 2000 ;
   end ;

   ACBrBAL1.LePeso( TimeOut );
end;

procedure TForm1.btEnviarPrecoKgClick(Sender: TObject);
var
  wTimeOut: Integer;
begin
  try
    wTimeOut := StrToInt(edtTimeOut.Text);
  except
    wTimeOut := 2000;
  end;

  if ACBrBAL1.EnviarPrecoKg(StringToFloatDef(edPrecoKg.Text, 0), wTimeOut) then
    Memo1.Lines.Add('Preço/Kg enviado com sucesso')
  else
    Memo1.Lines.Add('Erro ao enviar Preço/Kg');
end;

procedure TForm1.btSearchPortsClick(Sender: TObject);
begin
  cmbPortaSerial.Items.Clear;
  ACBrBAL1.Device.AcharPortasSeriais( cmbPortaSerial.Items );
  {$IfDef MSWINDOWS}
   ACBrBAL1.Device.AcharPortasUSB( cmbPortaSerial.Items );
  {$EndIf}
  {$IfDef HAS_BLUETOOTH}
   ACBrBAL1.Device.AcharPortasBlueTooth( cmbPortaSerial.Items, True );
  {$EndIf}

  cmbPortaSerial.Items.Add('LPT1') ;
  cmbPortaSerial.Items.Add('TCP:192.168.0.31:9100') ;

  {$IfNDef MSWINDOWS}
   cmbPortaSerial.Items.Add('/dev/ttyS0') ;
   cmbPortaSerial.Items.Add('/dev/ttyUSB0') ;
  {$EndIf}
end;

procedure TForm1.btZerarDispositivoClick(Sender: TObject);
begin
  if (ACBrBAL1.BAL is TACBrBALSelfCheckout) then
    TACBrBALSelfCheckout(ACBrBAL1.BAL).ZerarDispositivo;
end;

procedure TForm1.edtTimeOutKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9',#13,#8]) then
     Key := #0 ;
end;

procedure TForm1.chbMonitorarClick(Sender: TObject);
begin
   ACBrBAL1.MonitorarBalanca := chbMonitorar.Checked ;
end;

procedure TForm1.ACBrBAL1LePeso(Peso: Double; Resposta: String);
var
  valid: Integer;
begin
   sttPeso.Caption     := formatFloat('##0.000', Peso );
   sttResposta.Caption := Converte( Resposta ) ;

   if Peso > 0 then
      Memo1.Lines.Text := 'Leitura OK !'
   else
    begin
      valid := Trunc(ACBrBAL1.UltimoPesoLido);
      case valid of
         0 : Memo1.Lines.Text := 'TimeOut !'+sLineBreak+
                                 'Coloque o produto sobre a Balança!' ;
        -1 : Memo1.Lines.Text := 'Peso Instavel ! ' +sLineBreak+
                                 'Tente Nova Leitura' ;
        -2 : Memo1.Lines.Text := 'Peso Negativo !' ;
       -10 : Memo1.Lines.Text := 'Sobrepeso !' ;
      end;
    end ;
end;

procedure TForm1.FormCreate(Sender : TObject) ;
var
  I : TACBrBALModelo ;
begin
  cmbBalanca.Items.Clear ;
  For I := Low(TACBrBALModelo) to High(TACBrBALModelo) do
     cmbBalanca.Items.Add( GetEnumName(TypeInfo(TACBrBALModelo), integer(I) ) ) ;
  cmbBalanca.ItemIndex := 0;
end;

procedure TForm1.SbArqLogClick(Sender: TObject);
begin
  OpenURL( ExtractFilePath( Application.ExeName ) + edLog.Text);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ACBrBAL1.Desativar ;
end;

end.

