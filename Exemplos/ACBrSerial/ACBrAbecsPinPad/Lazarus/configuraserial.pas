{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit configuraserial;

{$mode objfpc}{$H+}

interface

uses
  ACBrDevice, ACBrDeviceSerial,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Spin;

type

  { TfrConfiguraSerial }

  TfrConfiguraSerial = class(TForm)
    gbSendBytes: TGroupBox;
    lEsperaBuffer: TLabel;
    lBuffer: TLabel;
    Label5: TLabel;
    cmbBaudRate: TComboBox;
    Label6: TLabel;
    cmbDataBits: TComboBox;
    Label7: TLabel;
    cmbParity: TComboBox;
    Label11: TLabel;
    cmbStopBits: TComboBox;
    Label8: TLabel;
    cmbHandShaking: TComboBox;
    Label4: TLabel;
    cmbPortaSerial: TComboBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    chHardFlow: TCheckBox;
    chSoftFlow: TCheckBox;
    seSendBytesCount: TSpinEdit;
    seSendBytesInterval: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure cmbPortaSerialChange(Sender: TObject);
    procedure cmbBaudRateChange(Sender: TObject);
    procedure cmbDataBitsChange(Sender: TObject);
    procedure cmbParityChange(Sender: TObject);
    procedure cmbStopBitsChange(Sender: TObject);
    procedure cmbHandShakingChange(Sender: TObject);
    procedure chHardFlowClick(Sender: TObject);
    procedure chSoftFlowClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seSendBytesCountChange(Sender: TObject);
    procedure seSendBytesIntervalChange(Sender: TObject);
  private
    { Private declarations }
    procedure VerificaFlow ;
  public
    { Public declarations }
    Device : TACBrDevice ;
  end;

implementation

{$R *.lfm}

{ TfrConfiguraSerial }

procedure TfrConfiguraSerial.FormCreate(Sender: TObject);
begin
  Device := TACBrDevice.Create(self);
end;

procedure TfrConfiguraSerial.FormDestroy(Sender: TObject);
begin
  Device.Free ;
end;

procedure TfrConfiguraSerial.FormShow(Sender: TObject);
begin
  cmbBaudRate.ItemIndex     := cmbBaudRate.Items.IndexOf(IntToStr( Device.Baud )) ;
  cmbDataBits.ItemIndex     := cmbDataBits.Items.IndexOf(IntToStr( Device.Data )) ;
  cmbParity.ItemIndex       := Integer( Device.Parity ) ;
  cmbStopBits.ItemIndex     := Integer( Device.Stop ) ;
  chHardFlow.Checked        := Device.HardFlow ;
  chSoftFlow.Checked        := Device.SoftFlow ;
  cmbHandShaking.ItemIndex  := Integer( Device.HandShake ) ;
  seSendBytesCount.Value    := Device.SendBytesCount;
  seSendBytesInterval.Value := Device.SendBytesInterval;
end;

procedure TfrConfiguraSerial.seSendBytesCountChange(Sender: TObject);
begin
  Device.SendBytesCount := seSendBytesCount.Value;
end;

procedure TfrConfiguraSerial.seSendBytesIntervalChange(Sender: TObject);
begin
  Device.SendBytesInterval := seSendBytesInterval.Value;
end;

procedure TfrConfiguraSerial.cmbPortaSerialChange(Sender: TObject);
begin
  Device.Porta := cmbPortaSerial.Text ;
end;

procedure TfrConfiguraSerial.cmbBaudRateChange(Sender: TObject);
begin
  Device.Baud := StrToInt(cmbBaudRate.Text) ;
end;

procedure TfrConfiguraSerial.cmbDataBitsChange(Sender: TObject);
begin
  Device.Data := StrToInt(cmbDataBits.Text) ;
end;

procedure TfrConfiguraSerial.cmbParityChange(Sender: TObject);
begin
  Device.Parity := TACBrSerialParity( cmbParity.ItemIndex ) ;
end;

procedure TfrConfiguraSerial.cmbStopBitsChange(Sender: TObject);
begin
  Device.Stop := TACBrSerialStop( cmbStopBits.ItemIndex ) ;
end;

procedure TfrConfiguraSerial.cmbHandShakingChange(Sender: TObject);
begin
  Device.HandShake := TACBrHandShake( cmbHandShaking.ItemIndex ) ;
  VerificaFlow ;
end;

procedure TfrConfiguraSerial.chHardFlowClick(Sender: TObject);
begin
  Device.HardFlow := chHardFlow.Checked ;
  VerificaFlow ;
end;

procedure TfrConfiguraSerial.chSoftFlowClick(Sender: TObject);
begin
  Device.SoftFlow := chSoftFlow.Checked ;
  VerificaFlow ;
end;

procedure TfrConfiguraSerial.VerificaFlow;
begin
  cmbHandShaking.ItemIndex := Integer( Device.HandShake ) ;
  chHardFlow.Checked := Device.HardFlow ;
  chSoftFlow.Checked := Device.SoftFlow ;
end;

end.

