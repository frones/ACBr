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

unit Frm_ConfiguraSerial;

interface

//** Converted with Mida 600     http://www.midaconverter.com - PROJETO.ACBR

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics, FMX.Controls.Presentation,
  ACBrDevice, ACBrDeviceSerial, FMX.ComboEdit;

type
  TfrmConfiguraSerial = class(TForm)
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
    cmbPortaSerial: TComboEdit;
    BitBtn1: TButton;
    BitBtn2: TButton;
    chHardFlow: TCheckBox;
    chSoftFlow: TCheckBox;
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
  private
    procedure VerificaFlow ;
  public
    Device : TACBrDevice ;
  end;

var
  frmConfiguraSerial: TfrmConfiguraSerial;

implementation

{$R *.FMX}

{ TfrConfiguraSerial }

procedure TfrmConfiguraSerial.FormCreate(Sender: TObject);
begin
  Device := TACBrDevice.Create(self);
end;

procedure TfrmConfiguraSerial.FormDestroy(Sender: TObject);
begin
  Device.Free ;
end;

procedure TfrmConfiguraSerial.FormShow(Sender: TObject);
begin
  cmbBaudRate.ItemIndex    := cmbBaudRate.Items.IndexOf(IntToStr( Device.Baud )) ;
  cmbDataBits.ItemIndex    := cmbDataBits.Items.IndexOf(IntToStr( Device.Data )) ;
  cmbParity.ItemIndex      := Integer( Device.Parity ) ;
  cmbStopBits.ItemIndex    := Integer( Device.Stop ) ;
  chHardFlow.IsChecked        := Device.HardFlow ;
  chSoftFlow.IsChecked        := Device.SoftFlow ;
  cmbHandShaking.ItemIndex := Integer( Device.HandShake ) ;
end;

procedure TfrmConfiguraSerial.cmbPortaSerialChange(Sender: TObject);
begin
  Device.Porta := cmbPortaSerial.Text ;
end;

procedure TfrmConfiguraSerial.cmbBaudRateChange(Sender: TObject);
begin
  if cmbBaudRate.ItemIndex >= 0 then
    Device.Baud := StrToInt(cmbBaudRate.Selected.Text) ;
end;

procedure TfrmConfiguraSerial.cmbDataBitsChange(Sender: TObject);
begin
  if cmbDataBits.ItemIndex >= 0 then
    Device.Data := StrToInt(cmbDataBits.Selected.Text) ;
end;

procedure TfrmConfiguraSerial.cmbParityChange(Sender: TObject);
begin
  if cmbParity.ItemIndex >= 0 then
    Device.Parity := TACBrSerialParity( cmbParity.ItemIndex ) ;
end;

procedure TfrmConfiguraSerial.cmbStopBitsChange(Sender: TObject);
begin
  if cmbStopBits.ItemIndex >= 0 then
    Device.Stop := TACBrSerialStop( cmbStopBits.ItemIndex ) ;
end;

procedure TfrmConfiguraSerial.cmbHandShakingChange(Sender: TObject);
begin
  if cmbHandShaking.ItemIndex >= 0 then
  begin
    Device.HandShake := TACBrHandShake( cmbHandShaking.ItemIndex ) ;
    VerificaFlow ;
  end;
end;

procedure TfrmConfiguraSerial.chHardFlowClick(Sender: TObject);
begin
  Device.HardFlow := chHardFlow.IsChecked  ;
  VerificaFlow ;
end;

procedure TfrmConfiguraSerial.chSoftFlowClick(Sender: TObject);
begin
  Device.SoftFlow := chSoftFlow.IsChecked  ;
  VerificaFlow ;
end;

procedure TfrmConfiguraSerial.VerificaFlow;
begin
  cmbHandShaking.ItemIndex := Integer( Device.HandShake ) ;
  chHardFlow.IsChecked  := Device.HardFlow ;
  chSoftFlow.IsChecked  := Device.SoftFlow ;
end;

end.

